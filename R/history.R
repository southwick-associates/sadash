# functions for producing license history

#' Check std. data formatting rules for Southwick dashboards
#'
#' This is a variation on \code{\link[salic]{data_check}} that has some 
#' differences in required variables and allowed values.
#' 
#' @inheritParams salic::data_check
#' @param cust_vars cust required_vars
#' @param lic_vals lic allowed_values
#' @param sale_vals sale allowed values
#' @param cust_vals sale allowed values
#' @family functions for producing license history
#' @export
data_check_sa <- function(
    cust, lic, sale,
    cust_vars = c("cust_id", "sex", "birth_year", "county_fips"),
    lic_vals = list(
        type = c("fish", "hunt", "combo", "trap", "other"),  
        duration = 1:99
    ),
    sale_vals = list(
        year = c(2000:substr(Sys.Date(), 1, 4)), 
        month = 0:15,  
        res = c(1, 0, NA)
    ),
    cust_vals = list(
        sex = c(1, 2, NA), 
        birth_year = c(1870:substr(Sys.Date(), 1, 4), NA)
    )
) {
    data_check_cust(cust, required_vars = cust_vars, allowed_values = cust_vals)
    data_foreign_key(sale, cust, "cust_id")
    data_check_lic(lic, allowed_values = lic_vals)
    data_foreign_key(sale, lic, "lic_id")
    data_check_sale(sale, allowed_values = sale_vals)
    
    # county_fips must be integer for joining with census data
    if ("county_fips" %in% names(cust)) {
        fips_type <- typeof(cust$county_fips)
        if (!fips_type == "integer") {
            warning("cust$county_fips must be integer, not ", fips_type,
                    call. = FALSE)
        }
    }
}

#' Load license data (cust, lic, sale) into a list
#' 
#' All columns for the lic table will be included. Columns to include for sale
#' and cust are specified by arguments. Note that the cust table likely isn't
#' needed for producing a license history, but is included for the call to
#' data_check.
#' 
#' @param db_license file path to license.sqlite3
#' @param yrs years to include in license history
#' @param sale_cols character vector of sale column names to include
#' @param cust_cols character vector of cust column names to include
#' @family functions for producing license history
#' @export
load_license <- function(
    db_license, yrs,
    sale_cols = c("cust_id", "lic_id", "year", "res", "month"),
    cust_cols = c("cust_id", "sex", "birth_year", "county_fips")
) {
    con <- DBI::dbConnect(RSQLite::SQLite(), db_license)
    lic <- tbl(con, "lic") %>% collect()
    sale <- tbl(con, "sale") %>%
        filter(year %in% yrs) %>%
        select(!!sale_cols) %>% 
        collect()
    cust <- tbl(con, "cust") %>%
        select(!!cust_cols) %>%
        collect()
    DBI::dbDisconnect(con)
    list(cust = cust, lic = lic, sale = sale)
}

#' Drop rows with missing cust_id from sale table
#' 
#' Not sure why the sales table would contain missing customer IDs, but there
#' are usually a handfull of NAs.
#'
#' @param sale data frame: sale table
#' @family functions for producing license history
#' @export
drop_na_custid <- function(sale) {
    drop <- filter(sale, is.na(cust_id))
    if (nrow(drop) == 0) {
        sale
    } else {
        message(nrow(drop), " sale records missing a cust_id were dropped")
        filter(sale, !is.na(cust_id))
    }
}

#' Write history table to sqlite
#'
#' @param history data frame: output history table
#' @param group name of permission group to output
#' @param lic_slct data frame: license table that identifies permission types
#' @param db_history sqlite history database
#' @param db_license sqlite license database
#' @family functions for producing license history
#' @export
write_history <- function(
    history, group, lic_slct, db_history, db_license
) {
    out_nm <- stringr::str_replace_all(group, " ", "_") # ensure sqlite compatibility
    
    ## 1. Permission History Data
    if (!file.exists(db_history)) {
        src_sqlite(db_history, create = TRUE)
    }
    con <- DBI::dbConnect(RSQLite::SQLite(), db_history)
    if (out_nm %in% DBI::dbListTables(con)) DBI::dbRemoveTable(con, out_nm)
    DBI::dbWriteTable(con, out_nm, data.frame(history))
    DBI::dbDisconnect(con)
    
    ## 2. License Permission Table
    # The idea here is to have a separate table that explicitly identifies all the 
    #   license types that go into a specific permission. This allows a simple join to
    #   associate license types with permissions
    permission <- lic_slct %>%
        mutate(permission = out_nm) %>%
        select(permission, lic_id, description)
    
    con <- DBI::dbConnect(RSQLite::SQLite(), db_license)
    if (!("permission" %in% DBI::dbListTables(con))) {
        DBI::dbWriteTable(con, "permission", data.frame(permission))
    } else {
        # overwrite selected priv records to ensure only the newest is kept
        permission_old <- tbl(con, "permission") %>% 
            collect() %>%
            filter(permission != out_nm)
        permission <- bind_rows(permission, permission_old)
        DBI::dbWriteTable(con, "permission", data.frame(permission), overwrite = TRUE)
    }
    DBI::dbDisconnect(con)
}
