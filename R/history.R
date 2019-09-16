# functions for producing license history

# load license data (cust, lic, sale) into a list
# - db_license: file path to license.sqlite3
# - yrs: years to include in license history
load_all <- function(db_license, yrs) {
    con <- dbConnect(RSQLite::SQLite(), db_license)
    lic <- tbl(con, "lic") %>% collect()
    sale <- tbl(con, "sale") %>%
        filter(year %in% yrs) %>%
        select(cust_id, lic_id, year, res, month) %>% 
        collect()
    cust <- tbl(con, "cust") %>%
        select(cust_id, sex, birth_year) %>%
        collect()
    dbDisconnect(con)
    list(cust = cust, lic = lic, sale = sale)
}

# build history table for given permission
# - sale, lic: input license data
# - lic_filter: query to be passed to filter_() on lic table (selects relevant lic_ids)
# - quarter: current quarter (if not quarter 4, the last year is excluded for lapse)
# - rank_var: passed to rank_sale()
# - carry_vars: passed to make_history()
build_history <- function(
    sale, lic, yrs, lic_filter, quarter, 
    rank_var = c("duration", "res"), carry_vars = c("month", "res")
) {
    # sale data often contains missing cust_ids for some reason
    drop_na_custid <- function(sale) {
        drop <- filter(sale, is.na(cust_id))
        if (nrow(drop) == 0) {
            sale
        } else {
            message(nrow(drop), " sale records missing a cust_id were dropped")
            filter(sale, !is.na(cust_id))
        }
    }
    # lapse should only be computed for full years
    yrs_lapse <- if (quarter == 4) yrs else yrs[-length(yrs)]
    
    lic %>% 
        filter_(lic_filter) %>%
        select(lic_id, duration) %>%
        inner_join(sale, by = "lic_id") %>%
        drop_na_custid() %>%
        rank_sale(rank_var, first_month = TRUE) %>%
        make_history(yrs, carry_vars, yrs_lapse)
}

# for subtype permissions: use reference permission for R3 & lapse
# - df_subtype: subtype license history table
# - ref_name: name of permission that provides R3 & lapse (for subtypes)
# - db_history: file path to history.sqlite3
adjust_subtype <- function(df_subtype, ref_name, db_history) {
    if (is.null(ref_name)) {
        return(df_subtype)
    }
    con <- dbConnect(RSQLite::SQLite(), db_history)
    df_ref <- tbl(con, ref_name) %>% 
        select(cust_id, year, lapse, R3) %>% 
        collect()
    dbDisconnect(con)
    
    df_subtype %>%
        select(-R3, -lapse) %>%
        left_join(df_ref, by = c("cust_id", "year"))
}

# write history table to sqlite
# - group: name of permission group to output
# - lic_slct: license table for types in permission
write_history <- function(
    df_history, group, lic_slct, db_history, db_license
) {
    out_nm <- stringr::str_replace_all(group, " ", "_") # ensure sqlite compatibility
    
    ## 1. Permission History Data
    if (!file.exists(db_history)) {
        src_sqlite(db_history, create = TRUE)
    }
    con <- dbConnect(RSQLite::SQLite(), db_history)
    if (out_nm %in% dbListTables(con)) dbRemoveTable(con, out_nm)
    dbWriteTable(con, out_nm, data.frame(df_history))
    dbDisconnect(con)
    
    ## 2. License Permission Table
    # The idea here is to have a separate table that explicitly identifies all the 
    #   license types that go into a specific permission. This allows a simple join to
    #   associate license types with permissions
    permission <- lic_slct %>%
        mutate(permission = out_nm) %>%
        select(permission, lic_id, description)
    
    con <- dbConnect(RSQLite::SQLite(), db_license)
    if (!("permission" %in% dbListTables(con))) {
        dbWriteTable(con, "permission", data.frame(permission))
    } else {
        # overwrite selected priv records to ensure only the newest is kept
        permission_old <- tbl(con, "permission") %>% 
            collect() %>%
            filter(permission != out_nm)
        permission <- bind_rows(permission, permission_old)
        dbWriteTable(con, "permission", data.frame(permission), overwrite = TRUE)
    }
    dbDisconnect(con)
}
