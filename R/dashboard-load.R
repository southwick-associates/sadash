# functions to load license data from sqlite

#' Load sqlite data
#'
#' @param db file path to sqlite database
#' @param query DBI statement for sqlite connection 
#' @param yrs years to be included from sqlite table
#' @param state 2-character abbreviation of selected state
#' @param group name of selected permission
#' @export
load_sqlite <- function(db, query) {
    con <- DBI::dbConnect(RSQLite::SQLite(), db)
    x <- query(con)
    DBI::dbDisconnect(con)
    x
}

# State-specific ----------------------------------------------------------

#' @describeIn load_sqlite Load customer data
#' @export
load_cust <- function(db) {
    load_sqlite(db, function(con) {
        tbl(con, "cust") %>% 
            select(cust_id, sex, birth_year, county_fips) %>% 
            collect()
    })
}

#' @describeIn load_sqlite Load standard customer data
#' @export
load_cust_standard <- function(db) {
    load_sqlite(db, function(con) {
        tbl(con, "cust") %>% 
            select(cust_id, sex, dob, last, first, state, cust_res, cust_period, 
                   raw_cust_id) %>% 
            collect()
    })
}

#' @describeIn load_sqlite Load sales for selected years
#' @export
load_sale <- function(db, yrs) {
    load_sqlite(db, function(con) {
        tbl(con, "sale") %>% 
            filter(year %in% yrs) %>%
            select(cust_id, lic_id, year, month) %>% 
            collect()
    })
}

#' @describeIn load_sqlite Load standard sale data
#' @export
load_sale_standard <- function(db) {
    load_sqlite(db, function(con) {
        tbl(con, "sale") %>% 
            select(cust_id, lic_id, year, dot, start_date, end_date, 
                   sale_period, raw_sale_id) %>% 
            collect()
    })
}

#' @describeIn load_sqlite Load county fips-names for attaching to cust & pop_county
#' @export
load_counties <- function(
    db = "E:/SA/Data-production/Data-Dashboards/_Shared/census.sqlite3", state
) { 
    load_sqlite(db, function(con) {
        tbl(con, "county_fips") %>% 
            filter(state_abbrev == state) %>%
            select(county_fips, county = county_name) %>%
            collect()
    })
}

#' @describeIn load_sqlite Load population for selected state (by county-age-sex)
#' @export
load_pop <- function(db, state) {
    load_sqlite(db, function(con) {
        tbl(con, "pop_acs") %>%
            select(-state) %>% # needed for the next line to run correctly
            filter(state_abbrev == state) %>%
            collect()
    })
}

# Permission-specific  ----------------------------------------------------

#' @describeIn load_sqlite Load license IDs for selected permission
#' @export
load_lic_ids <- function(db, group) {
    load_sqlite(db, function(con) {
        tbl(con, "permission") %>%
            filter(permission == group) %>%
            collect() %>%
            pull(lic_id)
    })
}

#' @describeIn load_sqlite Load license history for permission & join customers
#' @export
load_history <- function(db, group, yrs) {
    load_sqlite(db, function(con) {
        tbl(con, group) %>% 
            filter(year %in% yrs) %>%
            collect()
    })
}
