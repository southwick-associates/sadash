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
    con <- dbConnect(RSQLite::SQLite(), db)
    x <- query(con)
    dbDisconnect(con)
    x
}

# State-specific ----------------------------------------------------------

#' @describeIn load_sqlite Load customer data
load_cust <- function(db) {
    load_sqlite(db, function(con) {
        tbl(con, "cust") %>% 
            select(cust_id, sex, birth_year, county_fips) %>% 
            collect()
    })
}

#' @describeIn load_sqlite Load sales for selected years
load_sale <- function(db, yrs) {
    load_sqlite(db, function(con) {
        tbl(con, "sale") %>% 
            filter(year %in% yrs) %>%
            select(cust_id, lic_id, year, month) %>% 
            collect()
    })
}

#' @describeIn load_sqlite Load county fips-names for attaching to cust & pop_county
load_counties <- function(db, state) {
    load_sqlite(db, function(con) {
        tbl(con, "county_fips") %>% 
            filter(state_abbrev == state) %>%
            select(county_fips, county = county_name) %>%
            collect()
    })
}

#' @describeIn load_sqlite Load population for selected state (by county-age-sex)
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
load_lic_ids <- function(db, group) {
    load_sqlite(db, function(con) {
        tbl(con, "permission") %>%
            filter(permission == group) %>%
            collect() %>%
            pull(lic_id)
    })
}

#' @describeIn load_sqlite Load license history for permission & join customers
load_history <- function(db, group) {
    load_sqlite(db, function(con) {
        tbl(con, group) %>% collect()
    })
}
