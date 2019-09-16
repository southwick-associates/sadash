# functions to load license data from sqlite

# helper function to open connection, pull data, close connection
# use 1 documentation group for all the load_[data] functions
# - db
# - query
# - state
# - yrs
load_sqlite <- function(db, query) {
    con <- dbConnect(RSQLite::SQLite(), db)
    x <- query(con)
    dbDisconnect(con)
    x
}

# State-specific ----------------------------------------------------------

# load customer data
load_cust <- function(db) {
    load_sqlite(db, function(con) {
        tbl(con, "cust") %>% 
            select(cust_id, sex, birth_year, county_fips) %>% 
            collect()
    })
}

# load sales for data for selected years
load_sale <- function(db, yrs) {
    load_sqlite(db, function(con) {
        tbl(con, "sale") %>% 
            filter(year %in% yrs) %>%
            select(cust_id, lic_id, year, month) %>% 
            collect()
    })
}

# load county fips-names for attaching to cust & pop_county
load_counties <- function(db, state) {
    load_sqlite(db, function(con) {
        tbl(con, "county_fips") %>% 
            filter(state_abbrev == state) %>%
            select(county_fips, county = county_name) %>%
            collect()
    })
}

# load population for selected state (by county-age-sex)
load_pop <- function(db, state) {
    load_sqlite(db, function(con) {
        tbl(con, "pop_acs") %>%
            select(-state) %>% # needed for the next line to run correctly
            filter(state_abbrev == state) %>%
            collect()
    })
}

# Permission-specific  ----------------------------------------------------

# get license IDs for selected permission
load_lic_ids <- function(db, group) {
    load_sqlite(db, function(con) {
        tbl(con, "permission") %>%
            filter(permission == group) %>%
            collect() %>%
            pull(lic_id)
    })
}

# load license history for permission & join customers
# - group: permission group name
load_history <- function(db, group) {
    load_sqlite(db, function(con) {
        tbl(con, group) %>% collect()
    })
}

