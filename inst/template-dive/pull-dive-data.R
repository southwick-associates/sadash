# pull data into CSV files for input to Tableau data dive

library(tidyverse)
library(sadash)

source("params.R")
samp_pct <- 10

### Temporary
library(tidyverse)
devtools::load_all()

state <- "IA"
period <- "2018-q4"
firstyr <- 2006 # first year to include in dashboard results

dir_production <- "E:/SA/Data-production/Data-Dashboards"
db_license <- file.path(dir_production, state, "license.sqlite3")
db_history <- file.path(dir_production, state, "history.sqlite3")
db_census <- file.path(dir_production, "_Shared", "census.sqlite3")

# for building license histories & dashboard summaries
lastyr <- as.integer(substr(period, 1, 4))
quarter <- as.integer(substr(period, 7, 7))
yrs <- firstyr:lastyr
dashboard_yrs <- lastyr # focus years to be available in dashboard dropdown menu

# Pull License Histories ----------------------------------------------------
# we only need a 10% sample of customers

# pull data into a list (one data frame for each permission)
cust_samp <- load_cust_sample(db_history, yrs, samp_pct)
permissions <- load_sqlite(db_history, function(con) DBI::dbListTables(con))
hist_samp <- lapply(permissions, function(x) {
    load_history(db_history, x, yrs) %>% inner_join(cust_samp, by = "cust_id") 
})

# Check Summaries ---------------------------------------------------------
# check using dashboard visual - sample & total should roughly align

county_map <- get_county_map(state) # for joining geometry (map) data
county_census <- load_counties(db_census, state) # for joining on county_fips
dash_list <- join_county_map(dat, county_map, county_census)

# sample
dash_samp <- lapply(permissions, function(x) {
    dashtemplate::calc_metrics(hist_samp[[x]]) %>%
        dashtemplate::format_metrics("full-year", x) %>%
        mutate(value = ifelse(metric == "churn", value, value * samp_pct))
}) %>% 
    bind_rows() %>%
    join_county_map(county_map, county_census)
run_visual(dash_samp)
run_visual_county(dash_samp)

# total 
coltyp <- cols(
    .default = col_character(), quarter = col_integer(), year = col_integer(), 
    value = col_double() 
)
dash_all <- list.files("3-dashboard-results/dash", full.names = TRUE) %>%
    lapply(read_csv, col_types = coltyp) %>%
    bind_rows() %>%
    join_county_map(county_map, county_census)
run_visual(dash_all)
run_visual_county(dash_all)

# Formatting & Save -------------------------------------------------------

# prepare
# - age categories, etc. (probably the same as dashboard production)
# - dealing with counties & nonresidents
# - missing data for demographics?

# scale 10% to totals (unless Nick did this on his end)
