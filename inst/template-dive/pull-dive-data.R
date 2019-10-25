# pull data into CSV files for input to Tableau data dive

library(tidyverse)
library(sadash)

source("params.R")
samp_pct <- 10 # sample size (fraction) to pull in whole percentage points

### Temporary
library(tidyverse)
devtools::load_all()

samp_pct <- 10
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
cust_samp <- left_join(
    load_cust_samp(db_history, yrs, samp_pct),
    load_cust(db_license)
)
permissions <- load_sqlite(db_history, function(con) DBI::dbListTables(con))
hist_samp <- lapply(permissions, function(x) {
    load_history(db_history, x, yrs) %>% 
        inner_join(cust_samp, by = "cust_id") %>%
        set_nonres_county_na() %>%
        salic::recode_agecat() %>%
        mutate(priv = x) %>%
        select(priv, cust_id, year, lapse, R3, res, sex, fips = county_fips, age)
}) %>% bind_rows()

# Check Summaries ---------------------------------------------------------
# check using dashboard visual - sample & total should roughly align

# pull all history data for comparison
hist <- lapply(permissions, function(x) {
    load_history(db_history, x, yrs) %>% mutate(priv = x)
}) %>% bind_rows()

# compare
# - the full vs. samp bars shoul be nearly identical
cnt <- bind_rows(
    count(hist, priv, year) %>% mutate(grp = "full"),
    count(hist_samp, priv, year) %>% 
        mutate(grp = "samp", n = n / (samp_pct / 100))
)
ggplot(cnt, aes(year, n, fill = grp)) +
    geom_col(position = position_dodge()) +
    facet_wrap(~ priv, scales = "free_y")

# TODO: run_visual_dive()?
# - to use as a rough exploration/validation step instead of dashboard summaries
# - would probably be worth testing on WI 2015

# pull map data
county_map <- get_county_map(state) # for joining geometry (map) data
county_census <- load_counties(db_census, state) # for joining on county_fips


# Formatting & Save -------------------------------------------------------

# probably no additional formatting needed
# maybe include county-level pop data though
