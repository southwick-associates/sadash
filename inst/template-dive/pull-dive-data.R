# pull data into CSV files for input to Tableau data dive

library(tidyverse)
library(sadash)

source("params.R")

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

# Prepare license data ----------------------------------------------------
# we only need a 10% sample (of customers) for each permission

permissions <- load_sqlite(db_history, function(con) DBI::dbListTables(con))
hist <- lapply(permissions, samp_history_10pct) %>% bind_rows()
cust <- load_cust(db_license)
hist <- left_join(hist, cust, by = "cust_id")
load_history()
