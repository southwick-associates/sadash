# state/period-specific parameters to be called in production code

state <- __state__
period <- __period__
firstyr <- 2008 # first year to include in dashboard results

dir_production <- "E:/SA/Data-production/Data-Dashboards"
db_license <- file.path(dir_production, state, "license.sqlite3")
db_history <- file.path(dir_production, state, "history.sqlite3")
db_census <- file.path(dir_production, "_Shared", "census.sqlite3")

# for building license histories & dashboard summaries
lastyr <- as.integer(substr(period, 1, 4))
quarter <- as.integer(substr(period, 7, 7))
yrs <- firstyr:lastyr
