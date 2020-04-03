# state/period-specific parameters to be called in production code

state <- "__state__"
period <- "__period__"

# for data processing
dir_production <- "E:/SA/Data-production/Data-Dashboards"
dir_sensitive <- "E:/SA/Data-sensitive/Data-Dashboards"
dir_raw <- file.path(dir_sensitive, state, paste0("raw-", period))

db_raw <- file.path(dir_sensitive, state, paste0("raw-", period, ".sqlite3"))
db_standard <- file.path(dir_sensitive, state, "standard.sqlite3")
db_production <- file.path(dir_production, state, "license.sqlite3")

# for building license histories & dashboard summaries
db_license <- db_production
db_history <- file.path(dir_production, state, "history.sqlite3")
db_census <- file.path(dir_production, "_Shared", "census.sqlite3")

firstyr <- 2008 # first year to include in dashboard results
lastyr <- as.integer(substr(period, 1, 4))
quarter <- as.integer(substr(period, 7, 7))
yrs <- firstyr:lastyr
dashboard_yrs <- lastyr # focus years to be available in dashboard dropdown menu
