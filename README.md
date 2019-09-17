
# sadash

An R package for Southwick internal use: functions and templates to run individual state dashboard workflows.

## Installation

``` r
install.packages(
    "E:/SA/Projects/R-Software/Southwick-packages/sadash_0.1.zip", 
    repos = NULL, type = "win.binary"
)
```

## Usage

### Setup templates

Use new_dashboard() to setup code for a given state and time period:

``` r
sadash::new_dashboard("YY", "yyyy-qn")
```

### License History

### Dashboard Metrics

``` r
library(tidyverse)
library(DBI)
library(salic)
library(sadash)

# set parameters
state <- "MO"
db_license <- "E:/SA/Data-production/Data-Dashboards/MO/license.sqlite3"
db_history <- "E:/SA/Data-production/Data-Dashboards/MO/history.sqlite3"
db_census <- "E:/SA/Data-production/Data-Dashboards/_Shared/census.sqlite3"
yrs <- 2007:2019
quarter <- 2
dashboard_yrs <- 2018:2019 # focus years to be available in dashboard dropdown menu

# load state data
counties <- load_counties(db_census, state)
cust <- load_cust(db_license) %>% left_join(counties)
sale <- load_sale(db_license, yrs)
pop_county <- load_pop(db_census, state) %>% prep_pop(yrs) %>% left_join(counties)

# run permission summary for hunting full-year (quarter 4)
select_quarter <- 4
lic_ids <- load_lic_ids(db_license, "hunt")
sale_group <- filter(sale, lic_id %in% lic_ids) %>% # for monthly sales
    distinct(cust_id, year, month)
history <- db_history %>%
    load_history("hunt") %>%
    left_join(cust, by = "cust_id") %>%
    prep_history()
metrics <- history %>%
    quarterly_filter(quarter, select_quarter, yrs) %>%
    quarterly_lapse(select_quarter, yrs)
    calc_metrics(pop_county, sale_group, dashboard_yrs)
dashboard <- metrics %>%
    format_metrics(select_quarter, "hunt")

# do some checking
count(dashboard, metric, segment) %>% spread(metric, n)
filter(dashboard, metric == "participants", segment == "All")
```
