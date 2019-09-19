
# sadash

An R package for Southwick internal use: functions and templates to run individual state dashboard workflows. This extends [package salic](https://southwick-associates.github.io/salic/) to cover the more complex use-case of state-level dashboards (county-level summaries, privilege/subtype permissions, etc.).

## Installation

Install the necessary packages using devtools. 

``` r
# salic
devtools::install_github("southwick-associates/salic")

# dashtemplate - need to use server since package docs aren't included in repo
# - sadash doesn't depend on dashtemplate, but it is used in template code for run_visual()
devtools::install("E:/SA/Projects/R-Software/Templates/dashboard-template")

# sadash
devtools::install_github("southwick-associates/sadash")
```

## Usage

Use new_dashboard() to setup code for a given state and time period:

``` r
# initialize template
sadash::new_dashboard("YY", "2019-q2")

# alternatively, update code from a previous period
# - this isn't yet implemented
# sadash::new_dashboard("YY", "2019-q2", reference_period = "2018-q4")
```

### Preparing License Data

No data preparation templates are included in sadash currently. Chelsea's templates are available under state "XX" on the server.

### Summarizing for Dashboards & Data Dive

The dashboard summary workflow essentially mirrors the [national/regional template](https://github.com/southwick-associates/dashboard-template). It is more complex for a number of reasons:

- Summarizing by County
- Participation Rates
- Additional overhead to save license history in sqlite databases
- Allowing for privileges & subtypes (with calculation of privilege rates)
- Additional steps needed for subtypes specifically (e.g., spousal license) since they don't represent unique permissions
- Accounting for permissions that are residency-specific

#### License History

This is fairly straightfoward, see the relevant template code in 2-license-history

#### Dashboard Metrics

Less straightforward (details in the template code). An example workflow is included below.

``` r
# MO full-year hunting dashboard
library(tidyverse)
library(DBI)
library(salic)
library(sadash)

# set parameters
state <- "MO"
db_license <- "E:/SA/Data-production/Data-Dashboards/MO/license.sqlite3"
db_history <- "E:/SA/Data-production/Data-Dashboards/MO/history.sqlite3"
db_census <- "E:/SA/Data-production/Data-Dashboards/_Shared/census.sqlite3"
yrs <- 2010:2018
quarter <- 4
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
    quarterly_lapse(select_quarter, yrs) %>%
    calc_metrics(pop_county, sale_group, dashboard_yrs)
dashboard <- metrics %>%
    format_metrics(select_quarter, "hunt")

# visualize
write_csv(dashboard, file.path(tempdir(), "hunt-q4.csv"))
dashtemplate::run_visual(tempdir())
```
