
# sadash

An R package for Southwick internal use: functions and templates to run individual state dashboard workflows. This extends [package salic](https://southwick-associates.github.io/salic/) to cover the more complex use-case of state-level dashboards (county-level summaries, privilege/subtype permissions, etc.).

## Installation

These should be installed on the server in the 'data-dashboards2' project library. Otherwise, install the necessary packages from R:

``` r
install.packages("remotes")
remotes::install_github("southwick-associates/sadash")
```

## Usage

Sadash serves two primary purposes: (1) providing boilerplate (template) code for state processing, and (2) providing a modular workflow through data processing functions. Run `?sadash` for an overview of the included functionality.

### Setup Templates

Use `new_dashboard()` or `update_dashboard()` to setup code for a given state and time period:

``` r
# initialize new template
sadash::new_dashboard("YY", "2018-q4")

# alternatively, update code from a previous period
sadash::update_dashboard("YY", "2019-q2", "2018-q4")

# if a data dive tool is needed
sadash::setup_data_dive("YY", "2018-q4")
```

### Preparing License Data

No data preparation templates are included in sadash currently. Chelsea's templates are available under state "XX" on the server.

### Summarizing for Dashboards & Data Dive

The dashboard summary workflow essentially mirrors the [national/regional template](https://github.com/southwick-associates/dashboard-template). It is more complex for a number of reasons:

- Summarizing by County
- Participation Rates
- Additional overhead for working with sqlite databases
- Allowing for privileges & subtypes (with calculation of privilege rates)
- Additional steps needed for subtypes specifically (e.g., spousal license) since they don't represent unique permissions
- Accounting for permissions that are residency-specific

#### License History

Producing license histories is fairly straightfoward (see the template code in 2-license-history). An example workflow for one permission is included below.

``` r
# MO hunting license history
library(tidyverse)
library(salic)
library(sadash)

# set parameters
db_license <- "E:/SA/Data-production/Data-Dashboards/MO/license.sqlite3"
db_history <- "E:/SA/Data-production/Data-Dashboards/MO/history.sqlite3"
yrs <- 2009:2018

# pull license data into a list
all <- load_license(db_license, yrs)
data_check_sa(all$cust, all$lic, all$sale)

# run hunting license history
lic_group <- all$lic %>%
        filter(type %in% c("hunt", "combo", "trap"))
    
history <- lic_group  %>%
    select(lic_id, duration) %>%
    inner_join(all$sale, by = "lic_id") %>%
    drop_na_custid() %>%
    rank_sale(rank_var = c("duration", "res"), first_month = TRUE) %>%
    make_history(yrs, carry_vars = c("month", "res"))
```

#### Dashboard Metrics

Producing dashboard metrics is a bit more involved than running license history (see template code in 3-dashboard-results). An example workflow for one permission-quarter is included below.

``` r
# MO full-year hunting dashboard
library(tidyverse)
library(salic)
library(sadash)

# set parameters
state <- "MO"
db_license <- "E:/SA/Data-production/Data-Dashboards/MO/license.sqlite3"
db_history <- "E:/SA/Data-production/Data-Dashboards/MO/history.sqlite3"
db_census <- "E:/SA/Data-production/Data-Dashboards/_Shared/census.sqlite3"
yrs <- 2009:2018
quarter <- 4 # current quarter
dashboard_yrs <- 2018 # focus years to be available in dashboard dropdown menu

# load state data
counties <- load_counties(db_census, state)
cust <- load_cust(db_license) %>% left_join(counties)
sale <- load_sale(db_license, 2017:2018) # only 2 yrs needed for monthly breakouts
pop_county <- load_pop(db_census, state) %>% prep_pop(yrs) %>% left_join(counties)

# run permission summary for hunting full-year (quarter 4)
qtr <- 4 # quarter to summarize
group <- "hunt"

lic_ids <- load_lic_ids(db_license, group)
sale_group <- filter(sale, lic_id %in% lic_ids) %>% 
    distinct(cust_id, year, month)
history <- db_history %>%
    load_history(group, yrs) %>%
    left_join(cust, by = "cust_id") %>%
    recode_history()
metrics <- history %>%
    quarterly_filter(quarter, qtr, yrs) %>%
    quarterly_lapse(qtr, yrs) %>%
    calc_metrics(pop_county, sale_group, dashboard_yrs)
dashboard <- metrics %>%
    format_metrics(qtr, group)

# visualize
write_csv(dashboard, file.path(tempdir(), "dash.csv"))
dashtemplate::run_visual(tempdir())
```

### Building Data Dive

Fairly straightforward: use `setup_data_dive()` to get template code. You can make use of `hist_samp` sample data to see an example data dive:

``` r
library(sadash)
data(hist_samp)
county_map <- get_county_map_dive("WI")

run_visual_dive(hist_samp, county_map, pct = 1)
```