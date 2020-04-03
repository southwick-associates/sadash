# run dashboard results for each permission

library(tidyverse)
library(salic)
library(sadash)

source("code/params.R")
source("code/3-dashboard-results/functions.R") # run_dash()

# define additional parameters
all_quarters <- quarter  # quarters to be estimated
month_yrs <- c(dashboard_yrs[1]-1, dashboard_yrs) # for sales by month

# pull customer, sales, & population data for state
counties <- load_counties(db_census, state)
cust <- load_cust(db_license) %>% left_join(counties)
sale <- load_sale(db_license, month_yrs)
pop_county <- load_pop(db_census, state) %>% prep_pop(yrs) %>% left_join(counties)

# Run by Permission -------------------------------------------------------

sink("3-dashboard-results/log.txt") # log errors/warnings

hunt <- run_dash("hunt", return_ref = TRUE)
fish <- run_dash("fish", return_ref = TRUE)
all_sports <- run_dash("all_sports", return_ref = TRUE)
run_dash("firearm_deer", hunt) # privilege example
# etc.

sink()

# Notes on logged errors/warnings
# - you can investigate errors by opening log.txt & searching for "Error"
# - there can be alot of threshold warnings (usually harmless)
# - errors should stop only the current permission-quarter run 
#   (i.e., code will continue to run for remaining permissions/quarters)
