# run license history for each permission
# note that code may need to be tweaked for state-specific needs

library(tidyverse)
library(DBI)
library(salic)
library(sadash)

source("params.R")

# pull license data into a list
all <- load_all(db_license, yrs)
data_check(all$cust, all$lic, all$sale)

# run license history by permission (group)
run_group <- function(group, lic_filter, ref_name = NULL) {
    build_history(all$sale, all$lic, yrs, lic_filter, quarter) %>% 
        adjust_subtype(ref_name, db_history) %>%
        write_history(group, filter_(all$lic, lic_filter), db_history, db_license)
}

run_group("hunt", 'type %in% c("hunt", "combo", "trap")')
run_group("fish", 'type %in% c("fish", "combo")')
run_group("all_sports", 'type %in% c("hunt", "trap", "fish", "combo")')
# etc.

