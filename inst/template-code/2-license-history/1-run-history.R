# run license history for each permission
# note that code may need to be tweaked for state-specific needs

library(tidyverse)
library(DBI)
library(salic)
library(sadash)

source("params.R")

# pull license data into a list
all <- load_license(db_license, yrs)
data_check(all$cust, all$lic, all$sale)

# run license history by permission (group)
# - group: name of group to output to db_history
# - lic_filter: lic table filter condition for selecting permission sales
# - group_parent: name of parent group (for subtype permissions, otherwise NULL)
# - rank_var: passed to rank_sale()
# - carry_vars: passed to make_history()
run_group <- function(
    group, lic_filter, group_parent = NULL, 
    rank_var = c("duration", "res"), carry_vars = c("month", "res")
) {
    # lapse should only be computed for full years
    yrs_lapse <- if (quarter == 4) yrs else yrs[-length(yrs)]
    
    lic_group <- all$lic %>%
        filter_(lic_filter) %>%
        select(lic_id, duration)
    
    history <- lic_group  %>%
        inner_join(all$sale, by = "lic_id") %>%
        drop_na_custid() %>%
        rank_sale(rank_var, first_month = TRUE) %>%
        make_history(yrs, carry_vars, yrs_lapse)
    
    if (!is.null(group_parent)) {
        # subtypes: use parent group for R3 & lapse
        history_parent <- load_history(db_history, group_parent) %>%
            select(cust_id, year, lapse, R3)
        history <- select(history, -R3, -lapse) %>%
            left_join(history_parent, by = "cust_id", "year")
    }
    write_history(history, group, lic_group, db_history, db_license)
}

run_group("hunt", 'type %in% c("hunt", "combo", "trap")')
run_group("fish", 'type %in% c("fish", "combo")')
run_group("all_sports", 'type %in% c("hunt", "trap", "fish", "combo")')
# etc.
