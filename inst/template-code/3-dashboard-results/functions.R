# function(s) for producing dashboard summary results
# - uses functions from salic & sadash

# summarize by permission for all_quarters, outputting to csv
# - group: name of permission
# - part_ref: reference permission data for use in privilege rates
#   + use NULL for participation rates (e.g., for overall permissions like "hunt")
# - return_ref: if TRUE, will also return a list (as reference for privilege rates)
# - res_type: for residency specific permissions ("Resident", "Nonresident", NULL)
# - write_csv: if TRUE, will write csv file(s) for permission-quarter(s)
run_dash <- function(
    group, part_ref = NULL, return_ref = FALSE, res_type = NULL, write_csv = TRUE
) {
    # get data for permission
    lic_ids <- load_lic_ids(db_license, group)
    sale_group <- filter(sale, lic_id %in% lic_ids) %>% 
        distinct(cust_id, year, month)
    history <- load_history(db_history, group) %>%
        left_join(cust, by = "cust_id") %>%
        prep_history()
    
    # function to run selected quarter
    run_quarter <- function(qtr) {
        metrics <- history %>%
            quarterly_filter(quarter, qtr, yrs) %>%
            quarterly_lapse(qtr, yrs) %>%
            calc_metrics(pop_county, sale_group, dashboard_yrs, 
                         part_ref[[paste0("q", qtr)]], res_type)
        if (write_csv) {
            metrics %>%
                format_metrics(qtr, group) %>%
                write_dash(qtr, group, "3-dashboard-results/dash")
        }
        metrics
    }
    # run over quarters with error handling (prints errors/warnings as code progresses)
    # - the threshold warnings are typically harmless, but good to check
    # - warnings/errors will also be printed in a blob at the end (less useful)
    out <- list()
    for (qtr in all_quarters) {
        tryCatch(
            withCallingHandlers(
                out[[paste0("q", qtr)]] <- run_quarter(qtr),
                warning = function(w) { print(w); cat("\n") },
                finally = cat("\nRun for", group, "quarter", qtr, "--------------------\n\n")
            ),
            error = function(e) { 
                message("Caught an error: ", group, " quarter ", qtr)
                print(e); cat("\n") 
            }
        )
    }
    if (return_ref) out
}

