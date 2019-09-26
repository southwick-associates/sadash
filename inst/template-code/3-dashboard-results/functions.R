# function(s) to be called from 1-run-dash.R

# Summarize one permission for all_quarters, outputting to csv
# 
# This function encapsulates the workflow for producing dashboard metrics for 
# a specified permission. It is included in template code (rather than in sadash)
# because state-specific tweaking of the workflow may be necessary.
# 
# - group: name of permission
# - part_ref: reference permission data for use in privilege rates
#   use NULL for participation rates (e.g., for overall permissions like "hunt")
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
    history <- load_history(db_history, group, yrs) %>%
        left_join(cust, by = "cust_id") %>%
        recode_history()
    
    # function to produce metrics for one quarter
    # - wraps run_qtr_handler() for error/warning handling on provided code
    run_qtr <- function(qtr, group) {
        run_qtr_handler(code_to_run = {
            history %>%
                quarterly_filter(quarter, qtr, yrs) %>%
                quarterly_lapse(qtr, yrs) %>%
                calc_metrics(pop_county, sale_group, dashboard_yrs, 
                             part_ref[[paste0("q", qtr)]], res_type)
        }, qtr, group)
    }
    
    # produce metrics for all quarters
    out <- lapply(all_quarters, function(x) run_qtr(x, group))
    names(out) <- paste0("q", all_quarters)
    
    # wrap up
    if (write_csv) mapply(write_output, out, all_quarters, group)
    if (return_ref) out
}

# To run metrics with error/warning handling: only to be called from run_qtr()
#
# This provides a couple useful features:
# 1. stops the current quarter run (on error) but continue running any remaining quarters
# 2. logs errors & warnings with headers showing current permission-quarter. 
#    These can be saved to a file with sink() to facilitate automation
run_qtr_handler <- function(code_to_run, qtr, group) {
    # using tryCatch() allows remaining quarters to be run if error is caught
    tryCatch(
        # use withCallingHandlers() to log every warning
        withCallingHandlers(
            code_to_run,
            warning = function(w) { print(w); cat("\n") },
            finally = cat("\nRun for", group, "quarter", qtr, "--------------------\n\n")
        ),
        error = function(e) { 
            message("Caught an error: ", group, " quarter ", qtr)
            print(e); cat("\n") 
        }
    )
}

# Write output metrics for a selected permission-quarter
write_output <- function(metrics, qtr, group) {
    if (length(metrics) == 0) {
        return(invisible())
    }
    metrics %>%
        format_metrics(qtr, group) %>%
        write_dash(qtr, group)
}
