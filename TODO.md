
## Version 1.1

It may be that functions will need to be tweaked, improved, etc. on the backend. However, it is difficult to do that reliably without tests (and probably sample data). Therefore, any changes should begin with tests to ensure that functionality is not broken, or a change to a function leads to an error in calculation.

- Consider limiting the county results to dashboard_yrs by making sadash-specific versions of est_part, etc. (would look something like the code below)
    + first make at least one sample dataset (dashboard) and at least one test (output of calc_metrics() matches sample data) to ensure that the primary functionality doesn't get broken with code changes (instead of hoping for the best or laboriously comparing to existing state code)
    + would also want to drop the filter at the end of calc_metrics(), which is nice because the code is then cleaner & fewer threshold warnings would be produced (a benefit on the user end)
    
``` r
# Estimate participants (a wrapper for salic::est_part)
# - dashboard_yrs  years for dashboard focus
est_part <- function(
    history, segment = "tot", test_threshold = 20, show_test_stat = FALSE,
    suppress_warning = FALSE, outvar = "participants", dashboard_yrs
) {
    if (segment == "county") {
        history <- filter(history, year %in% dashboard_yrs)
    }
    salic::est_part(history, segment, test_threshold, show_test_stat, 
                    suppress_warning, outvar)
}
```

- write tests (maybe also incorporating sample data)
- maybe improve function documentation (e.g., with examples)
- maybe make a fill_segs() function to ensure tableau gets res/nonres, etc.
    + filling with zeroes for any missing categories (in res, sex, agecat)
    + probably won't happen for sex, agecat, but might be worth generalizing
- maybe investigate scaleup warnings with VA resident permissions (in counties)
    + rounding error? > might be worth a closer look
    + at least none of the discrepancies appear enormous
    