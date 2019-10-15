# functions to calculate & format dashboard metrics

# Estimate Metrics ------------------------------------------------
# extends salic::est_part(), salic::est_churn(), etc.

#' Summarize population by year for a given segment
#'
#' @param pop_acs data frame: population dataset 
#' @param segment character: name of variable that holds grouping segment
#' @family dashboard functions
#' @export
est_pop <- function(pop_acs, segment) {
    if (segment == "tot") segment <- NULL
    pop_acs %>%
        group_by_at(unique(c(segment, "year"))) %>%
        summarise(pop = sum(pop)) %>%
        ungroup()
}

#' Estimate rate based on participant and population counts
#'
#' @param part_estimate data frame: table that holds participant counts
#' @param pop_estimate data frame: table that holds population counts
#' @param test_threshold  numeric: threshold for output rate (e.g., 0.5 = 50
#' percent). A warning will be printed for every row that exceeds the threshold
#' @family dashboard functions
#' @export
est_rate <- function(
    part_estimate, pop_estimate, test_threshold = 0.5
) {
    joincols <- intersect(names(part_estimate), names(pop_estimate))
    
    out <- part_estimate %>%
        left_join(pop_estimate, by = joincols) %>%
        mutate(rate = participants / pop)
    salic::check_threshold(out, test_threshold, test_variable = "rate")
    out %>%
        select(-pop, -participants)
}

#' Estimate monthly sales
#'
#' @param sale data frame: table to use for summarizing sales by month
#' @param history data frame: history table used for filter if use_recruits = TRUE
#' @param dashboard_yrs numeric: years to be included in summary
#' @param use_recruits logical: if TRUE, only include records where 
#' history$R3 == "Recruit"
#' @family dashboard functions
#' @export
est_month <- function(
    sale, history, dashboard_yrs, use_recruits = FALSE
) {
    # error handling: one leading year is needed for monthly comparisons
    month_yrs <- c(dashboard_yrs[1]-1, dashboard_yrs)
    missing_yrs <- setdiff(month_yrs, unique(sale$year))
    if (length(missing_yrs) > 0) {
        stop("Incomplete years in sale table for monthly breakouts:\n", 
             "- missing yrs: ", paste(missing_yrs, collapse = ", "), 
             call. = FALSE)
    }
    if (use_recruits) {
        history <- filter(history, R3 == "Recruit")
        metric = "recruits"
    } else {
        metric = "participants"
    }
    out <- semi_join(sale, history, by = c("cust_id", "year")) %>%
        count(year, month) %>%
        mutate(month = as.character(month)) %>%
        select(month, year, n)
    names(out) <- c("month", "year", metric)
    out
}

# Calculate Metrics -----------------------------------------------------------------

#' Convenience function for use in calc_metrics() that wraps sapply
#'
#' @param x expression for sapply
#' @param ... other arguments passed to sapply
#' @keywords internal
#' @export
sapply2 <- function(x, ...) {
    sapply(x, simplify = FALSE, ...)
}

#' Calculate all 5 metrics for a permission
#' 
#' @inheritParams est_month
#' @param history data frame: input history table for permission
#' @param pop_county data frame: population table for state
#' @param sale data frame: sales table for permission
#' @param part_ref list: summary of parent permission to be used as reference 
#' for privilege rate. If NULL, calculates participation rate using pop_county
#' @param res_type identifies residency specific permissions ("Resident", 
#' "Nonresident", NULL)
#' @param segs character: vector of segment names to summarize
#' @param tests test values to pass to participants & churn calculation
#' @param tests_recruits test values to pass to recruits calculation
#' @param scaleup_test test values to pass to scaleup_part()
#' @param rate_test test values to pass to est_rate()
#' @family dashboard functions
#' @export
calc_metrics <- function(
    history, pop_county, sale, dashboard_yrs, part_ref = NULL, res_type = NULL,
    segs = c("tot", "res", "sex", "agecat", "county"),
    tests = c(tot = 20, res = 35, sex = 35, agecat = 35, county = 70), 
    tests_recruits = c(tot = 30, res = 45, sex = 45, agecat = 45, county = 150), 
    scaleup_test = 25, 
    rate_test = 1
) {
    # handle errors for dashboard_yrs
    missing_yrs <- setdiff(dashboard_yrs, unique(history$year))
    dashboard_yrs <- setdiff(dashboard_yrs, missing_yrs)
    if (length(dashboard_yrs) == 0) {
        stop("No dashboard_yrs available in the history table:", 
             "\n- years in history: ", paste(unique(history$year), collapse = ", "),
             "\n- missing yrs: ", paste(missing_yrs, collapse = ", "), 
             "\nPerhaps this permission is missing sales in selected quarter?",
             "\nAlternatively, quarterly_filter() can cause this if 'quarter'",
             " (i.e., current quarter) is set incorrectly.", call. = FALSE)
    }
    # prepare residency-specific logic
    if (!is.null(res_type)) {
        history <- filter(history, res == res_type)
        if (res_type == "Nonresident") segs <- setdiff(segs, "county")
    }
    
    # calculate 5 metrics
    part <- calc_part(history, segs, tests, scaleup_test, res_type)
    participants <- part$participants
    
    rate <- if (is.null(part_ref)) {
        calc_part_rate(part$residents, pop_county, rate_test, res_type)
    } else {
        calc_priv_rate(part$participants, part_ref$participants, rate_test)
    }
    month <- list("participants" = est_month(sale, history, dashboard_yrs))
    
    if ("R3" %in% names(history)) {
        recruits <- calc_part(
            history, segs, tests_recruits, scaleup_test, res_type, use_recruits = TRUE
        )
        month$recruits <- est_month(sale, history, dashboard_yrs, use_recruits = TRUE)
    }
    churn <- sapply2(segs, function(x) est_churn(history, x, tests[x]))
    
    # collect metrics into a list
    out <- mget(c("participants", "churn", "month"))
    if (exists("recruits")) out$recruits <- recruits$participants
    if (is.list(rate)) out$rate <- rate
    
    # drop non-dashboard_yrs for county-level results
    for (i in setdiff(names(out), "month")) {
        if (!"county" %in% segs) return(out)
        out[[i]]$county <- filter(out[[i]]$county, year %in% dashboard_yrs)
    }
    out
}

#' Calculate privilege rate
#' 
#' A privilege rate refers to the ratio between a given privilege (or subtype)
#' permission and a reference overall permission.
#' 
#' @inheritParams calc_metrics
#' @param part named list that holds participant summary for given selected
#' privilege/subtype permission
#' @param part_ref name list that holds reference participant summary
#' @family dashboard functions
#' @export
calc_priv_rate <- function(part, part_ref, rate_test) {
    pop <- part_ref %>%
        lapply(function(x) rename(x, pop = participants))
    names(part) %>%
        sapply2(function(x) est_rate(part[[x]], pop[[x]], rate_test))
}

#' Calculate participation rate
#' 
#' Refers to the ratio of permission resident participants to the state 
#' population.
#' 
#' @inheritParams calc_metrics
#' @param part named list holding resident participant summary for privilege
#' @family dashboard functions
#' @export
calc_part_rate <- function(part, pop_county, rate_test, res_type) {
    if (!is.null(res_type) && res_type == "Nonresident") {
        return(invisible)
    }
    pop <- names(part) %>%
        sapply2(function(x) est_pop(pop_county, x))
    out <- names(part) %>%
        sapply2(function(x) est_rate(part[[x]], pop[[x]], rate_test))
    
    # residency-flagged rates are also included for Tableau (a bit of a hack)
    # - all nonresident values will show zero
    out[["res"]] <- bind_rows(
        mutate(out$tot, res = "Resident") %>% select(res, year, rate),
        mutate(out$tot, res = "Nonresident", rate = 0) %>% select(res, year, rate)
    )
    out
}

#' Calculate participants: return list of length 2 (participants, residents)
#'
#' @inheritParams calc_metrics
#' @param use_recruits logical: if TRUE, filter the history table with 
#' history$R3 == "Recruit"
#' @family dashboard functions
#' @export
calc_part <- function(
    history, segs, tests, scaleup_test, res_type = NULL, use_recruits = FALSE
) {
    if (use_recruits) {
        history <- filter(history, R3 == "Recruit")
        est_part <- function(...) est_recruit(...)
        scaleup_part <- function(...) scaleup_recruit(...)
    }
    
    # summarize participants by segment
    # - county-level is treated separately
    part <- setdiff(segs, "county") %>%
        sapply2(function(x) est_part(history, x, tests[x]))
    part <- part %>%
        lapply(function(x) scaleup_part(x, part$tot, scaleup_test))
    
    # return participants for nonresident permissions
    if (!is.null(res_type) && res_type == "Nonresident") {
        return(list("participants" = part))
    }
    
    # summarize resident participants
    # - used for county-level & as an intermediate result for part. rates
    history_res <- filter(history, res == "Resident")
    scale_res <- filter(part[["res"]], res == "Resident") # for scaleup_part()
    
    res <- setdiff(segs, "res") %>%
        sapply2(function(x) est_part(history_res, x, tests[x]))
    res <- res %>%
        lapply(function(x) scaleup_part(x, scale_res, scaleup_test))
    
    # return participants & residents
    part[["county"]] <- res[["county"]]
    list("participants" = part, "residents" = res)
}

# Format Metrics ----------------------------------------------------------

#' Format metrics (list) into a single table output (data frame)
#'
#' @inheritParams salic::format_result
#' @param metrics named list that holds summary data produced by calc_metrics()
#' @family dashboard functions
#' @export
format_metrics <- function(
    metrics, timeframe, group = "hunt"
) {
    lapply_format <- function(metric) {
        lapply(metric, function(x) format_result(x, timeframe, group))
    }
    bind_rows(
        lapply_format(metrics$participants),
        if ("recruits" %in% names(metrics)) lapply_format(metrics$recruits),
        lapply_format(metrics$rate),
        if ("churn" %in% names(metrics)) lapply_format(metrics$churn),
        lapply_format(metrics$month)
    ) %>%
        rename(quarter = timeframe)
}

#' Convenience function: output a csv file for given permission-quarter results
#'
#' @inheritParams salic::format_result
#' @param dash data frame output of format_metrics()
#' @param quarter integer value of selected quarter
#' @param outdir file path for output directory
#' @family dashboard functions
#' @export
write_dash <- function(
    dash, quarter, group, outdir = "3-dashboard-results/dash"
) {
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    write.csv(dash, file = file.path(outdir, paste0(group, "-", quarter, ".csv")),
              row.names = FALSE)
}
