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
    # one leading year is needed for monthly comparisons
    month_yrs <- c(dashboard_yrs[1]-1, dashboard_yrs)
    missing_yrs <- setdiff(month_yrs, unique(sale$year))
    if (length(missing_yrs) > 0) {
        stop("Incomplete years in sale table for monthly breakouts:\n", 
             "- missing yrs: ", paste(missing_yrs, collapse = ", "), 
             call. = FALSE)
    }
    
    if (use_recruits) {
        history <- filter(history, R3 == "Recruit")
        sale <- semi_join(sale, history, by = c("cust_id", "year"))
        metric = "recruits"
    } else {
        metric = "participants"
    }
    out <- sale %>%
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
    # error handling for dashboard_yrs
    missing_yrs <- setdiff(dashboard_yrs, unique(history$year))
    dashboard_yrs <- setdiff(dashboard_yrs, missing_yrs)
    if (length(dashboard_yrs) == 0) {
        stop("No dashboard_yrs available in the history table:\n", 
             "- missing yrs: ", paste(missing_yrs, collapse = ", "), 
             call. = FALSE)
    }
    # residency-specific logic
    if (!is.null(res_type)) {
        history <- filter(history, res == res_type)
        if (res_type == "Nonresident") segs <- setdiff(segs, "county")
    }
    # calculating metrics
    part <- calc_part(
        history, segs, tests, scaleup_test, res_type
    )
    recruits <- calc_part(
        history, segs, tests_recruits, scaleup_test, res_type, use_recruits = TRUE
    )
    rate <- calc_rate(
        part$residents, pop_county, 
        part$participants, part_ref$participants, res_type, rate_test
    )
    churn <- sapply2(segs, function(x) est_churn(history, x, tests[x]))
    month <- list(
        "participants" = est_month(sale, history, dashboard_yrs),
        "recruits" = est_month(sale, history, dashboard_yrs, use_recruits = TRUE)
    )
    # collecting metrics into a list
    participants <- part$participants
    recruits <- recruits$participants
    out <- mget(c("participants", "recruits", "churn", "month"))
    if (is.list(rate)) out[["rate"]] <- rate
    
    # dropping non-dashboard_yrs for county-level results
    for (i in setdiff(names(out), "month")) {
        if (!"county" %in% segs) return(out)
        out[[i]]$county <- filter(out[[i]]$county, year %in% dashboard_yrs)
    }
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
    # participants by segment
    # - county-level is treated separately
    part <- setdiff(segs, "county") %>%
        sapply2(function(x) est_part(history, x, tests[x]))
    part <- lapply(part, function(x) scaleup_part(x, part$tot, scaleup_test))
    
    # resident participants
    # - for county-level & intermediate result for part. rates
    if (!is.null(res_type)) {
        # of course this isn't done for nonresident permissions
        if (res_type == "Nonresident") return(list("participants" = part))
    }
    history_res <- filter(history, res == "Resident")
    scale_res <- filter(part[["res"]], res == "Resident") # for scaleup_part()
    
    res <- setdiff(segs, "res") %>%
        sapply2(function(x) est_part(history_res, x, tests[x]))
    res <- lapply(res, function(x) scaleup_part(x, scale_res, scaleup_test))
    part[["county"]] <- res[["county"]]
    list("participants" = part, "residents" = res)
}

#' Calculate participation rate (or privilege rate if part_ref is not NULL)
#'
#' @inheritParams calc_metrics
#' @param part_res (for part. rate) named list that holds participant summary
#' @param pop_county (for part. rate) data frame that holds population data
#' @param part (for priv. rate) named list that holds participant summary
#' @family dashboard functions
#' @export
calc_rate <- function(
    part_res, pop_county, part, part_ref = NULL, res_type = NULL, rate_test = 1
) {
    # convenience function
    mapply_rate <- function(part, pop) {
        mapply(est_rate, part, pop, SIMPLIFY = FALSE, 
               MoreArgs = list(test_threshold = rate_test))
    }
    if (!is.null(part_ref)) {
        # calculate a privilege rate
        pop <- lapply(part_ref, function(x) rename(x, pop = participants))
        mapply_rate(part, pop)
    } else {
        # calculate a participation rate
        # - not for nonresident-specific permissions though
        if (!is.null(res_type) && res_type == "Nonresident") {
            return(invisible)
        }
        pop <- sapply2(names(part_res), function(x) est_pop(pop_county, x))
        out <- mapply_rate(part_res, pop)
        
        # residency-flagged rates are also included for Tableau (a bit of a hack)
        # - all nonresident values will show zero
        out[["res"]] <- bind_rows(
            mutate(out$tot, res = "Resident") %>% select(res, year, rate),
            mutate(out$tot, res = "Nonresident", rate = 0) %>% select(res, year, rate)
        )
        out
    }
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
    dir.create(outdir, showWarnings = FALSE)
    write.csv(dash, file = file.path(outdir, paste0(group, "-", quarter, ".csv")),
              row.names = FALSE)
}
