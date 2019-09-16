# functions to calculate & format dashboard metrics

# Estimate Metrics ------------------------------------------------
# extends salic::est_part(), salic::est_churn(), etc.

# estimate population for given segment
est_pop <- function(pop_acs, segment) {
    if (segment == "tot") segment <- NULL
    pop_acs %>%
        group_by_at(unique(c(segment, "year"))) %>%
        summarise(pop = sum(pop)) %>%
        ungroup()
}

# estimate rate based on participant and population counts
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

# estimate monthly sales
est_month <- function(
    sale, history, dashboard_yrs, use_recruits = FALSE
) {
    # one leading year is needed for monthly comparisons
    month_yrs <- c(dashboard_yrs[1]-1, dashboard_yrs)
    sale <- filter(sale, year %in% month_yrs)
    
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

# convenience function for use in calc_metrics()
sapply2 <- function(x, ...) {
    sapply(x, simplify = FALSE, ...)
}

# calculate all 5 metrics for a permission
# - res_type: for residency specific permissions ("Resident", "Nonresident", NULL)
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
        history, dashboard_yrs, segs, tests, scaleup_test, res_type
    )
    recruits <- calc_part(
        history, dashboard_yrs, segs, tests_recruits, scaleup_test,  
        res_type, use_recruits = TRUE
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

# calculate participants: return list of length 2 (participants, residents)
calc_part <- function(
    history, dashboard_yrs, segs, tests, scaleup_test, 
    res_type = NULL, use_recruits = FALSE
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

# calculate participation rate or privilege rate
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

# slightly modified version of format_result() from salic (just 1 line)
format_result <- function(
    df, timeframe, group, rename_input = c("category", "year", "value")
) {
    # expecting exactly 3 columns in the input data frame
    out <- df
    names(out) <- rename_input
    
    # stored as input variable names >> placed in output variable values
    segment <- colnames(df)[1]
    metric <- colnames(df)[3]
    
    # adding variables to represent structure in a single table
    out$segment <- segment
    out$metric <- metric
    out$timeframe <- timeframe
    out$group <- group
    out$category <- as.character(out$category)
    
    # modify segment names
    out <- out %>% dplyr::mutate(
        segment = dplyr::case_when(
            segment == "tot" ~ "All",
            segment == "res" ~ "Residency",
            segment == "sex" ~ "Gender",
            segment == "agecat" ~ "Age",
            segment == "county" ~ "County",
            TRUE ~ segment ### not included in salic 2.0.0
        )
    )
    out %>% dplyr::select(
        .data$timeframe, .data$group, .data$segment, .data$year,  
        .data$category, .data$metric, .data$value
    )
}

# format metrics (list) into a single table output (data frame)
# - metrics: list produced by calc_metrics()
# - timeframe: time period covered ("full-year" or "mid-year")
# - group: name of permission group ("fish", "hunt", "all_sports")
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

# convenience function: output a csv file for given permission-quarter results
write_dash <- function(
    dash, quarter, group, outdir = "3-dashboard-results/dash"
) {
    dir.create(outdir, showWarnings = FALSE)
    write_csv(dash, file.path(outdir, paste0(group, "-", quarter, ".csv")))
}
