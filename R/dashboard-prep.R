# functions to prepare data for summarization

# Population data ------------------------------------------------------------

#' Prepare population data for dashboard summaries - to be called after load_pop()
#'
#' This is partly a wrapper for aggregate_pop() & extrapolate_pop()
#' 
#' @param pop data frame: input population data for state
#' @param yrs numeric: yaers to include in summary
#' @family functions to prepare data for summarization
#' @export
prep_pop <- function(pop, yrs) {
    pop %>%
        filter(year %in% yrs) %>%
        aggregate_pop() %>% # collapse to 7 age categories
        label_categories() %>% # convert numeric categories to factor
        df_factor_age() %>%
        rename(agecat = age) %>%
        extrapolate_pop(yrs) # filling in missing population data (if needed)
}

#' Convenience function: aggregate population data to 7 age categories
#' 
#' Collapses the larger number of age categories provided by acs
#' 
#' @inheritParams prep_pop
#' @family functions to prepare data for summarization
#' @export
aggregate_pop <- function(pop) {
    pop %>%
        group_by(county_fips, year, sex, age) %>% # collapse to 7 age categories
        summarise(pop = sum(pop)) %>%
        ungroup()
}

#' Extrapolate population forward for years without estimates
#' 
#' @inheritParams prep_pop
#' @family functions to prepare data for summarization
#' @export
extrapolate_pop <- function(pop, yrs) {
    yrs_to_extrapolate <- yrs[yrs > max(pop$year)]
    
    if (length(yrs_to_extrapolate) == 0) {
        return(pop) # no extrapolation needed
    } 
    if (length(yrs_to_extrapolate) > 1) {
        warning(
            "Extrapolating population estimates ", length(yrs_to_extrapolate),
            " yrs forward.\n",
            "- Newer census estimates may be available (see '_Shared' code).",
            call. = FALSE
        )
    }
    # estimate statewide % change per year
    # it's a simplistic method, but probably fine our purposes
    growth_rate <- group_by(pop, year) %>%
        summarise(pop = sum(pop)) %>%
        mutate(change = pop / lag(pop)) %>% 
        summarise(mean(change, na.rm = TRUE)) %>%
        pull()
    
    # extrapolate forward
    extrapolate_yr <- function(yr) {
        yrs_forward <- yr - max(pop$year)
        filter(pop, year == max(year)) %>% 
            mutate(year = yr, pop = pop * growth_rate^yrs_forward)
    }
    lapply(yrs_to_extrapolate, extrapolate_yr) %>% 
        bind_rows(pop)
}

# Permission data ---------------------------------------------------------

#' Prepare license history for selected quarter (recoding)
#' 
#' @param history data frame: table produced by load_history()
#' @param month_to_quarter function used to calculate quarter (based on month values)
#' @family functions to prepare data for summarization
#' @export
recode_history <- function(
    history, 
    month_to_quarter = function(x) case_when(x <= 3 ~ 1, x <= 6 ~ 2, x <= 9 ~ 3, TRUE ~ 4)
) {
    history %>%
        label_categories() %>%
        recode_agecat() %>%
        select(-age_year, -age, -birth_year) %>%
        mutate(quarter = month_to_quarter(month)) # quarter of sale date
}

#' Filter by quarter in prepartion for calculating metrics
#' 
#' @param history data frame that holds license history for selected permission
#' @param quarter current quarter
#' @param select_quarter quarter to be summarized
#' @param yrs years to summarize
#' @family functions to prepare data for summarization
#' @export
quarterly_filter <- function(history, quarter, select_quarter, yrs) {
    # if selected quarter is ahead of current, need to drop current year
    if (select_quarter > quarter) {
        history <- filter(history, year < max(yrs))
    }
    # only want records occurring up to selected quarter
    if (select_quarter != 4) {
        history <- filter(history, quarter <= select_quarter)
    }
    select(history, -quarter)
}

#' Calculate lapse for a quarterly subset (where quarter != 4)
#' 
#' @inheritParams quarterly_filter
#' @family functions to prepare data for summarization
#' @export
quarterly_lapse <- function(history, select_quarter, yrs) {
    if (select_quarter == 4) {
        return(history)
    }
    # prepare table to run lapse
    yrs <- prep_yrs(yrs, history, "quarterly_lapse")
    x <- history %>%
        select(cust_id, year, duration_run) %>%
        data.table() %>%
        split(history$year)
    
    # make new lapse variable
    for (i in 2:length(yrs)) {
        # make_lapse() wasn't written for this in mind, but it works
        salic::make_lapse(x[[i-1]], x[[i]])
    }
    x <- bind_rows(x) %>% 
        select(cust_id, year, lapse)
    
    # attach lapse
    history %>%
        select(-lapse, -duration_run) %>%
        left_join(x, by = c("cust_id", "year"))
}
