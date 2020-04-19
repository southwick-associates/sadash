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

#' Convert age_acs from character to factor
#' 
#' This will ensure correct ordering in plots, etc. Uses \code{\link{age_map}}
#' sample data to identify factor levels.
#' 
#' @inheritParams prep_pop
#' @inheritParams salic::factor_var
#' @family functions to prepare data for summarization
#' @export
#' @examples 
#' \dontrun{
#' db_census <- "E:/SA/Data-production/Data-Dashboards/_Shared/census.sqlite3"
#' pop <- load_pop(db_census, "NC")
#' pop <- factor_age_acs(pop, suppress_check = FALSE)
#' }
factor_age_acs <- function(pop, suppress_check = TRUE) {
    utils::data("age_map", envir = environment())
    levs <- unique(age_map$age_acs)
    
    new <- factor(pop$age_acs, levels = levs)
    if (!suppress_check) {
        dplyr::bind_cols(new = new, old = pop$age_acs) %>% 
            dplyr::count(.data$new, .data$old) %>% 
            print(n = Inf)
    }
    pop$age_acs <- new
    pop
}

# Permission data ---------------------------------------------------------

#' Prepare license history for selected quarter (recoding)
#' 
#' This is basically a wrapper for several functions called in sequence, particularly
#' \code{\link[salic]{label_categories}} and \code{\link[salic]{recode_agecat}}.
#' The month_to_quarter parameter is used to specify an output "quarter" variable
#' based on the input "month" variable.
#' 
#' @param history data frame: table produced by load_history()
#' @param month_to_quarter function used to calculate quarter (based on month values).
#' The default function assumes a calendar year, so it will need to be modified
#' in cases where fiscal year is used (see VA 2019-q2 code for an example).
#' 
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
#' There are 2 basic filter scenarios: (1) the select_quarter is less than 4
#' (i.e., partial year) and records for later quarters are removed, or (2)
#' the current quarter (quarter argument) is behind select_quarter, in which
#' case the most recent year is dropped.
#' 
#' @param df data frame that holds data (e.g., sales or license history) for 
#' selected permission
#' @param quarter current quarter
#' @param select_quarter quarter to be summarized
#' @param yrs years to summarize
#' @family functions to prepare data for summarization
#' @export
quarterly_filter <- function(df, quarter, select_quarter, yrs) {
    # if selected quarter is ahead of current, need to drop current year
    if (select_quarter > quarter) {
        df <- filter(df, year < max(yrs))
    }
    # only want records occurring up to selected quarter
    if (select_quarter != 4) {
        df <- filter(df, quarter <= select_quarter)
    }
    select(df, -quarter)
}

#' Calculate lapse for a quarterly subset (where quarter != 4)
#' 
#' @inheritParams quarterly_filter
#' @param history data frame that holds history table for selected permission
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
