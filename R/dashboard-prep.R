# functions to prepare dashboard data

# Population data ------------------------------------------------------------

# prepare population data for analysis - to be called after load_pop()
prep_pop <- function(pop_county, yrs) {
    pop_county %>%
        filter(year %in% yrs) %>%
        aggregate_pop() %>% # collapse to 7 age categories
        label_categories() %>% # convert numeric categories to factor
        df_factor_age() %>%
        rename(agecat = age) %>%
        extrapolate_pop(yrs) # filling in missing population data (if needed)
}

# convenience function: aggregate population data to 7 age categories
# - (i.e., collapse the larger number of census age categories)
aggregate_pop <- function(pop_county) {
    pop_county %>%
        group_by(county_fips, year, sex, age) %>% # collapse to 7 age categories
        summarise(pop = sum(pop)) %>%
        ungroup()
}

# extrapolate population forward for years in which estimates are not yet available
extrapolate_pop <- function(pop_acs, yrs) {
    yrs_to_extrapolate <- yrs[yrs > max(pop_acs$year)]
    
    if (length(yrs_to_extrapolate) == 0) {
        return(pop_acs) # no extrapolation needed
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
    growth_rate <- group_by(pop_acs, year) %>%
        summarise(pop = sum(pop)) %>%
        mutate(change = pop / lag(pop)) %>% 
        summarise(mean(change, na.rm = TRUE)) %>%
        pull()
    
    # extrapolate forward
    extrapolate_yr <- function(yr) {
        yrs_forward <- yr - max(pop_acs$year)
        filter(pop_acs, year == max(year)) %>% 
            mutate(year = yr, pop = pop * growth_rate^yrs_forward)
    }
    lapply(yrs_to_extrapolate, extrapolate_yr) %>% 
        bind_rows(pop_acs)
}

# Permission data ---------------------------------------------------------

# prepare license history for selected quarter (recoding)
# - history: table produced by load_history()
# - month_to_quarter: function used to calculate quarter (based on month values)
prep_history <- function(
    history, 
    month_to_quarter = function(x) case_when(x <= 3 ~ 1, x <= 6 ~ 2, x <= 9 ~ 3, TRUE ~ 4)
) {
    history %>%
        label_categories() %>%
        recode_agecat() %>%
        select(-age_year, -age, -birth_year) %>%
        mutate(quarter = month_to_quarter(month)) # quarter of sale date
}

# filter by quarter in prepartion for calculating metrics
# - quarter: current quarter (from params.R)
# - select_quarter: quarter to be summarized
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

# calculate lapse for a quarterly subset (where quarter != 4)
# - imports data.table
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
