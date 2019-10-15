# combine results, check, and save

library(tidyverse)
library(salic)
library(sadash)

# Combine Results -----------------------------------------------------------

# stack into a single table
coltyp <- cols(
    .default = col_character(), quarter = col_integer(), year = col_integer(), 
    value = col_double() 
)
dat <- list.files("3-dashboard-results/dash", full.names = TRUE) %>%
    lapply(read_csv, col_types = coltyp) %>%
    bind_rows()

# save quarter-specific csv files for run_visual()
x <- split(dat, dat$quarter)
outdir <- "3-dashboard-results/dash-combine"
dir.create(outdir, showWarnings = FALSE)
for (i in names(x)) {
    write_csv(x[[i]], file.path(outdir, paste0("qtr", i, ".csv")))
}

# Check ---------------------------------------------------------------

# visualize - overall & by res/sex/age
dashtemplate::run_visual(outdir)

# check - row counts by group-year
# may vary by permission, but follows some predictable patterns:
# - first year won't have churn
# - first 5 years won't have recruitment
# - only dashboard_yrs will have county-level results
# - only dashboard_yrs (plus 1 preceeding year) will have monthly sales
dat %>%
    filter(segment != "month") %>%
    count(group, year) %>%
    spread(year, n)

# check - month counts by year
dat %>%
    filter(segment == "month") %>%
    count(quarter, group, year) %>%
    spread(year, n)

# check - county counts by year
dat %>%
    filter(segment == "County") %>%
    count(quarter, group, year) %>%
    spread(year, n)

# Write for Tableau -------------------------------------------------------

# After writing to csv, these 2 tables should be manually saved as Excel in
#  O365 > Data Dashboards > [state] > data

# direct input to Tableau
dat %>% mutate(
    segment = tolower(segment),
    metric = case_when(
        metric == "recruits" ~ "participants - recruited",
        metric == "rate" ~ "participation rate",
        TRUE ~ metric
    )
) %>% 
    write_csv("out/dash-out.csv", na = "")

# summary for Tableau designer
# - counts by year for each group (permission)
dat %>%
    filter(segment == "All", metric == "participants") %>% 
    select(quarter, group, year, value) %>% 
    spread(year, value) %>%
    write.csv(file = "out/priv-counts.csv")
