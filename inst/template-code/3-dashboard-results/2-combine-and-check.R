# combine results by state (for tableau input) & check

library(tidyverse)
library(salic)
library(sadash)

# stack results
dat <- list.files("3-dashboard-results/dash", full.names = TRUE) %>%
    lapply(read_csv) %>%
    bind_rows()

# Visualize ---------------------------------------------------------------

# get csv files by quarter
x <- split(dat, dat$quarter)
outdir <- "3-dashboard-results/dash-combine"
for (i in names(x)) {
    write_csv(x[[i]], file.path(outdir, paste0("qtr", i, ".csv")))
}

# visualize
dashtemplate::run_visual(outdir)

# Checks ------------------------------------------------------------------

# check row counts
# - first year won't have churn
# - first 5 years won't have recruitment
dat %>%
    filter(segment != "month") %>%
    count(group, year) %>%
    spread(year, n)

# provide to Ben for summary
# - counts by year for each group (permission)
dat %>%
    filter(segment == "All", metric == "participants", quarter == 2) %>% 
    select(group, year, value) %>% 
    spread(year, value) %>%
    write.csv(file = "out/priv-counts.csv")

# Write for Tableau -------------------------------------------------------

dat %>% mutate(
    segment = tolower(segment),
    metric = case_when(
        metric == "recruits" ~ "participants - recruited",
        metric == "rate" ~ "participation rate",
        TRUE ~ metric
    )
) %>% write_csv("out/dash-out.csv", na = "")
