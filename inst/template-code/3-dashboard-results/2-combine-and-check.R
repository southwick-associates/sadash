# combine results by state (for tableau input) & check

library(tidyverse)
library(salic)
library(sadash)

# stack results
coltyp <- cols(
    .default = col_character(), quarter = col_integer(), year = col_integer(), 
    value = col_double() 
)
dat <- list.files("3-dashboard-results/dash", full.names = TRUE) %>%
    lapply(read_csv, col_types = coltyp) %>%
    bind_rows()

# Check ---------------------------------------------------------------

# get csv files by quarter
x <- split(dat, dat$quarter)
outdir <- "3-dashboard-results/dash-combine"
dir.create(outdir, showWarnings = FALSE)
for (i in names(x)) {
    write_csv(x[[i]], file.path(outdir, paste0("qtr", i, ".csv")))
}

# visualize
dashtemplate::run_visual(outdir)

# check row counts
# - first year won't have churn
# - first 5 years won't have recruitment
dat %>%
    filter(segment != "month") %>%
    count(group, year) %>%
    spread(year, n) %>%
    View()

# Write for Tableau -------------------------------------------------------

# After writing to csv, these 2 tables should be manually save as Excel in
#  O365 > Data Dashboards > [state] > data

# direct input to Tableau
dat %>% mutate(
    segment = tolower(segment),
    metric = case_when(
        metric == "recruits" ~ "participants - recruited",
        metric == "rate" ~ "participation rate",
        TRUE ~ metric
    )
) %>% write_csv("out/dash-out.csv", na = "")

# summary for Tableau designer
# - counts by year for each group (permission)
dat %>%
    filter(segment == "All", metric == "participants") %>% 
    select(quarter, group, year, value) %>% 
    spread(year, value) %>%
    write.csv(file = "out/priv-counts.csv")
