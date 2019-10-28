# pull data into CSV files for input to Tableau data dive

library(tidyverse)
library(sadash)

source("params.R")

if (quarter != 4) lastyr <- lastyr - 1 # we don't want partial years
yrs <- firstyr:lastyr
samp_pct <- 10 # customer sample size to pull (in whole percentage points)

# Pull License Histories ----------------------------------------------------

# we only need a sample of customers
cust <- load_cust(db_license)
cust_samp <- db_history %>%
    load_cust_samp(yrs, samp_pct) %>%
    left_join(cust, by = "cust_id") %>%
    set_other_county_na(state)
    
# pull history data into one data frame
permissions <- load_sqlite(db_history, function(con) DBI::dbListTables(con))
hist_samp <- lapply(permissions, function(x) {
    load_history(db_history, x, yrs) %>% 
        inner_join(cust_samp, by = "cust_id") %>%
        set_nonres_county_na() %>%
        salic::recode_agecat() %>%
        mutate(priv = x) %>%
        select(priv, cust_id, year, lapse, R3, res, sex, fips = county_fips, age)
}) %>% bind_rows()

# Check & Visualize ---------------------------------------------------------

# pull all history data for comparison
hist <- lapply(permissions, function(x) {
    load_history(db_history, x, yrs) %>% mutate(priv = x)
}) %>% bind_rows()

# compare - the full vs. samp values should be nearly identical
# (i.e., the sample should be representative of the total)
cnt <- bind_rows(
    count(hist, priv, year) %>% mutate(grp = "full"),
    count(hist_samp, priv, year) %>% 
        mutate(grp = "samp", n = n / (samp_pct / 100))
)
ggplot(cnt, aes(year, n, fill = grp)) +
    geom_col(position = position_dodge()) +
    facet_wrap(~ priv, scales = "free_y")

# visualize data dive
county_map <- get_county_map_dive(state, drop_state_code = FALSE)
x <- salic::label_categories(hist_samp) %>% salic::df_factor_age()
run_visual_dive(x, county_map, pct = samp_pct)

# Write to CSV -------------------------------------------------------

dir_out <- file.path(dir_production, state, "data-dive")
dir.create(dir_out, showWarnings = FALSE)

out_file <- paste0("dive-", lastyr, "-", samp_pct, "pct", ".csv")
write_csv(hist_samp, file.path(dir_out, out_file))
glimpse(hist_samp)
