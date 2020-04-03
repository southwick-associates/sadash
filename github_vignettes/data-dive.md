
# Building Data Dive

Fairly straightforward: use `setup_data_dive()` to get template code. You can make use of `hist_samp` sample data to see an example data dive:

``` r
library(sadash)
data(hist_samp)
county_map <- get_county_map_dive("WI")

run_visual_dive(hist_samp, county_map, pct = 1)
```