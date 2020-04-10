
# Data Dive

Building data dive summary data is fairly straightforward: use `lictemplate::setup_data_dive()` to get template code. 

### Getting Started

From the R console:

```r
lictemplate::setup_data_dive("YY", "2019-q4")
## A data dive folder has been initialized:
##  E:/SA/Projects/Data-Dashboards/YY/2019-q4/code/5-data-dive
```

### Example

You can make use of `hist_samp` sample data to see an example data dive:

``` r
library(sadash)
data(hist_samp)
county_map <- get_county_map_dive("WI")

run_visual_dive(hist_samp, county_map, pct = 1)
```
