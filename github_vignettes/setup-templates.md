
# Setup Templates

Use `new_dashboard()` or `update_dashboard()` to setup code for a given state and time period:

``` r
# initialize new template
sadash::new_dashboard("YY", "2018-q4")

# alternatively, update code from a previous period
sadash::update_dashboard("YY", "2019-q2", "2018-q4")

# if a data dive tool is needed
sadash::setup_data_dive("YY", "2018-q4")
```
