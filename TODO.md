
## Version 0.2

- tweak dashboard calc_metrics() arguments >> group_parent
    + to match history workflow
- data_check_sa() to account for different month allowed values, etc.
    + although maybe do this in 0.3
- generalize with fill_segs() explained below? Only if it can be done quickly.
- any tests worth implementing now?
    + can compare to some expected results, e.g for calc_metrics()
- run on VA/MO/TN/SC?

## Version 0.3

- maybe improve function documentation (e.g., with examples)
- probably make a fill_segs() function to ensure tableau gets res/nonres, etc.
    + filling with zeroes for any missing categories (in res, sex, agecat)
    + probably won't happen for sex, agecat, but probably worth generalizing
- probably write some tests
