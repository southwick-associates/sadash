
## TODO

- take a look at the MO example again. The mid-year results don't look odd in nat/reg dashboard
    + do they look odd here? If so, what's up?
- tweak dashboard calc_metrics() arguments >> group_parent
    + to match history workflow
- test with OR

### Later

- improve function documentation (e.g., with examples)
- data_check_sa() to account for different month allowed values, etc.
- probably make a fill_segs() function to ensure tableau gets res/nonres, etc.
    + filling with zeroes for any missing categories (in res, sex, agecat)
    + probably won't happen for sex, agecat, but probably worth generalizing
- probably write some tests
