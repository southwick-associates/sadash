
## Version 1.0

I expect this version to have a more or less finalized user-facing interface. Basically, the template code is complete, and there won't be breaking changes to the functions in the template code (unless something needs to be fixed.)

- maybe investigate scaleup warnings with VA resident permissions (in counties)
    + rounding error? > might be worth a closer look
    + at least none of the discrepancies appear enormous
- run on VA, MO, etc.


I think the below specification in calc_metrics() would be better, but it would take a bit of time to employ it (which isn't something I have much of at the moment).

It probably is worth doing if it takes less than 30 min. The lack of testing is biting me a bit since I'm hesitant to change things and unintentionally break things.


```
# optional alt. specification for calc_rate
# - clarifies distinction in how rate vs priv is handled
rate <- if (is.null(part_ref)) {
    calc_part_rate(part$residents, pop_county, rate_test, res_type)
} else {
    calc_priv_rate(part$participants, part_ref$participants, rate_test)
}

# vs. existing
rate <- calc_rate(
    part$residents, pop_county, 
    part$participants, part_ref$participants, res_type, rate_test
)
```

## Version 1.1

It may be that functions will need to be tweaked, improved, etc. on the backend. However, it is difficult to do that reliably without tests (and probably sample data). Therefore, any changes should begin with tests to ensure that functionality is not broken, or a change to a function leads to an error in calculation.

- write tests (maybe also incorporating sample data)
- maybe improve function documentation (e.g., with examples)
- maybe make a fill_segs() function to ensure tableau gets res/nonres, etc.
    + filling with zeroes for any missing categories (in res, sex, agecat)
    + probably won't happen for sex, agecat, but might be worth generalizing
