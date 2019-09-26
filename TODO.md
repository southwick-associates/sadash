
## Version 1.1

It may be that functions will need to be tweaked, improved, etc. on the backend. However, it is difficult to do that reliably without tests (and probably sample data). Therefore, any changes should begin with tests to ensure that functionality is not broken, or a change to a function leads to an error in calculation.

- write tests (maybe also incorporating sample data)
- maybe improve function documentation (e.g., with examples)
- maybe make a fill_segs() function to ensure tableau gets res/nonres, etc.
    + filling with zeroes for any missing categories (in res, sex, agecat)
    + probably won't happen for sex, agecat, but might be worth generalizing
- maybe investigate scaleup warnings with VA resident permissions (in counties)
    + rounding error? > might be worth a closer look
    + at least none of the discrepancies appear enormous
    