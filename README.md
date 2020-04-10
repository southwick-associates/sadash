
# sadash

An R package for Southwick internal use, includes functions for individual state dashboard workflows. Sadash extends [package salicprep](https://github.com/southwick-associates/salicprep) to cover the more complex use-case of state-level dashboards (county-level summaries, privilege/subtype permissions, etc.).

## Installation

From the R console:

``` r
install.packages("remotes")
remotes::install_github("southwick-associates/sadash")
```

## Usage

Sadash provides a modular workflow through data processing functions. Run `?sadash` for an overview of the included functionality. Several vignettes are included to describe the workflow:

- In progress: [Prepare License Data](github_vignettes/prepare-license-data.md)
- Needs updating: [Produce Summary Data](github_vignettes/dashboard-summaries.md)
- [Produce Data Dive](github_vignettes/data-dive.md)
