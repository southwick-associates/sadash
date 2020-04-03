
# sadash

An R package for Southwick internal use: functions and templates to run individual state dashboard workflows. This extends [package salic](https://southwick-associates.github.io/salic/) to cover the more complex use-case of state-level dashboards (county-level summaries, privilege/subtype permissions, etc.).

## Installation

From the R console:

``` r
install.packages("remotes")
remotes::install_github("southwick-associates/sadash")
```

## Usage

Sadash serves two primary purposes: (1) providing boilerplate (template) code for state processing, and (2) providing a modular workflow through data processing functions. Run `?sadash` for an overview of the included functionality.

- TODO: [Setup Templates](github_vignettes/setup-templates.md)
- TODO: [Prepare License Data](github_vignettes/prepare-license-data.md)
- TODO: [Produce Summary Data](github_vignettes/dashboard-summaries.md)
- TODO: [Produce Data Dive](github_vignettes/data-dive.md)
