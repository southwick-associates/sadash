
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

### Getting Started

Use `new_dashboard()` or `update_dashboard()` to setup code for a given state and time period:

``` r
# initialize new template
sadash::new_dashboard("YY", "2018-q4")

# alternatively, update code from a previous period
sadash::update_dashboard("YY", "2019-q2", "2018-q4")

# if a data dive tool is needed
sadash::setup_data_dive("YY", "2018-q4")
```

### Workflow Steps

Several vignettes are included to describe the workflow:

- Needs updating: [Prepare License Data](github_vignettes/prepare-license-data.md)
- Needs updating: [Produce Summary Data](github_vignettes/dashboard-summaries.md)
- [Produce Data Dive](github_vignettes/data-dive.md)
