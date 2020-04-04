
# sadash

An R package for Southwick internal use, includes functions for individual state dashboard workflows. This is similar to [package salicprep](https://github.com/southwick-associates/salicprep), but covers the more complex use-case of state-level dashboards (county-level summaries, privilege/subtype permissions, etc.).

## Installation

From the R console:

``` r
install.packages("remotes")
remotes::install_github("southwick-associates/sadash")
```

## Usage

Sadash provides a modular workflow through data processing functions. Run `?sadash` for an overview of the included functionality.

### Getting Started

Use `lictemplate::new_project_individual()` or `lictemplate::update_project()` to setup code for a given state and time period:

``` r
remotes::install_github("southwick-associates/lictemplate")

# initialize new template
lictemplate::new_project_individual("YY", "2018-q4")

# alternatively, update code from a previous period
lictemplate::update_project("YY", "2019-q2", "2018-q4")

# if a data dive tool is needed
lictemplate::setup_data_dive("YY", "2018-q4")
```

### Workflow Steps

Several vignettes are included to describe the workflow:

- TODO: [Prepare License Data](github_vignettes/prepare-license-data.md)
- Needs updating: [Produce Summary Data](github_vignettes/dashboard-summaries.md)
- [Produce Data Dive](github_vignettes/data-dive.md)
