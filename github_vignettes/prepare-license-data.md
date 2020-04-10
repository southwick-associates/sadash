
# Preparing License Data

Preparation of license data is similar to the requirements for the [National/Regional Dashboard](https://github.com/southwick-associates/salicprep/blob/master/github_vignettes/workflow-overview.md), with some additional complexity described below. 

### Getting Started

When data comes in for a new state, create template files using the R console:

```r
# for initializing new template code:
lictemplate::new_project_individual("YY", "2019-q4")
## A new individual state dashboard has been initialized:
##  E:/SA/Projects/Data-Dashboards/YY/2019-q4

# altenatively, for updating code from a previous period:
lictemplate::update_project("YY", "2020-q2", "2019-q4")
## An updated project has been initialized:
##  E:/SA/Projects/Data-Dashboards/YY/2020-q2
```

## Differences to National/Regional

The workflow generally matches that of the national/regional dashboards, but with some additional requirements:

- [Legacy Data Processing](#legacy-data-processing)
- [State License Years](#state-license-years)
- [Database Schemas](#database-schemas)
- [Geocoding](#geocoding)
- [Privileges](#privileges)

### Legacy Data Processing

Individual state dashboards have been created since 2016, and the workflow has evolved over that time. For 2019-q4, I recommend rewriting dashboard code to reflect the current lictemplate workflow and following some conventions for organizing/documenting:

- Store a README file at the top-level (`E:/SA/Projects/Data-Dashboads/[state]/README.txt`) to keep track of information useful when updating the workflow for new time periods.
    + The dashboard manager can help with referencing documentation stored on Office 365 (individual state methodologies) 
    + Previous validation summaries may have been produced using LaTeX, and are usually stored within `./1-prep-license-data/latex_documentation/documentation.pdf`
    + See WI for an example

### State License Years

The individual dashboards rely up state-defined license years based on effective dates (unlike sales dates as is done for the national/regional dashboards). These sometimes (but not always) follow calendar years, and some percentage of sales for a given license year will fall outside the relevant dates. As a consequence the "month" variable is defined based on an unbounded integer range: ..., -1 (previous Nov), 0 (previous Dec), 1 (Jan), 2 (Feb), ..., 12 (Dec), 13 (Jan), 14 (Feb), ... 

#### Examples

Any particular state will have its own set of reasonable month ranges. For example:

- A state may have an April 1st through March 31st license year, and people will buy licenses before the start of the license year. The "month" range might reasonably run from 2 (previous Feb) through 15 (March, the last month in the calendar year). 
- Another state may have a calendar-based license year where some people buy licenses anywhere from the previous Nov (-1) to the current-year December (12).

### Database Schemas

Data should be structure in a set of sqlite databases (similar to national/regional but with some additions):

- Data-Sensitive:
    + raw-[period].sqlite3
    + standard.sqlite3
- Data-Production:
    + license.sqlite3
    + history.sqlite3
    
#### License.sqlite3 Additional/Modified columns

- county_fips (produced using geocoding)
- zip4dp (temporary for checking for customer duplicates, produced using geocoding
- month with more allowed values (..., -1, 0, 1, 2, ...)

#### History.sqlite3

The national/regional workflow creates [license history](https://southwick-associates.github.io/salic/articles/salic.html#license-history) tables only as a temporary datasets in producing [dashbord summaries](https://southwick-associates.github.io/salic/articles/salic.html#dashboard-metrics). For individual state dashboards, these history tables are stored in a database, where each table corresponds to a given privilege (hunters, anglers, deer hunters, etc.).

### Geocoding

The BulkMailer software is used for geocoding (details to be added.)

### Privileges

TODO.
