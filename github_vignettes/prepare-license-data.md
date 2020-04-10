
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

The workflow requirements generally match that of the national/regional dashboards, but with some additional needs

- [Legacy Data Processing](#legacy-data-processing)
- [Database Schemas](#database-schemas)
- [Geocoding](#geocoding)
- [Privileges](#privileges)

### Legacy Data Processing

Individual state dashboards have been created since 2016, and the workflow has evolved over that time. For 2019-q4, I recommend rewriting dashboard code to reflect the current lictemplate workflow and following some conventions for organizing/documenting:

- Store a README file at the top-level (`E:/SA/Projects/Data-Dashboads/[state]/README.txt`) to keep track of information useful when updating the workflow for new time periods.
    + The dashboard manager can help with referencing documentation stored on Office 365 (individual state methodologies) 
    + Previous validation summaries may have been produced using LaTeX, and are usually stored within `./1-prep-license-data/latex_documentation/documentation.pdf`
    + See WI for an example

### Database Schemas

Data should be structure in a set of sqlite databases (similar to national/regional but with some additions):

- Data-Sensitive:
    + raw-[period].sqlite3
    + standard.sqlite3
- Data-Production:
    + license.sqlite3
    + history.sqlite3
    


### Geocoding

### Privileges

