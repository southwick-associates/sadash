
# Preparing License Data

Preparation of license data is similar to the requirements for the [National/Regional Dashboard](), with some additional complexity. 

### Getting Started

From the R console:

```r
# initialize new template
lictemplate::new_project_individual("YY", "2019-q4")
## A new individual state dashboard has been initialized:
##  E:/SA/Projects/Data-Dashboards/YY/2019-q4

# alternatively, update code from a previous period
lictemplate::update_project("YY", "2020-q4", "2019-q4")
## An updated project has been initialized:
##  E:/SA/Projects/Data-Dashboards/YY/2020-q2
```
