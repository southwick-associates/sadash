
## Version 1.0.7

- Removed dashtemplate dependency
- Added age_map sample data (relation to ACS census age categories)

## Version 1.0.6

Added functionality for producing data dive: setup_data_dive(), run_visual_dive(), etc.

## Version 1.0.5

- Added a plot_county() for including in run_visual()
- Using package plotly for interactivity and slightly nicer-looking plots in run_visual()

## Version 1.0.4

- Added a dashboard sample data table based on SC 2019-Q2.
- Added plot_month() & run_visual() functions to more closely replicate the Tableau dashboard view (to check/explore before sending to Ben).

## Version 1.0.3

Fixed a bug in calc_metrics() that would hit an error if R3 wasn't included in the history table (i.e., fewer than 5 years for a permission).

## Version 1.0.2

Small template code tweaks & more flexible update_dashboard().

## Version 1.0.1

This includes some small tweaks and improved documentation. An update_dashboard() function was also added.

## Version 1.0

This is intended to be a stable production version; expecting it to have a more or less finalized user-facing interface. Basically, the template code is complete, and there won't be breaking changes to the functions in the template code (unless something needs to be fixed.)
