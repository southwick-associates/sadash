% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualize.R
\name{plot_month}
\alias{plot_month}
\title{Make a sales by month plot}
\usage{
plot_month(df, plot_title = "Sales by Month")
}
\arguments{
\item{df}{data frame with summary results}

\item{plot_title}{caption to show in plot}
}
\description{
Intended to be run from \code{\link{run_visual}}. Expects a \code{\link{dashboard}} 
formatted table as input filtered to include a single group & quarter.
}
\examples{
library(dplyr)
data(dashboard)
filter(dashboard, group == "all_sports", quarter == 4) \%>\%
    plot_month()
}
\seealso{
Other functions to run dashboard visualization: 
\code{\link{get_county_map}()},
\code{\link{int_breaks}()},
\code{\link{join_county_map}()},
\code{\link{plot_bar}()},
\code{\link{plot_county}()},
\code{\link{plot_value2}()},
\code{\link{plotly_config}()},
\code{\link{run_visual_county}()},
\code{\link{ui_button_layout}()}
}
\concept{functions to run dashboard visualization}
