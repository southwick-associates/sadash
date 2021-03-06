% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualize.R
\name{plot_bar}
\alias{plot_bar}
\title{Make a bar plot: measure by year (facetted using metric & category)}
\usage{
plot_bar(df, plot_title = "", measure = "value")
}
\arguments{
\item{df}{data frame with summary results}

\item{plot_title}{caption to show in plot}

\item{measure}{variable to be plotted on the y axis}
}
\description{
This is used as a generic function in \code{\link{plot_value2}}. It was 
copied from dashboard-template code shared with state agencies. Included
in entirety here to reduce dependencies.
}
\seealso{
Other functions to run dashboard visualization: 
\code{\link{get_county_map}()},
\code{\link{int_breaks}()},
\code{\link{join_county_map}()},
\code{\link{plot_county}()},
\code{\link{plot_month}()},
\code{\link{plot_value2}()},
\code{\link{plotly_config}()},
\code{\link{run_visual_county}()},
\code{\link{ui_button_layout}()}
}
\concept{functions to run dashboard visualization}
