% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-dive.R
\name{filter_demo}
\alias{filter_demo}
\title{Filter data for given variable}
\usage{
filter_demo(
  priv,
  var = "sex",
  var_select = c("Male", "Female"),
  var_all = c("Male", "Female")
)
}
\arguments{
\item{priv}{data for one permission}

\item{var}{name of variable for filtering}

\item{var_select}{currently selted values of var}

\item{var_all}{set of values that var can take}
}
\description{
To be run from \code{\link{run_visual_dive}}. Applies a filter as needed
based on the checked categories. R3 gets special treatment since it should
only be missing in the first 5 years of data.
}
\examples{
library(dplyr)
data(hist_samp)
priv <- filter(hist_samp, priv == "all_sports")
count(priv, R3, year) \%>\% tidyr::spread(year, n)

priv <- filter_demo(priv, "R3", c("Renew", "Reactivate"))
count(priv, R3, year) \%>\% tidyr::spread(year, n)
}
\seealso{
Other data dive functions: 
\code{\link{drop_state_code}()},
\code{\link{get_county_map_dive}()},
\code{\link{load_cust_samp}()},
\code{\link{plot_county_dive}()},
\code{\link{plot_dist}()},
\code{\link{plot_trend}()},
\code{\link{run_visual_dive}()},
\code{\link{set_nonres_county_na}()},
\code{\link{set_other_county_na}()},
\code{\link{summarize_trend}()}
}
\concept{data dive functions}
