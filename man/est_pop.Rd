% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dashboard-metrics.R
\name{est_pop}
\alias{est_pop}
\title{Summarize population by year for a given segment}
\usage{
est_pop(pop_acs, segment)
}
\arguments{
\item{pop_acs}{data frame: population dataset}

\item{segment}{character: name of variable that holds grouping segment}
}
\description{
Summarize population by year for a given segment
}
\seealso{
Other dashboard functions: 
\code{\link{calc_metrics}()},
\code{\link{calc_part_rate}()},
\code{\link{calc_part}()},
\code{\link{calc_priv_rate}()},
\code{\link{est_month}()},
\code{\link{est_rate}()},
\code{\link{fill_missing_months}()},
\code{\link{format_metrics}()},
\code{\link{write_dash}()}
}
\concept{dashboard functions}
