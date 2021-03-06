% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-dive.R
\name{drop_state_code}
\alias{drop_state_code}
\title{Remove the state portion of county fips}
\usage{
drop_state_code(x)
}
\arguments{
\item{x}{vector that holds the 5-digit fips code (e.g., 19001 for Iowa 
Adair county)}
}
\description{
Tableau seems to require just the county portion of the code for a given state.
}
\examples{
\dontrun{
db_license <- f <- "E:/SA/Data-production/Data-Dashboards/IA/license.sqlite3"
fips <- load_cust(db_license) \%>\% pull(county_fips)
fips_new <- drop_state_code(fips)
df <- data.frame(fips, fips_new, stringsAsFactors = FALSE)
head(df)
filter(df, nchar(fips) == 4) \%>\% tail()

# stop with error
drop_state_code(fips_new)
}
}
\seealso{
Other data dive functions: 
\code{\link{filter_demo}()},
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
