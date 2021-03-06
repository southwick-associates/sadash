% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-doc.R
\docType{data}
\name{county_map_us}
\alias{county_map_us}
\title{Sample Data: Shapefile for all counties in the US}
\format{
a data frame with 5 variables
\describe{
\item{long}{longitude}
\item{lat}{latitude}
\item{state}{2-letter abbreviation}
\item{state_name}{name of state}
\item{county_fips}{fips code}
\item{county}{county name}
}
}
\description{
Pulled into sadash for convenience for use in \code{\link{get_county_map}}.
I was annoyed by the fact that \code{\link[ggplot2]{map_data}} attaches
the maps package which was the main motivation to placing it here.
}
\seealso{
Other Sample Data: 
\code{\link{age_map}},
\code{\link{dashboard}},
\code{\link{hist_samp}}
}
\concept{Sample Data}
\keyword{datasets}
