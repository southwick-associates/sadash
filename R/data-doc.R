# sample data documentation

#' Sample Data: Metrics formatted for Dashboard Input
#' 
#' This data frame holds national/regional dashboard metrics formatted for 
#' dashboard input pulled from SC 2019-Q2 in Oct, 2019.
#'
#' @docType data
#' @keywords datasets
#' @name dashboard
#' @format A data frame with 7 variables
#' \describe{
#' \item{quarter}{Time period covered (2 or 4)}
#' \item{group}{Permission group ("all_sports", "hunt", "fish", etc.)}
#' \item{segment}{Breakout ("All", "Age", "Gender", "Residency", "County", "month")}
#' \item{year}{Calendar Year (2008, 2009, ..., 2019)}
#' \item{category}{Category breakouts for segment}
#' \item{metric}{Metric Summarized ("participants", "recruits", "churn", "rate")}
#' \item{value}{Value of metric for given dimension}
#' }
#' @family Sample Data
NULL

#' Sample Data: Shapefile for all counties in the US
#' 
#' Pulled into sadash for convenience for use in \code{\link{get_county_map}}.
#' I was annoyed by the fact that \code{\link[ggplot2]{map_data}} attaches
#' the maps package which was the main motivation to placing it here.
#' 
#' @docType data
#' @keywords datasets
#' @name county_map_us
#' @format a data frame with 5 variables
#' \describe{
#' \item{long}{longitude}
#' \item{lat}{latitude}
#' \item{state}{2-letter abbreviation}
#' \item{state_name}{name of state}
#' \item{county_fips}{fips code}
#' \item{county}{county name}
#' }
#' @family Sample Data
NULL

#' Sample Data: Permission data for use in \code{\link{run_visual_dive}}
#' 
#' A 1 percent sample of one state's permission dynamics
#' 
#' @docType data
#' @keywords datasets
#' @name hist_samp
#' @format a data frame with 8 variables and approx. 560K observations
#' \describe{
#' \item{priv}{name of permission}
#' \item{cust_id}{customer ID}
#' \item{year}{license year}
#' \item{lapse}{lapsed next year (1 = TRUE, 0 = FALSE)}
#' \item{R3}{R3 category}
#' \item{res}{state residency}
#' \item{sex}{gender}
#' \item{age}{age category}
#' }
#' @family Sample Data
NULL
