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
