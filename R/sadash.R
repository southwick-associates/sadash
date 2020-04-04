# package-level documentation & imports

#' @import dplyr salic shiny ggplot2
#' @importFrom utils write.csv
#' @rawNamespace import(data.table, except = c(first, between, last))
NULL

if (getRversion() >= "2.15.1") {
    utils::globalVariables(
        c("R3", "age", "age_year", "birth_year", "change", "county_fips",
          "county_name", "cust_id", "description", "duration_run", "lapse",
          "lic_id", "participants", "permission", "pop", "rate", "res", "sex",
          "state_abbrev", "county_map_us", "age_map")
    )
}

#' sadash: Prepare state data dashboards
#' 
#' This includes a summary of the core functions grouped by task.
#' 
#' @section Preparing License History:
#' \itemize{
#'   \item use \code{\link{load_license}} to pull standardized license data 
#'   from sqlite
#'   \item use \code{\link{data_check_sa}} to check formatting rules of 
#'   standardized data
#'   \item use \code{\link{drop_na_custid}} to remove any records with a missing
#'   customer ID from the sale table
#'   \item use functions from salic (\code{\link{rank_sale}}, \code{\link{make_history}}) 
#'   to build license history
#'   \item finally use \code{\link{write_history}} to write to sqlite
#' }
#' 
#' @section Preparing Dashboard Metrics:
#' \itemize{
#'   \item use \code{\link{load_sqlite}} functions to pull in state-level 
#'   customer, sales, and census population data
#'   \item use \code{\link{load_history}} and \code{\link{recode_history}} to
#'   prepare data for a specific permission
#'   \item use \code{\link{quarterly_filter}}, \code{\link{quarterly_lapse}},
#'   and \code{\link{calc_metrics}} in sequence to produce a list that stores
#'   all metrics for a permission
#'   \item use \code{\link{format_metrics}} and \code{\link{write_dash}}
#'   in sequence to produce csv files by permission-quarter
#'   \item use \code{\link{run_visual}} to check results
#' }
#' 
#' @docType package
#' @name sadash
NULL
