# package-level documentation & imports

#' @import dplyr salic DBI
#' @importFrom utils write.csv
#' @rawNamespace import(data.table, except = c(first, between, last))
NULL

if (getRversion() >= "2.15.1") {
    utils::globalVariables(
        c("R3", "age", "age_year", "birth_year", "change", "county_fips",
          "county_name", "cust_id", "description", "duration_run", "lapse",
          "lic_id", "participants", "permission", "pop", "rate", "res", "sex",
          "state_abbrev")
    )
}
