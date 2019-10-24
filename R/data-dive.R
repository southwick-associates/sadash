# work on the data dive workflow

#' Setup data dive template
#' 
#' This is to be run for an existing state-time_period dashboard project. 
#' It creates a "5-data-dive" folder with template code.
#' 
#' @param state character: Two letter state designation. If NULL, uses the 
#' working directory
#' @param time_period character: Most recent time period ("2018-q4", etc.). If
#' NULL, uses the working directory
#' @family data dive functions
#' @seealso \code{\link{new_dashboard}}
#' @export
#' @examples 
#' # include examps
setup_data_dive <- function(state = NULL, time_period = NULL) {
    
    # template code stored in inst/template-dive
    
}

#' Load history data for a 10 percent customer sample
#' 
#' Pulls in all history data for a particular permission (group) for a 
#' randomly-selected 10 percent sample of customers.
#' 
#' @inheritParams load_sqlite
#' @family data dive functions
#' @seealso \code{\link{load_history}}
#' @export
#' @examples 
#' # include examps
load_history_10pct <- function(db, group) {
    # pull distinct cust_ids & take 10% sample
    
    # pull remaining history data
    # - alternatively pull in all data first & then filter
}
