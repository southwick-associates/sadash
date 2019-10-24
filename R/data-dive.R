# producing data dive input for Tableau

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

#' Load a 10 percent sample all sportspersons
#' 
#' Every customer who holds a hunting or fishing permission at some point over
#' the timeframe has an equal change of being selected.
#' 
#' @inheritParams load_sqlite
#' @param pct Sample size to draw, in whole percentage point (defaults to 10
#' percent)
#' @family data dive functions
#' @seealso \code{\link{new_dashboard}}
#' @export
#' @examples 
#' # include examps
load_cust_sample <- function(db, yrs, pct = 10, group = "all_sports") {
    # Need to make sure only anglers & hunters are included
    # - which means filtering by sale$lic_id
}
