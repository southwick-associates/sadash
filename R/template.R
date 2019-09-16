# make template files/folders for selected state/time-period

# TODO: 
# for state updates: copy existing code with replacements (__period__, first = FALSE)
update_dashboard <- function() {
    
}

#' Setup a new state dashboard with default directories and template scripts
#' 
#' This is intended to be run before data processing for a new state begins
#' @param state character: Two letter state designation
#' @param time_period character: Time period for the first dashboard 
#' (e.g., "2015", "2016-q1", etc.)
#' @param sa_path character: File path to the Southwick main folder 
#' (for analysis and data, etc.)
#' @param R_version character: Version of R to use for this project
#' @param project_library character: Name of project-specific R package library
#' @family functions for making directories and files
#' @export
#' @examples
#' # new_dashboard("YY", "2018-q4")
new_dashboard <- function(
    state, time_period, sa_path = "E:/SA", 
    R_version = "3.5.1", project_library = "data-dashboards2"
) {
    # initial variable prep
    state <- toupper(state)
    time_period <- as.character(time_period)
    analysis_path <- file.path(
        sa_path, "Projects", "Data-Dashboards", state, time_period
    )
    # error - don't run if a directory with that time period already exists
    if (dir.exists(analysis_path)) {
        stop("That time_period already exists!: ", analysis_path, call. = FALSE)
    }
    # make analysis folders
    dir.create(analysis_path, recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(analysis_path, "data"), showWarnings = FALSE)
    dir.create(file.path(analysis_path, "out"), showWarnings = FALSE)
    
    # make data folders (sensitive, production)
    dir.create(
        file.path(sa_path, "Data-sensitive", "Data-Dashboards", state,  
                  paste0("raw-", time_period)), 
        recursive = TRUE, showWarnings = FALSE
    )
    dir.create(
        file.path(sa_path, "Data-production", "Data-Dashboards", state),  
        showWarnings = FALSE
    )
    # copy project template files to analysis_path
    template_paths <- list.files(
        system.file("template-code", package = "sadash"), full.names = TRUE
    )
    for (i in template_paths) {
        file.copy(i, analysis_path, recursive = TRUE, overwrite = FALSE)
    }
    # make .Rprofile (using specified R version and project library)
    x <- readLines(system.file("template-setup", ".Rprofile", package = "sadash"))
    x[9] <- paste0("r_version <- '", R_version, "'")
    x[10] <- paste0("proj_libname <- '", project_library, "'")
    writeLines(x, file.path(analysis_path, ".Rprofile"))
    
    # make .Rproj (for RStudio)
    file.copy(
        system.file("template-setup", "XX.Rproj", package = "sadash"), 
        file.path(analysis_path, paste0(state, "-", time_period, ".Rproj"))
    )
    # replace placeholder strings in template code
    # doing this saves the analyst some time and helps enforce naming conventions
    replace_strings(analysis_path, "__state__", state, showmessage = FALSE)
    replace_strings(analysis_path, "__period__", time_period, showmessage = FALSE)
    
    # print message
    message("A new dashboard project has been initialized:\n  ", analysis_path)
    
}

#' Search and replace string across files with R
#' 
#' This is a helper function for preparing templates in sadash. It uses gsub, see 
#' \code{\link[base]{grep}} (https://gist.github.com/mages/1544009)
#' @inheritParams base::list.files
#' @param find_string character: String (to find) that will be replaced
#' @param replacement_string character: New string to use
#' @param showmessage logical: If TRUE, prints a message about replacement
#' @keywords internal
#' @export
#' @examples
#' # replace_strings()
replace_strings <- function(
    path = ".", find_string, replacement_string, pattern = ".R", showmessage = TRUE
) {
    # get file names in which to apply replacement
    filenames <- list.files(
        path, pattern = pattern, full.names = TRUE, recursive = TRUE
    )
    # stop with error if no filenames in path match pattern (i.e., no files to replace)
    if (length(filenames) == 0) {
        stop(
            "There are no files matching pattern '", pattern, "' in:\n  ",
            normalizePath(path), call. = FALSE
        )    
    }
    # Replace find_string with replacement_string
    for( f in filenames ) {
        x <- readLines(f)
        y <- gsub( find_string, replacement_string, x)
        cat(y, file=f, sep="\n")
    }
    # Print output message about replacements
    if (showmessage) {
        message(
            "Occurences of '", find_string, "' have been replaced with '",
            replacement_string, "' in '", pattern, "' files at:\n  ", 
            normalizePath(path)
        )
    }
}
