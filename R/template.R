# make template files/folders for selected state/time-period

#' Helper function for for dashboard templates
#' 
#' Called from \code{\link{new_dashboard}} and \code{\link{update_dashboard}}. 
#' Runs initial variable preparation, sets up folders, and creates a ".Rproj"
#' file in the analysis folder.
#' 
#' @inheritParams new_dashboard
#' @family functions for making template files/folders
#' @export
setup_dashboard <- function(state, time_period, sa_path) {
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
    # make .Rproj (for RStudio)
    file.copy(
        system.file("template-setup", "XX.Rproj", package = "sadash"), 
        file.path(analysis_path, paste0(state, "-", time_period, ".Rproj"))
    )
    mget(c("state", "time_period", "analysis_path"))
}

#' Setup a new period for a dashboard state
#' 
#' This will setup files based on a reference time period, rather than 
#' creating new files based on the default template. By default, the new analysis 
#' folder will include all ref_period R files (.R, .Rmd, etc.) and their
#' containing folders, as well as the documentation.tex file.
#' 
#' @inheritParams new_dashboard
#' @param ref_period folder name of reference time period (e.g., 2018-q4)
#' @param files_to_keep file matching for grep (not case sensitive). Files
#' with matching patterns will be copied from ref_period
#' @param files_to_drop file exclusion for grep (not case sensitive). The
#' default is to exclude R studio .Rproj files (which get matched in files_to_keep)
#' 
#' @family functions for making template files/folders
#' @export
update_dashboard <- function(
    state, time_period, ref_period, sa_path = "E:/SA",
    files_to_keep = c("\\.r", "documentation\\.tex"),
    files_to_drop = c("\\.rproj")
) {
    ref_path <- file.path(
        sa_path, "Projects", "Data-Dashboards", toupper(state), ref_period
    )
    if (!dir.exists(ref_path)) {
        stop("The reference path (", ref_path, ") doesn't exist", call. = FALSE)
    }
    params <- setup_dashboard(state, time_period, sa_path)
    
    # identify files/folders to copy
    match_files <- function(file_match, all_files) {
        file_match %>%
            lapply(grep, x = tolower(all_files)) %>% 
            unlist() %>% 
            unique()
    }
    all_files <- list.files(ref_path, recursive = TRUE, all.files = TRUE)
    keep_files <- all_files[match_files(files_to_keep, all_files)]
    keep_files <- keep_files[-match_files(files_to_drop, keep_files)]
        
    # copy R files
    for (i in keep_files) {
        dir.create(
            file.path(params$analysis_path, dirname(i)), 
            recursive = TRUE, showWarnings = FALSE
        )
        file.copy(
            file.path(ref_path, i), 
            file.path(params$analysis_path, i),
            overwrite = FALSE
        )
    }
    # replace strings for previous time period
    replace_strings(
        params$analysis_path, ref_period, params$time_period, showmessage = FALSE
    )
    # print message
    message("A new dashboard project has been initialized:\n  ", params$analysis_path)
}

#' Setup a new state dashboard with default directories and template scripts
#' 
#' This is intended to be run before data processing for a new state begins.
#' 
#' @param state character: Two letter state designation
#' @param time_period character: Time period for the first dashboard 
#' (e.g., "2015", "2016-q1", etc.)
#' @param sa_path character: File path to the Southwick main folder 
#' (for analysis and data, etc.)
#' @param R_version character: Version of R to use for this project
#' @param project_library character: Name of project-specific R package library
#' @family functions for making template files/folders
#' @export
#' @examples
#' # new_dashboard("YY", "2018-q4")
new_dashboard <- function(
    state, time_period, sa_path = "E:/SA", 
    R_version = "3.5.1", project_library = "data-dashboards2"
) {
    params <- setup_dashboard(state, time_period, sa_path)
    
    # copy project template files to analysis_path
    template_paths <- list.files(
        system.file("template-code", package = "sadash"), full.names = TRUE
    )
    for (i in template_paths) {
        file.copy(i, params$analysis_path, recursive = TRUE, overwrite = FALSE)
    }
    # make .Rprofile (using specified R version and project library)
    x <- readLines(system.file("template-setup", "Rprofile", package = "sadash"))
    x[9] <- paste0("r_version <- '", R_version, "'")
    x[10] <- paste0("proj_libname <- '", project_library, "'")
    writeLines(x, file.path(params$analysis_path, ".Rprofile"))
    
    # replace placeholder strings in template code
    # doing this saves the analyst some time and helps enforce naming conventions
    replace_strings(
        params$analysis_path, "__state__", params$state, showmessage = FALSE
    )
    replace_strings(
        params$analysis_path, "__period__", params$time_period, showmessage = FALSE
    )
    # print message
    message("A new dashboard project has been initialized:\n  ", params$analysis_path)
}

#' Search and replace string across files with R
#' 
#' This is a helper function for preparing templates in sadash. It uses gsub, see 
#' \code{\link[base]{grep}} (https://gist.github.com/mages/1544009)
#' 
#' @inheritParams base::list.files
#' @param find_string character: String (to find) that will be replaced
#' @param replacement_string character: New string to use
#' @param showmessage logical: If TRUE, prints a message about replacement
#' @keywords internal
#' @export
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
