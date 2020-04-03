# make template files/folders for selected state/time-period

#' Setup a new state dashboard with default directories and template scripts
#' 
#' This is intended to be run before data processing for a new state begins. 
#' It's just a wrapper for \code{\link[lictemplate]{new_project}} from the 
#' lictemplate package.
#' 
#' @inheritParams lictemplate::new_project
#' @param ... other arguments passed to \code{\link[lictemplate]{new_project}} 
#' 
#' @family functions for making template files/folders
#' @export
#' @examples
#' # new_dashboard("YY", "2018-q4")
new_dashboard <- function(
    state, period, package = "sadash", 
    print_message = "A new dashboard project has been initialized",
    ...
) {
    lictemplate::new_project(
        state, period, package = package, print_message = print_message, ...
    )
}

#' Helper function for for dashboard templates
#' 
#' Called from \code{\link{new_dashboard}} and \code{\link{update_dashboard}}. 
#' Runs initial variable preparation, sets up folders, and creates a ".Rproj"
#' file in the analysis folder.
#' 
#' @param state character: Two letter state designation
#' @param time_period character: Time period for the first dashboard 
#' (e.g., "2015", "2016-q1", etc.)
#' @param sa_path character: File path to the Southwick main folder 
#' (for analysis and data, etc.)
#' 
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
    # make analysis folder
    dir.create(analysis_path, recursive = TRUE, showWarnings = FALSE)
    
    # make raw data folders (sensitive, production)
    dir.create(
        file.path(sa_path, "Data-sensitive", "Data-Dashboards", state,  
                  paste0("raw-", time_period)), 
        recursive = TRUE, showWarnings = FALSE
    )
    dir.create(
        file.path(sa_path, "Data-production", "Data-Dashboards", state),  
        showWarnings = FALSE
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
#' @inheritParams setup_dashboard
#' @param ref_period folder name of reference time period (e.g., 2018-q4)
#' @param files_to_keep file matching for grep (not case sensitive). Files
#' with matching patterns will be copied from ref_period
#' 
#' @family functions for making template files/folders
#' @export
update_dashboard <- function(
    state, time_period, ref_period, sa_path = "E:/SA",
    files_to_keep = c("\\.r", "documentation\\.tex")
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
    code_path <- file.path(ref_path, "code")
    all_files <- list.files(code_path, recursive = TRUE, all.files = TRUE)
    keep_files <- all_files[match_files(files_to_keep, all_files)]
        
    # copy R files
    copy_files <- function(file_names) {
        for (i in file_names) {
            old <- file.path(ref_path, i)
            new <- file.path(params$analysis_path, i)
            if (file.exists(old)) {
                dir.create(dirname(new), recursive = TRUE, showWarnings = FALSE)
                file.copy(old, new, overwrite = FALSE)
            }
        }
    }
    copy_files(file.path("code", keep_files))
    
    # copy top-level files
    copy_files(c(".Rprofile", "README.md", "renv.lock", "renv/activate.R",  
               "renv/settings.dcf"))
    
    # copy .Rproj and rename
    oldname <- paste0(params$state, "-", ref_period, ".Rproj")
    newname <- paste0(params$state, "-", params$time_period, ".Rproj")
    copy_files(oldname)
    file.rename(
        file.path(params$analysis_path, oldname),
        file.path(params$analysis_path, newname)
    )
    
    # replace strings for previous time period
    replace_strings(
        code_path, ref_period, params$time_period, showmessage = FALSE
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

#' Setup data dive template
#' 
#' This is to be run for an existing state-time_period dashboard project. 
#' It creates a new folder with template code. Defaults to placing a "5-data-dive"
#' folder in the working directory (i.e., when state and time_period are NULL).
#' 
#' @inheritParams setup_dashboard
#' @param analysis_dir full path name for analysis directory (only used if state
#' and time_period are NULL)
#' @param dive_dir folder for data dive code
#' @family functions for making template files/folders
#' @seealso \code{\link{new_dashboard}}
#' @export
#' @examples 
#' # setup_data_dive()
setup_data_dive <- function(
    state = NULL, time_period = NULL, analysis_dir = getwd(), 
    sa_path = "E:/SA", dive_dir = "5-data-dive"
) {
    # identify analysis_dir
    if (!(is.null(state) && is.null(time_period))) {
        if (is.null(state) || is.null(time_period)) {
            stop("The state & time_period arguments must both be NULL (or neither)",
                 call. = FALSE)
        }
        analysis_dir <- file.path(
            sa_path, "Projects", "Data-Dashboards", toupper(state), 
            as.character(time_period)
        )
    }
    
    # create dive_dir
    dive_dir <- file.path(analysis_dir, dive_dir)
    dir.create(dive_dir)
    
    # copy template files to dive_dir
    template_dir <- system.file("template-dive", package = "sadash")
    template_files <- list.files(template_dir)
    
    for (i in template_files) {
        file.copy(
            file.path(template_dir, i), 
            file.path(dive_dir, i), 
            overwrite = FALSE
        )
    }
    message("A data dive folder has been initialized:\n  ", dive_dir)
}
