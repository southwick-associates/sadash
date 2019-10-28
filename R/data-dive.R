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
#' the timeframe has an equal chance of being selected. We use a sample for the
#' data dive to ensure a usable file size & sufficient responsiveness.
#' 
#' @inheritParams load_sqlite
#' @param pct Sample size to draw, in whole percentage points (defaults to 10
#' percent)
#' @family data dive functions
#' @seealso \code{\link{new_dashboard}}
#' @export
#' @examples 
#' \dontrun{
#' db_history <- "E:/SA/Data-production/Data-Dashboards/IA/history.sqlite3"
#' cust_samp <- load_cust_samp(db_history, 2006:2018)
#' dplyr::glimpse(cust_samp)
#' }
load_cust_samp <- function(db, yrs, pct = 10, group = "all_sports") {
    load_history(db, group, yrs) %>%
        distinct(cust_id) %>%
        sample_frac(pct / 100)
}

#' Set county_fips to missing if not resident
#' 
#' Ensure county_fips isn't populated where res == 0 or is.na(res). There are
#' typically a few of these and we don't want them showing up in the data dive.
#' 
#' @param x license history with county_fips & res variables
#' @family data dive functions
#' @export
set_nonres_county_na <- function(x) {
    # TODO: maybe include a summary here as well
    x$county_fips <- ifelse(is.na(x$res) | x$res == 0, NA_integer_, x$county_fips)
    x
}


# Shiny App ---------------------------------------------------------------

#' Plot trendline for run_visual_dive()
#' 
#' To be run from \code{\link{run_visual_dive}}. 
#' This will plot a count of participants, optionally facetted down and/or across
#' by additional variables. If facetting (i.e., grouping) missing values will
#' be removed from the corresponding variables before plotting.
#' 
#' @param down variable name for optional facetting down. If "None", no facetting
#' will be done.
#' @param across variable name for optional facetting across. If "None", no facetting
#' will be done.
#' @inheritParams run_visual_dive
#' @family data dive functions
#' @export
plot_trend <- function(hist_samp, down = "None", across = "None", pct = 10) {
    # Using NULL when no facetting is to be performed
    down <- if(down == "None") NULL else down
    across <- if(across == "None") NULL else across
    
    # drop NAs for group (facet direction) variables
    drop_na <- function(df, var) {
        if (is.null(var)) return(df)
        filter(df, !is.na(.data[[var]]))
    }
    hist_samp <- hist_samp %>% drop_na(down) %>% drop_na(across)
    
    # produce a count using dplyr
    cnt <- hist_samp %>% 
        group_by_(.dots = c(down, across, "year")) %>%
        summarize(n = n()) %>%
        ungroup() %>%
        mutate(n = n / (pct / 100)) 
    
    # plot using ggplot
    p <- cnt %>%
        ggplot(aes(year, n)) +
        geom_line()
    if (!is.null(down) || !is.null(across)) {
        # add facetting as needed
        # - dot values in ggplot indicate no facetting in corresponding direction
        if (is.null(down)) down <- "."
        if (is.null(across)) across <- "."
        p <- p + 
            facet_grid(stats::as.formula(paste(down, "~", across)), scales = "free")
    }
    p
}

#' Filter data for given variable
#' 
#' To be run from \code{\link{run_visual_dive}}. Applies a filter as needed
#' based on the checked categories.
#' 
#' @param priv data for one permission
#' @param var name of variable for filtering
#' @param var_select currently selted values of var
#' @param var_all set of values that var can take
#' @family data dive functions
#' @export
filter_demo <- function(
    priv, var = "sex", var_select = c("Male", "Female"), 
    var_all = c("Male", "Female")
) {
    # no filter should be applied if all options are checked
    if (identical(var_select, var_all)) {
        return(priv) 
    }
    
    # R3 will only be NA for the first 5 years
    if (var == "R3") {
        return(filter(priv, .data[[var]] %in% var_select))
    }
    
    # We do want to keep NA values for demographic variables
    # - does obscure NAs, but not obvious how to make that work (low priority)
    filter(priv, .data[[var]] %in% c(var_select, NA))
}

#' Run shiny app version of data dive
#' 
#' This is a rough mock-up, intended to ensure (1) no surprises on the Tableau
#' end and (2) results look correct. 
#' 
#' @param hist_samp data frame with a sample of license history for all privileges 
#' containing at least 9 variables: priv, cust_id, year, lapse, R3, res, sex, fips, age
#' @param pct sample size (in whole percentage points) for hist_samp
#' @family data dive functions
#' @seealso \code{\link{run_visual}}
#' @export
#' @examples 
#' \dontrun{
#' f <- "E:/SA/Data-production/Data-Dashboards/WI/2015-q4/WI-data-dive-10pct-2015q4/priv-WI-10pct.csv"
#' hist_samp <- readr::read_csv(f, progress = FALSE)
#' hist_samp <- salic::label_categories(hist_samp)
#' hist_samp <- salic::df_factor_age(hist_samp)
#' run_visual_dive(hist_samp)
#' }
run_visual_dive <- function(hist_samp, pct = 10) {
    
    # prepare demographic filter checkboxes for given variable ("sex", etc.)
    check_filter <- function(var, selected = levels(hist_samp[[var]])) {
        checkboxGroupInput(
            inputId = var, label = var, selected = selected, 
            choiceNames = as.list(selected), choiceValues = as.list(selected)
        )
    }
    # prepare plot facetting selections for given direction (down or across) 
    demos <- c("res", "sex", "R3", "age")
    check_plot_facet <- function(direction) {
        selectInput(
            inputId = direction, label = paste("Compare", direction),  
            choices = as.list(c("None", demos)), selected = "None"
        )
    }
    
    ui <- fluidPage(mainPanel(
        splitLayout(
            selectInput("priv", "Choose Permission", unique(hist_samp$priv)),
            check_plot_facet("down"), check_plot_facet("across"),
            ui_prevent_clipping()
        ),
        splitLayout(
            actionButton("button", "APPLY FILTER"),
            check_filter("res"), check_filter("sex"), 
            check_filter("R3"), check_filter("age")
        ),
        plotly::plotlyOutput("trendPlot"),
        width = 12
    ))
    
    server <- function(input, output, session) {
        ### Data Filtering
        # - by permission
        dataPriv <- reactive({
            filter(hist_samp, .data$priv == input$priv)
        })
        
        # - by variable in checkboxes (sex, age, etc.)
        dataInput <- reactive({
            # isolate ensures filter only runs after the button is pressed
            input$button
            priv <- dataPriv()
            isolate({
                for (var in demos) {
                    priv <- filter_demo(priv, var, input[[var]], levels(hist_samp[[var]]))
                }
                priv
            })
        })
        
        ### Plotting participants by year
        output$trendPlot <- plotly::renderPlotly({
            p <- plot_trend(dataInput(), input$down, input$across)
            plotly::ggplotly(p) %>% plotly_config()
        })
    }
    shinyApp(ui, server)
}
