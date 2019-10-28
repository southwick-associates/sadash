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

#' Filter data for given variable
#' 
#' To be run from \code{\link{run_visual_dive}}. Applies a filter as needed
#' based on the checked categories. R3 gets special treatment since it should
#' only be missing in the first 5 years of data.
#' 
#' @param priv data for one permission
#' @param var name of variable for filtering
#' @param var_select currently selted values of var
#' @param var_all set of values that var can take
#' @family data dive functions
#' @export
#' @examples 
#' library(dplyr)
#' data(hist_samp)
#' priv <- filter(hist_samp, priv == "all_sports")
#' count(priv, R3, year) %>% tidyr::spread(year, n)
#' 
#' priv <- filter_demo(priv, "R3", c("Renew", "Reactivate"))
#' count(priv, R3, year) %>% tidyr::spread(year, n)
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

#' Summarize trend for either participants or churn
#' 
#' To be run from \code{\link{run_visual_dive}} for preparing data for 
#' \code{\link{plot_trend}}
#' 
#' @param metric name of metric to summarize, either "participants" or "churn"
#' @param down variable name for optional facetting down. If "None", no facetting
#' will be done.
#' @param across variable name for optional facetting across. If "None", no facetting
#' will be done.
#' @inheritParams run_visual_dive
#' @inheritParams filter_demo
#' @family data dive functions
#' @export
#' @examples 
#' data(hist_samp)
#' priv <- dplyr::filter(hist_samp, priv == "all_sports")
#' summarize_trend(priv, pct = 1)
#' summarize_trend(priv, "sex", "age", pct = 1)
#' 
#' summarize_trend(priv, metric = "churn")
#' summarize_trend(priv, "age", metric = "churn")
summarize_trend <- function(
    priv, down = "None", across = "None", metric = "participants", pct = 10
) {
    # Using NULL when no facetting is to be performed
    down <- if(down == "None") NULL else down
    across <- if(across == "None") NULL else across
    
    # drop NAs for group (facet direction) variables
    drop_na <- function(df, var) {
        if (is.null(var)) return(df)
        filter(df, !is.na(.data[[var]]))
    }
    priv <- priv %>% drop_na(down) %>% drop_na(across)
    
    # make summary table
    priv <- group_by_(priv, .dots = c(down, across, "year"))
    
    if (metric == "participants") {
        priv %>% 
            summarize(value = n()) %>%
            ungroup() %>%
            mutate(value = .data$value / (pct / 100))
    } else {
        priv %>% 
            summarize(value = mean(.data$lapse) * 100) %>%
            ungroup() %>%
            mutate(year = .data$year + 1) %>%
            filter(!is.na(.data$value))
    }
}
    
#' Plot trendline for run_visual_dive()
#' 
#' To be run from \code{\link{run_visual_dive}}. 
#' This will plot a count of participants, optionally facetted down and/or across
#' by additional variables. If facetting (i.e., grouping) missing values will
#' be removed from the corresponding variables before plotting.
#' 
#' @param tbl data frame holding table produced by \code{\link{summarize_trend}}
#' @inheritParams summarize_trend
#' @family data dive functions
#' @export
#' @examples 
#' library(dplyr)
#' data(hist_samp)
#' priv <- filter(hist_samp, priv == "all_sports")
#' 
#' summarize_trend(priv, pct = 1) %>% plot_trend()
#' summarize_trend(priv, "sex", pct = 1) %>% plot_trend("sex")
#' summarize_trend(priv, "sex", "age", pct = 1) %>% plot_trend("sex", "age")
#' 
#' summarize_trend(priv, pct = 1, metric = "churn") %>% plot_trend()
#' summarize_trend(priv, "sex", pct = 1, metric = "churn") %>% plot_trend("sex")
plot_trend <- function(tbl, down = "None", across = "None") {
    p <- tbl %>%
        ggplot(aes_string("year", "value")) +
        geom_line()
    
    # facetting will be needed if there are more than 2 variables in tbl
    if (ncol(tbl) > 2) {
        # dot values in ggplot indicate no facetting in corresponding direction
        if (down == "None") down <- "."
        if (across == "None") across <- "."
        p <- p + 
            facet_grid(stats::as.formula(paste(down, "~", across)), scales = "free")
    }
    p
}

#' Plot distribution for selected variable-year
#' 
#' To be run from \code{\link{run_visual_dive}} to show a bar plot distribution.
#' 
#' @inheritParams filter_demo
#' @param var name of variable to be plotted
#' @family data dive functions
#' @export
#' @examples 
#' library(dplyr)
#' data(hist_samp)
#' priv <- filter(hist_samp, priv == "all_sports", year == 2015)
#' plot_dist(priv, "age")
plot_dist <- function(priv, var = "sex") {
    dist <- priv %>%
        dplyr::count(.data[[var]]) %>%
        mutate(pct = n / sum(n) * 100)
        
    ggplot(dist, aes_string("var", "pct")) +
        geom_col() +
        theme(
            axis.title = element_blank(),
            axis.text.x = element_text(angle = 30, hjust = 1)
        )
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
#' data(hist_samp)
#' \dontrun{
#' run_visual_dive(hist_samp, pct = 1)
#' }
run_visual_dive <- function(hist_samp, pct = 10) {
    
    # define some ui menu selection options
    demos <- c("res", "sex", "R3", "age")
    years <- sort(unique(hist_samp$year), decreasing = TRUE)
    
    # define convenience functions for ui elements
    # - to prepare demographic filter checkboxes for given variable ("sex", etc.)
    ui_check_filter <- function(var = "sex", selected = levels(hist_samp[[var]])) {
        checkboxGroupInput(
            inputId = var, label = var, selected = selected, 
            choiceNames = as.list(selected), choiceValues = as.list(selected)
        )
    }
    # - to prepare plot facetting selections for given direction (down or across) 
    ui_check_facet <- function(direction = "down") {
        selectInput(
            inputId = direction, label = paste("Compare", direction),  
            choices = as.list(c("None", demos)), selected = "None"
        )
    }
    # - to  display distribution plot for given variable
    ui_plot_dist <- function(varPlot = "sexPlot") {
        plotly::plotlyOutput(varPlot, height = "200px")
    }
    
    ui <- fluidPage(mainPanel(
        splitLayout(
            selectInput("priv", "Choose Permission", unique(hist_samp$priv)),
            selectInput("metric", "Choose Metric", choices = c("participants", "churn"),
                        selected = "participants"),
            ui_check_facet("down"), ui_check_facet("across"),
            ui_prevent_clipping()
        ),
        splitLayout(
            actionButton("button", "APPLY FILTER"),
            ui_check_filter("res"), ui_check_filter("sex"), 
            ui_check_filter("R3"), ui_check_filter("age")
        ),
        plotly::plotlyOutput("trendPlot"),
        selectInput("year", "Select year for distributions", 
                    choices = years, selected = years[1]),
        splitLayout(
            ui_plot_dist("sexPlot"), ui_plot_dist("resPlot"),
            ui_plot_dist("agePlot"), ui_plot_dist("R3Plot"),
            cellWidths = c("20%", "20%", "40%", "20%")
        ),
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
        
        ### Plotting
        # - participants by year
        output$trendPlot <- render_dash({ function() {
            dataInput() %>%
                summarize_trend(input$down, input$across, input$metric, pct) %>% 
                plot_trend(input$down, input$across)
        }})
        
        # - distributions for selected year
        dataDist <- reactive({
            filter(dataInput(), .data$year == input$year)
        })
        output$sexPlot <- render_dash({
            function() plot_dist(dataDist(), "sex")
        })
        output$resPlot <- render_dash({
            function() plot_dist(dataDist(), "res")
        })
        output$agePlot <- render_dash({
            function() plot_dist(dataDist(), "age")
        })
        output$R3Plot <- render_dash({
            function() plot_dist(dataDist(), "R3")
        })
    }
    shinyApp(ui, server)
}
