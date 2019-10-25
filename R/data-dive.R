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
#' the timeframe has an equal chance of being selected.
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
#' A convenience function to ensure county_fips isn't populated where
#' res == 0 or is.na(res). We wouldn't want these showing up in the data dive.
#' 
#' @param x license history with county_fips & res variables
#' @family data dive functions
#' @export
set_nonres_county_na <- function(x) {
    # TODO: maybe include a summary here as well
    x$county_fips <- ifelse(is.na(x$res) | x$res == 0, NA_integer_, x$county_fips)
    x
}

#' Run shiny app version of data dive
#' 
#' This is a rough mock-up, intended to ensure (1) no surprises on the Tableau
#' end and (2) results look correct. 
#' 
#' @param hist_samp data frame with a sample of license history for all privileges 
#' containing 9 variables: priv, cust_id, year, lapse, R3, res, sex, fips, age
#' @param pct sample size (in whole percentage points) for hist_samp
#' @family data dive functions
#' @seealso \code{\link{run_visual}}
#' @export
#' @examples 
#' \dontrun{
#' f <- "E:/SA/Data-production/Data-Dashboards/WI/2015-q4/WI-data-dive-10pct-2015q4/priv-WI-10pct.csv"
#' hist_samp <- readr::read_csv(f, progress = FALSE)
#' run_visual_dive(hist_samp)
#' }
run_visual_dive <- function(hist_samp, pct = 10) {
    privs <- unique(hist_samp$priv)
    
    ui <- fluidPage(mainPanel(
        splitLayout(
            selectInput("priv", "Choose Permission", privs)
            # filtering options (sex, age, etc.)
        ),
        # splitLayout() # Metric, Compare Down, Compare Across
        plotly::plotlyOutput("trendPlot")
    ))
    
    server <- function(input, output, session) {
        dataPriv <- reactive({
            filter(hist_samp, .data$priv == input$priv)
        })
        output$trendPlot <- plotly::renderPlotly({
            p <- count(dataPriv(), year) %>%
                mutate(n = n / (pct / 100)) %>%
                ggplot(aes(year, n)) +
                geom_line()
            plotly::ggplotly(p) %>% plotly_config()
        })
        # TODO:
        # 1. add filtering options (sex, age, etc.)
        # 2. add side-panel distributions & county chloropleth
        # 3. add compare down/across facet options
        # 4. add metric select options (participants, churn)
    }
    shinyApp(ui, server)
}
