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
#' hist_samp <- salic::label_categories(hist_samp)
#' hist_samp <- salic::df_factor_age(hist_samp)
#' run_visual_dive(hist_samp)
#' }
run_visual_dive <- function(hist_samp, pct = 10) {
    
    # prepare demographic filter checkboxes
    demos <- c("res", "sex", "R3", "age")
    check_filter <- function(var, selected = levels(hist_samp[[var]])) {
        checkboxGroupInput(
            inputId = var, label = var, selected = selected, 
            choiceNames = as.list(selected), choiceValues = as.list(selected)
        )
    }
    # prepare plot facetting selections (down or across)
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
        dataPriv <- reactive({
            filter(hist_samp, .data$priv == input$priv)
        })
        # demographic filtering: function to run for each variable in dataInput
        filter_var <- function(priv, var, var_lab = levels(hist_samp[[var]])) {
            if (identical(input[[var]], var_lab)) {
                return(priv) # no filter if all options are checked
            }
            if (var == "R3") {
                # R3 will only be NA for the first 5 years
                # so NA values should be removed if R3 filter is applied
                return(filter(priv, .data[[var]] %in% input[[var]]))
            }
            # we want to keep NA values for demographic variables
            # - does obscure NAs, but not obvious how to make that work (low priority)
            filter(priv, .data[[var]] %in% c(input[[var]], NA))
        }
        dataInput <- reactive({
            # isolate ensures filter only runs after the button is pressed
            input$button
            priv <- dataPriv()
            isolate({
                for (var in demos) priv <- filter_var(priv, var)
                priv
            })
        })
        
        ### Grouping (for plot facets) & participants by year
        # - use NULL where no group is selected (for dplyr filters to work properly)
        downInput <- reactive({
            if (input$down == "None") NULL else input$down
        })
        acrossInput <- reactive({
            if (input$across == "None") NULL else input$across
        })
        # - get counts by year (and selected groups if applicable)
        tblInput <- reactive({
            x <- dataInput()
            
            # drop NAs for group variables
            if (!is.null(downInput())) x <- x[!is.na(x[[downInput()]]), ]
            if (!is.null(acrossInput())) x <- x[!is.na(x[[acrossInput()]]), ]
            
            x %>% 
                group_by_(.dots = c(downInput(), acrossInput(), "year")) %>%
                summarize(n = n()) %>%
                ungroup() %>%
                mutate(n = n / (pct / 100)) 
        })
        
        ### Plotting
        output$trendPlot <- plotly::renderPlotly({
            p <- tblInput() %>%
                ggplot(aes(year, n)) +
                geom_line()
            if (!is.null(downInput()) || !is.null(acrossInput())) {
                # add facetting as needed
                grp1 <- downInput()
                grp2 <- acrossInput()
                if (is.null(grp1)) grp1 <- "."
                if (is.null(grp2)) grp2 <- "."
                p <- p + 
                    facet_grid(stats::as.formula(paste(grp1, "~", grp2)), 
                               scales = "free")
            }
            plotly::ggplotly(p) %>% plotly_config()
        })
    }
    shinyApp(ui, server)
}
