# producing data dive input for Tableau

#' Load a 10 percent sample of all sportspersons
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

#' Set cust$county_fips to missing if from another state
#' 
#' We may have counties from entirely different states attached to the customer
#' table and we don't want these in the data dive
#' 
#' @param cust customer table
#' @param state 2-character state abbreviation
#' @param county_map function to use for pulling a list of counties in state
#' @family data dive functions
#' @export
#' @examples 
#' library(dplyr)
#' 
#' \dontrun{
#' db_history <- "E:/SA/Data-production/Data-Dashboards/IA/history.sqlite3"  
#' db_license <- "E:/SA/Data-production/Data-Dashboards/IA/license.sqlite3"
#' cust_samp <- left_join(
#'     load_cust_samp(db_history, 2006:2018),
#'     load_cust(db_license)
#' )
#' count(cust_samp, county_fips)
#' set_other_county_na(cust_samp, "IA") %>% count(county_fips)
#' }
set_other_county_na <- function(
    cust, state, county_map = function() get_county_map_dive(state, FALSE)
) {
    counties <- county_map() %>% 
        pull(.data$fips) %>% 
        unique()
    
    # check for counties from other states
    x <- filter(cust, !is.na(.data$county_fips))
    if (all(x$county_fips %in% counties)) {
        return(cust)
    }
    other_states <- filter(x, !.data$county_fips %in% counties)
    message("There were ", nrow(other_states), " cust$county_fips from ",
            "other states set to missing.")
    
    cust %>% mutate(
        county_fips = case_when(
            county_fips %in% counties ~ .data$county_fips,
            TRUE ~ NA_integer_
        )
    )
}

#' Set county_fips to missing if nonresident
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

# App Prep/Plotting ---------------------------------------------------------

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
    # - Note that this does obscure NAs from appearing in the checkboxes
    #   but not obvious how to (easily) make that work with filtering (low priority)
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
        # churn
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
        geom_col() +
        theme(axis.title = element_blank())
    
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
            # axis.text.x = element_text(angle = 20, hjust = 1),
            axis.title = element_blank()
        )
}

#' Remove the state portion of county fips
#' 
#' Tableau seems to require just the county portion of the code for a given state.
#' 
#' @param x vector that holds the 5-digit fips code (e.g., 19001 for Iowa 
#' Adair county)
#' @family data dive functions
#' @export
#' @examples 
#' \dontrun{
#' db_license <- f <- "E:/SA/Data-production/Data-Dashboards/IA/license.sqlite3"
#' fips <- load_cust(db_license) %>% pull(county_fips)
#' fips_new <- drop_state_code(fips)
#' df <- data.frame(fips, fips_new, stringsAsFactors = FALSE)
#' head(df)
#' filter(df, nchar(fips) == 4) %>% tail()
#' 
#' # stop with error
#' drop_state_code(fips_new)
#' }
drop_state_code <- function(x) {
    # full fips codes (state + county) will be length 5
    # (or 4 if the leading zero for a state like AL is stripped)
    fips_nchar <- unique(nchar(x[!is.na(x)]))
    if (any(!fips_nchar %in% c(4,5))) {
        stop("Running drop_state_code() likely won't produce correct results ",
             "because some of the codes have < 4 characters (or > 5)", call. = FALSE)
    }
    
    # we just want the last 3 digits
    # - these should have leading zeroes, but we want these stripped in the output
    start_point <- nchar(x) - 2
    stringr::str_sub(x, start = start_point) %>% as.integer()
}

#' Load county spatial data for data dive
#' 
#' Mostly a wrapper for \code{\link{get_county_map}} with some formatting used
#' in data dive.
#' 
#' @inheritParams get_county_map
#' @param drop_state_code If TRUE, the 2-digit state code will be stripped. This
#' may be necessary depending on how the county_fips is stored in the license
#' data.
#' @family data dive functions
#' @export
#' @examples 
#' get_county_map_dive("AL")
#' get_county_map_dive("AL", drop_state_code = FALSE)
get_county_map_dive <- function(state, drop_state_code = TRUE) {
    x <- get_county_map(state)
    
    if (drop_state_code) {
        x <- mutate(x, fips = drop_state_code(.data$county_fips))
    } else {
        x <- rename(x, fips = .data$county_fips)
    }
    select(x, .data$fips, .data$county, .data$long, .data$lat)
}

#' Chloropleth county plot for data dive
#' 
#' @inheritParams filter_demo
#' @inheritParams summarize_trend
#' @inheritParams run_visual_dive
#' @param county_map spatial table produced by \code{\link{get_county_map_dive}}
#' @family data dive functions
#' @export
#' @examples 
#' library(dplyr)
#' data(hist_samp)
#' priv <- filter(hist_samp, priv == "all_sports", year == 2014)
#' county_map <- get_county_map_dive("WI")
#' plot_county_dive(priv, county_map, pct = 1)
#' plot_county_dive(priv, county_map, "churn")
plot_county_dive <- function(priv, county_map, metric = "participants", pct = 10) {
    priv <- priv %>%
        filter(!is.na(.data$fips)) %>%
        group_by(.data$fips)
    
    if (metric == "participants") {
        tbl <- priv %>%
            summarize(value = n()) %>%
            ungroup() %>%
            mutate(value = .data$value / (pct / 100))
    } else {
        # churn
        tbl <- priv %>%
            summarise(value = round(mean(lapse) * 100), 3) %>%
            ungroup()
    }
    
    left_join(tbl, county_map, by = "fips") %>%
        ggplot() +
        geom_polygon(aes_string("long", "lat", group = "county", fill = "value")) +
        theme(
            axis.text = element_blank(), 
            axis.title = element_blank(),
            axis.ticks = element_blank()
        ) +
        guides(fill = FALSE)
}

# Shiny App ---------------------------------------------------------------

#' Run shiny app version of data dive
#' 
#' This is a rough mock-up, intended to ensure (1) no surprises on the Tableau
#' end and (2) results look correct. 
#' 
#' @param hist_samp data frame with a sample of license history for all privileges 
#' containing at least 9 variables: priv, cust_id, year, lapse, R3, res, sex, fips, age
#' @param pct sample size (in whole percentage points) for hist_samp
#' @inheritParams plot_county_dive
#' @family data dive functions
#' @seealso \code{\link{run_visual}}
#' @export
#' @examples 
#' data(hist_samp)
#' county_map <- get_county_map_dive("WI")
#' 
#' \dontrun{
#' run_visual_dive(hist_samp, county_map, pct = 1)
#' }
run_visual_dive <- function(hist_samp, county_map, pct = 10) {
    
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
    # - to display distribution plot for given variable
    ui_plot_dist <- function(varPlot = "sexPlot") {
        plotly::plotlyOutput(varPlot, height = "150px")
    }
    
    ui <- fluidPage(mainPanel(
        # data filtering
        splitLayout(
            actionButton("button", "APPLY FILTERS"),
            ui_check_filter("res"), ui_check_filter("sex"), 
            ui_check_filter("R3"), ui_check_filter("age")
        ),
        
        # menu drop-downs
        splitLayout(
            selectInput("priv", "Choose Permission", unique(hist_samp$priv)),
            selectInput("metric", "Choose Metric", choices = c("participants", "churn"),
                        selected = "participants"),
            ui_check_facet("down"), ui_check_facet("across"),
            ui_prevent_clipping()
        ),
        
        # plotting
        plotly::plotlyOutput("trendPlot", height = "400px"), 
        
        column(4, 
            selectInput("year", "Year (Distributions & County)",  
                        choices = years, selected = years[1]),
            plotly::plotlyOutput("countyPlot", height = "220px")
        ),
        column(8,
            splitLayout(ui_plot_dist("sexPlot"), ui_plot_dist("agePlot"),
                        cellWidths = c("35%", "65%")),
            splitLayout(ui_plot_dist("resPlot"), ui_plot_dist("R3Plot"),
                        cellWidths = c("48%", "52%"))
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
        
        # - county plot for selected year
        dataCounty <- reactive({
            # churn lags by one year
            if (input$metric == "churn") {
                filter_year = as.numeric(input$year) - 1 
                filter(dataInput(), .data$year == filter_year)
            } else {
                dataDist()
            }
        })
        output$countyPlot <- render_dash({
            function() plot_county_dive(dataCounty(), county_map, input$metric, pct)
        })
    }
    shinyApp(ui, server)
}
