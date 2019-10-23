# functions for visualizing results in a shiny app

# Bar Plotting ------------------------------------------------------

#' Use only integer values for a plot's axis labels
#' 
#' A helper function to be included in \code{\link{plot_month}} and
#' \code{\link{plot_value2}} to ensure axis labels don't include non-integer
#' values.
#' 
#' @param x placeholder of vector to be plotted
#' @param n integer value to control 
#' @family functions to run dashboard visualization
#' @export
int_breaks <- function(x, n = 5) {
    pretty(x, n)[pretty(x, n) %% 1 == 0]
}

#' Plot value by year for metric-category
#' 
#' Mostly a wrapper for \code{\link[dashtemplate]{plot_bar}} with some 
#' additional formatting. It's expected that the input table will only contain
#' a single group-quarter-segment.
#' 
#' @inheritParams dashtemplate::plot_value
#' @param n passed to \code{\link{int_breaks}} for x-axis labelling
#' @family functions to run dashboard visualization
#' @export
#' @examples 
#' library(dplyr)
#' data(dashboard)
#' x <- filter(dashboard, group == "all_sports", quarter == 4, segment == "Gender")
#' plot_value2(x)
plot_value2 <- function(df, plot_title = "", measure = "value", n = 5) {
    df %>% 
        dashtemplate::plot_bar(plot_title, measure) +
        scale_x_continuous(breaks = function(x) int_breaks(x, n)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 2),
                           labels = scales::comma) +
        theme(text = element_text(size = 10), plot.title = element_blank())
}

#' Make a sales by month plot
#' 
#' Intended to be run from \code{\link{run_visual}}. Expects a \code{\link{dashboard}} 
#' formatted table as input filtered to include a single group & quarter.
#' 
#' @param df data frame with summary results
#' @inheritParams plot_value2
#' @family functions to run dashboard visualization
#' @export
#' @examples 
#' library(dplyr)
#' data(dashboard)
#' filter(dashboard, group == "all_sports", quarter == 4) %>%
#'     plot_month()
plot_month <- function(df, plot_title = "Sales by Month") {
    dat <- df %>%
        filter(.data$segment == "month") %>%
        mutate(
            category = as.integer(.data$category), 
            year = as.character(.data$year)
        )
    ggplot(dat, aes_string("category", "value", fill = "year")) +
        geom_col(position = position_dodge()) +
        facet_wrap(~ metric, scales = "free_y") +
        scale_fill_brewer(type = "qual", palette = 7) +
        scale_x_continuous(breaks = int_breaks) +
        theme(
            axis.title = element_blank(),
            text = element_text(size = 10)
        ) +
        ggtitle(plot_title)
}

# County Plotting ---------------------------------------------------------

#' Load county spatial data for selected state
#' 
#' @param state abbreviation of state to pull
#' @family functions to run dashboard visualization
#' @export
#' @examples 
#' library(ggplot2)
#' county_map <- get_county_map("SC")
#' ggplot(county_map) + 
#'     geom_polygon(aes(long, lat, group = county))
get_county_map <- function(state) {
    utils::data("county_map_us", envir = environment())
    county_map_us %>%
        rename(state_abbrev = .data$state) %>%
        filter(.data$state_abbrev == state) %>%
        select(.data$long, .data$lat, .data$county, .data$county_fips)
}

#' Join dashboard with county spatial data
#' 
#' This takes the output of \code{\link{get_county_map}} and joins with 
#' \code{\link{dashboard}} data. The result is a list split by segment. The 
#' county_census table is used for linking on a more precise variable (county_fips
#' as oppossed to county name).
#' 
#' @param dashboard summary \code{\link{dashboard}} data 
#' @param county_map data produced by \code{\link{get_county_map}}
#' @param county_census county names by fips, to provided more precise joining
#' between dashboard results and county_sf shapefile
#' @family functions to run dashboard visualization
#' @export
#' @examples 
#' library(dplyr)
#' data(dashboard)
#' 
#' county_map <- get_county_map("SC")
#' county_census <- load_counties(state = "SC")
#' dash_list <- join_county_map(dashboard, county_map, county_census)
#' 
#' # produce a warning by using the wrong state
#' county_map <- get_county_map("ME")
#' county_census <- load_counties(state = "ME")
#' dash_list <- join_county_map(dashboard, county_map, county_census)
#' 
#' # Maine and South Carolina actually share one county name
#' x <- filter(dash_list$county, group == "all_sports", quarter == 4)
#' plot_county(x) %>% gridExtra::grid.arrange(grobs = .)
join_county_map <- function(dashboard, county_map, county_census) {
    
    # split dashboard by segment
    df <- mutate(dashboard, segment = tolower(.data$segment)) 
    df <- split(df, df$segment)
    
    # join county_fips to dashboard data
    county_census <- county_census %>%
        rename(category = .data$county)
    df$county <- left_join(df$county, county_census, by = "category")
    if (any(is.na(df$county$county_fips))) {
        no_fips <- sum(is.na(df$county$county_fips))
        warning(
            "Missing county_fips from county_census for ", no_fips, 
            " rows in the dashboard summary data\n",
            "- These won't appear in run_visual() or plot_county()", call. = FALSE 
        )
    }
    
    # join map data with dashboard data
    df$county <- left_join(df$county, county_map, by = "county_fips")
    if (any(is.na(df$county$long))) {
        no_geom <- df$county$county_fips[is.na(df$county$long)] %>% unique()
        warning(
            "Missing geometries from county_map for ", length(no_geom), 
            " rows in the dashboard summary data\n", 
            "- These won't appear in run_visual() or plot_county()", call. = FALSE
        )
    }
    df
}

#' Make a county chloropleth for all metrics
#' 
#' Intended to be run on the output of \code{\link{join_county_map}} (the 
#' "county" element specifically). Returns a list with one element per value 
#' of the dat$metric variable.
#' 
#' @param dat a dashboard table where segment == "County" that has a "geometry"
#' column (e.g., produced from \code{\link{join_county_map}})
#' @family functions to run dashboard visualization
#' @export
#' @examples 
#' library(dplyr)
#' data(dashboard)
#' county_map <- get_county_map("SC")
#' county_census <- load_counties(state = "SC")
#' dash_list <- join_county_map(dashboard, county_map, county_census)
#' 
#' x <- filter(dash_list$county, group == "all_sports", quarter == 4)
#' p <- plot_county(x)
#' p$churn
#' gridExtra::grid.arrange(grobs = p)
#' 
#' # interactive with plotly
#' plotly::ggplotly(p$churn)
#' plotly::subplot(p, nrows = 2) %>% plotly::hide_colorbar()
plot_county <- function(dat) {
    # function to plot a single metric
    plot_one <- function(dat_one, measure) {
        ggplot(dat_one) +
            geom_polygon(aes_string("long", "lat", group = "category", fill = "value")) +
            facet_wrap(~ metric) +
            theme(
                axis.text = element_blank(), 
                axis.title = element_blank(),
                axis.ticks = element_blank()
            )
    }
    dat <- split(dat, dat$metric)
    sapply(names(dat), function(nm) plot_one(dat[[nm]], nm), simplify = FALSE)
}

# Shiny Convenience ---------------------------------------------------

#' Convenience functions for shiny plots
#' 
#' Use plotly_config() to remove extra plotly buttons. Use plot_dash() and
#' render_dash() for the shiny UI and Server respectively.
#' 
#' @param plot Plot object for plotly
#' @param plot_code Code passed to \code{\link[plotly]{renderPlotly}}
#' @param height passed to \code{\link[plotly]{plotlyOutput}}
#' @param ... other arguments passed to \code{\link[plotly]{plotlyOutput}}
#' @family functions to run dashboard visualization
#' @export
plotly_config <- function(plot) {
    modebar_remove <- c(
        "pan2d", "zoomIn2d", "sendDataToCloud", "zoomOut2d", "autoScale2d", 
        "zoom2d", "hoverClosestCartesian", "hoverCompareCartesian", "resetScale2d",
        "toggleSpikelines", "lasso2d", "select2d"
    )
    plotly::config(
        plot,
        modeBarButtonsToRemove = modebar_remove,  
        collaborate = FALSE, 
        displaylogo = FALSE
    ) %>%
        plotly::layout(yaxis = list(hoverformat = ".2f"))
}

#' @rdname plotly_config
#' @export
plot_dash <- function(plot, height = "350px", ...) {
    plotly::plotlyOutput(plot, height = height, ...)
}

#' @rdname plotly_config
#' @export
render_dash <- function(plot_code) {
    plotly::renderPlotly({
        p <- plot_code()
        plotly::ggplotly(p) %>% plotly_config()
    })
}

#' Define UI layout for run_visual()
#' 
#' A convenience function run from \code{\link{run_visual}} and
#' \code{\link{run_visual_county}}
#' 
#' @inheritParams run_visual
#' @family functions to run dashboard visualization
#' @export
ui_button_layout <- function(dash_list) {
    # defining options for menu dropdowns
    quarters <- unique(dash_list$all$quarter)
    permissions <- unique(dash_list$all$group)
    years <- unique(dash_list$county$year)
    
    splitLayout(
        selectInput("quarter", "Choose Quarter", quarters),
        selectInput("group", "Choose Permission Group", permissions),
        selectInput("year", "Choose Year (for month/county)", years),
        
        # prevent clipping: https://github.com/rstudio/shiny/issues/1531
        tags$head(tags$style(HTML(
            ".shiny-split-layout > div {overflow: visible;}"
        )))
    )
}

# Shiny App ------------------------------------------------------

#' @rdname run_visual
#' @export
run_visual_county <- function(dash_list) {
    ui <- fluidPage(mainPanel(
        ui_button_layout(dash_list),
        plot_dash("countyPlot", height = "650px"),
        width = 12
    ))
    
    server <- function(input, output, session) {
        dataCounty <- reactive({
            filter(dash_list$county, .data$group == input$group, 
                   .data$quarter == input$quarter, .data$year == input$year)
        })
        output$countyPlot <- plotly::renderPlotly({
            p <- plot_county(dataCounty())
            plotly::subplot(p, nrows = 2) %>% 
                plotly::hide_colorbar() %>%
                plotly_config()
        })
    }
    shinyApp(ui, server)
}

#' Run shiny app summary of dashboard results
#' 
#' The idea here is to replicate the functionality of the Tableau dashboard
#' to check/explore the results prior to sending to the Tableau analyst. 
#' 
#' @param dash_list a list produced after running \code{\link{join_county_map}}
#' @family functions to run dashboard visualization
#' @export
#' @examples 
#' data(dashboard)
#' county_map <- get_county_map("SC")
#' county_census <- load_counties(state = "SC")
#' dash_list <- join_county_map(dashboard, county_map, county_census)
#' 
#' \dontrun{
#' run_visual(dash_list)
#' 
#' # including county makes things a bit slow currently
#' run_visual_county(dash_list)
#' }
run_visual <- function(dash_list) {
    
    ### Prepare Data
    # some minor formatting for dash_list
    dash_prep <- function(x) {
        mutate_at(x, c("metric", "category"), "tolower") %>%
        mutate(metric = ifelse(.data$metric == "participants", "part", .data$metric))
    }
    dash_list <- lapply(dash_list, dash_prep)
    
    ### Define user interface
    ui <- fluidPage(mainPanel(
        ui_button_layout(dash_list),
        splitLayout(
            plot_dash("allPlot"), plot_dash("agePlot"),
            cellWidths = c("35%", "65%")
        ),
        splitLayout(
            plot_dash("residencyPlot"), plot_dash("genderPlot")
        ),
        plot_dash("monthPlot", height = "170px"),
        width = 12
    ))
    
    ### Define data selection & plotting
    server <- function(input, output, session) {
        
        # By gender, age, etc.
        dataGroup <- reactive({
            flt <- function(x) {
                filter(x, .data$group == input$group, .data$quarter == input$quarter)
            }
            lapply(dash_list, flt)
        })
        
        output$allPlot <- render_dash({
            function() plot_value2(dataGroup()[["all"]])
        })
        output$residencyPlot <- render_dash({
            function() plot_value2(dataGroup()[["residency"]], n = 4)
        })
        output$genderPlot <- render_dash({
            function() plot_value2(dataGroup()[["gender"]], n = 4)
        })
        output$agePlot <- render_dash({
            function() plot_value2(dataGroup()[["age"]], n = 2)
        })
        
        # Sales by month
        dataMonth <- reactive({
            dash_list$month %>%
                filter(.data$group == input$group, .data$quarter == input$quarter,
                       .data$year %in% c(input$year, as.numeric(input$year) - 1))
        })
        output$monthPlot <- render_dash({
            function() plot_month(dataMonth(), "")
        })
    }
    shinyApp(ui, server)
}
