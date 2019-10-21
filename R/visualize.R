# functions for visualizing results in a shiny app

# Bar Plotting Functions ------------------------------------------------------

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

#' Define the modebar config for plotly
#' 
#' Basically I want to remove the extra cruft at the top of the plots, except 
#' for the "save png" button.
#' 
#' @param plot Plot object for plotly
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
    ) 
}

# County Plotting ---------------------------------------------------------

#' Pull county spatial data
#' 
#' This uses the urbnmapr package to pull a spatial features table with 1 row
#' per county. The geometry variable is a special type: a list of class
#' "sfc_GEOMETRY". The \code{\link[ggplot2]{geom_sf}} function will recognize
#' this variable for map plotting purposes.
#' 
#' @param state abbreviation of state to pull
#' @param dTolerance passed to \code{\link[sf]{st_simplify}} to reduce object
#' size at the expense of detail (higher number is less detail)
#' @family functions to run dashboard visualization
#' @export
#' @examples 
#' library(ggplot2)
#' county_sf <- pull_county_sf("SC")
#' ggplot(county_sf, aes()) + geom_sf(fill = "grey")
pull_county_sf <- function(state, dTolerance = 1000) {
    urbnmapr::get_urbn_map("counties", sf = TRUE) %>%
        filter(.data$state_abbv == state) %>%
        select(.data$county_fips, .data$geometry) %>%
        mutate(county_fips = as.integer(.data$county_fips)) %>%
        sf::st_simplify(preserveTopology = TRUE, dTolerance = dTolerance)
} 

#' Join dashboard with county spatial data
#' 
#' This takes the ouptut of  \code{\link{pull_county_sf}} and joins with 
#' \code{\link{dashboard}} data. The result is a list split by segment, 
#' where the county element includes an extra column: geometry. The 
#' county_census table is used for linking on a more precise variable (county_fips
#' as oppossed to county name).
#' 
#' @param dashboard summary \code{\link{dashboard}} data 
#' @param county_sf data produced by \code{\link{pull_county_sf}}
#' @param county_census county names by fips, to provided more precise joining
#' between dashboard results and county_sf shapefile
#' @family functions to run dashboard visualization
#' @export
#' @examples 
#' library(dplyr)
#' data(dashboard)
#' 
#' county_sf <- pull_county_sf("SC")
#' county_census <- load_counties(state = "SC")
#' df <- join_county_sf(dashboard, county_sf, county_census)
#' 
#' # produce a warning by using the wrong state
#' county_sf <- pull_county_sf("ME")
#' county_census <- load_counties(state = "ME")
#' df <- join_county_sf(dashboard, county_sf, county_census)
#' 
#' # Maine and South Carolina actually share one county name
#' df$county <- filter(df$county, group == "all_sports", quarter == 4)
#' plot_county(df$county)
join_county_sf <- function(dashboard, county_sf, county_census) {
    
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
    
    # join geometry with dashboard data
    df$county <- left_join(df$county, county_sf, by = "county_fips")
    has_geom <- sf::st_dimension(sf::st_sfc(df$county$geometry))
    if (any(is.na(has_geom))) {
        no_county <- sum(is.na(has_geom))
        warning(
            "Missing geometries from county_sf for ", no_county, 
            " rows in the dashboard summary data\n", 
            "- These won't appear in run_visual() or plot_county()", call. = FALSE
        )
    }
    df
}

#' Make a county chloropleth for all metrics
#' 
#' Intended to be run on the output of \code{\link{join_county_sf}} (the 
#' "county" element specifically). Uses \code{\link[gridExtra]{grid.arrange}}
#' to include a set of side-by-side chloropleths for every metric.
#' 
#' @inheritParams dashtemplate::plot_bar
#' @param dat a dashboard table where segment == "County" that has a "geometry"
#' column (e.g., produced from \code{\link{join_county_sf}})
#' @family functions to run dashboard visualization
#' @export
#' @examples 
#' library(dplyr)
#' data(dashboard)
#' county_sf <- pull_county_sf("SC")
#' county_census <- load_counties(state = "SC")
#' df <- join_county_sf(dashboard, county_sf, county_census)
#' 
#' df$county <- filter(df$county, group == "all_sports", quarter == 4)
#' plot_county(df$county)
plot_county <- function(dat) {
    # function to plot a single metric
    plot_one <- function(dat_one, measure) {
        ggplot(dat_one, aes_string(fill = "value")) +
            geom_sf() +
            coord_sf(datum = NA)  +
            theme(legend.position = "bottom", legend.key.width = unit(1, "cm")) +
            ggtitle(measure)
    }
    dat <- split(dat, dat$metric)
    plots <- lapply(names(dat), function(nm) plot_one(dat[[nm]], nm))
    # plotly::subplot(plots, nrows = 1)
    gridExtra::grid.arrange(grobs = plots, nrow = 1)
}

# Shiny App Function ------------------------------------------------------

#' Run shiny app summary of dashboard results
#' 
#' The idea here is to replicate the functionality of the Tableau dashboard
#' to check/explore the results prior to sending to the Tableau analyst. 
#' 
#' @param dash_list a list produced after running \code{\link{join_county_sf}}
#' @param include_county if TRUE, county chloropleths will also be displayed
#' @family functions to run dashboard visualization
#' @export
#' @examples 
#' data(dashboard)
#' county_sf <- pull_county_sf("SC")
#' county_census <- load_counties(state = "SC")
#' dash_list <- join_county_sf(dashboard, county_sf, county_census)
#' 
#' \dontrun{
#' run_visual(dash_list)
#' 
#' # including county makes things a bit slow currently
#' run_visual(dash_list, include_county = TRUE)
#' }
run_visual <- function(dash_list, include_county = FALSE) {
    # some minor formatting for dash_list
    dash_prep <- function(x) {
        mutate_at(x, c("metric", "category"), "tolower") %>%
        mutate(metric = ifelse(.data$metric == "participants", "part", .data$metric))
    }
    dash_list <- lapply(dash_list, dash_prep)
    
    # the data are separated based on their filtering needs
    df_month <- dash_list[["month"]]
    df_county <- dash_list[["county"]]
    ls_other <- dash_list[setdiff(names(dash_list), c("month", "county"))]
    
    # defining options for menu dropdowns
    quarters <- unique(ls_other$all$quarter)
    permissions <- unique(ls_other$all$group)
    years <- unique(df_county$year)
    
    # convenience functions for shiny plots    
    # - for ui
    plot_dash <- function(x, height = "300px", ...) {
        plotly::plotlyOutput(x, height = height, ...)
    }
    # - for server
    render_dash <- function(plot_code) {
        plotly::renderPlotly({
            p <- plot_code()
            plotly::ggplotly(p) %>% plotly_config()
        })
    }
        
    # define user interface
    ui <- fluidPage(mainPanel(
        splitLayout(
            selectInput("quarter", "Choose Quarter", quarters),
            selectInput("group", "Choose Permission Group", permissions),
            selectInput("year", "Choose year for Counties/Months", years),
            
            # prevent clipping: https://github.com/rstudio/shiny/issues/1531
            tags$head(tags$style(HTML(
                ".shiny-split-layout > div {overflow: visible;}"
            )))
        ),
        splitLayout(
            plot_dash("allPlot"), plot_dash("agePlot"),
            cellWidths = c("35%", "65%")
        ),
        splitLayout(
            plot_dash("residencyPlot"), plot_dash("genderPlot")
        ),
        plot_dash("monthPlot", height = "150px"),
        if (include_county) plotOutput("countyPlot", height = "250px"),
        width = 12
    ))
    
    # define data selection & plotting
    server <- function(input, output, session) {
        
        # filtering data
        dataGroup <- reactive({
            flt <- function(x) {
                filter(x, .data$group == input$group, .data$quarter == input$quarter)
            }
            lapply(ls_other, flt)
        })
        
        dataMonth <- reactive({
            df_month %>%
                filter(.data$group == input$group, .data$quarter == input$quarter,
                       .data$year %in% c(input$year, as.numeric(input$year) - 1))
        })
        
        if (include_county) {
            dataCounty <- reactive({
                df_county %>%
                    filter(.data$group == input$group, .data$quarter == input$quarter,
                           .data$year == input$year)
            })
        }
        
        # plotting data
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
        output$monthPlot <- render_dash({
            function() plot_month(dataMonth(), "")
        })
        if (include_county) {
            output$countyPlot <- renderPlot({ plot_county(dataCounty()) })
        }
    }
    shinyApp(ui, server)
}
