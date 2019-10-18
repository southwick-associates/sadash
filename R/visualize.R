# functions for visualizing results in a shiny app

# Plotting Functions ------------------------------------------------------

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

#' Plot value by year for segment
#' 
#' Mostly a wrapper for \code{\link[dashtemplate]{plot_bar}} with some 
#' additional formatting.
#' 
#' @inheritParams dashtemplate::plot_value
#' @param n passed to \code{\link{int_breaks}} for x-axis labelling
#' @family functions to run dashboard visualization
#' @export
#' @examples 
#' library(dplyr)
#' data(dashboard)
#' filter(dashboard, group == "all_sports", quarter == 4) %>%
#'     plot_value2("Gender", "By Gender")
plot_value2 <- function(df, seg, plot_title = "", measure = "value", n = 5) {
    filter(df, .data$segment == seg) %>% 
        dashtemplate::plot_bar(plot_title, measure) +
        scale_x_continuous(breaks = function(x) int_breaks(x, n)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 2),
                           labels = scales::comma)
}

#' Make a sales by month plot
#' 
#' Intended to be run from \code{\link{run_visual}}. Expects a \code{\link{dashboard}} 
#' formatted table as input filtered to include a single group & quarter.
#' 
#' @param df data frame with summary results
#' @family functions to run dashboard visualization
#' @export
#' @examples 
#' library(dplyr)
#' data(dashboard)
#' filter(dashboard, group == "all_sports", quarter == 4) %>%
#'     plot_month()
plot_month <- function(df) {
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
            text = element_text(size = 15)
        ) +
        ggtitle("Sales by Month")
}


# County Plotting ---------------------------------------------------------

#' Pull county spatial data
#' 
#' This uses the urbnmapr package to pull a spatial features shapefile
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
        # "category" for joining with dashboard
        mutate(category = stringr::str_remove(.data$county_name, " County")) %>%
        select(.data$category, .data$geometry) %>%
        sf::st_simplify(preserveTopology = TRUE, dTolerance = dTolerance)
} 

#' Join dashboard with county spatial data
#' 
#' This takes the ouptut of  \code{\link{pull_county_sf}} and joins with 
#' \code{\link{dashboard}} data. The result is a list with county results
#' separated from all other results
#' 
#' @param dashboard summary \code{\link{dashboard}} data 
#' @param county_sf data produced by \code{\link{pull_county_sf}}
#' @family functions to run dashboard visualization
#' @export
#' @examples 
#' library(dplyr)
#' data(dashboard)
#' county_sf <- pull_county_sf("SC")
#' df <- join_county_sf(dashboard, county_sf)
#' 
#' # produce a warning by using the wrong state
#' county_sf <- pull_county_sf("ME")
#' df <- join_county_sf(dashboard, county_sf)
#' 
#' # Maine and South Carolina actually share one county name
#' df$county <- filter(df$county, group == "all_sports", quarter == 4)
#' plot_county(df$county)
join_county_sf <- function(dashboard, county_sf) {
    # split dashboard into a list (only county records need joining)
    df <- dashboard %>% mutate(
        is_county = ifelse(.data$segment == "County", "county", "other")
    )
    df <- split(df, df$is_county)
    
    # standardize category column for joining
    prep_category <- function(x) stringr::str_trim(tolower(x))
    df$county$category <- prep_category(df$county$category)
    county_sf$category <- prep_category(county_sf$category)
    
    # join & output
    df$county <- left_join(df$county, county_sf, by = "category")
    has_geom <- sf::st_dimension(sf::st_sfc(df$county$geometry))
    if (any(is.na(has_geom))) {
        no_county <- sum(is.na(has_geom))
        warning(
            "Missing geometries from county_sf for ", no_county, 
            " rows in the dashboard summary data\n", 
            "- These won't appear in run_visual() or plot_county()", call. = FALSE
        )
    }
    lapply(df, function(x) select(x, -.data$is_county))
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
#' df <- join_county_sf(dashboard, county_sf)
#' df$county <- filter(df$county, group == "all_sports", quarter == 4)
#' plot_county(df$county)
plot_county <- function(dat) {
    # function to plot a single metric
    plot_one <- function(dat_one, measure) {
        ggplot(dat_one, aes_string(fill = "value")) +
            geom_sf() +
            coord_sf(datum = NA) +
            theme(legend.position = "bottom", legend.key.width = unit(1, "cm")) +
            ggtitle(measure)
    }
    dat <- split(dat, dat$metric)
    plots <- lapply(names(dat), function(nm) plot_one(dat[[nm]], nm))
    gridExtra::grid.arrange(grobs = plots, nrow = 1)
}

# Shiny App Function ------------------------------------------------------

#' Run shiny app summary of dashboard results
#' 
#' The idea here is to replicate the functionality of the Tableau dashboard
#' to check/explore the results prior to sending to the Tableau analyst. 
#' 
#' @param dash_list a list produced after running \code{\link{join_county_sf}}
#' @family functions to run dashboard visualization
#' @export
#' @examples 
#' data(dashboard)
#' county_sf <- pull_county_sf("SC")
#' dash_list <- join_county_sf(dashboard, county_sf)
#' 
#' \dontrun{
#' run_visual(dash_list)
#' }
run_visual <- function(dash_list) {
    # prepare dash_list$other (i.e., all summaries except those by county)
    dash_list$other <- dash_list$other %>%
        mutate_at(c("segment", "metric", "category"), "tolower") %>%
        mutate(metric = ifelse(.data$metric == "participants", "part", .data$metric))
    
    # defining options for menu dropdowns
    quarters <- unique(dash_list$other$quarter)
    permissions <- unique(dash_list$other$group)
    years <- unique(dash_list$county$year)
    
    # convenience function for shiny plots    
    plot_dash <- function(x, height = "270px", ...) {
        plotOutput(x, height = height, ...)
    }
        
    # define user interface
    ui <- fluidPage(mainPanel(
        splitLayout(
            selectInput("quarter", "Choose Quarter", quarters),
            selectInput("group", "Choose Permission Group", permissions),
            selectInput("year", "Choose year for Counties", years),
            
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
        plot_dash("monthPlot", height = "130px"),
        plot_dash("countyPlot", height = "250px"),
        width = 12
    ))
    
    # define data selection & plotting
    server <- function(input, output, session) {
        
        # filtering data
        dataGroup <- reactive({
            dash_list$other %>%
                filter(.data$group == input$group, .data$quarter == input$quarter)
        })
        dataCounty <- reactive({
            dash_list$county %>%
                filter(.data$group == input$group, .data$quarter == input$quarter,
                       .data$year == input$year)
        })
        
        # plotting data
        output$allPlot <- renderPlot({ 
            plot_value2(dataGroup(), "all", "Overall")
        })
        output$residencyPlot <- renderPlot({ 
            plot_value2(dataGroup(), "residency", "By Residency", n = 4)
        })
        output$genderPlot <- renderPlot({ 
            plot_value2(dataGroup(), "gender", "By Gender", n = 4)
        })
        output$agePlot <- renderPlot({ 
            plot_value2(dataGroup(), "age", "By Age", n = 2)
        })
        output$monthPlot <- renderPlot({ 
            plot_month(dataGroup()) 
        })
        output$countyPlot <- renderPlot({
            plot_county(dataCounty())
        })
    }
    shinyApp(ui, server)
}
