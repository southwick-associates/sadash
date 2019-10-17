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

# Shiny App Function ------------------------------------------------------

#' Run shiny app summary of dashboard results
#' 
#' The idea here is to replicate the functionality of the Tableau dashboard
#' to check/explore the results prior to sending to the Tableau analyst.
#' 
#' @param dashboard a dataframe formatted as a \code{\link{dashboard}} table
#' @family functions to run dashboard visualization
#' @export
#' @examples 
#' data(dashboard)
#' 
#' \dontrun{
#' run_visual(dashboard)
#' }
run_visual <- function(dashboard) {
    
    quarters <- unique(dashboard$quarter)
    permissions <- unique(dashboard$group)
    dashboard <- dashboard %>%
        mutate_at(c("segment", "metric", "category"), "tolower") %>%
        mutate(metric = ifelse(.data$metric == "participants", "part", .data$metric))
    
    # convenience function for shiny plots    
    plot_dash <- function(x, height = "300px", ...) {
        plotOutput(x, height = height, ...)
    }
        
    # define user interface
    ui <- fluidPage(mainPanel(
        splitLayout(
            selectInput("quarter", "Choose Quarter", quarters),
            selectInput("group", "Choose Permission Group", permissions),
            
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
        width = 12
    ))
    
    # define data selection & plotting
    server <- function(input, output, session) {
        
        # filtering data
        dataGroup <- reactive({
            filter(dashboard, .data$group == input$group, 
                   .data$quarter == input$quarter)
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
        output$monthPlot <- renderPlot({ plot_month(dataGroup()) })
    }
    shinyApp(ui, server)
}
