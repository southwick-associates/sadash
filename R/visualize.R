# functions for visualizing results in a shiny app

# Plotting Functions ------------------------------------------------------

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
    df %>%
        filter(.data$segment == "month") %>%
        mutate(
            category = as.numeric(.data$category), 
            year = as.character(.data$year)
        ) %>%
        ggplot(aes_string("category", "value", fill = "year")) +
        geom_col(position = position_dodge()) +
        facet_wrap(~ metric, scales = "free_y") +
        scale_fill_brewer(type = "qual", palette = 7) +
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
        mutate_at(c("segment", "metric", "category"), "tolower")
    
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
            plotOutput("allPlot", height = "350px"), 
            plotOutput("agePlot", height = "350px"), 
            cellWidths = c("35%", "65%")
        ),
        splitLayout(
            plotOutput("residencyPlot", height = "350px"), 
            plotOutput("genderPlot", height = "350px")
        ),
        plotOutput("monthPlot", height = "150px"),
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
        output$allPlot <- renderPlot({ dashtemplate::plot_value(
            dataGroup(), "all", "Overall"
        )})
        output$residencyPlot <- renderPlot({ dashtemplate::plot_value(
            dataGroup(), "residency", "By Residency"
        )})
        output$genderPlot <- renderPlot({ dashtemplate::plot_value(
            dataGroup(), "gender", "By Gender"
        )})
        output$agePlot <- renderPlot({ dashtemplate::plot_value(
            dataGroup(), "age", "By Age"
        )})
        output$monthPlot <- renderPlot({ plot_month(dataGroup()) })
    }
    shinyApp(ui, server)
}
