# functions for visualizing results in a shiny app

#' Run shiny app summary of dashboard results
#' 
#' @param dashboard a dataframe formatted as a \code{\link[salic]{dashboard}}
#' table described in salic
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
            plotOutput("allPlot"), plotOutput("agePlot"), 
            cellWidths = c("35%", "65%")
        ),
        splitLayout(plotOutput("residencyPlot"), plotOutput("genderPlot")),
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
    }
    shinyApp(ui, server)
}
