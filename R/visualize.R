# functions to run dashboard visualization using shiny & ggplot2

# Plotting Functions ------------------------------------------------------

# make a bar plot: measure by year (facetted using metric & category)
# - df: data frame with summary results
# - measure: variable to be plotted on the y axis
plot_bar <- function(df, plot_title = "", measure = "value") {
    df %>%
        ggplot(aes_string("year", measure, fill = "metric")) +
        geom_col() +
        facet_grid(metric ~ category, scales = "free_y") +
        scale_fill_brewer(type = "qual", palette = 7) +
        theme(
            axis.title = element_blank(),
            text = element_text(size = 15),
            legend.position = "none"
        ) +
        ggtitle(plot_title)
}

# plot value by year for a given segment
# - seg: segment to include in plot (e.g., "gender")
plot_value <- function(df, seg, plot_title = "", measure = "value") {
    filter(df, segment == seg) %>%
        plot_bar(plot_title, measure) +
        scale_y_continuous(labels = scales::comma)
}

# plot % change by year for a given segment
# - pct_range: y-axis range
plot_pct <- function(
    df, seg, plot_title = "", measure = "pct_change", pct_range = 0.5
) {
    x <- filter(df, segment == seg) %>%
        group_by(group, metric, category) %>%
        arrange(year) %>%
        mutate(pct_change = (value - lag(value)) / lag(value)) %>%
        ungroup() %>%
        filter(!is.na(pct_change))
    x %>%
        plot_bar(plot_title, measure) +
        scale_y_continuous(labels = scales::percent) +
        coord_cartesian(ylim = c(-pct_range/2, pct_range/2)) +
        geom_hline(yintercept = 0, color = "gray47")
}

# wrapper function: run either "value" or "pct_change"
plot_segment <- function(df, seg, plot_title = "", measure, pct_range) {
    if (measure == "value") {
        plot_value(df, seg, plot_title)
    } else {
        plot_pct(df, seg, plot_title, pct_range = pct_range)
    }
}

# Shiny App Function ------------------------------------------------------

# run the shiny app
# - indir: folder that holds summary results (in csv files)
# - groups: permission groups to visualize
# - pct_range: y-axis range for % change per year
run_visual <- function(
    indir = "out", groups = c("hunt", "fish", "all_sports"), pct_range = 0.5
) {
    # setup
    infiles <- list.files(indir)
    infiles <- infiles[grep(".csv", infiles)] # only want csv files
    
    if (length(infiles) == 0) {
        stop(
            "The 'indir' folder must contain csv files.", 
            " Have you generated results?\n", 
            "- Try using: source('code/run.R')", call. = FALSE
        )
    }
    
    # define user interface
    ui <- fluidPage(mainPanel(
        splitLayout(
            selectInput("file", "Choose Results File", infiles),
            uiOutput("groupOptions"),
            selectInput("measure", "Choose Measure", c("value", "pct_change")),
            
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
        # plotOutput("countyPlot"), 
        width = 12
    ))
    
    # define data selection & plotting
    server <- function(input, output) {
        dataFile <- reactive({
            x <- read.csv(
                file.path(indir, input$file), stringsAsFactors = FALSE
            )
            if (!"segment" %in% names(x)) {
                stop("The '", input$file,  "' file doesn't have a segment column.",
                     call. = FALSE)
            }
            mutate_at(x, vars(segment, category, metric), "tolower")
        })
        output$groupOptions <- renderUI({tagList(
            selectInput("group", "Choose Permission Group", unique(dataFile()$group))
        )})
        dataGroup <- reactive({
            x <- filter(dataFile(), group == input$group)
            if (nrow(x) == 0) {
                stop("The '", input$file, "' file doesn't have any rows for the '",
                     input$group, "' group.", call. = FALSE)
            }
            x
        })
        output$allPlot <- renderPlot({ plot_segment(
            dataGroup(), "all", "Overall", input$measure, pct_range
        )})
        output$residencyPlot <- renderPlot({ plot_segment(
            dataGroup(), "residency", "By Residency", input$measure, pct_range 
        )})
        output$genderPlot <- renderPlot({ plot_segment(
            dataGroup(), "gender", "By Gender", input$measure, pct_range 
        )})
        output$agePlot <- renderPlot({ plot_segment(
            dataGroup(), "age", "By Age", input$measure, pct_range 
        )})
        output$countyPlot <- renderPlot({ plot_segment(
            dataGroup(), "county", "By County", input$measure, pct_range 
        )})
    }
    shinyApp(ui, server)
}
