#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram





ui <- dashboardPage(
  dashboardHeader(title = " EastLink Trip Change"),
  sidebar =  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "Map" ), 
      menuItem( "Notes", tabName = "Notes")
      
    )),
  dashboardBody(
    tabItems( 
      tabItem(
        tabName ="Map", #need to wrap body in tabItems to ID which tab the values show on
              fluidRow(
                column(width = 4,
                       jqui_resizable(
                         box(title = "Metric Filters", 
                             width = NULL, 
                             solidHeader = TRUE,
                             background = "navy",
                             collapsible = T,
                             
                             selectInput("metric",
                                         "Metric",
                                         choices = c(1:10), 
                                         multiple = FALSE, 
                                         selected = 1),
                             
                             selectInput("day_type",
                                         "Day",
                                         choices = c(0:2), 
                                         multiple = FALSE, 
                                         selected = 1
                             ),
                             selectInput("period",
                                         "Period",
                                         choices = c(1:7), 
                                         multiple = FALSE)
                         )
                       )
                ),
                
                column(width = 4,
                       jqui_resizable(
                         box( title = "Route Filters",
                              width = NULL,
                              solidHeader = TRUE, 
                              background = "navy",
                              collapsible = T,
                              selectInput("network",
                                          "Network",
                                          choices = c("Baseline", "Final Proposal"), 
                                          multiple = FALSE, 
                                          selected = "Baseline"), 
                              
                              selectInput("routes",
                                          "Routes",
                                          choices = c(1:10), 
                                          multiple = TRUE)
                         )
                       ) 
                       
                ),
                column(width = 4,
                       jqui_resizable(
                         box( title = "Load Map",
                              width = NULL,
                              solidHeader = TRUE, 
                              background = "navy",
                              collapsible = T,
                              sliderInput("metric_range",
                                          label = "Filter Data Range",
                                          min = -100,
                                          max = 100,
                                          value = c(-100, 100)),
                              
                              selectInput("geography", "Geography",
                                          choices = c("Block Groups" = "block_group",
                                                      "1/4 Mile Hex" = "quarter_mile_hex"
                                          ),
                                          selected = "quarter_mile_hex"),
                              
                              checkboxInput("legend", "Show legend", TRUE),
                              actionButton("recalc", "Load Map & Filters")
                         ))),
                #  fluidRow(
                column(width = 12,  
                       
                       box(height = "100%",
                           id = "Map",
                           width = NULL, solidHeader = TRUE,
                           jqui_resizable( box( title = "Accessibility",
                                                width = NULL,
                                                solidHeader = TRUE, 
                                                background = "teal",
                                                collapsible = T,
                                                renderPlot("distPlot")#, 
                                                
                           )))
                       
                )
              )
    ),
    tabItem( 
      tabName = "Notes",
              h6("test")
              
    ))
))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
