#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinydashboard)

library(shiny)

# body <- dashboardBody(
#   tabItems( 
#     tabItem(
#       tabName = "Map",
#   
#              plotOutput("distPlot")#, 
#                                
#                ),
# 
# tabItem(
#   tabName = "Notes",
# h6("notes page")
# ) 
# )
# )



ui <- dashboardPage(
  dashboardHeader(title = "EastLink Trip Change"),
  sidebar =  dashboardSidebar(
    sidebarMenu( menuItem(
      "Map", 
      menuItem(
        tabName = "Map", 
        sliderInput("bins",
                   "Number of bins:",
                   min = 1,
                   max = 50,
                   value = 30)),
        
        menuItem(     tabName = "Map",
          selectInput("geography", "Geography",
                    choices = c("Block Groups" = "block_group",
                                "1/4 Mile Hex" = "quarter_mile_hex"
                                ),
                    selected = "quarter_mile_hex")),
      menuItem(     tabName = "Map",
        checkboxInput("legend", "Show legend", TRUE)),
       menuItem(tabName = "Map", actionButton("recalc", "Load Map & Filters")) #tabnames all have to be the same. No subnames
      
    ), 
    menuItem( "Notes", tabName = "Notes")
    
    )),
  dashboardBody(
    tabItems( 
      tabItem(
        tabName = "Map",
                  fluidRow(
                    column(width = 6,
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
                                             selected = 0
                                 )
                             )
                           )
                    ),
                    
                    column(width = 6,
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
                                              choices = c(1:100) ,
                                              multiple = TRUE)
                             )
                           ) 
                           
                    ),
                    
                    #  fluidRow(
                    column(width = 12,  
                           
                               jqui_resizable(   plotOutput("distPlot"))#, 
        
      )
      )
      )
      ,
      
      tabItem(
        tabName = "Notes",
        plotOutput("distPlot1")
      ))))
    
      
    
  


# Define UI for application that draws a histogram

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
    
    
    output$distPlot1 <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'blue', border = 'white',
           xlab = 'Waiting time to next eruption (in mins)',
           main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
