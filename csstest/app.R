library(shiny)

ui <- fluidPage(
  tags$head(
    tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.js"),
    tags$link(rel = "stylesheet", href = "resizableColumns.css"),
    tags$script(src = "resizableColumns.js")
  ),
  tags$div(
    id = "layout",
    fluidRow(
      column(
        width = 3,
        h3("column1")
      ),
      column(
        width = 3,
        h3("column2")
      ),
      column(
        width = 3,
        h3("column3")
      ),
      column(
        width = 3,
        h3("column4")
      )
    )
  )
)

server <- function(input, output){}

shinyApp(ui, server) 