#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://github.com/IBM-DSE/Shiny-Examples-with-Blog/blob/master/1%20-%20Leaflet%20-%20Center%20Diverging%20Colors/app.R

library(shiny)
library(shinydashboard)
library(shinyjqui)
#library(shinydashboardPlus)
library(tidyverse) 
library(RColorBrewer)
library(leaflet)
library(sf)
library(rsconnect)
library(here)
library(leafem)
library(shinyalert)
library(plotly)
# LOAD IN DATA ####
source("utils.R")
source("load_data.R")

#TASKS#

# 1. Grey out asset selection if basdket of goods metrics selected # DONE
# 2. Grey out jobs if basket of goods is selected # DONE as part of 1
#3. Table: if basket of goods is loaded, show table of access for each type of asset at the selected time # show all metrics for associated geoid? #DONE
#4. Make percents have percent labels # this is fixed for hover, still need to work on legends and tables. I think this will need to involve cracking open utils.R
#5. make ggplot table not use sci notation
#6. ggplot:" hover for data #DONE
#7. in processing, ensure that values are not crazy. dividing by .01 is making distributions really off. #DONE
#8 change plotly title to be smaller
#9 Make display table filterable, sortable
#10. Remove character values from data tables to reduce file size as much as possible. Check that lookups are being used consistently 10.29.24
#UI #####


body <- dashboardBody(
tabItems(
  tabItem(
    tabName = "Map", #need to wrap body in tabItems to ID which tab the values show on
    tags$head(
      tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.js"),
      tags$link(rel = "stylesheet", href = "resizableColumns.css"),
      tags$script(src = "resizableColumns.js")
    ),
    tags$div(
      id = "layout",
    fluidRow(

         #jqui_resizable(
         box(title = "Metric Filters", 
             width = 12, 
             solidHeader = TRUE,
             status = "warning", 
             collapsible = T,
              column(width = 2, 
                     strong("How many places can someone get to using transit in a certain about of time?"), 
                     p("Use the filters to select a day, start time, trip length, geography and metric. If you select a Basket of Goods metric, you do not need to select an asset type."), 
                     p("Learn more about the analysis in the FAQ")),
             column(width = 5,
           selectInput("metric",
                       "Metric",
                       choices = metric_choices, 
                       multiple = FALSE, 
                       selected = "Count Proposed"),
           selectInput("asset",
                       "Asset Type",
                       choices = NULL, 
                       multiple = FALSE), 
           selectInput("geography", "Geography",
                       choices = geography_choices,
                       selected = "quarter_mile_hexagon")
           
         
         
         ),
  column(width = 5,
        selectInput("day_type",
                    "Day",
                    choices = day_type_choices, 
                    multiple = FALSE, 
                    selected = "weekday"
                     ),
        selectInput("start_time",
                    "Trip Start Time",
                    choices = start_time_choices, 
                    multiple = FALSE, 
                    selected = "12 PM"
                    ),
        selectInput("trip_length",
                    "Max Trip Length",
                    choices = trip_length_choices, 
                    multiple = FALSE, 
                    selected = 45), 
       
        actionButton("recalc", "Load", class = "btn btn-warning")
        
   )
   )
   ) 
   
   ,
  
  column(width = 8,  
         box(title = "Map of Transit Access (click a shape to view details)",
           height = "100%",   status = "primary",
             id = "map_container",
           width = NULL, solidHeader = TRUE,
           collapsible = TRUE,
            # tableOutput("test_table" ) 
            leaflet::leafletOutput("metric_map")#, 
                           )),
  column(width = 4, 
           box(title = "Distribution of County-Wide Accessibility",
               status = "primary", collapsible = TRUE,
             height = "100%",width = NULL, solidHeader = TRUE,            
              plotlyOutput("plot" ) 
             )), 
  column(width = 12, 
         box(title = "Details Table (click on map to populate)",
             status = "primary",
           height = "100%",width = NULL, solidHeader = TRUE,
           collapsible = TRUE,
          DT::dataTableOutput("table" )
             )
         ))
          
        )
,
tabItem( "Notes",
         tags$head(
           tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.js"),
           tags$link(rel = "stylesheet", href = "resizableColumns.css"),
           tags$script(src = "resizableColumns.js")
         ),
         tags$div(
           id = "layout",
         fluidRow(
           column(width = 10,
          # jqui_resizable(
             box(title = "Documentation", 
                 width = 10, 
                 height = "100%",
                 solidHeader = TRUE,
                 status = "warning", 
                 collapsible = F,
        
          includeMarkdown("faq/help.rmd")))
     )))))



ui <- dashboardPage(
  dashboardHeader(title = "Accessibility Analysis"),
sidebar =  dashboardSidebar(
  sidebarMenu( menuItem(
    "Map", tabName = "Map" ), 
  menuItem( "FAQ", tabName = "Notes")
     
   )),
  body
)

# SERVER#####
server <- function(input, output) {
 

   #MAP FUNCTIONS #####
   
epa_hatch_reactive <- reactive({
  epa <- files$epa_hatch %>% 
    sf::st_as_sf()
})

asset_reactive<- reactive({
  if(input$metric %in% unique(files$network_data_details$`Lookup Metric`)){
    asset_group_choices
  
  } else{
    ""
  }
  
})

observeEvent(asset_reactive(), {
  #req(input$network)
  #freezeReactiveValue(input, "routes")
  choices <-  asset_reactive()
  updateSelectInput( inputId = "asset", choices = choices)
})





      #filter data for user input on metrics#####
  metric_data <- eventReactive(input$recalc, {
    req(input$recalc)
    if(input$metric %in% unique(files$network_data_details$`Lookup Metric`)){
        filtered_hex_data <- files$network_data_details %>% 
          filter(`Lookup Metric` ==   input$metric & #filter by asset here
                   `Lookup Asset Group` == input$asset &
                   `Lookup Start Time` == input$start_time &
                   `Lookup Day Type` == input$day_type &
                   `Lookup Geography`  == input$geography &
                   `Lookup Max Trip Duration` == input$trip_length ) %>%
          drop_na(Value) %>%
          filter(!is.nan(Value)) %>%
          filter(!is.infinite(Value))
        
        } else{
          filtered_hex_data <- files$network_data %>% 
        filter(`Lookup Metric` ==   input$metric & #no asset filter here
                 `Lookup Start Time` == input$start_time &
                 `Lookup Day Type` == input$day_type &
                 `Lookup Geography`  == input$geography &
             `Lookup Max Trip Duration` == input$trip_length ) %>%
        drop_na(Value) %>%
        filter(!is.nan(Value)) %>%
        filter(!is.infinite(Value)) 
        }
      
         #  print(nrow(filtered_hex_data))
         # print("network data details")
         # 

      minVal <- min(filtered_hex_data$Value, na.rm = T)
      maxVal <- max(filtered_hex_data$Value,  na.rm = T)
      domain <- c(minVal,maxVal)
      values_df <-  filtered_hex_data$Value
      center <- as.numeric(0)
      interval <- ifelse((maxVal - minVal)>10,10,
                         ifelse((maxVal - minVal)>5,1,0.2))
      color_bucket <- calculateBucket(min_val = minVal,max_val = maxVal,values_df = values_df,
                                      max_bin=7,interval=interval,#interval_options=seq(from = -100, to = 100, by = 20),
                                      center=0,floor_at=NULL,ceil_at=NULL) 
      # 
      # color_bucket_df <- as_tibble(color_bucket)
      # class(color_bucket_df)
      # print(color_bucket_df)
      
      #Percent change still breaking
      #parks missing from asset choices
      
      df_pal <- inferColor(color_bucket,
                           color_below = "#b2182b",
                           color_above = "#2166ac",
                           interval=interval,
                           center=center)
      
      # print( df_pal)
      # print("colorbuck breaks")
      # print(color_bucket$breaks)
      # print("colorbucket labels")
      # print(color_bucket$breaks_label)
      
      filtered_hex_data <- filtered_hex_data %>%
        mutate(metric_color_label = cut(Value, breaks = color_bucket$breaks,
                                        labels =color_bucket$breaks_label,
                                        include.lowest = TRUE)) %>%
        mutate(metric_color_label = as.factor(metric_color_label)) %>%
        dplyr::left_join(df_pal) %>%
        arrange(metric_color_label)
    
  
     }, ignoreNULL = FALSE)

pct_format <- scales::label_percent(accuracy = 0.1, scale = 1, big.mark = ",")

  
  metric_data_sf <- eventReactive(input$recalc,{

    if(input$geography == 1 & input$metric %in% c(1,2, 3,7)){ # had to hardcode in the lookupval of block group geoid. need to refactor #bg, percent metrics
    block_groups <- files$block_groups %>% 
      left_join(metric_data(), by = "Geoid") %>% 
     drop_na(Value) %>%
    filter(Value != 0) %>%
     mutate(Value = pct_format(Value)) %>% 
      sf::st_as_sf() #added because R was making this a table not a spatial object
    } else if(input$geography != 1 & !(input$metric %in% c(3,7))){ # qm, count metrics
      quarter_mile <- files$quarter_mile_hex_grid %>% 
        left_join(metric_data(), by = "Geoid") %>% 
      drop_na(Value) %>%
       filter(Value != 0) %>%
        sf::st_as_sf()
      
    } else if(input$geography == 1 & !(input$metric %in% c(1, 2, 3,7))){ # bg, count metrics
      block_groups <- files$block_groups %>% 
        left_join(metric_data(), by = "Geoid") %>% 
        drop_na(Value) %>%
        filter(Value != 0) %>%
      
        sf::st_as_sf()
    } else   {    #(input$geography != 1 & !(input$metric %in% c(1, 2, 3,7))){ #qm, percent metrics
      quarter_mile <- files$quarter_mile_hex_grid %>% 
        left_join(metric_data(), by = "Geoid") %>% 
        drop_na(Value) %>%
        filter(Value != 0) %>%
        mutate(Value = pct_format(Value)) %>% 
        sf::st_as_sf()
    } 
  },  ignoreNULL = FALSE)
  
# responsive labels for multiple geos
  metric_data_labels <- eventReactive(input$recalc,{
    
    
    if(input$geography == 1){ # had to hardcode in the lookupval of block group geoid. need to refactor
    files$block_group_centroids %>%
      left_join(metric_data()) %>%
      drop_na(Value) %>%
      filter(Value != 0) %>%
      sf::st_as_sf() #added because R was making this a table not a spatial object
      } else {
        files$quarter_mile_hex_grid %>% 
          left_join(metric_data()) %>%
          drop_na(Value) %>%
          filter(Value != 0) %>%
          sf::st_as_sf()
      } 
  },  ignoreNULL = FALSE)
  
  #recalc legend to respond to new breaks
  
  reactive_legend <- reactive({
    label_data <-  metric_data_sf() %>%
      select(metric_color_label, metric_color_group) %>% 
      distinct(metric_color_label, metric_color_group) %>% 
      arrange(metric_color_label)
  })

#event_recalc <- reactiveVal(update_recalc = NULL)


click_county <- reactiveVal()

observeEvent(input$metric_map_shape_click, {
  # Capture the info of the clicked polygon
  if(!is.null(click_county()) && click_county() == input$metric_map_shape_click$id )
    click_county(NULL)     # Reset filter
  else
    click_county(input$metric_map_shape_click$id)
  #print(click_county)
})

observeEvent(input$recalc, {
click_county(NULL)
  
})






metric_data_detail <- reactive({

      if(is.null(click_county())) {
        NULL    # Not filtered
     } else {
       files$network_data_details %>%  #metric data is already set to a sepcific table--either basker of goods or details
         filter( Geoid==click_county()) %>% 
         filter(#`Lookup Metric` ==   input$metric & #filter by asset here
                        `Lookup Start Time` == input$start_time &
                        `Lookup Day Type` == input$day_type &
                       # `Lookup Geography`  == input$geography &
                        `Lookup Max Trip Duration` == input$trip_length ) %>% 
        
         select(c(Assettype,  `Metric`, `Value`)) %>% 
         pivot_wider(id_cols = Assettype, names_from = Metric, values_from = Value) %>% 
         arrange(desc(`Percent Change In Asset Count`))
}



})
#https://stackoverflow.com/questions/46732849/shiny-detect-a-change-in-input-with-a-warning-in-ui-r

observeEvent(input$recalc, {
  
output$table <- DT::renderDataTable(
  metric_data_detail(), 
  options = list(
  
  )
)
})

metric_label_plot <- reactive({
  metric_choices[metric_choices ==input$metric] })

geography_label <- reactive({
 geography_choices[geography_choices ==input$geography] })

start_time_label <- reactive({
  start_time_choices[start_time_choices ==input$start_time] })

day_type_label <- reactive({
  day_type_choices[day_type_choices ==input$day_type] })

trip_length_label <- reactive({
  trip_length_choices[trip_length_choices ==input$trip_length] })

 observeEvent(input$recalc, {
   
   output$plot <-renderPlotly({ 
      plot_ly(
        x= metric_data()$Value, 
        type = "histogram", 
        alpha = .8
      )%>%
       layout(title =paste0( names(metric_label_plot()), "<br><sup>", names(geography_label()), " at ", #use html to create superscript subtitle. 
                                        names(start_time_label()), " on ", names(day_type_label()),", ",
                                        names(trip_length_label()), " minute max trip length</sup>" ), 
             
              xaxis = list(title = names(metric_label_plot()),
                           zerolinecolor = '#ffff',
                           zerolinewidth = 2,
                           gridcolor = 'ffff'),
              yaxis = list(title = "Count of Geographies",
                           zerolinecolor = '#ffff',
                           zerolinewidth = 2,
                           gridcolor = 'ffff'),
              plot_bgcolor='#e5ecf6')

   
       
     })  
  
},ignoreNULL = FALSE)
  
  
  
#output$click_info <- renderTable(metric_data_detail())

#output$test_table <- renderTable(metric_data())
  # Map reactives ####
  output$metric_map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet::leaflet() %>%
      leaflet::addProviderTiles("CartoDB.Positron") %>%
      leaflet::setView(lng = -122.3321, lat = 47.6062, zoom = 11 ) %>%
      leaflet.extras::addSearchOSM() %>%
      leafem::addLogo( img =   metro,
                       src="remote", 
                       position = "bottomright",
                       offset.x = 30,
                       offset.y = 100,
                       height = 30, 
                       width = 80) %>% 
      leaflet::addScaleBar(position = "topright")   %>% 
      addLayersControl(
        overlayGroups = c( "EPA Overlay", "Labels"),
        options = layersControlOptions(collapsed = FALSE), 
        position = "topleft"
      ) %>% 
      hideGroup(c("EPA Overlay",  "Labels") )
  })
  
 metric_label <- eventReactive(input$recalc, {
  test <-  files$lookup_table_metric %>% 
     filter(lookup_metric == input$metric)
  out <- as.vector(test$Metric)
   
 }
                              ) 
  
   observeEvent(input$recalc, {
   
   proxy <- leafletProxy("metric_map")   %>%
     clearShapes() %>%
     clearGroup("Labels") %>% #"Labels", "EPA Overlay", "Routes"
    
      addPolygons( data = metric_data_sf() ,
                   weight = .5, opacity = 1,
                   color = "white",
                  # dashArray = "3",
                   layerId = metric_data_sf()$Geoid,
                   fillOpacity = 0.7 ,
                   highlightOptions = highlightOptions(
                               weight = 2,
                               color = "#666",
                               #dashArray = "",
                               fillOpacity = 0.7,
                               bringToFront = FALSE) ,   #)#,
                 label = ~paste0(Value,""),
                   labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "15px",
                           direction = "auto"),
                 fillColor = ~metric_data_sf()$metric_color_group,
                 popup = ~paste0(metric_label(), ": ", Value
                                 
                                 )
      ) %>%
      addPolylines(
        data = epa_hatch_reactive(),
        color = "black",
        weight = 0.6,
        group = "EPA Overlay"
      )  %>%
     leafem::addStaticLabels(
    # addLabelOnlyMarkers(
                   data = metric_data_labels(),
                  # lat = metric_data_labels()$Y,
                    #lng = metric_data_labels()$X,
                    label = metric_data_labels()$Value,
                     group = "Labels") %>%
      addLayersControl(
        overlayGroups = c( "EPA Overlay", "Labels"), #
        options = layersControlOptions(collapsed = FALSE), 
        position = "topleft"
      ) #%>%

    # hideGroup(c("EPA Overlay", "Routes") ) #
     # myVariable <<- proxy
  } ,ignoreNULL = FALSE)


  
   observeEvent(input$recalc,{
     proxy <- leafletProxy("metric_map", data = metric_data_sf())
     
     # Remove any existing legend, and only if the legend is
     # enabled, create a new one.
     proxy %>% clearControls() %>%
       addLegend(position = "topright",
                           colors = reactive_legend()$metric_color_group,
                           labels = reactive_legend()$metric_color_label,
                           opacity =  0.9,
                           title = metric_label())
     
   }, ignoreNULL = FALSE)
   
   


  
   shinyalert(
     title = "King County Metro Transit Accessibility",
     text = ("<b>Visit the FAQ page!</b> </br>This app compares the transit accessibility of various community assets using the Fall 2024 King County Metro network (baseline network) and the South Link Connections Phase 2 Network (proposed network)."),
       
     size = "s", 
     closeOnEsc = TRUE,
     closeOnClickOutside = TRUE,
     html = TRUE,
     type = "",
     showConfirmButton = TRUE,
     showCancelButton = FALSE,
     confirmButtonText = "OK",
     confirmButtonCol = "#AEDEF4",
     timer = 0,
     imageUrl = metro,
     animation = TRUE
   )
   

 
}

# Run the application 
shinyApp(ui = ui, server = server)
