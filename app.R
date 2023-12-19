#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://github.com/IBM-DSE/Shiny-Examples-with-Blog/blob/master/1%20-%20Leaflet%20-%20Center%20Diverging%20Colors/app.R

#NOTE Need to handle the centroid call outside of the reactive function
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
# LOAD IN DATA ####
source("utils.R")
#filter NAs from hex grids when both baseline and proposed are NA
#add new metrics : 
  # > areas with new service #I think the coloring function is failing because it has no negative values
  # > change in trips per hour
  # > % change trips per hour #add # formatting to legend?
 #test color scheme for new metrics
  # add documentation of metrics on separate page 
      # need to add tabs to side bar

# look at folder, read in folder names, remove.zip from name 
files_list <- list.files(here::here("input", "r-objects"), full.names = T)
files_short_names <- list.files(here::here("input", "r-objects"), full.names = F)
file_names <-  gsub(pattern = "\\.RDS$", replacement = "", x = basename(files_short_names))


files <- map(files_list, readRDS)

names(files) <- file_names
                            

# UI Choices #####
metro <- "https://upload.wikimedia.org/wikipedia/en/thumb/b/bf/King_County_Metro_logo.svg/1280px-King_County_Metro_logo.svg.png"


day_type_choices <- unique(files$network_data$`Day Type`)
start_time_choices <- unique(files$network_data$`Start Time`)
metric_choices <- sort( c(unique(files$network_data$Metric), unique(files$network_data_details$Metric))) #trying to add in the asset level choices
trip_length_choices <-  unique(files$network_data$`Max Trip Duration`)
asset_group_choices <- unique(files$network_data_details$Assettype)
                                         
                                       

#UI #####


body <- dashboardBody(
tabItems(
  tabItem(
    tabName = "Map", #need to wrap body in tabItems to ID which tab the values show on
  fluidRow(

         jqui_resizable(
         box(title = "Metric Filters", 
             width = 12, 
             solidHeader = TRUE,
             status = "warning", 
             collapsible = T,
             column(width = 6,
           selectInput("metric",
                       "Metric",
                       choices = metric_choices, 
                       multiple = FALSE, 
                       selected = "Count Proposed"),
           selectInput("asset",
                       "Asset Type",
                       choices = asset_group_choices, 
                       multiple = FALSE, 
                       selected = "shopping_center"),
           
           selectInput("geography", "Geography",
                       choices = c("Block Groups" = "block_group",
                                   "1/4 Mile Hex" = "quarter_mile_hexagon"
                       ),
                       selected = "quarter_mile_hexagon"),
           
          # checkboxInput("legend", "Show legend", TRUE),
           actionButton("recalc", "Load Map & Filters")
           
         
         ),
  column(width = 6,
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
        sliderInput("metric_range",
                    label = "Filter Data Range",
                    min = -100,
                    max = 100,
                    value = c(-100, 100))
   )
   )
   ) 
   
   ),

  #  fluidRow(
  column(width = 12,  
     
         box(height = "100%",
             id = "map_container",
           width = NULL, solidHeader = TRUE,
           jqui_resizable( 
             
             leaflet::leafletOutput("metric_map")#, 
                          
                           )),
    
           box(width = NULL, solidHeader = TRUE,            
               jqui_resizable( tableOutput("click_info" ) 
          
             )))
          
        )
),
tabItem( "Notes",
          textOutput("note")

     ))



ui <- dashboardPage(
  dashboardHeader(title = " EastLink Trip Change"),
sidebar =  dashboardSidebar(
  sidebarMenu( menuItem(
    "Map", tabName = "Map" ), 
  menuItem( "Notes", tabName = "notes")
     
   )),
  body
)
# SERVER#####
server <- function(input, output) {
 
  # output$markdown <- renderUI({
  #   HTML(markdown::markdownToHTML('help.md'))
  # }) 
  
   #MAP FUNCTIONS #####
      #handle route reactivity####
  
 
  

  
  
  
# conditional <- function(condition, success){
#     if(condition) success else TRUE
#   }  
 

epa_hatch_reactive <- reactive({
  epa <- files$epa_hatch %>% 
    sf::st_as_sf()
})

#
# update range of filter for user controls

      #filter data for user input on metrics#####
  metric_data <- eventReactive(input$recalc, {
    req(input$recalc)
    if(input$metric %in% unique(files$network_data_details$Metric)){
      filtered_hex_data <- files$network_data_details %>% 
        filter(Metric ==   input$metric &
                 Assettype == input$asset &
                 `Start Time` == input$start_time &
                 `Day Type` == input$day_type &
                 Geography  == input$geography &
             `Max Trip Duration` == input$trip_length ) %>%
        drop_na(Value) %>%
        filter(!is.nan(Value)) %>%
        filter(!is.infinite(Value)) #%>%
        # filter( Value >= input$metric_range[1] &
        #           Value <= input$metric_range[2] )
      
      #print(nrow(filtered_hex_data))
     
     minVal <- min(filtered_hex_data$Value)
     maxVal <- max(filtered_hex_data$Value)
     domain <- c(minVal,maxVal)
     values_df <-  filtered_hex_data$Value
     center <- as.numeric(0)
     interval <- ifelse((maxVal - minVal)>10,10,
                        ifelse((maxVal - minVal)>5,1,0.2))
     color_bucket <- calculateBucket(min_val = minVal,max_val = maxVal,values_df = values_df,
                                     max_bin=10,interval=1,#interval_options=seq(from = -100, to = 100, by = 20),
                                     center=0,floor_at=NULL,ceil_at=NULL)
     print(color_bucket)
     df_pal <- inferColor(color_bucket,
                          color_below = "#b2182b",
                          color_above = "#2166ac",
                          interval=interval,
                          center=center)
     print(df_pal)
     filtered_hex_data <- filtered_hex_data %>%
       mutate(metric_color_label = cut(Value, breaks = color_bucket$breaks,
                                       labels = color_bucket$breaks_label,
                                       include.lowest = TRUE)) %>%
       mutate(metric_color_label = as.factor(metric_color_label)) %>%
       dplyr::left_join(df_pal) %>%
       arrange(metric_color_label)
    } else {
      filtered_hex_data <- files$network_data %>% 
        filter(Metric == input$metric &
                 `Start Time` == input$start_time &
                 `Day Type` == input$day_type &
                 Geography  == input$geography &
                 `Max Trip Duration` == input$trip_length) %>% 
        drop_na(Value) %>%
        filter(!is.nan(Value)) %>%
        filter(!is.infinite(Value)) %>%
        filter( Value >= input$metric_range[1] &
                  Value <= input$metric_range[2])
      
      minVal <- min(filtered_hex_data$Value)
      maxVal <- max(filtered_hex_data$Value)
      domain <- c(minVal,maxVal)
      values_df <- filtered_hex_data$Value
      
      center <- as.numeric(0)
      interval <- ifelse((maxVal - minVal)>10,10,
                         ifelse((maxVal - minVal)>5,1,0.2))
      
      color_bucket <- calculateBucket(min_val = minVal,max_val = maxVal,values_df = values_df,
                                      max_bin=7,interval=10,interval_options=seq(10,5000,10),
                                      center=100,floor_at=NULL,ceil_at=NULL)
      df_pal <- inferColor(color_bucket,
                           color_below = "#b2182b",
                           color_above = "#2166ac",
                           interval=interval,
                           center=center)
      
      
      filtered_hex_data <- filtered_hex_data %>%
        mutate(metric_color_label = cut(Value, breaks = color_bucket$breaks,
                                        labels = color_bucket$breaks_label,
                                        include.lowest = TRUE)) %>%
        mutate(metric_color_label = as.factor(metric_color_label)) %>%
        dplyr::left_join(df_pal) %>%
        arrange(metric_color_label)
      
    }
     }, ignoreNULL = FALSE)

  
  metric_data_sf <- eventReactive(input$recalc,{
    if(input$geography == "block_group"){
    block_groups <- files$block_groups %>% 
      left_join(metric_data(), by = "Geoid") %>% 
     drop_na(Value) %>%
    filter(Value != 0) %>%
      sf::st_as_sf() #added because R was making this a table not a spatial object
    } else {
      quarter_mile <- files$quarter_mile_hex_grid %>% 
        left_join(metric_data(), by = "Geoid") %>% 
      drop_na(Value) %>%
       filter(Value != 0) %>%
        sf::st_as_sf()
      
    } 
  },  ignoreNULL = FALSE)
  
# responsive labels for multiple geos
  metric_data_labels <- eventReactive(input$recalc,{
    
    
    if(input$geography == "block_group"){
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

rv_location <- reactiveValues(id=NULL,lat=NULL,lng=NULL)

observeEvent(input$metric_map_shape_click, {
  map_land_shape_click_info <- input$metric_map_shape_click
  # map_land_click_info <- input$map_land_click
  
  rv_location$id <-  map_land_shape_click_info$id #str_split_fixed(map_land_shape_click_info$id,'\\|',2)[2] # take the second part which is county name
  # rv_location$lat <- round(map_land_click_info$lat,4)
   #rv_location$lng <- round(map_land_click_info$lng,4)
})


metric_data_detail <- eventReactive(input$metric_map_shape_click, {
  files$network_data_details %>% 
    filter(Metric == input$metric &
             Assettype == input$asset&
             `Start Time` == input$period &
             `Day Type` == input$day_type &
             Geography  == input$geography &
             `Max Trip Duration` == input$trip_length ) 
   # arrange(`Change in Trips`)
  
  
})

output$click_info <- renderTable(metric_data_detail())


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
        overlayGroups = c( "EPA Overlay", "Labels", "Routes"),
        options = layersControlOptions(collapsed = FALSE), 
        position = "topleft"
      ) %>% 
      hideGroup(c("EPA Overlay", "Routes", "Labels") )
  })
  
  
  
   observeEvent(input$recalc, {
   
   proxy <- leafletProxy("metric_map")   %>%
     clearShapes() %>%
     clearGroup("Labels") %>% #"Labels", "EPA Overlay", "Routes"
    
      addPolygons( data = metric_data_sf() ,
                   weight = 2, opacity = 1,
                   color = "white",
                   dashArray = "3",
                   layerId = metric_data_sf()$Geoid,
                   fillOpacity = 0.7 ,
                   highlightOptions = highlightOptions(
                               weight = 5,
                               color = "#666",
                               dashArray = "",
                               fillOpacity = 0.7,
                               bringToFront = FALSE) ,   #)#,
                 label = ~paste0(Value,""),
                   labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "15px",
                           direction = "auto"),
                 fillColor = ~metric_data_sf()$metric_color_group,
                 popup = ~paste0(input$metric, ": ", Value
                                 
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
        overlayGroups = c( "EPA Overlay", "Labels", "Routes"), #
        options = layersControlOptions(collapsed = FALSE), 
        position = "topleft"
      ) #%>%

    # hideGroup(c("EPA Overlay", "Routes") ) #
     # myVariable <<- proxy
  } ,ignoreNULL = FALSE)


  
  
   
   


  
  
    
  # NOTES SERVER #####
  
  output$note <- renderText("This app shows the difference in vehicle trips and vehicle capacity for the EastLink Restructure 2022 Final Proposal.
This tool is for planning purposes only and does not show final data.
Please contact Melissa Gaughan with questions. Last updated 2023.11.30.")
  
 
  #TABLE FUNCTIONS #####  
  
  # Ok time for some dev work here. 
  # If user input == headways, go to GTFS folder, grab specified GTFS 
  #Calculate avg headways for weekdays, weekends by period
  #display by route, else calculate trips by time period
  
  #If user is on change tab, use results from network 1 and 2 to find differences. 
  # Routes that are not in baseline network get flagged as new. Rotues that are 
  # not in second network get flagged as deleted. Both new/deleted routes sent to second table on change tab
  

 
  # 
  # output$click_info <- renderText({  
  #   location_info <- reactiveValuesToList(rv_location)
  #   
  #   HTML(paste(h3(rv_location$id)))
  #   })
    
   
  
  
  output$note2 <- renderText("note2")
  
  output$note3 <- renderText("note3")
  
 # output$geography <- renderText(paste0(input$geography))

 
}

# Run the application 
shinyApp(ui = ui, server = server)
