# Parkss Accessibility Analysis Example Script
# 2024.06.05
options(java.parameters = "-Xmx12G")
#install.packages("osmextract")
library(osmextract)
library(tidyverse)
library(sf)
library(r5r)
library(data.table)
library(mapview)
library(here)
library(tidytransit)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)
# read in spatial data #####
king_county <- block_groups( state = "Washington", county = "King", year = 2020) %>% 
  st_transform(4326) %>% 
  st_make_valid() %>% 
  st_union() %>% 
  st_sf()

st_crs(king_county)
mapview(king_county)


hexagon_grid <- read_sf("data/shapefiles/quarter_mile_hex_grid.shp")
mapview(hexagon_grid)

block_groups <- read_sf("data/shapefiles/blkgrp20_shore.shp") %>% 
  st_cast(to = "POLYGON")
#sf interpolation function did not work with a multipolygon, so I cast block groups to a simplified polygon

blocks <- blocks("WA", 
                 "King", 
                 year = 2020) %>% #has to be a decennial year to get the requisite POP20 field
  st_transform(2926)

# Parks Data ####
parks <- read_sf(here::here("data", "shapefiles", "KCParkAccess.gdb"), layer = "KCParkAccessPoints")%>%
  st_transform(2926) %>% 
  janitor::clean_names() %>% 
  mutate(name = sitename, 
         address = closest_int, assettype = "park", 
         codetype = "PK", 
         year = 2023,
         asset_group = "Park") %>% 
  select(name, address, assettype, codetype, point_x, 
         point_y, year, Shape, asset_group)

parks_hex <-  hexagon_grid %>% 
  st_join(. ,parks, join = st_intersects, left = TRUE) %>% 
  st_drop_geometry() %>% 
  drop_na(name) %>% 
  group_by(rowid, name) %>% 
  summarise(park_access_points = n()) %>% #sum all access points in the hex per park
  group_by(rowid) %>% 
  summarise(parks = n())   # roll up to count of unique parks per hex


hex_dest <- hexagon_grid %>%  #join table data to spatial dataframe. THis is what will be used to measure access. 
  left_join(parks_hex) %>% 
  st_centroid() %>% 
  st_transform(4326) %>% 
  rename(id = rowid) %>% 
  mutate(parks =  replace_na(parks, 0))


 # get OSM extract ####
osm_file <- oe_match(king_county) #get osm extract covering extent of the WA block groups

oe_download(
  file_url = osm_file$url, 
  file_size = osm_file$file_size,
  provider = "test",
  download_directory = here::here("analyses", "accessibility", "data", "233_network") #change this to be the file where you stash your GTFS
) #doing this with areas larger than WA state will likely result in download timeout errors. 

#block_group_error <- rbind(block_groups[1483,], block_groups[1519,]) # if you think there are issues, you can subset and investigate

# STOP!  ####
#Go get your GTFS files and put them in the folder where the OSM files downloaded. The GTFS files need to be zipped. 

data_path <-here::here("analyses","accessibility", "data","233_network") # path of where your OSM and GTFS files are locaed

r5r_core <- setup_r5(data_path = data_path, verbose = F) # has to be rerun anytime you are setting up with a different transit network

#create points from the hex grid. Snap to street network. r5r core has to be turned on before this will work.

hex_points <- no_water_hex_grid %>% 
  st_centroid() %>% 
  st_transform(4326) %>% 
  rename(id = rowid)

hex_points_snap <- find_snap(r5r_core, hex_points, mode = "WALK") %>% 
  mutate(id = as.numeric(point_id))

dest_hex_points <- hex_points_snap %>% 
  left_join(hex_dest) %>% 
  select(-c(lat, lon)) %>% 
  #dropping the original lat lon to avoid confusing r5r. 
  #renaming and cast forcing to meet requirements
  rename(lat = snap_lat, 
         lon= snap_lon) %>% 
  mutate(id = as.character(id)) %>% 
  st_as_sf() %>% 
  st_cast(to="POINT")

parameters_raw <- read_csv("analyses/accessibility/data/input_parameters_weekday_233.csv") %>% 
  mutate(departure_datetime= lubridate::ymd_hm(departure_datetime) ) #make sure departure time is read in as date time

# function for measuring accessibility iteratively


travel_time_compare <- function(run_id, 
                                departure_datetime,
                                geography, 
                                max_trip_duration ,
                                origins, 
                                destinations, 
                                breakdown, 
                                verbose, 
                                decay_function = "logistic",
                                cutoffs) {
  
  
  #using a decay function produces more believable results
  ttm <- accessibility(r5r_core = r5r_core,
                       decay_function = "logistic",
                       origins = eval(parse(text=origins)),#references dest_hex_points, but could be changed in the input parameters
                       destinations = eval(parse(text=destinations)),
                      
                       opportunities_colname = c( "parks" )  , #add other columns for eval as desired
                       mode = c("WALK", "TRANSIT"),
                       departure_datetime = as.POSIXct(departure_datetime, 
                                                       format = "%d/%m/%Y %H:%M"),
                       max_walk_time = 15,
                       #  time_window = 10L, # not relevant because we don't use a frequency based GTFS. 
                       max_trip_duration = max_trip_duration,
                       verbose = verbose, 
                       decay_value = 4, #controls standard deviation
                       cutoffs = cutoffs)#controls median of decay function
  
  ttm_check <- hexagon_grid %>% 
    mutate(id = as.character(rowid)) %>% 
    left_join(ttm) %>% 
    st_transform(4326) %>% 
    filter(accessibility > 0) 
  
  
  if(geography != "block_group"){
    
    out <- ttm_check %>% 
      mutate(run_id = run_id, 
             count = round(accessibility, 0),) %>% #return quarter mile hex results without block pop interpolation
      rename(GEO_ID_GRP = rowid, 
             assettype = opportunity) %>% 
      select(-accessibility) %>% 
      mutate(asset_score = case_when(count == 0 ~ 0,
                                     count == 1 ~ 1, 
                                     count >= 2 ~ 2, 
                                     TRUE ~ 99), 
             GEO_ID_GRP = as.character(GEO_ID_GRP))
    print(run_id)
    
    return(out)
    
  } else {
    access_summary_geo <-  pivot_wider(ttm_check,
                                       
                                       names_from = "opportunity",
                                       values_from = "accessibility")
    
    
    
    interpolated_bg_access <- access_summary_geo %>% 
      select(-rowid) %>%
      interpolate_pw(to = block_groups, 
                     to_id = "GEO_ID_GRP", 
                     weights = blocks, 
                     weight_column = "POP20", 
                     crs = 2926, 
                     extensive = FALSE,
                     weight_placement = "surface"
                     
      )
    
    interpolated_bg_access_df <- interpolated_bg_access %>% 
      st_drop_geometry() %>% 
      pivot_longer(cols = -c(GEO_ID_GRP,  cutoff, percentile), 
                   names_to = "assettype", 
                   values_to = "count") %>% 
      mutate(count = round(count, 0), 
             run_id = run_id) %>% 
      mutate(asset_score = case_when(count == 0 ~ 0,
                                     count == 1 ~ 1, 
                                     count >= 2 ~ 2, 
                                     TRUE ~ 99))
    
    print(run_id)
    return(interpolated_bg_access_df)
    
    
  }
}
r5r::stop_r5() #stop r5r to get ready for a different r5r analysis or just save your computer's memory

baseline_accessibility <- pmap( parameters, .f= travel_time_compare ) #use pmap to iterate over the input parameters using the ttm function

#after running function, save your work as an rdata object or continue on for further analysis. 

baseline_accessibility_df <- bind_rows(baseline_accessibility)