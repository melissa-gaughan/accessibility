options(java.parameters = "-Xmx12G")

library(r5r)
library(sf)
library(data.table)
library(mapview)
library(here)
library(tidyverse)
library(tidytransit)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)

#get block groups


block_groups <- read_sf("data/shapefiles/blkgrp20_shore.shp")
blocks <- blocks("WA", 
                 "King", 
                 year = 2020) %>% 
  st_transform(2926)


# hex grid creation #####

#make a hex grid from the block groups, erase the water, get ready for spatial analysis

hexagon_grid <- st_make_grid(block_groups, 
                             cellsize = 1320,
                             #what = "polygons", 
                             square = F #, 
                             # flat_topped = T
) 
no_water_hex_grid <-   st_as_sf(hexagon_grid) %>% 
  tigris::erase_water()


no_water_hex_grid <- no_water_hex_grid %>% 
  rowid_to_column() %>% 
  st_transform(2926)

write_sf(no_water_hex_grid, "quarter_mile_hex_grid.shp")

hexagon_grid_eigth_mile <- st_make_grid(block_groups, 
                             cellsize = (1320/2),
                             #what = "polygons", 
                             square = F #, 
                             # flat_topped = T
) 
no_water_hex_grid_eigth_mile <-   st_as_sf(hexagon_grid_eigth_mile) %>% 
  tigris::erase_water()


no_water_hex_grid_eigth_mile <- no_water_hex_grid_eigth_mile %>% 
  rowid_to_column() %>% 
  st_transform(2926)

write_sf(no_water_hex_grid_eigth_mile, "eigth_mile_hex_grid.shp")

#create points from the hex grid. Snap to street network. r5r core has to be turned on before this will work.

#for r5, crs has to be 4326

hex_points <- no_water_hex_grid %>% 
  st_centroid() %>% 
  st_transform(4326) %>% 
  rename(id = rowid)

data_path <- "C:/Users/mgaughan/Documents/Projects/service-planning/analyses/accessibility/data/mc_network"

r5r_core <- setup_r5(data_path = data_path, verbose = F)

# static input #####

hex_points <- find_snap(r5r_core, hex_points, mode = "WALK") %>% 
  select(id = "point_id", 
         lat = "snap_lat", 
         lon = "snap_lon")
# hex_points_sf <-hex_points %>% 
#   filter(!is.na(lat)) %>% 
#   st_as_sf(., 
#                           coords = c("lon", "lat")) 
# hex_points_sf <- hex_points_sf %>% 
#   st_set_crs(4326)
# 
# final_output_no_sf <- hex_points_sf %>% 
#   
#   st_drop_geometry()
# 
# missing <- hex_points %>% 
#   mutate(id = as.character(id)) %>% 
#   anti_join(final_output_no_sf)
# mapview(missing)
# 
# mapview(hex_points_sf) + mapview(hex_points, col.regions = "black")
# load and clean community assets. Join to hex dataframe
community_assets <-st_read("C:/Users/mgaughan/Documents/Projects/baseline-data-viewer/data/ALLCommunityAssets2022.gdb", "CommunityAssets_January2022") %>%
  st_transform(2926) %>% 
  janitor::clean_names()

community_asset_groups <- read_csv("analyses/accessibility/data/community_asset_groups.csv")


community_assets <- left_join(community_assets, community_asset_groups) %>% 
  group_by(asset_group)

hex_data <- no_water_hex_grid %>% 
  st_join(. ,community_assets, join = st_intersects, left = TRUE)

asset_summary <- hex_data %>% 
  st_drop_geometry() %>% 
 filter(!is.na(name)) %>% 
  group_by(rowid, asset_group) %>% 
  summarise(count_assets = n()) %>% 
  pivot_wider(names_from = asset_group, 
              values_from = count_assets, 
              values_fill = 0) %>% 
  janitor::clean_names()

# load and clean jobs. Join to hex dataframe


# load and clean jobs. Join to hex dataframe

jobs <- st_read(here::here("data", "shapefiles", "points_2019.shp")) %>% 
  st_transform(2926) %>% 
  select(total_jobs = c000, low_wage_jobs = ce01, 
         mid_wage_jobs = ce02, high_wage_jobs = ce03)

jobs_hex <- no_water_hex_grid %>% 
  st_join(. ,jobs, join = st_intersects, left = TRUE)

jobs_summary <- jobs_hex %>% 
  st_drop_geometry() %>% 
  filter(!is.na(total_jobs)) %>% 
  group_by(rowid) %>% 
  summarise(across(total_jobs:high_wage_jobs, ~sum(.x, na.rm = T))) #%>% 
  #pivot_longer(!rowid, names_to = "assettype", values_to = "count_assets" )

hex_summary <- left_join( asset_summary, jobs_summary)

#testing input #####
# can I do this long? Do I need spatial data for the join? Can I just do it on the row id?
# need to figure out how to do this with interpolate_pw. I think this is the piece that is inherently spatial and
# may need to be wide rather than long. 


# 
# mode <- c("WALK", "TRANSIT")
# max_walk_dist <- 500
# max_trip_duration <- 45
# departure_datetime <- as.POSIXct("22-03-2021 12:00:00",
#                                  format = "%d-%m-%Y %H:%M:%S")
# r5r_core$setTimeWindowSize(45L)
# origins <- hex_points
# destinations <- hex_points
# breakdown <- F
# verbose <- F
# time_window <- 45L
# function #####
travel_time_compare <- function(departure_datetime,
                                #baseline_network_path, 
                               # mode,
                                max_walk_dist ,
                                max_trip_duration ,
                               time_window, 
                               origins, 
                               destinations, 
                               breakdown, 
                               verbose, 
                               run_id) {
  
  
  ttm <- travel_time_matrix(r5r_core = r5r_core,
                            origins =  eval(parse(text=origins)),
                            destinations = eval(parse(text =destinations)),
                            mode = c("WALK", "TRANSIT"),
                            breakdown = breakdown,
                            departure_datetime = as.POSIXct(departure_datetime, 
                                                            format = "%d-%m-%Y %H:%M:%S"),
                            max_walk_dist = max_walk_dist,
                            time_window = time_window,
                            max_trip_duration = max_trip_duration,
                            verbose = F)
  
  
access_summary <- hex_summary %>% 
    mutate(rowid = as.character(rowid)) %>% 
    left_join(ttm , by = c("rowid" = "toId")) %>%
    group_by(fromId) %>% 

  summarise(across(public_spaces:high_wage_jobs, ~sum(.x, na.rm = T)))
y

access_summary_geo <- no_water_hex_grid %>% 
  mutate(rowid = as.character(rowid)) %>% 
 #full join?
   left_join(access_summary, by = c("rowid" = "fromId")) 
  
#interpolation does not respect grouping. Needed to do this wide rather than long. 

#make static hex matrix to bg interpolation? would need to get in the guts
#https://rdrr.io/cran/tidycensus/man/interpolate_pw.html

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
  pivot_longer(cols = -GEO_ID_GRP, 
               names_to = "assettype", 
               values_to = "count") %>% 
  mutate(count = round(count, 0), 
         run_id = run_id)
  
  
return(interpolated_bg_access_df)

  

  
  
}



}
r5r::stop_r5()

# first run #####

#datapath with GTFS and OSM extract. Eventually we will need a second folder with the other GTFS
data_path <- "C:/Users/mgaughan/Documents/Projects/service-planning/analyses/accessibility/data/mc_network"

# r5r core setup

r5r_core <- setup_r5(data_path = data_path, verbose = F)

parameters <- read_csv("analyses/accessibility/data/mc_input_parameters.csv") %>% 
  select( -c(starts_with("mode" )| starts_with("baseline")))

mc_accessibility <- pmap( parameters, .f= travel_time_compare )


# second run #####
r5r::stop_r5()

data_path_2 <- "C:/Users/mgaughan/Documents/Projects/service-planning/analyses/accessibility/data/221_network"

r5r_core <- setup_r5(data_path = data_path_2, verbose = F, overwrite = T)
  
parameters <- read_csv("analyses/accessibility/data/input_parameters.csv") %>% 
  select( -c(starts_with("mode" )| starts_with("baseline")))
  
spring_22_accessibility <-  pmap( parameters, .f= travel_time_compare )   

mc_accessibility_df <- bind_rows(mc_accessibility) %>% 
  left_join(parameters %>% select(run_id, departure_datetime, 
                                  max_walk_dist, max_trip_duration, time_window))


spring_22_accessibility_df <- bind_rows(spring_22_accessibility)%>% 
  left_join(parameters %>% select(run_id, departure_datetime, 
                                  max_walk_dist, max_trip_duration, time_window))

test_compare <- mc_accessibility_df %>% 
  #filter(run_id == 1) %>% 
left_join(spring_22_accessibility_df, by = c("GEO_ID_GRP", "assettype", "run_id"), 
          suffix = c("_mc", "_221")) 

write_csv(test_compare, "analyses/accessibility/output/mc_221_comparison_2022-08-22.csv")

write_csv(mc_accessibility_df ,"analyses/accessibility/output/mc_accessibility_2022-09-02.csv" )


write_csv(spring_22_accessibility_df ,"analyses/accessibility/output/221_accessibility_2022-09-02.csv" )

