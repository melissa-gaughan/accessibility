options(scipen=999)
#library(shinydashboardPlus)
library(tidyverse)

library(sf)
library(gg.layers)
#library(cartography)
library(here)
project_name <- "SLC_P2_periods_v1"
# LOAD IN DATA ####
block_groups_raw <- sf::read_sf(here::here("raw_data", "2020_block_groups", "blkgrp20_shore.shp")) %>% 
  mutate(Geoid = as.numeric(GEO_ID_GRP))

#block_group_test <- block_groups_raw[block_groups_raw$geoid=="530330323161",]

#mapview::mapview(block_group_test)

block_group_centroids <- block_groups_raw %>% 
  st_centroid() %>% 
  st_transform(4326) 

centroids <- st_coordinates(block_group_centroids)



block_group_centroids <- bind_cols(block_group_centroids, centroids)

rm(centroids)

block_groups <- block_groups_raw %>% 
  st_transform(4326) %>% 
  rmapshaper::ms_simplify(keep = .05) %>% 
  select(Geoid, geometry)

rm(block_groups_raw)

load(file = here::here( "raw_data","route_and_block_group_equity_data_243.RDS"))

block_group_need_scores <- block_group_need_scores %>% 
  mutate(Geoid = as.numeric(geoid))

rm(percent_stops_in_equity_bg_no_geo)

#hex grids
 
quarter_mile_hex_grid <- sf::read_sf(here::here("raw_data", "hex_grids", "quarter_mile_hex_grid.shp"))%>% 
  mutate(Geoid = as.numeric(rowid))%>% 
  st_transform(2926) %>% 
  rmapshaper::ms_simplify(keep = .05)

parks <- read_sf(here::here("raw_data", "KCParkAccess.gdb"), layer = "KCParkAccessPoints")%>%
  st_transform(2926) %>% 
  janitor::clean_names() %>% 
  mutate(name = sitename, 
         address = closest_int, 
         assettype = "park", 
         codetype = "PK", 
         year = 2023,
         asset_group = "Park") %>% 
  select(name, address, assettype, codetype, point_x, 
         point_y, year, Shape, asset_group) %>% 
  distinct(address, .keep_all = T)

community_asset_groups <- read_csv(here::here("raw_data", "community_asset_groups.csv")) %>% 
  select(-type)
# load and clean community assets. Join to hex dataframe
community_assets <-sf::read_sf(here::here("raw_data", "CommunityAssets2023.gdb"), 
                               layer = "CommunityAssets_January2023") %>%
  st_transform(2926) %>% 
  janitor::clean_names() %>% 
  left_join(community_asset_groups) %>% 
  bind_rows(parks)

rm(parks)

#route shapefiles ####

excluded_routes <- c('629', '634', '635', '636', '641', '643',
                     '650', '654', '656', '657', '696', '697', 
                     '699', '715', '716', '717', '718', '719', 
                     '720', '722', '723', '724', '725', '730', 
                     '732', '733', '772', '779', '786', '791', 
                     '794', '795', '797', '851', '852', '853', 
                     '854', '855', '856', '857', '858', '859', '
                     862', '863', '880', '881', '882', '9999', 
                     '627', '630', '893', '895', 
                     '981', '982', '986', '987', '988', '989')
proposed_network <- sf::read_sf("raw_data/scenario/wk_allplanner_var_shape.shp") %>%
  rename(route_short_name = VAR_ROUTE,
         variant = VAR_IDENT,
         direction = VAR_DIREC,
         description = VAR_DESCR) %>%
  distinct(route_short_name, .keep_all = T)  %>% 
  filter(!route_short_name %in% excluded_routes ) %>% 
  st_transform(2926) %>%
  rmapshaper::ms_simplify(keep = .2) 

#mapview::mapview(proposed_network)
baseline_network <- sf::read_sf("raw_data/baseline/simplified_routes_243.shp") %>%
  rename(route_short_name = RTE_NAME,
        # variant = VAR_IDENT, #not in simplified routes
         direction = DIR_CODE #, #IN OUT vs I/O may need to update code later?
         #description = VAR_DESCR
         )%>%
  #st_set_crs(4326) %>%
  st_transform(2926)%>%
  rmapshaper::ms_simplify(keep = .2)

baseline_grid <-  st_filter(quarter_mile_hex_grid , baseline_network) 


proposed_grid <- st_filter(quarter_mile_hex_grid , proposed_network) 

transit_grid <- bind_rows(baseline_grid, proposed_grid) #create hex sf of areas within quarter mile route buffer of transit

quarter_mile_hex_grid <- quarter_mile_hex_grid %>%  #filter full network by rowid. Taking out dupes. 
  filter(rowid %in% transit_grid$rowid) %>% 
  st_transform(4326)

rm(baseline_grid)
rm(proposed_grid)
rm(transit_grid)

baseline_network <- baseline_network %>% 
  st_transform(4326)

proposed_network <- proposed_network %>% 
  st_transform(4326)

parameters_raw <- read_csv(here::here("raw_data", paste0("input_parameters_SLC_P2_periods", ".csv"))) %>% 
  mutate(day_type  = stringr::str_replace_all(day_type, "_", " ")) %>% 
  mutate(day_type = stringr::str_to_title(day_type)) %>% 
  mutate(start_time = factor(start_time, levels = c("All Day", "AM", "MID", "PM", "EVE")) ) %>% 
  mutate(geography  = stringr::str_replace_all(geography, "_", " ")) %>% 
  mutate(geography = stringr::str_to_title(geography)) %>% 
  mutate(geography = factor(geography, levels = c( "Quarter Mile Hexagon", "Block Group"))) 


#create lookup tables

lookup_table_day_type <- tibble(day_type = unique(parameters_raw$day_type)) %>% 
  arrange() %>%
  rowid_to_column() %>% 
  rename(lookup_day_type = rowid)

lookup_table_start_time <- tibble(start_time = unique(parameters_raw$start_time)) %>%
  arrange() %>% 
  rowid_to_column() %>% 
  rename(lookup_start_time = rowid) %>% 
  mutate(start_time = factor(start_time, levels = c("All Day", "AM", "MID", "PM", "EVE")) )

lookup_table_trip_length <- tibble(max_trip_duration = unique(parameters_raw$max_trip_duration)) %>% 
  arrange() %>%
  rowid_to_column() %>% 
  rename(lookup_max_trip_duration = rowid)

lookup_table_geography <- tibble(geography= unique(parameters_raw$geography)) %>% 
  arrange() %>%
  rowid_to_column() %>% 
  rename(lookup_geography = rowid) %>% 
 
  arrange(geography)

lookup_table_asset_group <- tibble(assettype= unique(community_asset_groups$asset_group)) %>% 
  arrange() %>%
  rowid_to_column() %>% 
  rename(lookup_asset_group = rowid)

parameters_df <- parameters_raw %>% 
  select(run_id, geography, day_type, start_time, max_trip_duration) %>% 
  left_join( lookup_table_day_type) %>% 
  left_join(  lookup_table_start_time) %>% 
  left_join( lookup_table_geography) %>% 
  left_join( lookup_table_trip_length)

#coverage (map 2)

#baseline_transit_matrix <- readr::read_csv(here::here("raw_data", "SLC_Baseline_travel_time_matrix_periods.csv")) not used at the moment
  

proposed_transit_matrix <- readr::read_csv(here::here("raw_data", "SLC_Proposed_travel_time_matrix_periods.csv"))


#  metrics #####
network_data <-  read_csv(here::here( "raw_data",paste0("summary_comparison_", project_name, ".csv"))) %>% 
rename(geoid = GEO_ID_GRP) %>% 
  mutate(across(starts_with("access_strength"), ~round(.x*100, 2)) )%>% #convert basket of goods score from 0-1 scale to 0-100
  select(-c(percentile, cutoff, run_id, cutoffs, departure_datetime_baseline, departure_datetime_proposal)) %>% 
  pivot_longer(cols = !c(geoid:geography, change_in_coverage_label), 
               names_to = "Metric", 
               values_to = "Value") %>% 
  filter(!(Value == .001)) %>%  #remove values that were added as zero replacements for percentages
mutate(Value = case_when(Value == Inf ~ 1, 
                         Value == -Inf ~ -1, 
                         is.na(Value) ~ 0, 
                         is.nan(Value) ~ 0, 
                         TRUE ~ Value)) %>% 

  mutate(geography  = stringr::str_replace_all(geography, "_", " ")) %>% 
  mutate(geography = stringr::str_to_title(geography)) %>% 
  mutate(day_type  = stringr::str_replace_all(day_type, "_", " ")) %>% 
  mutate(day_type = stringr::str_to_title(day_type)) %>% 
  left_join(lookup_table_day_type) %>% 
  left_join(lookup_table_geography) %>% 
  left_join(lookup_table_start_time) %>% 
  left_join(lookup_table_trip_length) %>% 
  mutate(Metric  = stringr::str_replace_all(Metric, "_", " ")) %>% 
  mutate(Metric = stringr::str_to_title(Metric)) %>% 
  janitor::clean_names("title")
  


network_data_details <- read_csv(here::here( "raw_data", paste0("asset_group_comparison_", project_name, ".csv"))) %>% 
  select(-c( percentile, cutoff, run_id, cutoffs,departure_datetime_baseline, 
             departure_datetime_proposal,id_baseline, id_proposal, row_number_baseline, 
             row_number_proposal, time_window_baseline, time_window_proposal, starts_with("count_"), starts_with("asset_score"))) %>% # geometry_baseline, geometry_proposal, 
  mutate(assettype  = stringr::str_replace_all(assettype, "_", " ")) %>% 
  mutate(assettype = stringr::str_to_title(assettype)) %>% 
  rename(geoid = GEO_ID_GRP) %>% 
  pivot_longer(cols = !c(geoid, day_type, start_time,  max_trip_duration, geography, assettype), 
               names_to = "Metric", 
               values_to = "Value") %>% 
 # filter(!(Value == .001)) %>%  #remove values that were added as zero replacements for percentages
  mutate(Value = case_when(Value == Inf ~ 1, 
                           Value == -Inf ~ -1, 
                           is.na(Value) ~ 0, 
                           is.nan(Value) ~ 0, 
                           TRUE ~ Value)) %>% 
  mutate(Value = case_when( stringr::str_detect(Metric, "percent") ~ round(Value*100, 2), 
                            TRUE ~ round(Value, 2))) %>% 
  mutate(geography  = stringr::str_replace_all(geography, "_", " ")) %>% 
  mutate(geography = stringr::str_to_title(geography)) %>% 
  mutate(day_type  = stringr::str_replace_all(day_type, "_", " ")) %>% 
  mutate(day_type = stringr::str_to_title(day_type)) %>% 
  left_join(lookup_table_asset_group) %>% 
  left_join(lookup_table_day_type) %>% 
  left_join(lookup_table_geography) %>% 
  left_join(lookup_table_start_time) %>% 
  left_join(lookup_table_trip_length) %>% 
  mutate(Metric  = stringr::str_replace_all(Metric, "_", " ")) %>% 
  mutate(Metric = stringr::str_to_title(Metric)) %>% 
  janitor::clean_names("title")


unique(network_data_details$Metric)
unique(network_data_details$assettype)
unique(network_data_details$lookup_asset_group)

na_check <- network_data_details %>% 
  filter(is.na(`Lookup Asset Group`)) %>% 
  distinct(Assettype)

study_area <- sf::read_sf(here::here("raw_data", "study_area", "slc_ph2.shp")) %>% 
  st_transform(2926)

quarter_mile_hex_grid_2926 <- sf::read_sf(here::here("raw_data", "hex_grids", "quarter_mile_hex_grid.shp"))%>% 
  mutate(geoid = as.numeric(rowid)) %>% 
  st_transform(2926) %>% 
  st_filter( study_area)#putting in more accurate harn for spatial clip operation

summary_table_bog <-   read_csv(here::here( "raw_data",paste0("summary_comparison_", project_name, ".csv"))) %>% 
  rename(geoid = GEO_ID_GRP) %>% 
  select(-c(percentile, cutoff, run_id, cutoffs, departure_datetime_baseline, departure_datetime_proposal)) %>% 
  filter(geography  == "quarter_mile_hexagon" & max_trip_duration == 45 & geoid %in% quarter_mile_hex_grid_2926$geoid) %>% 
  group_by(day_type) %>% 
  summarise( mean_access_strength_baseline = scales::percent(mean(access_strength_baseline, na.rm = T),scale = 100 , accuracy = 0.1), 
             mean_access_strength_proposal = scales::percent( mean(access_strength_proposal, na.rm = T), scale = 100, accuracy = 0.1),
             mean_change_in_access_strength = scales::percent(mean(change_in_access_strength, na.rm = T) ,  scale = 100, accuracy = 0.1)) %>% 
  mutate(day_type = factor(day_type, levels = c("weekday", "saturday", "sunday"))) %>% 
  arrange(day_type) %>% 
  mutate(day_type = str_to_title(day_type)) %>% 
  janitor::clean_names("title")


summary_table_bog_time <-   read_csv(here::here( "raw_data",paste0("summary_comparison_", project_name, ".csv"))) %>% 
  rename(geoid = GEO_ID_GRP) %>% 
  select(-c(percentile, cutoff, run_id, cutoffs, departure_datetime_baseline, departure_datetime_proposal)) %>% 
  filter(geography  == "quarter_mile_hexagon" & max_trip_duration == 45 & geoid %in% quarter_mile_hex_grid_2926$geoid) %>% 
  group_by(day_type, start_time) %>% 
  summarise( mean_access_strength_baseline = scales::percent(mean(access_strength_baseline, na.rm = T),scale = 100 , accuracy = 0.1), 
             mean_access_strength_proposal = scales::percent( mean(access_strength_proposal, na.rm = T), scale = 100, accuracy = 0.1),
             mean_change_in_access_strength = scales::percent(mean(change_in_access_strength, na.rm = T) ,  scale = 100, accuracy = 0.1)) %>% 
  #mutate(start_time = factor(start_time, levels = c("7 AM", "12 PM", "7 PM", "9 PM"))) %>% 
  mutate(day_type = factor(day_type, levels = c("weekday", "saturday", "sunday"))) %>% 
  arrange(day_type, start_time) %>% 
  mutate(day_type = str_to_title(day_type)) %>% 
  mutate(start_time= str_to_title(start_time)) %>% 
  janitor::clean_names("title")


summary_table_assets <- read_csv(here::here( "raw_data", paste0("asset_group_comparison_", project_name, ".csv"))) %>% 
  rename(geoid = GEO_ID_GRP) %>% 
  select(-c(percentile, cutoff, run_id, cutoffs, departure_datetime_baseline, departure_datetime_proposal)) %>% 
  filter(geography  == "quarter_mile_hexagon" & max_trip_duration == 45 & geoid %in% quarter_mile_hex_grid_2926$geoid
         & !(assettype %in% c("low_wage_jobs", "mid_wage_jobs", "high_wage_jobs", "total_jobs")) ) %>% 
  group_by(day_type, assettype) %>% #$
  summarise( mean_possible_destinations_baseline = round(mean(count_baseline, na.rm = T),1), 
             mean_possible_destinations_proposal = round(mean(count_proposal, na.rm = T),1),
             mean_change_in_possible_destinations = round(mean(change_in_possible_destinations, na.rm = T),1)) %>% 
  mutate(day_type = factor(day_type, levels = c("weekday", "saturday", "sunday"))) %>% 
  arrange(day_type, assettype) %>% 
  mutate(day_type = str_to_title(day_type)) %>% 
  rename(`Destination` = assettype) %>% 
  mutate(`Destination` = str_replace_all(`Destination`, "_", " ")) %>% 
  mutate(`Destination`  = str_to_title(`Destination`)) %>% 
  janitor::clean_names("title")

summary_table_jobs <- read_csv(here::here( "raw_data", paste0("asset_group_comparison_", project_name, ".csv"))) %>% 
  rename(geoid = GEO_ID_GRP) %>% 
  select(-c(percentile, cutoff, run_id, cutoffs, departure_datetime_baseline, departure_datetime_proposal)) %>% 
  filter(geography  == "quarter_mile_hexagon" & max_trip_duration == 45 & geoid %in% quarter_mile_hex_grid_2926$geoid
         & (assettype %in% c("low_wage_jobs", "mid_wage_jobs", "high_wage_jobs", "total_jobs")) ) %>% 
  group_by(day_type, assettype) %>% #$
  summarise( mean_reachable_jobs_baseline =  prettyNum(round(mean(count_baseline, na.rm = T),0), big.mark =","), 
             mean_reachable_jobs_proposal =  prettyNum(round(mean(count_proposal, na.rm = T),0), big.mark =","),
             mean_change_in_reachable_jobs =  prettyNum(round(mean(change_in_possible_destinations, na.rm = T),0), big.mark =",")) %>% 
  mutate(day_type = str_to_title(day_type)) %>% 
  rename(`Job Locations` = assettype) %>% 
  mutate(`Job Locations` = str_replace_all(`Job Locations`, "_", " ")) %>% 
  mutate(`Job Locations`  = str_to_title(`Job Locations`)) %>% 
  janitor::clean_names("title")

summary_table_jobs_time <- read_csv(here::here( "raw_data", paste0("asset_group_comparison_", project_name, ".csv"))) %>% 
  rename(geoid = GEO_ID_GRP) %>% 
  select(-c(percentile, cutoff, run_id, cutoffs, departure_datetime_baseline, departure_datetime_proposal)) %>% 
  filter(geography  == "quarter_mile_hexagon" & max_trip_duration == 45 & geoid %in% quarter_mile_hex_grid_2926$geoid
         & (assettype %in% c("total_jobs")) ) %>% 
  group_by(day_type, start_time) %>% #$
  summarise( mean_reachable_jobs_baseline = prettyNum(round(mean(count_baseline, na.rm = T),0), big.mark =","), 
             mean_reachable_jobs_proposal = prettyNum((round(mean(count_proposal, na.rm = T),0)), big.mark =","), 
             mean_change_in_reachable_jobs = prettyNum((round(mean(change_in_possible_destinations, na.rm = T),0)), big.mark =",")) %>% 
 # mutate(start_time = factor(start_time, levels = c("7 AM", "12 PM", "7 PM", "9 PM"))) %>% 
  mutate(day_type = factor(day_type, levels = c("weekday", "saturday", "sunday"))) %>% 
  arrange(day_type, start_time) %>% 
  mutate(day_type = str_to_title(day_type)) %>% 
  mutate(start_time= str_to_title(start_time)) %>% 
  janitor::clean_names("title")

  
summary_table_read_me <- tibble(Notes = c("This app compares the transit accessibility of various community destinations using the Fall
                                                                         2024 King County Metro network (baseline network) and the South Link Connections Phase 2 Network (proposal network). The results in this table
                                                                         present the mean changes in the South Link Connections study area for a 45 minute transit trip."))
  
#produce lookup table of Metrics last to merge two pivoted tables together
  
lookup_table_metric <- tibble(Metric = c(unique(network_data$Metric), unique(network_data_details$Metric))) %>% 
  mutate(Metric = factor(Metric, levels = c("Change In Access Range", "Change In Access Strength", "Change In Possible Destinations", 
                                            "Percent Change In Possible Destinations", "Access Range Baseline", "Access Range Proposal",
                                            "Access Strength Baseline", "Access Strength Proposal", "Possible Destinations Baseline", "Possible Destinations Proposal"
                                            ), ordered = TRUE)) %>% 
  mutate(Metric = Metric %>% 
           fct_relevel(c("Change In Access Range", "Change In Access Strength", "Change In Possible Destinations", 
                                            "Percent Change In Possible Destinations", "Access Range Baseline", "Access Range Proposal",
                                            "Access Strength Baseline", "Access Strength Proposal", "Possible Destinations Baseline", "Possible Destinations Proposal"))) %>% 
  arrange(Metric) %>% 
rowid_to_column() %>% 
  rename(lookup_metric = rowid) %>% 
  filter(!Metric %in% c("Id Baseline", "Id Proposed", "Row Number Baseline", "Time Window Baseline", 
                        "Row Number Proposed", "Time Window Proposed" 
                        ) ) 


lookup_table_metric
  
network_data <- network_data %>% 
  left_join(lookup_table_metric) %>% 
  janitor::clean_names("title") 



network_data_details <- network_data_details %>% 
  filter(!Metric %in% c("Id Baseline", "Id Proposed", "Row Number Baseline", "Time Window Baseline", 
                        "Row Number Proposed", "Time Window Proposed" ) ) %>% 
  left_join(lookup_table_metric) %>% 
  janitor::clean_names("title") 
    
sum(is.na(network_data_details$`Lookup Metric`))
sum(is.na(network_data$`Lookup Metric`))

#the data tables need to be separate because assettype is a separate field that cannot be easily appended to the summary data.


block_group_need_scores<- block_group_need_scores %>% 
  mutate(Geoid = as.numeric(Geoid)) %>% 
  select(Geoid, final_score)


epa_hatch <- block_groups %>%
  left_join(block_group_need_scores) %>% 
  mutate(equity_priority_area = ifelse(final_score >= 4, TRUE, FALSE)) %>%
  filter(equity_priority_area) %>% # filter for tracts that I want to receive a cross-hatch
  #st_make_valid(geos_method = "valid_structure") %>% #did't need this to complete 24.02.09
  st_cast(to= "MULTIPOLYGON") %>% 
  gg.layers::st_hatched_polygon(density = 700, angle = 60) %>% 
  #hatchedLayer( pattern = "left2right") %>% 
  #HatchedPolygons::hatched.SpatialPolygons(density = 700, angle = 60) %>%
  st_set_crs(4326) %>%
  mutate(col = 1) 


study_area <- study_area %>% 
  st_transform(4326) #convert to WGS 1984 for use in map

#export data objects #####
rm(project_name)
rm(test)
rm(na_check)

rm(community_assets) # adding in community assets to RDS to try adding them to app
rm(community_asset_groups)
rm(excluded_routes)
rm(parks)
rm(quarter_mile_hex_grid_2926)
rm(block_group_need_scores)
rm(block_group_centroids)
#rm(study_area)
library(purrr)
library(here)

object_saver <- function(object_name) {
  object <- get(object_name)
  saveRDS(object, here::here("input", "r-objects", paste0(object_name, ".RDS")))

}

object_list <- ls()

object_list <- object_list[! object_list %in% c("object_saver")]

map(object_list, object_saver)





#hatch testing #####

# mapview::mapview(epa_test)
# 
# epa_test <- epa_hatch[epa_hatch$geoid=="530330323161",]
# 
# 
# epa_test_hatched <- epa_test %>% 
#   HatchedPolygons::hatched.SpatialPolygons(density = 700, angle = 60)
#   
# mapview::mapview(epa_test_hatched)
# 
# sf_layer_polygon <- sf::st_cast(epa_hatch, "POLYGON") %>%
#   mutate(name = as.character(1:n()))
# 
# ggplot(sf_layer_polygon) +
#   geom_sf(aes(fill = name))
# 
# mapview::mapview(sf_layer_polygon[540,], zcol= "name")
#
# 
# # Try to find out which layer has a problem
# for (i in 541:nrow(sf_layer_polygon)) {
#   print(i)
#   
#   layer <- sf_layer_polygon %>% 
#     slice(540)
#   
#   layer.sp <- as(layer, "Spatial")
#   
#   # Try different densities, the smallest the polygon, the biggest the density
#   hatched <-  HatchedPolygons::hatched.SpatialPolygons(layer, density = c(500), angle = c(45))
#   
#   plot(layer["name"])
#   # plot(layer["ID"], type = "p")
#   # points(layer.sp@polygons[[1]]@Polygons[[1]]@coords)
   
# }



