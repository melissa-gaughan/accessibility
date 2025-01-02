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

#thoughts re: inaccurate results 24-01-18
  #1. you reused the network and the pbf files. Maybe need to recalc individually?
  #2. You reused the snapped centroids with the network. Could also be a problem
  #3. test with 1 run of both networks, not 48 :-(
  #4. I'm also kind of wondering if this is a Remix GTFS issue. Would the results be the same with a production GTFS?
  #5. Issues could be related to walk distances?

#Testing notes: 
  #1. I re-extracted the pbf files from the osmextract files. I put the files directly in the 233_test and 233_test_no_7 folders
  #2. Regenerated transit street networks for both networks
  #3. Snapped points to both networks
#get block groups


block_groups <- read_sf("data/shapefiles/blkgrp20_shore.shp") %>% 
  st_cast(to = "POLYGON")
#sf interpolation function did not work with a multipolygon, so I cast block groups to a simplified polygon

blocks <- blocks("WA", 
                 "King", 
                 year = 2020) %>% #has to be a decennial year to get the requisite POP20 field
  st_transform(2926)


# hex grid creation #####

#make a hex grid from the block groups, erase the water, get ready for spatial analysis

# hexagon_grid <- st_make_grid(block_groups, 
#                              cellsize = 1320,
#                              #what = "polygons", 
#                              square = F #, 
#                              # flat_topped = T
# ) 
no_water_hex_grid <-  read_sf(here::here("data", "shapefiles", "quarter_mile_hex_grid.shp")) %>% 
  #rowid_to_column() %>% 
  st_transform(2926) 
#st_layers(here::here("data", "shapefiles", "CommunityAssets2023.gdb"))
#st_layers(here::here("data", "shapefiles", "KCParkAccess.gdb"))

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

parks_hex <-  no_water_hex_grid %>% 
  st_join(. ,parks, join = st_intersects, left = TRUE) %>% 
  st_drop_geometry() %>% 
  drop_na(name) %>% 
  group_by(rowid, name) %>% 
  summarise(park_access_points = n()) %>% #sum all access points in the hex per park
  group_by(rowid) %>% 
  summarise(parks = n()) # roll up to count of unique parks per hex


jobs <- st_read(here::here("data", "shapefiles","2021_LEHD",  "points_2021.shp")) %>% 
  st_transform(2926) %>% 
  janitor::clean_names() %>% 
  rename(total_jobs = c000, 
         low_wage_jobs = ce01, 
         mid_wage_jobs = ce02, 
         high_wage_jobs = ce03) %>% 
  select(id, total_jobs, low_wage_jobs, mid_wage_jobs, high_wage_jobs, geometry)


jobs_hex <-  no_water_hex_grid %>% 
  st_join(. , jobs, join = st_intersects, left = TRUE) %>% 
  st_drop_geometry() %>% 
  drop_na(total_jobs) %>% 
  group_by(rowid) %>% 
  summarise(total_jobs = sum(total_jobs), 
            low_wage_jobs = sum(low_wage_jobs, na.rm = T), 
            mid_wage_jobs = sum(mid_wage_jobs, na.rm = T), 
            high_wage_jobs = sum(high_wage_jobs, na.rm = T))

# load and clean community assets. Join to hex dataframe
community_assets <-sf::read_sf(here::here("data", "shapefiles", "CommunityAssets2023.gdb"), 
                               layer = "CommunityAssets_January2023") %>%
  st_transform(2926) %>% 
  janitor::clean_names()

community_asset_groups <- read_csv("analyses/accessibility/data/community_asset_groups.csv") %>% #24.4.19 added parks
  select(-type)

community_assets <- left_join(community_assets, community_asset_groups) %>% 
  group_by(asset_group)

sum(is.na(community_assets$asset_group))
unique(community_assets$assettype)

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
  janitor::clean_names() %>% 
  left_join(parks_hex) %>% 
  left_join(jobs_hex) %>% 
 mutate(parks =  replace_na(parks, 0), # parks have now been simplified so that the count indicates the number of parks in the hex, not the number of access points
        total_jobs = replace_na(total_jobs, 0), 
        low_wage_jobs = replace_na(low_wage_jobs, 0), 
        mid_wage_jobs = replace_na(mid_wage_jobs, 0), 
        high_wage_jobs = replace_na(high_wage_jobs, 0)) #you will need to exclude jobs from the basket of goods summaries

hex_dest <- no_water_hex_grid %>%  #join table data to spatial dataframe
  left_join(asset_summary) %>% 
  st_centroid() %>% 
  st_transform(4326) %>% 
  rename(id = rowid)

  #create points from the hex grid. Snap to street network. r5r core has to be turned on before this will work.

#for r5, crs has to be 4326

hex_points <- no_water_hex_grid %>% 
  st_centroid() %>% 
  st_transform(4326) %>% 
  rename(id = rowid)

data_path <- "C:/Users/mgaughan/Documents/Projects/service-planning/analyses/accessibility/data/233_network"

r5r_core <- setup_r5(data_path = data_path, verbose = F)


#r5r::stop_r5(r5r_core)
#rJava::.jgc(R.gc = TRUE)

# static input #####


#

#testing input #####
# can I do this long? Do I need spatial data for the join? Can I just do it on the row id?
# need to figure out how to do this with interpolate_pw. I think this is the piece that is inherently spatial and
# may need to be wide rather than long. 
# destinations <- community_assets %>% 
#   ungroup %>% 
#   mutate(id = as.character(row_number()), opportunities = 1) %>% 
#   # st_drop_geometry() %>% 
#   rename(
#     lon = point_x, 
#     lat = point_y) %>% 
#   st_transform(4326)
# 
# 
# # 
#  mode <- c("WALK", "TRANSIT")
#  max_walk_dist <- 5000
#  max_trip_duration <- 45
# departure_datetime <- as.POSIXct("05-09-2022 12:00:00",
#                                  format = "%d-%m-%Y %H:%M:%S")
# r5r_core$setTimeWindowSize(1L)
#  origins <- hex_points_sf
#  destinations <- hex_points_sf
# breakdown <- F
#  verbose <- F

# function #####
travel_time_compare <- function(run_id, 
                                departure_datetime,
                                geography, 
                               # mode,
                               # max_walk_time = 15 ,
                             #  max_walk_dist, #done by time in accessibility function. 
                                max_trip_duration ,
                              # time_window = 1L, 
                               origins, 
                               destinations, 
                               breakdown, 
                               verbose, 
                             decay_function = "logistic",
                             cutoffs
                               ) {


#using a decay function does produce more believable results
#i am having a really hard time getting it to work with multiple output
#thinking about maybe wrapping this in a loop for each asset type and then flattening down. 
#you changed a bunch of stuff in here and were pretty non-linear in the development
# would recommend being VERY CAREFUL about using this until uipdates are made

ttm <- accessibility(r5r_core = r5r_core,
                            decay_function = "logistic",
                            origins = eval(parse(text=origins)),
                     destinations = eval(parse(text=destinations)),
                    # origins = dest_hex_points,  #eval(parse(text=origins)),
                     
                    # destinations = dest_hex_points,
                       opportunities_colname = c( "public_spaces", 
                                                  "assistance",
                                                 "health_wellness", 
                                                 "grocery",
                                                 "municipal_services", 
                                                 "primary_education", 
                                                 "orca_fare_outlet", 
                                                 "housing", 
                                                 "secondary_education", 
                                                 "day_care", 
                                                 "shopping_center", 
                                                 "parks", 
                                                 "total_jobs", 
                                                 "low_wage_jobs", 
                                                 "mid_wage_jobs", 
                                                 "high_wage_jobs"
                                                 )  , #eval(parse(text =destinations)),
                            mode = c("WALK", "TRANSIT"),
                            departure_datetime = as.POSIXct(departure_datetime, 
                                                            format = "%d/%m/%Y %H:%M"),
                            max_walk_time = 15,
                          #  time_window = 10L, # not relevant because we don't use a frequency based GTFS. 
                            max_trip_duration = max_trip_duration,
                      
                            verbose = verbose, 
                     decay_value = 4, #controls standard deviation
                     cutoffs = cutoffs)#controls median of decay function

ttm_check <- no_water_hex_grid %>% 
  mutate(id = as.character(rowid)) %>% 
  left_join(ttm) %>% 
  # filter( opportunity == "grocery") %>% 
  #mutate(accessibility = round(accessibility, 0)) %>% #not rounding here to prevent issues with interpolation later 23.10.18
  st_transform(4326) %>% 
  filter(accessibility > 0) 

#saveRDS(ttm_check, "C:/Users/mgaughan/Documents/Projects/accessibility-demo/221_noon_access.rds")


#ttm_check <- readRDS( "C:/Users/mgaughan/Documents/Projects/accessibility-demo/public_space_grocery_access.rds") 

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



# access_summary_geo <- no_water_hex_grid %>% 
#   mutate(rowid = as.character(rowid)) %>% 
#  #full join?
#    left_join(ttm, by = c("rowid" = "id")) 

# small_sum<- head(access_summary_geo, 100)
# 
# mapview(small_sum)
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



#}
r5r::stop_r5()

# first run #####
#Question---will this work with NetPlan GTFS? 23.11.16 - Yes, it works but you have to validate the NetPlan feed. 

#datapath with GTFS and OSM extract. Eventually we will need a second folder with the other GTFS
data_path <- "C:/Users/mgaughan/Documents/Projects/service-planning/analyses/accessibility/data/233_network"

# r5r core setup

r5r_core <- setup_r5(data_path = data_path, verbose = F)

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
   mutate(departure_datetime= lubridate::ymd_hm(departure_datetime) )
  #mutate(departure_datetime = as.POSIXct(departure_datetime)) Failed to parce times 24.01.12

 
parameters <- parameters_raw %>% 
 # filter(start_time == "12 PM" & cutoffs == 30 & max_trip_duration == 45) %>% 
  select(-c(day_type, start_time)) #%>% 
# slice_head(n=1)

class(parameters$departure_datetime)
print(parameters$departure_datetime)

#Metric lookup will need to be created later

#one_line_test <- head(parameters,1)

# test_accessibility <- travel_time_compare(run_id = one_line_test$run_id, 
#                                           departure_datetime = "5/9/2023 12:00", 
#                                           max_trip_duration = one_line_test$max_trip_duration, 
#                                           origins = one_line_test$origins, 
#                                           destinations = one_line_test$destinations, 
#                                           breakdown = one_line_test$breakdown, 
#                                           verbose = T, 
#                                          cutoffs = one_line_test$cutoffs )

baseline_accessibility <- pmap( parameters, .f= travel_time_compare )
saveRDS(baseline_accessibility, "C:/Users/mgaughan/Documents/Projects/service-planning/analyses/accessibility/output/233_all_days.rds")

baseline_accessibility <- readRDS( "C:/Users/mgaughan/Documents/Projects/service-planning/analyses/accessibility/output/233_all_days.rds")


# second run #####
r5r::stop_r5()

data_path_2 <- "C:/Users/mgaughan/Documents/Projects/service-planning/analyses/accessibility/data/mc_network"

r5r_core <- setup_r5(data_path = data_path_2, verbose = F, overwrite = T)

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

#second parameters call needed because the two networks may not have the same operational dates in the calendar.txt file
parameters_raw <- read_csv("analyses/accessibility/data/input_parameters_all_days_233.csv") %>% 
  mutate(departure_datetime= lubridate::mdy_hm(departure_datetime) )
 # mutate(departure_datetime = as.POSIXct(departure_datetime))

parameters <- parameters_raw %>%  
  filter(start_time == "12 PM" & cutoffs == 30 & max_trip_duration == 45) %>% 
  select(-c(day_type, start_time)) %>% 
  slice_head(n=1)

class(parameters$departure_datetime)
print(parameters$departure_datetime)

proposed_network_accessibility <-  pmap( parameters, .f= travel_time_compare )   

saveRDS(proposed_network_accessibility, "C:/Users/mgaughan/Documents/Projects/service-planning/analyses/accessibility/output/mc_all_days.rds")


proposed_network_accessibility <- readRDS( "C:/Users/mgaughan/Documents/Projects/service-planning/analyses/accessibility/output/mc_all_days.rds")

#bind results and compare #####

baseline_accessibility_df <- bind_rows(baseline_accessibility) %>% 
  distinct() %>% 
  filter(!is.na(count)) %>% 
  #select(!geometry) %>% 
  left_join(parameters_raw %>% select(run_id, departure_datetime, start_time, day_type,
                             max_trip_duration, cutoffs, geography))
unique(baseline_accessibility_df$asset_score)
unique(baseline_accessibility_df$assettype)

baseline_jobs_summary <- baseline_accessibility_df %>% 
  filter(assettype %in% c("high_wage_jobs",
                          "mid_wage_jobs", 
                          "low_wage_jobs", 
                          "total_jobs"))


baseline_accessibility_summary <- baseline_accessibility_df %>% 
  filter(assettype %in% c("public_spaces", 
                          "assistance",
                          "health_wellness", 
                          "grocery",
                          "municipal_services", 
                          "primary_education", 
                          "orca_fare_outlet", 
                          "housing", 
                          "secondary_education", 
                          "day_care", 
                          "shopping_center", 
                          "parks")) %>% 
  group_by(GEO_ID_GRP, percentile, cutoff,  run_id, 
         departure_datetime, start_time, day_type, max_trip_duration, cutoffs, geography) %>% 
summarise(sum_asset_score = sum(asset_score, na.rm = T)) %>% 
  mutate(basket_of_goods_score = sum_asset_score/24) #2 * 12, the number of asset categories

# test_baseline_dupe <- baseline_accessibility_df %>% 
#   group_by(GEO_ID_GRP, percentile, cutoff, assettype, count, run_id, departure_datetime, max_trip_duration, cutoffs, geography) %>% 
#   mutate(group_count = n()) %>% 
#   filter(group_count >= 2) %>% 
#   distinct()

proposed_accessibility_df <- bind_rows(proposed_network_accessibility)%>% 
  distinct() %>% 
  filter(!is.na(count)) %>% 
 # select(-geometry) %>% 
  left_join(parameters_raw %>% select(run_id, departure_datetime, start_time, day_type,
                           max_trip_duration, cutoffs, geography))

unique(proposed_accessibility_df$asset_score)


proposed_accessibility_summary <- proposed_accessibility_df %>% 
  filter(assettype %in% c("public_spaces",  #filter to remove jobs from basket of goods analysis
                          "assistance",
                          "health_wellness", 
                          "grocery",
                          "municipal_services", 
                          "primary_education", 
                          "orca_fare_outlet", 
                          "housing", 
                          "secondary_education", 
                          "day_care", 
                          "shopping_center", 
                          "parks")) %>%
  group_by(GEO_ID_GRP, percentile, cutoff,  run_id, 
           departure_datetime, start_time, day_type, max_trip_duration, cutoffs, geography) %>% 
  summarise(sum_asset_score = sum(asset_score, na.rm = T)) %>% 
  mutate(basket_of_goods_score = sum_asset_score/24)

asset_group_comparison <- baseline_accessibility_df %>% 
  #filter(run_id == 1) %>% 
full_join(proposed_accessibility_df, 
          suffix = c("_baseline", "_proposed"),  #need to join by start time, not departure date time, because dates can be different between the two networks
          by = c("GEO_ID_GRP", "percentile", "cutoff", "assettype",  "run_id", 
                 "start_time", "day_type", "max_trip_duration", "cutoffs", "geography")) %>% 
 mutate(count_baseline = replace_na(count_baseline, 0), 
    count_proposed = replace_na(count_proposed, 0),
         change_in_asset_count = count_proposed - count_baseline,
         percent_change_in_asset_count = (count_proposed - count_baseline)/ count_baseline )# %>% 
 # select(-c(geometry_baseline, geometry_proposed, id_baseline, id_proposed))


summary <- asset_group_comparison %>% 
  group_by(run_id, geography, assettype) %>% 
  summarise(mean_change = round(mean(change_in_asset_count , na.rm = T),3), 
            max_change = max(change_in_asset_count, na.rm = T), 
            min_change = min(change_in_asset_count, na.rm = T), 
            median_change = median(change_in_asset_count, na.rm = T), 
            zeros = sum(change_in_asset_count==0, na.rm = T), 
            na_count = sum(is.na(change_in_asset_count)),
            observations = n())

 na_correction <- asset_group_comparison %>% 
   filter(count_baseline == 0 | count_proposed == 0 )%>% 
   #address divide by zero issues
   mutate(percent_change_in_asset_count = case_when(count_baseline == 0 & count_proposed == 0 ~ 0, 
                                                    count_baseline == 0 & count_proposed != 0 ~ 1,
                                                    count_baseline != 0 & count_proposed == 0 ~ -1,
                                                    TRUE ~ percent_change_in_asset_count))
 
 
 asset_group_comparison <- asset_group_comparison %>% 
   filter(!(count_baseline == 0 | count_proposed == 0 ))%>%
   bind_rows(na_correction) %>% 
   select(-c(asset_score_baseline, asset_score_proposed))
 

summary_comparison <- baseline_accessibility_summary %>% 
  #filter(run_id == 1) %>% 
  full_join(proposed_accessibility_summary, 
            suffix = c("_baseline", "_proposed"), 
            by = c("GEO_ID_GRP", "percentile", "cutoff",  "run_id", "start_time", "day_type",  "max_trip_duration", "cutoffs", "geography")) %>% 
  mutate(basket_of_goods_score_baseline = replace_na(basket_of_goods_score_baseline, 0),
         basket_of_goods_score_proposed = replace_na(basket_of_goods_score_proposed, 0) ) %>% #,
         # sum_asset_score_baseline = replace_na(sum_asset_score_baseline, 0.001), 
         # sum_asset_score_proposed = replace_na(sum_asset_score_proposed, 0.001)
         # 
         # ) %>% 
  select(-(starts_with("sum_asset_score"))) %>% 
  mutate(basket_of_goods_difference = basket_of_goods_score_proposed - basket_of_goods_score_baseline) #%>% 
  # mutate(percent_change_in_basket_of_goods = basket_of_goods_difference/ basket_of_goods_score_baseline) %>%  #percent of percents--confusing, not needed?
  #address divide by zero issues
  # mutate(percent_change_in_basket_of_goods = case_when(basket_of_goods_score_baseline == 0 & basket_of_goods_score_proposed == 0 ~ 0, 
  #                                                      basket_of_goods_score_baseline == 0 & basket_of_goods_score_proposed != 0 ~ 1,
  #                                                      basket_of_goods_score_baseline != 0 & basket_of_goods_score_proposed == 0 ~ -1,
  #                                                  TRUE ~ percent_change_in_basket_of_goods))

  




write_csv(summary_comparison, "analyses/accessibility/output/summary_comparison_233_mc.csv")

write_csv(asset_group_comparison ,"analyses/accessibility/output/asset_group_comparison_233_mc.csv" )

colnames(asset_group_comparison)
