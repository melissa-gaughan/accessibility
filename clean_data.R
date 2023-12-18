
#library(shinydashboardPlus)
library(tidyverse)

library(sf)

library(here)
project_name <- "Lynnwood_Link_Accessibility"
# LOAD IN DATA ####
block_groups_raw <- sf::read_sf(here::here("input", "2020_block_groups", "blkgrp20_shore.shp")) %>% 
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
  rmapshaper::ms_simplify(keep = .05)

rm(block_groups_raw)

load(file =here::here( "input","route_and_block_group_equity_data.RDS"))

block_group_need_scores <- block_group_need_scores %>% 
  mutate(Geoid = as.numeric(geoid))

rm(percent_stops_in_equity_bg_no_geo)

#hex grids
  


quarter_mile_hex_grid <- sf::read_sf(here::here("input", "hex_grids", "quarter_mile_hex_grid.shp"))%>% 
  mutate(Geoid = as.numeric(rowid))%>% 
  st_transform(4326)


#route shapefiles ####
# proposed_network <- sf::read_sf("input/scenario/planner_var_shape.shp") %>% 
#   rename(route_short_name = VAR_ROUTE, 
#          variant = VAR_IDENT, 
#          direction = VAR_DIREC, 
#          description = VAR_DESCR) %>% 
#   st_set_crs(4326) %>% 
#   st_transform(4326) %>% 
#   rmapshaper::ms_simplify(keep = .2)
# 
# baseline_network <- sf::read_sf("input/baseline/planner_var_shape.shp") %>% 
#   rename(route_short_name = VAR_ROUTE, 
#          variant = VAR_IDENT, 
#          direction = VAR_DIREC, 
#          description = VAR_DESCR)%>% 
#   st_set_crs(4326) %>% 
#   st_transform(4326)%>% 
#   rmapshaper::ms_simplify(keep = .2)

# block group metrics #####
network_data <-  read_csv(here::here( "input",paste0("summary_comparison_233_LLink_4.csv"))) %>% 
rename(geoid = GEO_ID_GRP) %>% 
  select(-c(percentile, cutoff, run_id, cutoffs)) %>% 
  pivot_longer(cols = !c(geoid:geography), 
               names_to = "Metric", 
               values_to = "Value")


network_data_details <- read_csv(here::here( "input", paste0("asset_group_comparison_233_LLink_4.csv"))) %>% 
  select(-c(geometry_baseline, id_baseline, geometry_proposed, id_proposed, percentile, cutoff, run_id, cutoffs)) %>% 
  rename(geoid = GEO_ID_GRP) %>% 
  pivot_longer(cols = !c(geoid, departure_datetime, max_trip_duration, geography, assettype), 
               names_to = "Metric", 
               values_to = "Value")

#the data tables need to be separate because assettype is a separate field that cannot be easily appended to the summary data.
 
  mutate(across(.cols = -c(Geoid ,`Percent Change in Trips`), .fns = as.character))

block_group_need_scores<- block_group_need_scores %>% 
  mutate(Geoid = as.numeric(Geoid))

epa_hatch <- block_groups %>%
  left_join(block_group_need_scores) %>% 
  mutate(equity_priority_area = ifelse(final_score >= 4, TRUE, FALSE)) %>%
  filter(equity_priority_area) %>% # filter for tracts that I want to receive a cross-hatch
  st_make_valid(geos_method = "valid_structure") %>% 
  HatchedPolygons::hatched.SpatialPolygons(density = 700, angle = 60) %>%
  st_set_crs(4326) %>%
  mutate(col = 1) 

#export data objects #####
rm(project_name)

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



