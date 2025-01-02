#install.packages("osmextract")
library(osmextract)
library(tidyverse)
library(sf)
library(mapview)
block_groups <- read_sf("data/shapefiles/blkgrp20_shore.shp") %>% 
 # st_transform(2926) %>% 
  st_make_valid() %>% 
  st_union()

hexagon_grid <- read_sf("data/shapefiles/eigth_mile_hex_grid.shp")


hexagon_grid <- st_make_grid(block_groups, 
                             cellsize = 2640,
                             #what = "polygons", 
                             square = F #, 
                            # flat_topped = T
                            ) 
no_water_hex_grid <-   st_as_sf(hexagon_grid) %>% 
  tigris::erase_water()

no_water_hex_grid <- test %>% 
  st_transform(4326) %>% 
  rowid_to_column()

mapview(hexagon_grid)

mapview(test)

osm_file <- oe_match(block_groups)


oe_download(
  file_url = osm_file$url, 
  file_size = osm_file$file_size,
  provider = "test",
  download_directory = 'C:/Users/mgaughan/Documents/Projects/service-planning/analyses/accessibility/data'
)

block_group_error <- rbind(block_groups[1483,], block_groups[1519,])

mapview(block_groups)
