# look at folder, read in folder names, remove.zip from name 
files_list <- list.files(here::here("input", "r-objects"), full.names = T)
files_short_names <- list.files(here::here("input", "r-objects"), full.names = F)
file_names <-  gsub(pattern = "\\.RDS$", replacement = "", x = basename(files_short_names))


files <- purrr::map(files_list, readRDS)

names(files) <- file_names


# UI Choices #####
metro <- "https://upload.wikimedia.org/wikipedia/en/thumb/b/bf/King_County_Metro_logo.svg/1280px-King_County_Metro_logo.svg.png"


day_type_choices <- files$lookup_table_day_type$lookup_day_type

names(day_type_choices)<- files$lookup_table_day_type$day_type

start_time_choices <-files$lookup_table_start_time$lookup_start_time
names(start_time_choices) <- files$lookup_table_start_time$start_time

metric_choices <- files$lookup_table_metric$lookup_metric
names(metric_choices) <- files$lookup_table_metric$Metric

trip_length_choices <-  files$lookup_table_trip_length$lookup_max_trip_duration
names(trip_length_choices) <- files$lookup_table_trip_length$max_trip_duration


asset_group_choices <- files$lookup_table_asset_group$lookup_asset_group
names(asset_group_choices) <- files$lookup_table_asset_group$assettype

geography_choices <- files$lookup_table_geography$lookup_geography
names(geography_choices) <- files$lookup_table_geography$geography

#help page

rmdfiles <- c("help.rmd")
sapply(rmdfiles, knitr::knit , quiet = T)
