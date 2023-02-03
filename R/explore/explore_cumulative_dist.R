# prepare R environment
source("R/project_libraries.R")
source("R/project_load_movement_data.R")
source("R/project_load_breeding_data.R")

# make a spatial trajectory object of each animal's encounter history
tr_plover_wp <- 
  as.ltraj(xy = st_coordinates(plover_tagging_sf),
           date = plover_tagging_sf$timestamp_gmt,
           id = plover_tagging_sf$ring)


tr_plover_wp[[1]]