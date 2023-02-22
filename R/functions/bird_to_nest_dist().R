bird_to_nest_dist <- function(bird_ring, 
                              focal_nest_ID = NULL, 
                              tag_and_breeding_data,
                              local_time_zone,
                              focal_year, cut_time = NULL){
  
  
  if(is.null(cut_time)){
    tag_and_breeding_data$tagging %>% 
      mutate(timestamp_local = ymd_hms(timestamp_local, tz = local_time_zone)) %>% 
      filter(ring == bird_ring & 
               year(timestamp_local) == focal_year) %>% 
      left_join(., tag_and_breeding_data$nests %>% 
                  filter(ring == bird_ring & family_ID == focal_nest_ID) %>% 
                  dplyr::select(ring, lon, lat, nest_initiation_date, end_date) %>% distinct(), by = "ring") %>%
      filter(timestamp_local > nest_initiation_date) %>% 
      filter(timestamp_local < end_date) %>% 
      rename(bird_lat = lat.x,
             bird_lon = lon.x,
             nest_lat = lat.y,
             nest_lon = lon.y) %>% 
      mutate(dist_from_nest = distHaversine(p1 = matrix(c(bird_lon, bird_lat), ncol = 2),
                                            p2 = matrix(c(nest_lon, nest_lat), ncol = 2))) %>%
      mutate(rounded_hour = round(timestamp_local, "hours") %>%
               format(., format = "%H:%M:%S"),
             time_of_day = format(timestamp_local, format = "%H:%M:%S"),
             timestamp_local = as.character(timestamp_local))
  } else{
    tag_and_breeding_data$tagging %>% 
      mutate(timestamp_local = ymd_hms(timestamp_local, tz = local_time_zone)) %>% 
      filter(ring == bird_ring & 
               year(timestamp_local) == focal_year &
               timestamp_local < ymd_hms(cut_time, tz = local_time_zone)) %>% 
      left_join(., tag_and_breeding_data$nests %>% 
                  filter(ring == bird_ring & 
                           ymd_hms(cut_time, tz = local_time_zone) > nest_initiation_date &
                           ymd_hms(cut_time, tz = local_time_zone) < end_date + 1) %>% 
                  dplyr::select(ring, lon, lat) %>% distinct(), by = "ring") %>%
      rename(bird_lat = lat.x,
             bird_lon = lon.x,
             nest_lat = lat.y,
             nest_lon = lon.y) %>% 
      mutate(dist_from_nest = distHaversine(p1 = matrix(c(bird_lon, bird_lat), ncol = 2),
                                            p2 = matrix(c(nest_lon, nest_lat), ncol = 2))) %>%
      mutate(rounded_hour = round(timestamp_local, "hours") %>%
               format(., format = "%H:%M:%S"),
             time_of_day = format(timestamp_local, format = "%H:%M:%S"),
             timestamp_local = as.character(timestamp_local))
  }
}
