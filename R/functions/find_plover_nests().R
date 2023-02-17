find_plover_nests <- 
  function(tagging_data,
           longitude_col = "lon",
           latitude_col = "lat",
           timestamp_local_col = "timestamp_local",
           local_UTM_zone = 13,
           bird_ring,
           night_fix_col = "night_fix",
           circadian_search_period = c("all", "daytime", "nighttime"),
           local_time_zone = "America/Mazatlan",
           recurse_radius_size = 3,
           recurse_time_threshold,
           recurse_time_threshold_unit = c("hours", "secs", "mins", "days"),
           revisit_threshold = 1,
           cluster_distance_threshold = 30,
           nest_data,
           tag_year){
    
    if(circadian_search_period == "all"){
        bird_tagging_data <- filter(tagging_data, 
                                    ring == bird_ring_col)
        
      } else if(circadian_search_period == "daytime"){
        bird_tagging_data <- filter(tagging_data, 
                                    ring == bird_ring & night_fix == 0)
        
      } else if(circadian_search_period == "nighttime"){
        bird_tagging_data <- filter(tagging_data, 
                                    ring == bird_ring & night_fix == 1)
      }
    
    # classify the timestamp string as ymd_hms with the local timezone
    bird_tagging_data <- 
      bird_tagging_data %>% 
      mutate(timestamp_local = ymd_hms(timestamp_local, 
                                       tz = "America/Mazatlan")) %>% 
      filter(year(timestamp_local) == tag_year)
    
    # transform the tagging data into UTM so that the units are in meters
    tag_utm <- 
      bird_tagging_data %>% 
      st_as_sf(x = .,                         
               coords = c(longitude_col, latitude_col),
               crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
      st_transform(., crs = paste("+proj=utm +zone=", local_UTM_zone, sep = "")) %>% 
      sfc_as_cols(., names = c("easting", "northing")) %>% 
      st_drop_geometry()
    
    # transform the UTM data into a move object to be used by the recurse package
    tag_utm_move <-
      df2move(df = arrange(as.data.frame(tag_utm), timestamp_local),
              track_id = "ring",
              proj = paste("+proj=utm +zone=", local_UTM_zone, sep = ""),
              x = "easting", y = "northing",
              time = "timestamp_local")
    
    # also make a WGS84 move object for the cluster analysis later
    tag_latlon_move <-
      df2move(df = arrange(as.data.frame(bird_tagging_data), timestamp_local),
              track_id = "ring",
              proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
              x = "lon", y = "lat",
              time = "timestamp_local")
    
    # calculate the recursions based on the radius size specified above
    bird_visit = getRecursions(x = tag_utm_move, 
                               radius = recurse_radius_size, 
                               threshold = recurse_time_threshold, 
                               timeunits = recurse_time_threshold_unit) 
    
    # subset to regions with more than the revisit threshold given the recursion 
    # specifications then convert to lat long format for the cluster analysis
    high_visitation_points <- 
      bird_visit$revisitStats %>% 
      group_by(coordIdx, x, y) %>% 
      summarise(n_visits = max(visitIdx)) %>% 
      arrange(desc(n_visits)) %>% 
      filter(n_visits > revisit_threshold) %>% 
      st_as_sf(x = .,                         
               coords = c("x", "y"),
               crs = paste("+proj=utm +zone=", local_UTM_zone, sep = "")) %>%
      st_transform(., crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
      sfc_as_cols(., names = c("lon", "lat")) %>% 
      st_drop_geometry() %>% 
      as.data.frame() %>% 
      `coordinates<-` (c("lon", "lat")) %>% 
      `proj4string<-` (CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
    
    # calculate pair-wise distance matrix between high_visitation_points
    mdist <- distm(high_visitation_points)
    
    # cluster all points using a hierarchical clustering approach
    hc <- hclust(as.dist(mdist), method = "complete")
    
    # define the distance threshold, in this case 30 m
    d = cluster_distance_threshold
    
    # define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
    high_visitation_points$clust <- cutree(hc, h = d)
    
    # get the centroid coords for each cluster
    likely_nests <- matrix(ncol = 2, nrow = max(high_visitation_points$clust))
    
    for (i in 1:max(high_visitation_points$clust))
      # gCentroid from the rgeos package
      likely_nests[i,] <- 
      gCentroid(subset(high_visitation_points, clust == i))@coords
    
    # make an empty list to store results
    nest_visits_list <- 
      vector(mode = "list", length = max(high_visitation_points$clust))
    
    # calculate the recursions of the bird into the cluster polygons
    for(i in 1:max(high_visitation_points$clust)){
      nest_polygon_i <- 
        circles(matrix(likely_nests[i, 1:2], nrow = 1), d = d, lonlat = T)
      
      nest_visits_list[[i]] <- 
        getRecursionsInPolygon(tag_latlon_move, 
                               polygon = polygons(nest_polygon_i))
    }
    
    likely_nests <- as.data.frame(likely_nests) %>% rename(lon = V1, lat = V2)
    
    nest_search_plot <- 
      bird_visit$revisitStats %>% 
      st_as_sf(x = .,                         
               coords = c("x", "y"),
               crs = paste("+proj=utm +zone=", local_UTM_zone, sep = "")) %>%
      st_transform(., crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
      sfc_as_cols(., names = c("lon", "lat")) %>% 
      st_drop_geometry() %>% 
      ggplot() +
      geom_point(aes(x = lon, y = lat, 
                     color = visitIdx, 
                     alpha = visitIdx)) +
      geom_point(data = likely_nests, aes(x = lon, y = lat), 
                 size = 8, color = "red",
                 shape = 1) +
      geom_segment(data = filter(nest_data, 
                                 ring == bird_ring & 
                                   year(nest_initiation_date) == tag_year),
                   aes(x = lon * 1.00001, y = lat, xend = lon, yend = lat),
                   arrow = arrow(length = unit(0.2, "cm"))) +
      luke_theme +
      theme(legend.position = "none") +
      xlab("longitude") +
      ylab("latitude")
    
    nest_searching_list <- list(nest_visits_list = nest_visits_list,
                                nest_search_plot = nest_search_plot)
    
    return(nest_searching_list)
  }