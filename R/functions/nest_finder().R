tagging_data = tag_breeding_data_ceuta$tagging
longitude_col = "lon"
latitude_col = "lat"
# timestamp_local_col = "timestamp_local"
local_UTM_zone = 13
# bird_ring_col = "ring"
bird_ring = "CN0161"
# night_fix_col = "night_fix"
circadian_search_period = "daytime"
local_time_zone = "America/Mazatlan"
recurse_radius_size = 3
recurse_time_threshold = 3
recurse_time_threshold_unit = "days"
revisit_threshold = 1
cluster_distance_threshold = 30
nest_data = tag_breeding_data_ceuta$nests

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
           cluster_distance_threshold = 30){
    
    if(circadian_search_period == "all"){
        bird_tagging_data <- filter(tagging_data, 
                                    ring == bird_ring_col)
        
      } else if(circadian_search_period == "daytime"){
        bird_tagging_data <- filter(tagging_data, 
                                    ring == bird_ring & night_fix == 0)
        
      } else if(circadian_search_period == "nighttime"){
        bird_tagging_data <- filter(tagging_data, 
                                    ring == bird_ring_col & night_fix_col == 1)
      }
    
    # classify the timestamp string as ymd_hms with the local timezone
    bird_tagging_data <- 
      bird_tagging_data %>% 
      mutate(timestamp_local = ymd_hms(timestamp_local, 
                                       tz = "America/Mazatlan"))
    
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
    
    if(show_plot == TRUE){
      
      likely_nests <- as.data.frame(likely_nests) %>% rename(lon = V1, lat = V2)
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
                                     year(nest_initiation_date) == year(first_fix)),
                     aes(x = lon * 1.00001, y = lat, xend = lon, yend = lat),
                     arrow = arrow(length = unit(0.2, "cm"))) +
        luke_theme +
        theme(legend.position = "none") +
        xlab("longitude") +
        ylab("latitude")
    }
  }

                      
# bird_tagging_data_full <- 
#   filter(tag_breeding_data_ceuta$tagging, ring == bird_ring)
# bird_tagging_data_day <- 
#   filter(tag_breeding_data_ceuta$tagging, ring == bird_ring & night_fix == 0)
# bird_tagging_data_night <- 
#   filter(tag_breeding_data_ceuta$tagging, ring == bird_ring & night_fix == 1)

# bird_tagging_data <- 
#   bird_tagging_data_day %>% 
#   mutate(timestamp_local = ymd_hms(timestamp_local, tz = "America/Mazatlan"))







# # inspect the data
# plot(tag_utm$easting, tag_utm$northing, 
#      col = viridis_pal()(nrow(tag_utm)), pch = 20, 
#      xlab = "easting", ylab = "northing", asp = 1)

# also look at the data in regards to known nesting attempts
# tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
#                              bird_ring = "CN0161", map_year = 2018)

# # specify the radius size for the recursion (10m is adequate for the measurement 
# # error of the GPS tags)
# radius_size = 3
# 
# # calculate the recursions based on the radius size specified above
# bird_visit = getRecursions(x = tag_utm_move, 
#                            radius = radius_size, 
#                            threshold = 3, timeunits = "days") 
# 
# # plot the data and the recursions (also show the size of the radius in 
# # relation to the data and study site)
# par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
# plot(bird_visit, tag_utm_move, 
#      legendPos = c(min(tag_utm$easting)+1250, max(tag_utm$northing)))
# drawCircle(max(tag_utm$easting), max(tag_utm$northing), radius_size)
# hist(bird_visit$revisits, breaks = 20, main = "", xlab = paste("Revisits (radius = ", radius_size, ")", sep = ""))
# summary(bird_visit$revisits)
# bird_visit$revisitStats %>% arrange(desc(visitIdx))
# # The total residence time at each location, that is the sum of the individual 
# # visit durations
# bird_visit$residenceTime
# 
# bird_visit$revisits
# 
# boxplot(bird_visit$revisitStats$timeInside ~ as.numeric(format(bird_visit$revisitStats$entranceTime, "%H")),
#         xlab = "Entrance time", ylab = "Visit duration (h)")
# 
# par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
# hist(bird_visit$revisitStats$timeSinceLastVisit,
#      xlab = "Time since last visit (h)", main = "")
# plot(bird_visit$revisitStats$timeSinceLastVisit, bird_visit$revisitStats$timeInside,
#      xlab = "Time since last visit (h)", ylab = "Time inside (h)")
# lines(lowess(x = bird_visit$revisitStats$timeSinceLastVisit, y = bird_visit$revisitStats$timeInside, delta = 0.01 * diff(range(bird_visit$revisitStats$timeSinceLastVisit, na.rm = TRUE))), col = "red")

# high_visitation_points <- 
#   bird_visit$revisitStats %>% 
#   group_by(coordIdx, x, y) %>% 
#   summarise(n_visits = max(visitIdx)) %>% 
#   arrange(desc(n_visits)) %>% 
#   filter(n_visits > 1) %>% 
#   st_as_sf(x = .,                         
#            coords = c("x", "y"),
#            crs = "+proj=utm +zone=13") %>%
#   st_transform(., crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
#   sfc_as_cols(., names = c("lon", "lat")) %>% 
#   st_drop_geometry() %>% as.data.frame()

# xy <- 
#   SpatialPointsDataFrame(
#     matrix(c(high_visitation_points$lon,high_visitation_points$lat), ncol=2), 
#     data.frame(ID=high_visitation_points$coordIdx),
#     # matrix(c(x,y), ncol=2), data.frame(ID=seq(1:length(x))),
#     proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# use the distm function to generate a geodesic distance matrix in meters
mdist <- distm(xy)

# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(mdist), method = "complete")

# define the distance threshold, in this case 30 m
d=30

# define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
xy$clust <- cutree(hc, h=d)



# expand the extent of plotting frame
xy@bbox[] <- as.matrix(extend(extent(xy),0.001))

# get the centroid coords for each cluster
cent <- matrix(ncol=2, nrow=max(xy$clust))
for (i in 1:max(xy$clust))
  # gCentroid from the rgeos package
  cent[i,] <- gCentroid(subset(xy, clust == i))@coords

# compute circles around the centroid coords using a 40m radius
# from the dismo package
ci <- circles(cent, d=d, lonlat=T)