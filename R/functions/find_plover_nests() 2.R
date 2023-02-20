tagging_data = tag_breeding_data_ceuta$tagging
local_UTM_zone = 13
bird_ring = "CN0161"
sex = "F"
local_time_zone = "America/Mazatlan"
recurse_radius_size = 3
recurse_time_threshold = 3
recurse_time_threshold_units = "days"

find_plover_nests <- 
  function(tagging_data,
           local_UTM_zone,
           bird_ring,
           sex,
           local_time_zone,
           recurse_radius_size,
           recurse_time_threshold = 0,
           recurse_time_threshold_unit = c("hours", "secs", "mins", "days"),
           

#### Female CN0161 ----
# this female has a 9-hour sampling interval and shows a promising nesting 
# attempt in H that was undetected in the field (i.e., only "looks" like a 
# nesting attempt given the spatiotemporal pattern in the data)

bird_tagging_data_full <- 
  filter(tag_breeding_data_ceuta$tagging, ring == bird_ring)
bird_tagging_data_day <- 
  filter(tag_breeding_data_ceuta$tagging, ring == bird_ring & night_fix == 0)
bird_tagging_data_night <- 
  filter(tag_breeding_data_ceuta$tagging, ring == bird_ring & night_fix == 1)

bird_tagging_data <- 
  bird_tagging_data_day %>% 
  mutate(timestamp_local = ymd_hms(timestamp_local, tz = "America/Mazatlan"))

# first transform the tagging data into UTM so that the units are in meters
tag_utm <- 
  bird_tagging_data %>% 
  st_as_sf(x = .,                         
           coords = c("lon", "lat"),
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
  st_transform(., crs = paste("+proj=utm +zone=", local_UTM_zone, "\"", sep = "") %>% 
  sfc_as_cols(., names = c("easting", "northing")) %>% 
  st_drop_geometry()

# then transform the UTM data into a move object to be used by the recurse package
tag_utm_move <-
  df2move(df = arrange(as.data.frame(tag_utm), timestamp_local),
          track_id = "ring",
          proj = "+proj=utm +zone=13",
          x = "easting", y = "northing",
          time = "timestamp_local")

tag_latlon_move <-
  df2move(df = arrange(as.data.frame(bird_tagging_data), timestamp_local),
          track_id = "ring",
          proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
          x = "lon", y = "lat",
          time = "timestamp_local")

# inspect the data
plot(tag_utm$easting, tag_utm$northing, 
     col = viridis_pal()(nrow(tag_utm)), pch = 20, 
     xlab = "easting", ylab = "northing", asp = 1)

# also look at the data in regards to known nesting attempts
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0161", map_year = 2018)

# specify the radius size for the recursion (10m is adequate for the measurement 
# error of the GPS tags)
radius_size = 3

# calculate the recursions based on the radius size specified above
bird_visit = getRecursions(x = tag_utm_move, 
                           radius = radius_size, 
                           threshold = 3, timeunits = "days") 

# plot the data and the recursions (also show the size of the radius in 
# relation to the data and study site)
par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
plot(bird_visit, tag_utm_move, 
     legendPos = c(min(tag_utm$easting)+1250, max(tag_utm$northing)))
drawCircle(max(tag_utm$easting), max(tag_utm$northing), radius_size)
hist(bird_visit$revisits, breaks = 20, main = "", xlab = paste("Revisits (radius = ", radius_size, ")", sep = ""))
summary(bird_visit$revisits)
bird_visit$revisitStats %>% arrange(desc(visitIdx))
# The total residence time at each location, that is the sum of the individual 
# visit durations
bird_visit$residenceTime

bird_visit$revisits

boxplot(bird_visit$revisitStats$timeInside ~ as.numeric(format(bird_visit$revisitStats$entranceTime, "%H")),
        xlab = "Entrance time", ylab = "Visit duration (h)")

par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
hist(bird_visit$revisitStats$timeSinceLastVisit,
     xlab = "Time since last visit (h)", main = "")
plot(bird_visit$revisitStats$timeSinceLastVisit, bird_visit$revisitStats$timeInside,
     xlab = "Time since last visit (h)", ylab = "Time inside (h)")
lines(lowess(x = bird_visit$revisitStats$timeSinceLastVisit, y = bird_visit$revisitStats$timeInside, delta = 0.01 * diff(range(bird_visit$revisitStats$timeSinceLastVisit, na.rm = TRUE))), col = "red")

high_visitation_points <- 
  bird_visit$revisitStats %>% 
  group_by(coordIdx, x, y) %>% 
  summarise(n_visits = max(visitIdx)) %>% 
  arrange(desc(n_visits)) %>% 
  filter(n_visits > 1) %>% 
  st_as_sf(x = .,                         
           coords = c("x", "y"),
           crs = "+proj=utm +zone=13") %>%
  st_transform(., crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  sfc_as_cols(., names = c("lon", "lat")) %>% 
  st_drop_geometry() %>% as.data.frame()

xy <- 
  SpatialPointsDataFrame(
    matrix(c(high_visitation_points$lon,high_visitation_points$lat), ncol=2), 
    data.frame(ID=high_visitation_points$coordIdx),
    # matrix(c(x,y), ncol=2), data.frame(ID=seq(1:length(x))),
    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# use the distm function to generate a geodesic distance matrix in meters
mdist <- distm(xy)

# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(mdist), method = "complete")

# define the distance threshold, in this case 30 m
d=30

# define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
xy$clust <- cutree(hc, h=d)

library(dismo)
library(rgeos)

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