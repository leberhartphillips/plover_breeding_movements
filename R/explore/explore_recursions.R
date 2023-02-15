data(martin)
plot(martin$x, martin$y, col = viridis_pal()(nrow(martin)), pch = 20, 
     xlab = "x", ylab = "y", asp = 1)

plot(CN0161$tagging$lon, CN0161$tagging$lat, col = viridis_pal()(nrow(CN0161$tagging)), pch = 20, 
     xlab = "x", ylab = "y", asp = 1)

martinvisit = getRecursions(martin, 2) 
par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
plot(martinvisit, martin, legendPos = c(13, -10))
drawCircle(-15, -10, 2)

hist(martinvisit$revisits, breaks = 20, main = "", xlab = "Revisits (radius = 2)")
summary(martinvisit$revisits)

#### Female CN0161 ----
# this female has a 9-hour sampling interval and shows a promising nesting 
# attempt in H that was undetected in the field (i.e., only "looks" like a 
# nesting attempt given the spatiotemporal pattern in the data)

bird_ring = "CN0161"

bird_tagging_data = filter(tag_breeding_data_ceuta$tagging, ring == bird_ring)

# first transform the tagging data into UTM so that the units are in meters
tag_utm <- 
  bird_tagging_data %>% 
  st_as_sf(x = .,                         
           coords = c("lon", "lat"),
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
  st_transform(., crs = "+proj=utm +zone=13") %>% 
  sfc_as_cols(., names = c("easting", "northing")) %>% 
  st_drop_geometry() 

# then transform the UTM data into a move object to be used by the recurse package
tag_utm_move <-
  df2move(df = arrange(as.data.frame(tag_utm), timestamp_local),
          track_id = "ring",
          proj = "+proj=utm +zone=13",
          x = "easting", y = "northing",
          time = "timestamp_local")

# inspect the data
plot(tag_utm$easting, tag_utm$northing, 
     col = viridis_pal()(nrow(tag_utm)), pch = 20, 
     xlab = "easting", ylab = "northing", asp = 1)

# also look at the data in regards to known nesting attempts
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0930", map_year = 2022)

# specify the radius size for the recursion (10m is adequate for the measurement 
# error of the GPS tags)
radius_size = 10

# calculate the recursions based on the radius size specified above
bird_visit = getRecursions(x = tag_utm_move, radius = radius_size) 

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


#### Female CN0423 ----
# this female has a 12-hour sampling interval and shows a known nesting 
# attempt in D that was preceded by brood care and followed by desertion and 
# another nesting attempt

bird_ring = "CN0423"

bird_tagging_data = filter(tag_breeding_data_ceuta$tagging, ring == bird_ring)

# first transform the tagging data into UTM so that the units are in meters
tag_utm <- 
  bird_tagging_data %>% 
  st_as_sf(x = .,                         
           coords = c("lon", "lat"),
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
  st_transform(., crs = "+proj=utm +zone=13") %>% 
  sfc_as_cols(., names = c("easting", "northing")) %>% 
  st_drop_geometry() 

# then transform the UTM data into a move object to be used by the recurse package
tag_utm_move <-
  df2move(df = arrange(as.data.frame(tag_utm), timestamp_local),
          track_id = "ring",
          proj = "+proj=utm +zone=13",
          x = "easting", y = "northing",
          time = "timestamp_local")

# inspect the data
plot(tag_utm$easting, tag_utm$northing, 
     col = viridis_pal()(nrow(tag_utm)), pch = 20, 
     xlab = "easting", ylab = "northing", asp = 1)

# also look at the data in regards to known nesting attempts
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0930", map_year = 2022)

# specify the radius size for the recursion (10m is adequate for the measurement 
# error of the GPS tags)
radius_size = 10

# calculate the recursions based on the radius size specified above
bird_visit = getRecursions(x = tag_utm_move, radius = radius_size) 

# plot the data and the recursions (also show the size of the radius in 
# relation to the data and study site)
par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
plot(bird_visit, tag_utm_move, 
     legendPos = c(min(tag_utm$easting)+1250, max(tag_utm$northing)))
drawCircle(max(tag_utm$easting), max(tag_utm$northing), radius_size)
hist(bird_visit$revisits, breaks = 20, main = "", xlab = paste("Revisits (radius = ", radius_size, ")", sep = ""))
summary(bird_visit$revisits)
head(bird_visit$revisitStats)

#### Female CN0930 ----
# this female has a 12-hour sampling interval and shows a known nesting 
# attempt in D that was preceded by brood care and followed by desertion and 
# another nesting attempt
bird_ring = "CN0930"

bird_tagging_data = filter(tag_breeding_data_ceuta$tagging, ring == bird_ring)

# first transform the tagging data into UTM so that the units are in meters
tag_utm <- 
  bird_tagging_data %>% 
  st_as_sf(x = .,                         
           coords = c("lon", "lat"),
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
  st_transform(., crs = "+proj=utm +zone=13") %>% 
  sfc_as_cols(., names = c("easting", "northing")) %>% 
  st_drop_geometry() 

# then transform the UTM data into a move object to be used by the recurse package
tag_utm_move <-
  df2move(df = arrange(as.data.frame(tag_utm), timestamp_local),
          track_id = "ring",
          proj = "+proj=utm +zone=13",
          x = "easting", y = "northing",
          time = "timestamp_local")

# inspect the data
plot(tag_utm$easting, tag_utm$northing, 
     col = viridis_pal()(nrow(tag_utm)), pch = 20, 
     xlab = "easting", ylab = "northing", asp = 1)

# also look at the data in regards to known nesting attempts
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0930", map_year = 2022)

# specify the radius size for the recursion (10m is adequate for the measurement 
# error of the GPS tags)
radius_size = 10

# calculate the recursions based on the radius size specified above
bird_visit = getRecursions(x = tag_utm_move, radius = radius_size) 

# plot the data and the recursions (also show the size of the radius in 
# relation to the data and study site)
par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
plot(bird_visit, tag_utm_move, 
     legendPos = c(min(tag_utm$easting)+1250, max(tag_utm$northing)))
drawCircle(max(tag_utm$easting), max(tag_utm$northing), radius_size)
hist(bird_visit$revisits, breaks = 20, main = "", xlab = paste("Revisits (radius = ", radius_size, ")", sep = ""))
summary(bird_visit$revisits)
head(bird_visit$revisitStats)
