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

#### Aims of the recursion approach ----
# want to find nests and determine 

#### Female CN0161 ----
# this female has a 9-hour sampling interval and shows a promising nesting 
# attempt in H that was undetected in the field (i.e., only "looks" like a 
# nesting attempt given the spatiotemporal pattern in the data)

bird_ring = "CN0161"

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

ci1 <- circles(matrix(cent[1, 1:2], nrow = 1), d=d, lonlat=T)
ci2 <- circles(matrix(cent[2, 1:2], nrow = 1), d=d, lonlat=T)

# plot
plot(ci@polygons, axes=T)
plot(xy, col=rainbow(4)[factor(xy$clust)], add=T)

cent %>% 
  as.data.frame() %>% 
  rename(lon = V1,
         lat = V2) %>% 
  st_as_sf(x = .,                         
           coords = c("lon", "lat"),
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  mapview()

bird_visit_nest1 = 
  getRecursionsInPolygon(tag_latlon_move, 
                         polygon = polygons(ci1))

bird_visit_nest2 = 
  getRecursionsInPolygon(tag_latlon_move, 
                         polygon = polygons(ci2))

ymd_hms("2018-05-22 08:19:41", tz = "America/Mazatlan") - ymd_hms("2018-05-08 00:32:58", tz = "America/Mazatlan")
  

#### Female CN0423 ----
# this female has a 12-hour sampling interval and shows a known nesting 
# attempt in D that was preceded by brood care and followed by desertion and 
# another nesting attempt

bird_ring = "CN0423"

bird_tagging_data_full <- 
  filter(tag_breeding_data_ceuta$tagging, ring == bird_ring)
bird_tagging_data_day <- 
  filter(tag_breeding_data_ceuta$tagging, ring == bird_ring & night_fix == 0)
bird_tagging_data_night <- 
  filter(tag_breeding_data_ceuta$tagging, ring == bird_ring & night_fix == 1)

bird_tagging_data = 
  bird_tagging_data_day %>% 
  mutate(timestamp_local = ymd_hms(timestamp_local, tz = "America/Mazatlan"))

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

tag_latlon_move <-
  df2move(df = arrange(as.data.frame(bird_tagging_data), timestamp_local),
          track_id = "ring",
          proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
          x = "lon", y = "lat",
          time = "timestamp_local")

# view the move object to see clusters in the points that may correspond to 
# nesting attempts
mapview(tag_utm_move)

# inspect the data
plot(tag_utm$easting, tag_utm$northing, 
     col = viridis_pal()(nrow(tag_utm)), pch = 20, 
     xlab = "easting", ylab = "northing", asp = 1)

# also look at the data in regards to known nesting attempts
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0423", map_year = 2022)

# specify the radius size for the recursion (5m is adequate for the measurement 
# error of the GPS tags)
radius_size = 3

# calculate the recursions based on the radius size specified above
bird_visit = getRecursions(x = tag_utm_move, 
                           radius = radius_size, 
                           threshold = 3, timeunits = "days")

# plot the data and the recursions (also show the size of the radius in 
# relation to the data and study site)

 # ggplot() +
  
par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
plot(bird_visit, tag_utm_move, 
     col = brewer.pal(max(bird_visit$revisits), "Reds"),
     legendPos = c(min(tag_utm$easting)+1250, max(tag_utm$northing)))
drawCircle(max(tag_utm$easting), max(tag_utm$northing), radius_size)
hist(bird_visit$revisits, breaks = 20, main = "", 
     xlab = paste("Revisits (radius = ", radius_size, ")", sep = ""))

summary(bird_visit$revisits)
head(arrange(bird_visit$revisitStats, entranceTime))
head(arrange(as.data.frame(tag_utm_move), time))

nrow(bird_visit$revisitStats)
nrow(as.data.frame(tag_utm_move))

bird_visit$revisitStats %>% arrange(desc(visitIdx))

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

# define the distance threshold, in this case 20 m
d=25

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
ci1 <- circles(matrix(cent[1, 1:2], nrow = 1), d=d, lonlat=T)
ci2 <- circles(matrix(cent[2, 1:2], nrow = 1), d=d, lonlat=T)

mapview(polygons(ci1))

# plot
plot(ci@polygons, axes=T)
plot(xy, col=rainbow(4)[factor(xy$clust)], add=T)

cent %>% 
  as.data.frame() %>% 
  rename(lon = V1,
         lat = V2) %>% 
  st_as_sf(x = .,                         
           coords = c("lon", "lat"),
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  mapview()

bird_visit = 
  getRecursionsInPolygon(tag_latlon_move, 
                         polygon = polygons(ci1))

as.numeric(bird_visit$residenceTime)/24

# time that the female spent around the focal site
max(bird_visit$revisitStats$exitTime) - min(bird_visit$revisitStats$entranceTime)

getRecursionsInPolygon(martin, protectedArea)
poly = sp::SpatialPolygons( list(
  sp::Polygons( list(sp::Polygon(cbind(c(4,6,6,3,4),c(1,2,4,3,1)))), ID = 1 )
))

#%>% 
  # as.data.frame() %>% 
  # dplyr::select(lon, lat) %>% 
  # rename(long = lon)
  
as.data.frame(tag_utm_move) %>% 
  slice(high_visitation_points$coordIdx)

bird_visit$revisitStats %>% 
  filter(coordIdx %in% high_visitation_points$coordIdx)

df = read.table(header=T,text="long lat
                1 -74.20139 39.82806
                2 -74.20194 39.82806 
                3 -74.20167 39.82806 
                4 -74.20197 39.82824 
                5 -74.20150 39.82814 
                6 -74.26472 39.66639 
                7 -74.17389 39.87111 
                8 -74.07224 39.97353 
                9 -74.07978 39.94554")

stations <- data.table(id=as.integer(rownames(high_visitation_points)),high_visitation_points)
sep.ft <- 20
d <- function(x){                     # distance between station[i] and all subsequent stations
  r.ft <- 6378137             # radius of the earth, in feet
  if (x[1]==nrow(stations)) return()  # don't process last row
  ref <- stations[(x[1]+1):nrow(stations),]
  z <- distHaversine(ref[,2:3,with=F],x[2:3])
  z <- data.table(ST.1=x[1], ST.2=ref$id, dist=z, long.1=x[2], lat.1=x[3], long.2=ref$long, lat.2=ref$lat)
  return(z[z$dist>sep.ft,])
  # return(z[z$dist,])
}
coloc.2 = do.call(rbind,apply(stations,1,d))
coloc.2

# points of nest 1
high_visitation_points %>% slice(as.data.frame(coloc.2) %>% 
                                   pull(ST.1) %>% unique())


# points of nest 2
high_visitation_points %>% slice(as.data.frame(coloc.2) %>% 
                                   pull(ST.2) %>% unique())



coloc.2 = do.call(rbind, apply(high_visitation_points, 1, d))

as.data.frame(coloc.2) %>% filter(!is.na(dist))

max(bird_visit$revisitStats$visitIdx)

plot(high_visitation_points$lat, high_visitation_points$lon)

boxplot((bird_visit$revisitStats$timeInside)/24 ~ format(bird_visit$revisitStats$entranceTime, "%m-%d"),
        xlab = "Entrance time", ylab = "Visit duration (days)")

locations = data.frame(x = c(0, 10, 20), y = c(0, 10, 10))
locvisit = getRecursionsAtLocations(wren, locations, 2) 

bird_visit$revisitStats %>% 
  mutate(timeInside_days = timeInside/24,
         entranceTime_date = as.Date(entranceTime, tz = "America/Mazatlan"),
         exitTime_date = as.Date(exitTime, tz = "America/Mazatlan")) %>% 
  filter(timeInside_days > 1) %>% 
  arrange(desc(timeInside)) %>% 
  group_by(entranceTime_date, exitTime_date) %>% 
  summarise(total_timeInside_days = sum(timeInside_days),
            n_visits = n()) %>% 
  arrange(desc(n_visits)) %>% 
  mutate(time_period_days = exitTime_date - entranceTime_date)

#### Female CN0937 ----
bird_ring = "CN0937"

bird_tagging_data_full <- 
  filter(tag_breeding_data_ceuta$tagging, ring == bird_ring)
bird_tagging_data_day <- 
  filter(tag_breeding_data_ceuta$tagging, ring == bird_ring & night_fix == 0)
bird_tagging_data_night <- 
  filter(tag_breeding_data_ceuta$tagging, ring == bird_ring & night_fix == 1)

bird_tagging_data = bird_tagging_data_day

# first transform the tagging data into UTM so that the units are in meters
tag_utm <- 
  bird_tagging_data %>% 
  st_as_sf(x = .,                         
           coords = c("lon", "lat"),
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
  st_transform(., crs = "+proj=utm +zone=13") %>% 
  sfc_as_cols(., names = c("easting", "northing")) %>% 
  st_drop_geometry() %>% 
  mutate(timestamp_local = ymd_hms(timestamp_local, tz = "America/Mazatlan"))

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
                             bird_ring = "CN0937", map_year = 2022)

# specify the radius size for the recursion (5m is adequate for the measurement 
# error of the GPS tags)
radius_size = 3

# calculate the recursions based on the radius size specified above
bird_visit = getRecursions(x = tag_utm_move, 
                           radius = radius_size, 
                           threshold = 3, timeunits = "days")

# plot the data and the recursions (also show the size of the radius in 
# relation to the data and study site)

# ggplot() +

par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
plot(bird_visit, tag_utm_move, 
     col = brewer.pal(max(bird_visit$revisits), "Reds"),
     legendPos = c(min(tag_utm$easting)+1250, max(tag_utm$northing)))
drawCircle(max(tag_utm$easting), max(tag_utm$northing), radius_size)
hist(bird_visit$revisits, breaks = 20, main = "", 
     xlab = paste("Revisits (radius = ", radius_size, ")", sep = ""))

summary(bird_visit$revisits)
head(arrange(bird_visit$revisitStats, entranceTime))
head(arrange(as.data.frame(tag_utm_move), time))

nrow(bird_visit$revisitStats)
nrow(as.data.frame(tag_utm_move))

bird_visit$revisitStats %>% arrange(desc(visitIdx))

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

# plot
plot(ci@polygons, axes=T)
plot(xy, col=rainbow(4)[factor(xy$clust)], add=T)

cent %>% 
  as.data.frame() %>% 
  rename(lon = V1,
         lat = V2) %>% 
  st_as_sf(x = .,                         
           coords = c("lon", "lat"),
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  mapview()

#### Female CN0930 ----
bird_ring = "CN0930"

bird_tagging_data_full <- 
  filter(tag_breeding_data_ceuta$tagging, ring == bird_ring)
bird_tagging_data_day <- 
  filter(tag_breeding_data_ceuta$tagging, ring == bird_ring & night_fix == 0)
bird_tagging_data_night <- 
  filter(tag_breeding_data_ceuta$tagging, ring == bird_ring & night_fix == 1)

bird_tagging_data = bird_tagging_data_day

# first transform the tagging data into UTM so that the units are in meters
tag_utm <- 
  bird_tagging_data %>% 
  st_as_sf(x = .,                         
           coords = c("lon", "lat"),
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
  st_transform(., crs = "+proj=utm +zone=13") %>% 
  sfc_as_cols(., names = c("easting", "northing")) %>% 
  st_drop_geometry() %>% 
  mutate(timestamp_local = ymd_hms(timestamp_local, tz = "America/Mazatlan"))

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

# specify the radius size for the recursion (5m is adequate for the measurement 
# error of the GPS tags)
radius_size = 10

# calculate the recursions based on the radius size specified above
bird_visit = getRecursions(x = tag_utm_move, 
                           radius = radius_size, 
                           threshold = 3, timeunits = "days")

# plot the data and the recursions (also show the size of the radius in 
# relation to the data and study site)

# ggplot() +

par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
plot(bird_visit, tag_utm_move, 
     col = brewer.pal(max(bird_visit$revisits), "Reds"),
     legendPos = c(min(tag_utm$easting)+1250, max(tag_utm$northing)))
drawCircle(max(tag_utm$easting), max(tag_utm$northing), radius_size)
hist(bird_visit$revisits, breaks = 20, main = "", 
     xlab = paste("Revisits (radius = ", radius_size, ")", sep = ""))

summary(bird_visit$revisits)
head(arrange(bird_visit$revisitStats, entranceTime))
head(arrange(as.data.frame(tag_utm_move), time))

nrow(bird_visit$revisitStats)
nrow(as.data.frame(tag_utm_move))

bird_visit$revisitStats %>% arrange(desc(visitIdx))

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

# plot
plot(ci@polygons, axes=T)
plot(xy, col=rainbow(4)[factor(xy$clust)], add=T)

cent %>% 
  as.data.frame() %>% 
  rename(lon = V1,
         lat = V2) %>% 
  st_as_sf(x = .,                         
           coords = c("lon", "lat"),
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  mapview()