# prepare R environment
source("R/project/project_libraries.R")
source("R/project/project_plotting_misc.R")
source("R/project/project_load_movement_data.R")
source("R/project/project_load_breeding_data.R")
source("R/wrangle/wrangle_tag_nest_brood_resight_data.R")

# load functions
function.sources = list.files(path = "R/functions/", 
                              pattern = "*\\().R$", full.names = TRUE, 
                              ignore.case = TRUE)
sapply(function.sources, source, .GlobalEnv)

#### Individuals with at least two nesting attempts and first nest hatched ----
# CN0423 female (pair), three nests, first and second nests hatched (12-hour @ 1000/2200)
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0423", map_year = 2022)

F_CN0423_move_2wH <- 
  tag_data_move_wrangle(formatted_tag_data = 
                          lapply(tag_breeding_data_ceuta, 
                                 function(x) subset(x, ring == "CN0423"))$tagging, 
                        temporal_res = 1, 
                        temporal_unit = "hours", 
                        longitude_name = "lon", 
                        latitude_name = "lat", 
                        timestamp_name = "timestamp_local", 
                        ind_name = "ring",
                        projection = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

F_CN0423_move_2wH <- 
  F_CN0423_move_2wH$aligned_move_object %>% 
  as.data.frame() %>% 
  filter(timestamps > as.Date(paste(year(timestamps), "03-23", sep = "-")) &
           timestamps < as.Date(paste(year(timestamps), "08-01", sep = "-")))

# CA3340 male (pair), two nests, first nest hatched (20-min then 12-hour @ 1000/2200)
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CA3340", map_year = 2022)

M_CA3340_move_2wH <- 
  tag_data_move_wrangle(formatted_tag_data = 
                          lapply(tag_breeding_data_ceuta, 
                                 function(x) subset(x, ring == "CA3340"))$tagging, 
                        temporal_res = 1, 
                        temporal_unit = "hours", 
                        longitude_name = "lon", 
                        latitude_name = "lat", 
                        timestamp_name = "timestamp_local", 
                        ind_name = "ring",
                        projection = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# CN0161 female, two nests (2nd presumed), first nest hatched (9-hour)
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0161", map_year = 2018)

F_CN0161_move_2wH <- 
  tag_data_move_wrangle(formatted_tag_data = 
                          lapply(tag_breeding_data_ceuta, 
                                 function(x) subset(x, ring == "CN0161"))$tagging, 
                        temporal_res = 1, 
                        temporal_unit = "hours", 
                        longitude_name = "lon", 
                        latitude_name = "lat", 
                        timestamp_name = "timestamp_local", 
                        ind_name = "ring",
                        projection = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# CA3224 female, two nests, first nest hatched (24-hour @ 0600)
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CA3224", map_year = 2019)

F_CA3224_move_2wH <- 
  tag_data_move_wrangle(formatted_tag_data = 
                          lapply(tag_breeding_data_ceuta, 
                                 function(x) subset(x, ring == "CA3224"))$tagging, 
                        temporal_res = 1, 
                        temporal_unit = "hours", 
                        longitude_name = "lon", 
                        latitude_name = "lat", 
                        timestamp_name = "timestamp_local", 
                        ind_name = "ring",
                        projection = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# CN0930 female, two nests, first nest hatched (20-min then 12-hour @ 1000/2200)
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0930", map_year = 2022)

F_CN0930_move_2wH <- 
  tag_data_move_wrangle(formatted_tag_data = 
                          lapply(tag_breeding_data_ceuta, 
                                 function(x) subset(x, ring == "CN0930"))$tagging, 
                        temporal_res = 1, 
                        temporal_unit = "hours", 
                        longitude_name = "lon", 
                        latitude_name = "lat", 
                        timestamp_name = "timestamp_local", 
                        ind_name = "ring",
                        projection = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# CN0916 female, two nests, first nest hatched (20-min then 12-hour @ 1000/2200)
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0916", map_year = 2022)

F_CN0916_move_2wH <- 
  tag_data_move_wrangle(formatted_tag_data = 
                          lapply(tag_breeding_data_ceuta, 
                                 function(x) subset(x, ring == "CN0916"))$tagging, 
                        temporal_res = 1, 
                        temporal_unit = "hours", 
                        longitude_name = "lon", 
                        latitude_name = "lat", 
                        timestamp_name = "timestamp_local", 
                        ind_name = "ring",
                        projection = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# CN0609 female, two nests, first nest hatched (12-hour @ 1000/2200)
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0609", map_year = 2022)

F_CN0609_move_2wH <- 
  tag_data_move_wrangle(formatted_tag_data = 
                          lapply(tag_breeding_data_ceuta, 
                                 function(x) subset(x, ring == "CN0609"))$tagging, 
                        temporal_res = 1, 
                        temporal_unit = "hours", 
                        longitude_name = "lon", 
                        latitude_name = "lat", 
                        timestamp_name = "timestamp_local", 
                        ind_name = "ring",
                        projection = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# CN0318 female, two nests, first nest hatched (24-hour @ 0600)
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0318", map_year = 2019)

F_CN0318_move_2wH <- 
  tag_data_move_wrangle(formatted_tag_data = 
                          lapply(tag_breeding_data_ceuta, 
                                 function(x) subset(x, ring == "CN0318"))$tagging, 
                        temporal_res = 1, 
                        temporal_unit = "hours", 
                        longitude_name = "lon", 
                        latitude_name = "lat", 
                        timestamp_name = "timestamp_local", 
                        ind_name = "ring",
                        projection = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# CN0422 female, two nests, first nest hatched (24-hour @ 0600)
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0422", map_year = 2019)

F_CN0422_move_2wH <- 
  tag_data_move_wrangle(formatted_tag_data = 
                          lapply(tag_breeding_data_ceuta, 
                                 function(x) subset(x, ring == "CN0422"))$tagging, 
                        temporal_res = 1, 
                        temporal_unit = "hours", 
                        longitude_name = "lon", 
                        latitude_name = "lat", 
                        timestamp_name = "timestamp_local", 
                        ind_name = "ring",
                        projection = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# CN0918 male, two nests, first nest hatched (12-hour @ 1000/2200)
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0918", map_year = 2022)

M_CN0918_move_2wH <- 
  tag_data_move_wrangle(formatted_tag_data = 
                          lapply(tag_breeding_data_ceuta, 
                                 function(x) subset(x, ring == "CN0918"))$tagging, 
                        temporal_res = 1, 
                        temporal_unit = "hours", 
                        longitude_name = "lon", 
                        latitude_name = "lat", 
                        timestamp_name = "timestamp_local", 
                        ind_name = "ring",
                        projection = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# CN0138 male, two nests, first nest hatched with brood care (24-hour @ 2300)
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0138", map_year = 2019)

M_CN0138_move_2wH <- 
  tag_data_move_wrangle(formatted_tag_data = 
                          lapply(tag_breeding_data_ceuta, 
                                 function(x) subset(x, ring == "CN0138"))$tagging, 
                        temporal_res = 1, 
                        temporal_unit = "hours", 
                        longitude_name = "lon", 
                        latitude_name = "lat", 
                        timestamp_name = "timestamp_local", 
                        ind_name = "ring",
                        projection = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")



#### Individuals with at least two nesting attempts and first nest failed ----
# CN0312 female, two nests, first abandoned, second predated (24-hour @ 0600)
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0312", map_year = 2019)

F_CN0312_move_2wF <- 
  tag_data_move_wrangle(formatted_tag_data = 
                          lapply(tag_breeding_data_ceuta, 
                                 function(x) subset(x, ring == "CN0312"))$tagging, 
                        temporal_res = 1, 
                        temporal_unit = "hours", 
                        longitude_name = "lon", 
                        latitude_name = "lat", 
                        timestamp_name = "timestamp_local", 
                        ind_name = "ring",
                        projection = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# CN0130 female, two nests, first nest abandoned but started second immediately (hatched) (24-hour @ 0600)
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0130", map_year = 2019)

F_CN0130_move_2wF <- 
  tag_data_move_wrangle(formatted_tag_data = 
                          lapply(tag_breeding_data_ceuta, 
                                 function(x) subset(x, ring == "CN0130"))$tagging, 
                        temporal_res = 1, 
                        temporal_unit = "hours", 
                        longitude_name = "lon", 
                        latitude_name = "lat", 
                        timestamp_name = "timestamp_local", 
                        ind_name = "ring",
                        projection = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# CN0937 female, two nests, first nest predated (PinPoint 20-min then 12-hour @ 1000/2200)
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0937", map_year = 2022)

F_CN0937_move_2wF <- 
  tag_data_move_wrangle(formatted_tag_data = 
                          lapply(tag_breeding_data_ceuta, 
                                 function(x) subset(x, ring == "CN0937"))$tagging, 
                        temporal_res = 1, 
                        temporal_unit = "hours", 
                        longitude_name = "lon", 
                        latitude_name = "lat", 
                        timestamp_name = "timestamp_local", 
                        ind_name = "ring",
                        projection = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#### Individuals with one nesting attempt and hatched ----
# male, one nest hatched with brood care (96-hour @ 0000)
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CA3315", map_year = 2018)

M_CA3315_move_1wH <- 
  tag_data_move_wrangle(formatted_tag_data = 
                          lapply(tag_breeding_data_ceuta, 
                                 function(x) subset(x, ring == "CA3315"))$tagging, 
                        temporal_res = 1, 
                        temporal_unit = "hours", 
                        longitude_name = "lon", 
                        latitude_name = "lat", 
                        timestamp_name = "timestamp_local", 
                        ind_name = "ring",
                        projection = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# male, one nest, hatched with brood care (96-hour @ 0000)
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CA2100", map_year = 2018)

CA2100_move <- 
  tag_data_move_wrangle(formatted_tag_data = 
                          lapply(tag_breeding_data_ceuta, 
                                 function(x) subset(x, ring == "CA2100"))$tagging, 
                        temporal_res = 1, 
                        temporal_unit = "hours", 
                        longitude_name = "lon", 
                        latitude_name = "lat", 
                        timestamp_name = "timestamp_local", 
                        ind_name = "ring",
                        projection = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# male, one nest, only incubation (24-hour @ 2300)
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0311", map_year = 2019)

CN0311_move <- 
  tag_data_move_wrangle(formatted_tag_data = 
                          lapply(tag_breeding_data_ceuta, 
                                 function(x) subset(x, ring == "CN0311"))$tagging, 
                        temporal_res = 1, 
                        temporal_unit = "hours", 
                        longitude_name = "lon", 
                        latitude_name = "lat", 
                        timestamp_name = "timestamp_local", 
                        ind_name = "ring",
                        projection = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# male, one nest, only a few incubation fixes (96-hour @ 0000)
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0118", map_year = 2018)

CN0118_move <- 
  tag_data_move_wrangle(formatted_tag_data = 
                          lapply(tag_breeding_data_ceuta, 
                                 function(x) subset(x, ring == "CN0118"))$tagging, 
                        temporal_res = 1, 
                        temporal_unit = "hours", 
                        longitude_name = "lon", 
                        latitude_name = "lat", 
                        timestamp_name = "timestamp_local", 
                        ind_name = "ring",
                        projection = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#### summarise breeding events for all birds ----
nest_info <- 
  tag_breeding_data_ceuta$nests %>% 
  # filter(year(nest_initiation_date) == 2022 & cap_date > as.Date("2022-04-19")) %>% 
  dplyr::select(ring, family_ID, nest_initiation_date, 
                end_date, last_observation_alive, fate) %>% 
  distinct() %>% 
  mutate(year = year(nest_initiation_date))

brood_info <- 
  tag_breeding_data_ceuta$broods %>% 
  group_by(ring, family_ID) %>% 
  summarise(max_date = max(timestamp_brood),
            min_date = min(timestamp_brood)) %>% 
  group_by(family_ID) %>% 
  mutate(min_date = min_date[which.min(min_date)]) %>% 
  mutate(across(c("min_date", "max_date"), as.Date)) %>% 
  mutate(year = year(min_date))

#### make trajectories for each bird ----

# dftraj_snpl_AM_PM <- 
#   bind_rows(dftraj_snpl_AM, dftraj_snpl_PM) %>% 
#   filter(ring %in% c("CN0423",
#                      "CA3224",
#                      "CN0422",
#                      "CN0930",
#                      "CN0318",
#                      "CN0609",
#                      "CN0918",
#                      "CN0916",
#                      "CA3340",
#                      "CN0161",
#                      "CN0138"))

reduced_2wH <-
  tag_breeding_data_ceuta$tagging %>% 
  filter(ring %in% c("CN0423",
                     "CA3224",
                     "CN0422",
                     "CN0930",
                     "CN0318",
                     "CN0609",
                     "CN0918",
                     "CN0916",
                     "CA3340",
                     "CN0161",
                     "CN0138")) %>% 
  mutate(rounded_time_hms = hms::as_hms(round(ymd_hms(timestamp_local, tz = "America/Mazatlan"), "hours")),
         timestamp_local_ymd = as.Date(timestamp_local, tz = "America/Mazatlan")) %>% 
  mutate(remove = ifelse(ring == "CA3224" & year(timestamp_local_ymd) == 2022, 1, 0)) %>% 
  filter(remove == 0) %>% 
  dplyr::select(-remove) %>% 
  mutate(diff_time = hms::as_hms(ymd_hms(timestamp_local, tz = "America/Mazatlan")) - rounded_time_hms) %>% 
  filter(diff_time < 1000 & diff_time > -1000) %>% 
  mutate(keep = ifelse(ring %in% c("CN0423", "CA3340", "CA3224", 
                                   "CN0930", "CN0916", "CN0609", "CN0918") & 
                         as_hms(rounded_time_hms) %in% c(as_hms("22:00:00"), as_hms("10:00:00")), 1, 
                       ifelse(ring %in% c("CN0138", "CN0422", "CN0318") & 
                                as_hms(rounded_time_hms) %in% c(as_hms("23:00:00"), as_hms("06:00:00")), 1, 
                              ifelse(ring %in% c("CN0161") & 
                                       as_hms(rounded_time_hms) %in% c(as_hms("21:00:00"), as_hms("00:00:00"), as_hms("06:00:00"), as_hms("09:00:00")), 1, 0)))) %>% 
  filter(keep == 1) %>% 
  filter(timestamp_local_ymd > as.Date(paste(year(timestamp_local_ymd), "03-23", sep = "-")) & 
           timestamp_local_ymd < as.Date(paste(year(timestamp_local_ymd), "08-01", sep = "-"))) %>% 
  dplyr::select(ring, code, sex, species, population, lon, lat, 
                timestamp_local, timestamp_local_ymd, rounded_time_hms, night_fix) %>% 
  arrange(ring, timestamp_local)

# subset into morning and evening data
snpl_move_aligned_round_full <- 
  reduced_2wH

snpl_move_aligned_round_AM <- 
  reduced_2wH %>% 
  filter(night_fix == 0)

snpl_move_aligned_round_PM <- 
  reduced_2wH %>% 
  filter(night_fix == 1)

# specify coordinate columns
coordinates(snpl_move_aligned_round_full) <- c("lon","lat")
coordinates(snpl_move_aligned_round_AM) <- c("lon","lat")
coordinates(snpl_move_aligned_round_PM) <- c("lon","lat")

# define spatial projection as WGS84
proj4string(snpl_move_aligned_round_full) <- CRS("+init=epsg:4326")
proj4string(snpl_move_aligned_round_AM) <- CRS("+init=epsg:4326")
proj4string(snpl_move_aligned_round_PM) <- CRS("+init=epsg:4326")

# calculate trajectories for morning and evening
ltraj_snpl_full <- 
  as.ltraj(xy = sp::coordinates(snpl_move_aligned_round_full),
           date = ymd_hms(snpl_move_aligned_round_full$timestamp_local, tz = "America/Mazatlan"),
           id = snpl_move_aligned_round_full$ring, 
           proj4string = CRS("+init=epsg:4326"))

ltraj_snpl_AM <- 
  as.ltraj(xy = sp::coordinates(snpl_move_aligned_round_AM),
           date = ymd_hms(snpl_move_aligned_round_AM$timestamp_local, tz = "America/Mazatlan"),
           id = snpl_move_aligned_round_AM$ring, 
           proj4string = CRS("+init=epsg:4326"))

ltraj_snpl_PM <- 
  as.ltraj(xy = sp::coordinates(snpl_move_aligned_round_PM),
           date = ymd_hms(snpl_move_aligned_round_PM$timestamp_local, tz = "America/Mazatlan"),
           id = snpl_move_aligned_round_PM$ring, 
           proj4string = CRS("+init=epsg:4326"))

# bind breeding events to timestamps
dftraj_snpl_AM <-
  ld(ltraj_snpl_AM) %>% 
  dplyr::rename(distance = dist,
                dispersion = R2n,
                cardinal_angle = abs.angle,
                relative_angle = rel.angle,
                ring = id) %>% 
  mutate(distance_m = distance * 100000) %>% 
  mutate(cum_distance = cumsum(distance_m)) %>% 
  mutate(rounded_time_hms = hms::as_hms(date),
         rounded_time_ymd = as.Date(date, 
                                    tz = "America/Mazatlan"),
         dataset = "AM",
         year = year(rounded_time_ymd)) %>%
  left_join(., brood_info, by = c("ring", "year")) %>% 
  left_join(., nest_info, by = c("ring", "year")) %>% 
  mutate(nesting = ifelse(!is.na(end_date) & 
                            rounded_time_ymd >= nest_initiation_date & 
                            rounded_time_ymd <= end_date, 1, 
                          ifelse(is.na(end_date) & 
                                   rounded_time_ymd >= nest_initiation_date & 
                                   rounded_time_ymd <= last_observation_alive, 1, 0)),
         brooding = ifelse(rounded_time_ymd >= min_date & rounded_time_ymd <= max_date, 1, 0),
         between_nests = ifelse(!is.na(end_date) & family_ID.x == family_ID.y & rounded_time_ymd >= end_date, 1, 
                                ifelse(is.na(end_date) & family_ID.x == family_ID.y & rounded_time_ymd >= last_observation_alive, 1, 
                                       ifelse(family_ID.x != family_ID.y & rounded_time_ymd <= nest_initiation_date, 1, 0)))) %>% 
  group_by(ring, rounded_time_ymd) %>% 
  arrange(ring, rounded_time_ymd, desc(nesting), desc(brooding)) %>%
  slice(1) %>% 
  distinct() %>% 
  left_join(., tag_breeding_data_ceuta$tagging %>% dplyr::select(ring, sex) %>% distinct(), by = "ring") %>% 
  ungroup() %>% 
  mutate(status = ifelse(nesting == 0 & is.na(between_nests), "between_nests",#rounded_time_ymd < as.Date(paste(year, "03-23", sep = "-")) |
                         #rounded_time_ymd > as.Date(paste(year, "08-01", sep = "-")) | (nesting == 0 & is.na(between_nests)), "non-breeding",
                         ifelse(nesting == 1, "nesting", 
                                # ifelse(is.na(min_date), "unknown",
                                ifelse(between_nests == 1, "between_nests", "unknown")))) #%>% filter(is.na(status)) %>% 
  # dplyr::select(ring, rounded_time_ymd, year, sex,#family_ID.x, family_ID.y,
  #               min_date, max_date, nest_initiation_date,
  #               end_date, fate, nesting, brooding, status)
  
dftraj_snpl_PM <-
  ld(ltraj_snpl_PM) %>% 
  dplyr::rename(distance = dist,
                dispersion = R2n,
                cardinal_angle = abs.angle,
                relative_angle = rel.angle,
                ring = id) %>% 
  mutate(distance_m = distance * 100000) %>% 
  mutate(cum_distance = cumsum(distance_m)) %>% 
  mutate(rounded_time_hms = hms::as_hms(date),
         rounded_time_ymd = as.Date(date, 
                                    tz = "America/Mazatlan"),
         dataset = "PM",
         year = year(rounded_time_ymd)) %>%
  left_join(., brood_info, by = c("ring", "year")) %>% 
  left_join(., nest_info, by = c("ring", "year")) %>% 
  mutate(nesting = ifelse(!is.na(end_date) & 
                            rounded_time_ymd >= nest_initiation_date & 
                            rounded_time_ymd <= end_date, 1, 
                          ifelse(is.na(end_date) & 
                                   rounded_time_ymd >= nest_initiation_date & 
                                   rounded_time_ymd <= last_observation_alive, 1, 0)),
         brooding = ifelse(rounded_time_ymd >= min_date & rounded_time_ymd <= max_date, 1, 0),
         between_nests = ifelse(!is.na(end_date) & family_ID.x == family_ID.y & rounded_time_ymd >= end_date, 1, 
                                ifelse(is.na(end_date) & family_ID.x == family_ID.y & rounded_time_ymd >= last_observation_alive, 1, 
                                       ifelse(family_ID.x != family_ID.y & rounded_time_ymd <= nest_initiation_date, 1, 0)))) %>% 
  group_by(ring, rounded_time_ymd) %>% 
  arrange(ring, rounded_time_ymd, desc(nesting), desc(brooding)) %>%
  slice(1) %>% 
  distinct() %>% 
  left_join(., tag_breeding_data_ceuta$tagging %>% dplyr::select(ring, sex) %>% distinct(), by = "ring") %>% 
  ungroup() %>% 
  mutate(status = ifelse(nesting == 0 & is.na(between_nests), "between_nests",#rounded_time_ymd < as.Date(paste(year, "03-23", sep = "-")) |
                           #rounded_time_ymd > as.Date(paste(year, "08-01", sep = "-")) | (nesting == 0 & is.na(between_nests)), "non-breeding",
                         ifelse(nesting == 1, "nesting", 
                                # ifelse(is.na(min_date), "unknown",
                                ifelse(between_nests == 1, "between_nests", "unknown"))))

dftraj_snpl_full <-
  ld(ltraj_snpl_full) %>% 
  dplyr::rename(distance = dist,
                dispersion = R2n,
                cardinal_angle = abs.angle,
                relative_angle = rel.angle,
                ring = id) %>% 
  mutate(distance_m = distance * 100000) %>% 
  mutate(cum_distance = cumsum(distance_m)) %>% 
  mutate(rounded_time_hms = hms::as_hms(round(ymd_hms(date, tz = "America/Mazatlan"), "hours")),
         rounded_time_ymd = as.Date(date, 
                                    tz = "America/Mazatlan"),
         dataset = "full",
         year = year(rounded_time_ymd)) %>%
  left_join(., brood_info, by = c("ring", "year")) %>% 
  left_join(., nest_info, by = c("ring", "year")) %>% 
  mutate(nesting = ifelse(!is.na(end_date) & 
                            rounded_time_ymd >= nest_initiation_date & 
                            rounded_time_ymd <= end_date, 1, 
                          ifelse(is.na(end_date) & 
                                   rounded_time_ymd >= nest_initiation_date & 
                                   rounded_time_ymd <= last_observation_alive, 1, 0)),
         brooding = ifelse(rounded_time_ymd >= min_date & rounded_time_ymd <= max_date, 1, 0),
         between_nests = ifelse(!is.na(end_date) & family_ID.x == family_ID.y & rounded_time_ymd >= end_date, 1, 
                                ifelse(is.na(end_date) & family_ID.x == family_ID.y & rounded_time_ymd >= last_observation_alive, 1, 
                                       ifelse(family_ID.x != family_ID.y & rounded_time_ymd <= nest_initiation_date, 1, 0)))) %>% 
  group_by(ring, date) %>% 
  arrange(ring, date, desc(nesting), desc(between_nests)) %>% 
  left_join(., tag_breeding_data_ceuta$tagging %>% dplyr::select(ring, sex) %>% distinct(), by = "ring") %>% 
  slice(c(1)) %>% 
  ungroup() %>% 
  mutate(status = ifelse(nesting == 0 & is.na(between_nests), "between_nests",#rounded_time_ymd < as.Date(paste(year, "03-23", sep = "-")) |
                         #rounded_time_ymd > as.Date(paste(year, "08-01", sep = "-")) | (nesting == 0 & is.na(between_nests)), "non-breeding",
                         ifelse(nesting == 1, "nesting", 
                                # ifelse(is.na(min_date), "unknown",
                                ifelse(between_nests == 1, "between_nests", "unknown"))))

ggplot() +
  geom_line(data = dftraj_snpl_full %>% filter(ring == "CN0423"), 
            aes(y = distance_m, x = date, color = status, group = ring)) +
  facet_wrap(rounded_time_hms ~ .)

dftraj_snpl_full %>% 
  mutate(status = factor(status, 
                         levels = c("nesting", "between_nests", "unknown"))) %>% 
  ggplot() +
  geom_boxplot(aes(x = status, y = log(distance_m), fill = sex)) +
  facet_grid(dataset ~ .)

# bind together
dftraj_snpl_AM_PM <- 
  bind_rows(dftraj_snpl_AM, dftraj_snpl_PM) %>% 
  filter(ring %in% c("CN0423",
                     "CA3224",
                     "CN0422",
                     "CN0930",
                     "CN0318",
                     "CN0609",
                     "CN0918",
                     "CN0916",
                     "CA3340",
                     "CN0161",
                     "CN0138"))

dftraj_snpl_AM_PM %>% 
  mutate(status = factor(status, 
                         levels = c("nesting", "between_nests", 
                                    "non-breeding", "unknown"))) %>% 
  ggplot() +
  geom_boxplot(aes(x = status, y = distance_m, fill = sex)) +
  facet_grid(dataset ~ .)

sex_status_diel_step_length_mod <- 
  lmer(log(distance_m) ~ sex * status + dataset + (1|ring), 
       data = dftraj_snpl_AM_PM)

sex_status_step_length_mod <- 
  lmer(log(distance_m) ~ sex * status + (1|ring), 
       data = dftraj_snpl_full)

# run tidy bootstrap to obtain model diagnostics
tidy_sex_status_diel_step_length_mod <-
  tidy(sex_status_diel_step_length_mod, 
       conf.int = TRUE, 
       conf.method = "boot", 
       nsim = 1000)

tidy_sex_status_step_length_mod <-
  tidy(sex_status_step_length_mod, 
       conf.int = TRUE, 
       conf.method = "boot", 
       nsim = 1000)

tidy_sex_status_diel_step_length_mod

model_parameters(sex_status_step_length_mod)

ee <- Effect(c("sex","status"), sex_status_step_length_mod) 
# Age_Site_Phi_plot <- 
  ggplot(data = as.data.frame(ee),
         aes(x = status, y = exp(fit), color = sex)) +
  geom_errorbar(aes(ymin = exp(lower), ymax = exp(upper)), position = position_dodge(1), 
                size = 0.3, linetype = "solid", width = 0.1) +
  geom_point(size = 3, position = position_dodge(1)) +
  theme_bw() + 
  luke_theme +
  theme(legend.position = "top",
        axis.title.y = element_blank(),
        axis.text.y  = element_blank()) +
  ylab("diel step-length (log10, m) (± 95% CI)") +
  # scale_y_continuous(limits = c(0, 1)) +
  scale_color_brewer(palette = "Dark2") +
  annotate(geom = "text", y = 0.73, x = 0.75, angle = 0,
           label = "N = 12", 
           color = "grey30", size = 2.5, fontface = 'italic') +
  annotate(geom = "text", y = 0.25, x = 1.25, angle = 0,
           label = "N = 66", 
           color = "grey30", size = 2.5, fontface = 'italic')  +
  annotate(geom = "text", y = 0.8, x = 1.75, angle = 0,
           label = "N = 33", 
           color = "grey30", size = 2.5, fontface = 'italic') +
  annotate(geom = "text", y = 0.33, x = 2.25, angle = 0,
           label = "N = 93", 
           color = "grey30", size = 2.5, fontface = 'italic')

theme_set(theme_bw())
ggplot(as.data.frame(ee),
       aes(status,fit,colour=sex,fill=sex))+
  geom_line()+
  ## colour=NA suppresses edges of the ribbon
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+
  ## add rug plot based on original data
  geom_rug(data=ee$data,aes(y=NULL),sides="b")


predicted_values <- 
  modelr::data_grid(data = dftraj_snpl_full, sex, status, ring) %>% 
  modelr::add_predictions(sex_status_step_length_mod)

predicted_values %>% 
  ggplot(aes(x = interaction(sex, status), y = exp(pred), color = sex)) +
  geom_boxplot(aes(fill = sex), alpha = 0.5) +
  geom_line(aes(group = interaction(ring, sex))) #+
  # geom_boxplot(data = dftraj_snpl_AM_PM, aes(status, log(distance_m), color = sex)) +
  facet_grid(dataset ~ .)

#### summarise breeding events for pair ----
# extract data for each focal bird
snpl_pair <- 
  lapply(tag_breeding_data_ceuta, function(x) 
    subset(x, ring %in% c("CN0423", "CA3340")))

snpl_pair_nest_info <- 
  snpl_pair$nests %>% 
  filter(year(nest_initiation_date) == 2022 & cap_date > as.Date("2022-04-19")) %>% 
  dplyr::select(ring, family_ID, nest_initiation_date, end_date, fate) %>% 
  distinct()

snpl_pair_brood_info <- 
  snpl_pair$broods %>% 
  group_by(ring, family_ID) %>% 
  summarise(max_date = max(timestamp_brood),
            min_date = min(timestamp_brood)) %>% 
  ungroup() %>%  
  mutate(min_date = min_date[which.min(min_date)]) %>% 
  mutate(across(c("min_date", "max_date"), as.Date))



#### proximity between birds ----
round(median(CA3340_move$sampling_time_lags))
round(median(CN0423_move$sampling_time_lags))

snpl_pair$tagging %>% 
  group_by(ring, sex) %>% 
  summarise(min_date = min(timestamp_local),
            max_date = max(timestamp_local))

# subset tagging data to period with both birds tracked and calculate proximity
# distance between pair members
CN0423_move_aligned_round <- 
  CN0423_move$aligned_move_object %>% 
  as.data.frame() %>% 
  mutate(rounded_time = round(time, "hours")) %>% 
  filter(as_hms(rounded_time) %in% c(as_hms("22:00:00"), as_hms("10:00:00"))) %>% 
  rename(x_CN0423 = x,
         y_CN0423 = y)

subset(as.data.frame(CN0423_move$aligned_move_object), str_detect(rownames(as.data.frame(CN0423_move$aligned_move_object)), "27"))
subset(as.data.frame(CN0423_move$aligned_move_object), str_detect(rownames(as.data.frame(CN0423_move$aligned_move_object)), "28"))


CA3340_move_aligned_round <- 
  CA3340_move$aligned_move_object %>% 
  as.data.frame() %>% 
  mutate(rounded_time = round(time, "hours")) %>% 
  filter(as_hms(rounded_time) %in% c(as_hms("22:00:00"), as_hms("10:00:00"))) %>% 
  rename(x_CA3340 = x,
         y_CA3340 = y)

snpl_pair_poximity_dist <- 
  full_join(CN0423_move_aligned_round, CA3340_move_aligned_round, by = "rounded_time") %>%
  dplyr::select(rounded_time, x_CN0423, y_CN0423, x_CA3340, y_CA3340) %>% 
  drop_na() %>% 
  mutate(proximity = distGeo(p1 = matrix(c(x_CN0423, y_CN0423), ncol = 2),
                             p2 = matrix(c(x_CA3340, y_CA3340), ncol = 2))) %>% 
  mutate(rounded_time_hms = hms::as_hms(rounded_time),
         rounded_time_ymd = as.Date(rounded_time, 
                                    tz = "America/Mazatlan")) 

ggplot() +
  geom_line(data = snpl_pair_poximity_dist, aes(x = rounded_time, y = proximity, 
                                                group = rounded_time_hms,
                                                color = rounded_time_hms)) +
  # facet_grid(rounded_time_hms ~ .) +
  luke_theme +
  theme(legend.position = "right",
        axis.title.x = element_blank()) +#,
  # strip.background = element_blank(),
  # strip.text = element_text(size = 8, face = "italic")) +
  ylab("distance between individuals (m)") +
  xlab("season") +
  scale_x_datetime(expand = c(0, 0), 
                   breaks = date_breaks("1 week"), 
                   labels = date_format("%d-%B")) +
  labs(color = "time of day") 
#+
annotate("rect", 
         xmin = test_min, 
         xmax = test_max, 
         ymin = Inf, ymax = Inf,
         alpha = .1, fill = "blue")

test_min <- ymd_hms(paste(snpl_pair_nest_info$nest_initiation_date[1], "00:00:00", sep = " "), tz = "America/Mazatlan")
test_max <- ymd_hms(paste(snpl_pair_nest_info$end_date[1], "00:00:00", sep = " "), tz = "America/Mazatlan")

# geom_vline(xintercept = snpl_pair_nest_info$, linetype="dotted", 
#            color = "blue", size=1.5)

#### step-distances ----
# bind the two temporally aligned dataframes
snpl_move_aligned_round <- 
  CN0423_move_aligned_round %>% 
  rename(lon = x_CN0423,
         lat = y_CN0423) %>% 
  bind_rows(CA3340_move_aligned_round %>% 
              rename(lon = x_CA3340,
                     lat = y_CA3340)) %>% 
  rename(ring = trackId,
         timestamp_local = rounded_time) %>% 
  mutate(rounded_time_hms = hms::as_hms(timestamp_local),
         rounded_time_ymd = as.Date(timestamp_local, 
                                    tz = "America/Mazatlan"))

tag_breeding_data_ceuta

# subset into morning and evening data
snpl_move_aligned_round_full <- snpl_move_aligned_round
snpl_move_aligned_round_AM <- 
  snpl_move_aligned_round %>% 
  filter(rounded_time_hms == hms::as_hms("10:00:00"))

snpl_move_aligned_round_PM <- 
  snpl_move_aligned_round %>% 
  filter(rounded_time_hms == hms::as_hms("22:00:00"))

# specify coordinate columns
coordinates(snpl_move_aligned_round_full) <- c("lon","lat")
coordinates(snpl_move_aligned_round_AM) <- c("lon","lat")
coordinates(snpl_move_aligned_round_PM) <- c("lon","lat")

# define spatial projection as WGS84
proj4string(snpl_move_aligned_round_full) <- CRS("+init=epsg:4326")
proj4string(snpl_move_aligned_round_AM) <- CRS("+init=epsg:4326")
proj4string(snpl_move_aligned_round_PM) <- CRS("+init=epsg:4326")

# calculate trajectories for morning and evening
ltraj_snpl_pair_full <- 
  as.ltraj(xy = sp::coordinates(snpl_move_aligned_round_full),
           date = snpl_move_aligned_round_full$timestamp_local,
           id = snpl_move_aligned_round_full$ring, 
           proj4string = CRS("+init=epsg:4326"))

ltraj_snpl_pair_AM <- 
  as.ltraj(xy = sp::coordinates(snpl_move_aligned_round_AM),
           date = snpl_move_aligned_round_AM$timestamp_local,
           id = snpl_move_aligned_round_AM$ring, 
           proj4string = CRS("+init=epsg:4326"))

ltraj_snpl_pair_PM <- 
  as.ltraj(xy = sp::coordinates(snpl_move_aligned_round_PM),
           date = snpl_move_aligned_round_PM$timestamp_local,
           id = snpl_move_aligned_round_PM$ring, 
           proj4string = CRS("+init=epsg:4326"))

# convert to dataframes
dftraj_snpl_pair_full <-
  ld(ltraj_snpl_pair_full) %>%
  dplyr::rename(distance = dist,
                dispersion = R2n,
                cardinal_angle = abs.angle,
                relative_angle = rel.angle,
                ring = id) %>% 
  mutate(distance_m = distance * 100000) %>% 
  group_by(ring) %>% 
  mutate(cum_distance = cumsum(distance_m)) %>% 
  mutate(rounded_time_hms = hms::as_hms(date),
         rounded_time_ymd = as.Date(date, 
                                    tz = "America/Mazatlan"),
         dataset = "full",
         year = year(rounded_time_ymd)) %>%
  left_join(., brood_info, by = c("ring", "year")) %>% 
  left_join(., nest_info, by = c("ring", "year")) %>% 
  mutate(nesting = ifelse(rounded_time_ymd >= nest_initiation_date & rounded_time_ymd < end_date, 1, 0),
         # brooding = ifelse(rounded_time_ymd >= min_date & rounded_time_ymd <= max_date, 1, 0),
         between_nests = ifelse(family_ID.x == family_ID.y & rounded_time_ymd >= end_date, 1, 
                                ifelse(family_ID.x != family_ID.y & rounded_time_ymd <= nest_initiation_date, 1, 0))) %>% 
  # arrange(rounded_time_ymd)
  group_by(ring, rounded_time_ymd) %>%
  arrange(ring, rounded_time_ymd, desc(nesting), desc(between_nests)) %>%
  slice(c(1, 3)) %>%
  # dplyr::select(ring, rounded_time_ymd, family_ID.x, family_ID.y,
  #               min_date, max_date, nest_initiation_date,
  #               end_date, fate, nesting, #brooding, 
  #               between_nests) %>%
  distinct() %>% 
  left_join(., snpl_pair$tagging %>% dplyr::select(ring, sex) %>% distinct(), by = "ring") #%>% arrange(desc(between_nests))

dftraj_snpl_pair_AM <-
  ld(ltraj_snpl_pair_AM) %>% 
  dplyr::rename(distance = dist,
                dispersion = R2n,
                cardinal_angle = abs.angle,
                relative_angle = rel.angle,
                ring = id) %>% 
  mutate(distance_m = distance * 100000) %>% 
  group_by(ring) %>% 
  mutate(cum_distance = cumsum(distance_m)) %>% 
  mutate(rounded_time_hms = hms::as_hms(date),
         rounded_time_ymd = as.Date(date, 
                                    tz = "America/Mazatlan"),
         dataset = "AM",
         year = year(rounded_time_ymd)) %>%
  left_join(., brood_info, by = c("ring", "year")) %>% 
  left_join(., nest_info, by = c("ring", "year")) %>% 
  mutate(nesting = ifelse(rounded_time_ymd >= nest_initiation_date & rounded_time_ymd < end_date, 1, 0),
         # brooding = ifelse(rounded_time_ymd >= min_date & rounded_time_ymd <= max_date, 1, 0),
         between_nests = ifelse(family_ID.x == family_ID.y & rounded_time_ymd >= end_date, 1, 
                                ifelse(family_ID.x != family_ID.y & rounded_time_ymd <= nest_initiation_date, 1, 0))) %>% 
  # arrange(rounded_time_ymd)
  group_by(ring, rounded_time_ymd) %>%
  arrange(ring, rounded_time_ymd, desc(nesting), desc(between_nests)) %>%
  slice(1) %>%
  # dplyr::select(ring, rounded_time_ymd, family_ID.x, family_ID.y,
  #               min_date, max_date, nest_initiation_date,
  #               end_date, fate, nesting, #brooding, 
  #               between_nests) %>%
  distinct() %>% 
  left_join(., snpl_pair$tagging %>% dplyr::select(ring, sex) %>% distinct(), by = "ring") #%>% arrange(desc(between_nests))

dftraj_snpl_pair_PM <-
  ld(ltraj_snpl_pair_PM) %>% 
  dplyr::rename(distance = dist,
                dispersion = R2n,
                cardinal_angle = abs.angle,
                relative_angle = rel.angle,
                ring = id) %>% 
  mutate(distance_m = distance * 100000) %>% 
  group_by(ring) %>% 
  mutate(cum_distance = cumsum(distance_m)) %>% 
  mutate(rounded_time_hms = hms::as_hms(date),
         rounded_time_ymd = as.Date(date, 
                                    tz = "America/Mazatlan"),
         dataset = "PM",
         year = year(rounded_time_ymd)) %>%
  left_join(., brood_info, by = c("ring", "year")) %>% 
  left_join(., nest_info, by = c("ring", "year")) %>% 
  mutate(nesting = ifelse(rounded_time_ymd >= nest_initiation_date & rounded_time_ymd < end_date, 1, 0),
        # brooding = ifelse(rounded_time_ymd >= min_date & rounded_time_ymd <= max_date, 1, 0),
         between_nests = ifelse(family_ID.x == family_ID.y & rounded_time_ymd >= end_date, 1, 
                                ifelse(family_ID.x != family_ID.y & rounded_time_ymd <= nest_initiation_date, 1, 0))) %>% 
    # arrange(rounded_time_ymd)
  group_by(ring, rounded_time_ymd) %>%
  arrange(ring, rounded_time_ymd, desc(nesting), desc(between_nests)) %>%
  slice(1) %>%
  # dplyr::select(ring, rounded_time_ymd, family_ID.x, family_ID.y,
  #               min_date, max_date, nest_initiation_date,
  #               end_date, fate, nesting, #brooding, 
  #               between_nests) %>%
  distinct() %>% 
  left_join(., snpl_pair$tagging %>% dplyr::select(ring, sex) %>% distinct(), by = "ring") #%>% arrange(desc(between_nests))

# bind together
dftraj_snpl_pair <- 
  bind_rows(dftraj_snpl_pair_full, 
            dftraj_snpl_pair_AM, dftraj_snpl_pair_PM) %>% 
  ungroup() %>% 
  mutate(status = ifelse(rounded_time_ymd < as.Date(paste(year, "03-23", sep = "-")) |
                           rounded_time_ymd > as.Date(paste(year, "08-01", sep = "-")), "non-breeding",
                         ifelse(nesting == 1, "nesting", 
                                # ifelse(is.na(min_date), "unknown",
                                       ifelse(between_nests == 1, "between_nests", "unknown"))))

# inspect status-, sex-, and time-specific step-length histograms
dftraj_snpl_pair %>% 
  mutate(status = factor(status, 
                         levels = c("nesting", "between_nests", 
                                    "unknown", "non-breeding")),
         dataset = factor(dataset, c("AM", "PM", "full"))) %>% 
  ggplot() +
  geom_boxplot(aes(x = status, y = log(distance_m), fill = sex)) +
  facet_grid(dataset ~ .)

table(dftraj_snpl_pair$status, dftraj_snpl_pair$sex)

# inspect status-, sex-, and time-specific step-length histograms
dftraj_snpl_pair %>% 
  ggplot() +
  geom_histogram(aes(log(distance_m), fill = ring), binwidth = 1) +
  facet_grid(rounded_time_hms + ring ~ status) +
  scale_fill_manual(labels = c("male", "female"), values = c("blue", "red")) +
  geom_vline(xintercept = log(50), 
             color = "grey")

# plot spatiotemporal repeatability 
ring_sex_labels <- c(
  "CA3340" = "Male",
  "CN0423" = "Female")

timestamp_labels <- c(
  "10:00:00" = "Morning (1000)",
  "22:00:00" = "Evening (2200)")

ggplot() +
  # geom_point(data = dftraj_snpl_pair %>% filter(dataset == "full"),
  #            aes(x = rounded_time_ymd, y = distance_m, 
  #                group = ring, color = ring), alpha = 0.5) +
  # geom_line(data = dftraj_snpl_pair %>% filter(dataset == "full"),
  #           aes(x = rounded_time_ymd, y = distance_m, 
  #               group = ring, color = ring), alpha = 0.5) +
  geom_point(data = dftraj_snpl_pair %>% filter(dataset == "split"),
             aes(x = rounded_time_ymd, y = distance_m, 
                 group = ring, color = status)) +
  geom_line(data = dftraj_snpl_pair %>% filter(dataset == "split"),
            aes(x = rounded_time_ymd, y = distance_m, 
                group = ring, color = status)) +
  facet_grid(rounded_time_hms ~ ring, 
             # ring ~ ., 
             labeller = labeller(.rows = timestamp_labels)) +
  scale_color_manual(labels = c("brooding", "nesting", "between"), values = c("blue", "red", "green")) +
  scale_x_date(date_labels = "%B-%d", 
               expand = c(0.01, 0.01), 
               date_breaks = "1 weeks") +
  ylab("Displacement distance between consecutive \nsampling occasions (meters)") +
  # labs(color = "sex")
  theme_bw() +
  theme(
    # text = element_text(family = "Franklin Gothic Book"),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    panel.grid.major = element_line(colour = "grey70", size = 0.1),
    panel.grid.minor = element_line(colour = "grey70", size = 0.1),
    axis.ticks = element_line(size = 0.25, lineend = "round", colour = "grey60"),
    axis.ticks.length = unit(0.1, "cm"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.border = element_rect(linetype = "solid", colour = "grey")#,
    # plot.margin = unit(c(5, 1, 5, 1), "cm")
  ) #+
ggtitle(label = "Spatial displacement patterns \n of the 4-hour sampling schedule",
        subtitle = paste("Kentish Plover\nTagus Estuary, Portugal\n", 
                         tag_data_to_check_dist$sex, tag_data_to_check_dist$ring))