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

# nestR data formatting
nestR_data <- 
  tag_breeding_data_ceuta$tagging %>% 
  mutate(burst = paste(ring, year(timestamp_local), sep = "-")) %>% 
  dplyr::select(burst, timestamp_local, lon, lat, night_fix) %>% 
  rename(date = timestamp_local,
         long = lon,
         lat = lat)

tag_and_breeding_data$nests %>% 
  mutate(month_day = as.Date(format(nest_initiation_date, "%m-%d"), format = "%m-%d")) %>% 
  summarise(min(month_day, na.rm = TRUE),
            max(month_day, na.rm = TRUE))

sea_start = "03-23"
sea_end = "08-01"

#### Females ----
#### **CN0161 from 2018 (PinPoint 9-hour schedule) ----
# perfectly locates the presumed second nesting attempt
bird_ring = "CN0161"
map_year = 2018

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

nestR_out <- 
  find_nests(nestR_data %>% 
               arrange(date) %>% 
               mutate(diff_10 = round(ymd_hms(date, tz = "America/Mazatlan"), "hours") - 
                        hms(seconds = 00, minutes = 00, hours = 10),
                      diff_22 = round(ymd_hms(date, tz = "America/Mazatlan"), "hours") - 
                        hms(seconds = 00, minutes = 00, hours = 22))
               filter(str_detect(burst, bird_ring) & 
                        night_fix == 0 & hour(round(date, hours) %in% c(10, 22)), 
             buffer = 20,
             sea_start = sea_start, 
             sea_end = sea_end, 
             min_pts = 3,
             nest_cycle = 28, 
             min_d_fix = 1,
             min_consec = 1,
             min_top_att = 1,
             min_days_att = 1, discard_overlapping = TRUE)

nest_mapper(nestR_out, tag_and_breeding_data, bird_ring = bird_ring, map_year = map_year)

known_nests <-
  tag_and_breeding_data$nests %>%
  dplyr::filter(ring == bird_ring &
                  year(nest_initiation_date) == map_year) %>%
  mutate(burst = paste(ring, year(nest_initiation_date), sep = "-")) %>%
  dplyr::select(burst, lon, lat) %>%
  rename(long = lon,
         lat = lat) %>%
  as.data.frame()

# distance between nests
distGeo(nestR_out$nests %>% 
          arrange(attempt_start) %>% 
          dplyr::select(long, lat), 
        known_nests %>% dplyr::select(long, lat))

# time difference between nesting attempts (based on GPS data)
(nestR_out$nests %>% 
    arrange(attempt_start) %>% 
    slice(2) %>% 
    pull(attempt_start)) - 
  (nestR_out$nests %>% 
     arrange(attempt_start) %>% 
     slice(1) %>% 
     pull(attempt_end))

# time difference between nesting attempts (based on field data)
(tag_and_breeding_data$nests %>% 
    dplyr::filter(ring == bird_ring & 
                    year(nest_initiation_date) == map_year) %>% 
    arrange(nest_initiation_date) %>% 
    slice(2) %>% 
    pull(end_date)) - 
(tag_and_breeding_data$nests %>% 
   dplyr::filter(ring == bird_ring & 
                   year(nest_initiation_date) == map_year) %>% 
   arrange(nest_initiation_date) %>% 
   slice(1) %>% 
   pull(nest_initiation_date))

# nest fates
(tag_and_breeding_data$nests %>% 
    dplyr::filter(ring == bird_ring & 
                    year(nest_initiation_date) == map_year) %>% 
    arrange(nest_initiation_date) %>% 
    slice(1) %>% 
    pull(fate))

(tag_and_breeding_data$nests %>% 
    dplyr::filter(ring == bird_ring & 
                    year(nest_initiation_date) == map_year) %>% 
    arrange(nest_initiation_date) %>% 
    slice(2) %>% 
    pull(fate))

#### **CN0423 from 2022 (PinPoint 12-hour @ 1000/2200) ----
# perfectly locates both nesting attempts initiated post deployment
bird_ring = "CN0423"
map_year = 2022

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

nestR_out <- 
  find_nests(nestR_data %>% 
               filter(str_detect(burst, bird_ring) & 
                        night_fix == 0), 
             buffer = 20,
             sea_start = sea_start, 
             sea_end = sea_end, 
             min_pts = 3,
             nest_cycle = 28, 
             min_d_fix = 1,
             min_consec = 1,
             min_top_att = 1,
             min_days_att = 1, discard_overlapping = TRUE)

nest_mapper(nestR_out, tag_and_breeding_data, bird_ring = bird_ring, map_year = map_year)

known_nests <-
  tag_and_breeding_data$nests %>%
  dplyr::filter(ring == bird_ring &
                  year(nest_initiation_date) == map_year) %>%
  mutate(burst = paste(ring, year(nest_initiation_date), sep = "-")) %>%
  arrange(nest_initiation_date) %>% 
  dplyr::select(burst, lon, lat) %>%
  rename(long = lon,
         lat = lat) %>%
  as.data.frame()

# distance between nests
distGeo(nestR_out$nests %>% 
          arrange(attempt_start) %>% 
          dplyr::select(long, lat), 
        known_nests %>% slice(2:3) %>% dplyr::select(long, lat))

# time difference between nesting attempts (based on GPS data)
(nestR_out$nests %>% 
    arrange(attempt_start) %>% 
    slice(2) %>% 
    pull(attempt_start)) - 
  (nestR_out$nests %>% 
     arrange(attempt_start) %>% 
     slice(1) %>% 
     pull(attempt_end))

# time difference between nesting attempts (based on field data)
(tag_and_breeding_data$nests %>% 
    dplyr::filter(ring == bird_ring & 
                    year(nest_initiation_date) == map_year) %>% 
    arrange(nest_initiation_date) %>% 
    slice(3) %>% 
    pull(nest_initiation_date)) - 
  (tag_and_breeding_data$nests %>% 
     dplyr::filter(ring == bird_ring & 
                     year(nest_initiation_date) == map_year) %>% 
     arrange(nest_initiation_date) %>% 
     slice(2) %>% 
     pull(end_date))

# nest fates
(tag_and_breeding_data$nests %>% 
    dplyr::filter(ring == bird_ring & 
                    year(nest_initiation_date) == map_year) %>% 
    arrange(nest_initiation_date) %>% 
    slice(2) %>% 
    pull(fate))

(tag_and_breeding_data$nests %>% 
    dplyr::filter(ring == bird_ring & 
                    year(nest_initiation_date) == map_year) %>% 
    arrange(nest_initiation_date) %>% 
    slice(3) %>% 
    pull(fate))

#### *CN0937 from 2022 (PinPoint 20-min then 12-hour @ 1000/2200) ----
# locates second nesting attempt after first was predated 
# first nest had only 20-min sampling
bird_ring = "CN0937"
map_year = 2022

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

nestR_out <- 
  find_nests(nestR_data %>% 
             arrange(date) %>% 
             filter(str_detect(burst, bird_ring) & 
                      night_fix == 0) %>% 
             mutate(diff_10 = as_hms(round(ymd_hms(date), "mins") - 
                      hms(seconds = 00, minutes = 00, hours = 10)),
                    diff_22 = as_hms(round(ymd_hms(date), "mins") - 
                      hms(seconds = 00, minutes = 00, hours = 22))) %>%
             filter(diff_10 == as_hms("00:00:00") | diff_22 == as_hms("00:00:00")),
                    buffer = 20,
                    sea_start = sea_start, 
                    sea_end = sea_end, 
                    min_pts = 3,
                    nest_cycle = 28, 
                    min_d_fix = 1,
                    min_consec = 1,
                    min_top_att = 1,
                    min_days_att = 1, discard_overlapping = TRUE)

nest_mapper(nestR_out, time_zone_local = "America/Mazatlan", tag_and_breeding_data, 
            bird_ring = bird_ring, map_year = map_year)

known_nests <-
  tag_and_breeding_data$nests %>%
  dplyr::filter(ring == bird_ring &
                  year(nest_initiation_date) == map_year) %>%
  mutate(burst = paste(ring, year(nest_initiation_date), sep = "-")) %>%
  dplyr::select(burst, lon, lat) %>%
  rename(long = lon,
         lat = lat) %>%
  as.data.frame() %>% 
  distinct()

# distance between nests
distGeo(nestR_out$nests %>% 
          arrange(attempt_start) %>% 
          dplyr::select(long, lat), 
        known_nests %>% dplyr::select(long, lat))

# time difference between nesting attempts (based on GPS data)
(nestR_out$nests %>% 
    arrange(attempt_start) %>% 
    slice(2) %>% 
    pull(attempt_start)) - 
  (nestR_out$nests %>% 
     arrange(attempt_start) %>% 
     slice(1) %>% 
     pull(attempt_end))

# time difference between nesting attempts (based on field data)
(tag_and_breeding_data$nests %>% 
    dplyr::filter(ring == bird_ring & 
                    year(nest_initiation_date) == map_year) %>% 
    arrange(nest_initiation_date) %>% 
    slice(2) %>% 
    pull(end_date)) - 
  (tag_and_breeding_data$nests %>% 
     dplyr::filter(ring == bird_ring & 
                     year(nest_initiation_date) == map_year) %>% 
     arrange(nest_initiation_date) %>% 
     slice(1) %>% 
     pull(nest_initiation_date))

# nest fates
(tag_and_breeding_data$nests %>% 
    dplyr::filter(ring == bird_ring & 
                    year(nest_initiation_date) == map_year) %>% 
    arrange(nest_initiation_date) %>% 
    slice(1) %>% 
    pull(fate))

(tag_and_breeding_data$nests %>% 
    dplyr::filter(ring == bird_ring & 
                    year(nest_initiation_date) == map_year) %>% 
    arrange(nest_initiation_date) %>% 
    slice(2) %>% 
    pull(fate))

#### CN0916 from 2022 (PinPoint 20-min then 12-hour @ 1000/2200) ----
# issues with temporal resolution (incorrectly found nests in cluster of 20-min fixes)
# nestR found second nest but location is a bit off
bird_ring = "CN0916"
map_year = 2022

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

nestR_out <- 
  find_nests(nestR_data %>% 
               arrange(date) %>% 
               filter(str_detect(burst, bird_ring) & 
                        night_fix == 0) %>% 
               mutate(diff_10 = as_hms(round(ymd_hms(date), "mins") - 
                                         hms(seconds = 00, minutes = 00, hours = 10)),
                      diff_22 = as_hms(round(ymd_hms(date), "mins") - 
                                         hms(seconds = 00, minutes = 00, hours = 22))) %>%
               filter(diff_10 == as_hms("00:00:00") | diff_22 == as_hms("00:00:00")),
             buffer = 30,
             sea_start = sea_start, 
             sea_end = sea_end, 
             min_pts = 3,
             nest_cycle = 28, 
             min_d_fix = 1,
             min_consec = 1,
             min_top_att = 1,
             min_days_att = 1, discard_overlapping = TRUE)

nest_mapper(nestR_out, time_zone_local = "America/Mazatlan", tag_and_breeding_data, 
            bird_ring = bird_ring, map_year = map_year)

known_nests <-
  tag_and_breeding_data$nests %>%
  dplyr::filter(ring == bird_ring &
                  year(nest_initiation_date) == map_year &
                  min(nestR_out$visits$date) < end_date) %>%
  mutate(burst = paste(ring, year(nest_initiation_date), sep = "-")) %>%
  arrange(nest_initiation_date) %>% 
  dplyr::select(burst, lon, lat) %>%
  rename(long = lon,
         lat = lat) %>%
  as.data.frame() %>% 
  distinct()

# distance between nests
distGeo(nestR_out$nests %>% 
          arrange(attempt_start) %>% 
          dplyr::select(long, lat), 
        known_nests %>% dplyr::select(long, lat))

# time difference between nesting attempts (based on GPS data)
(nestR_out$nests %>% 
    arrange(attempt_start) %>% 
    slice(2) %>% 
    pull(attempt_start)) - 
  (nestR_out$nests %>% 
     arrange(attempt_start) %>% 
     slice(1) %>% 
     pull(attempt_end))

# time difference between nesting attempts (based on field data)
(tag_and_breeding_data$nests %>% 
    dplyr::filter(ring == bird_ring & 
                    year(nest_initiation_date) == map_year) %>% 
    arrange(nest_initiation_date) %>% 
    slice(1) %>% 
    pull(end_date)) - 
  (tag_and_breeding_data$nests %>% 
     dplyr::filter(ring == bird_ring & 
                     year(nest_initiation_date) == map_year) %>% 
     arrange(nest_initiation_date) %>% 
     slice(2) %>% 
     pull(nest_initiation_date))

# nest fates
(tag_and_breeding_data$nests %>% 
    dplyr::filter(ring == bird_ring & 
                    year(nest_initiation_date) == map_year) %>% 
    arrange(nest_initiation_date) %>% 
    slice(1) %>% 
    pull(fate))

(tag_and_breeding_data$nests %>% 
    dplyr::filter(ring == bird_ring & 
                    year(nest_initiation_date) == map_year) %>% 
    arrange(nest_initiation_date) %>% 
    slice(2) %>% 
    pull(fate))

#### **CN0609 from 2022 (PinPoint 12-hour @ 1000/2200) ----
# finds second nest but cluster is slightly "off", could be issues with visit times
# nestR finds second nest
bird_ring = "CN0609"
map_year = 2022

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

nestR_out <- 
  find_nests(nestR_data %>% 
               arrange(date) %>% 
               filter(str_detect(burst, bird_ring) & 
                        night_fix == 0) %>% 
               mutate(diff_10 = as_hms(round(ymd_hms(date), "mins") - 
                                         hms(seconds = 00, minutes = 00, hours = 10)),
                      diff_22 = as_hms(round(ymd_hms(date), "mins") - 
                                         hms(seconds = 00, minutes = 00, hours = 22))) %>%
               filter(diff_10 == as_hms("00:00:00") | diff_22 == as_hms("00:00:00")),
             buffer = 20,
             sea_start = sea_start, 
             sea_end = sea_end, 
             min_pts = 3,
             nest_cycle = 28, 
             min_d_fix = 1,
             min_consec = 1,
             min_top_att = 1,
             min_days_att = 1, discard_overlapping = TRUE)

nest_mapper(nestR_out, time_zone_local = "America/Mazatlan", tag_and_breeding_data, 
            bird_ring = bird_ring, map_year = map_year)

known_nests <-
  tag_and_breeding_data$nests %>%
  dplyr::filter(ring == bird_ring &
                  year(nest_initiation_date) == map_year &
                  min(nestR_out$visits$date) < end_date) %>%
  mutate(burst = paste(ring, year(nest_initiation_date), sep = "-")) %>%
  arrange(nest_initiation_date) %>% 
  dplyr::select(burst, lon, lat) %>%
  rename(long = lon,
         lat = lat) %>%
  as.data.frame() %>% 
  distinct()

# distance between nests
distGeo(nestR_out$nests %>% 
          arrange(attempt_start) %>% 
          dplyr::select(long, lat), 
        known_nests %>% dplyr::select(long, lat))

# time difference between nesting attempts (based on GPS data)
(nestR_out$nests %>% 
    arrange(attempt_start) %>% 
    slice(2) %>% 
    pull(attempt_start)) - 
  (nestR_out$nests %>% 
     arrange(attempt_start) %>% 
     slice(1) %>% 
     pull(attempt_end))

# time difference between nesting attempts (based on field data)
(tag_and_breeding_data$nests %>% 
    dplyr::filter(ring == bird_ring & 
                    year(nest_initiation_date) == map_year) %>% 
    arrange(nest_initiation_date) %>% 
    slice(1) %>% 
    pull(end_date)) - 
  (tag_and_breeding_data$nests %>% 
     dplyr::filter(ring == bird_ring & 
                     year(nest_initiation_date) == map_year) %>% 
     arrange(nest_initiation_date) %>% 
     slice(2) %>% 
     pull(nest_initiation_date))

# nest fates
(tag_and_breeding_data$nests %>% 
    dplyr::filter(ring == bird_ring & 
                    year(nest_initiation_date) == map_year) %>% 
    arrange(nest_initiation_date) %>% 
    slice(1) %>% 
    pull(fate))

(tag_and_breeding_data$nests %>% 
    dplyr::filter(ring == bird_ring & 
                    year(nest_initiation_date) == map_year) %>% 
    arrange(nest_initiation_date) %>% 
    slice(2) %>% 
    pull(fate))

#### *CM1858 from 2022 (PinPoint 20-min) ----
# locates single known nests nicely
bird_ring = "CM1858"
map_year = 2022

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

nestR_out <- 
  find_nests(nestR_data %>% 
               arrange(date) %>% 
               filter(str_detect(burst, bird_ring) & 
                        night_fix == 0),
               # mutate(diff_10 = as_hms(round(ymd_hms(date), "mins") - 
               #                           hms(seconds = 00, minutes = 00, hours = 10)),
               #        diff_22 = as_hms(round(ymd_hms(date), "mins") - 
               #                           hms(seconds = 00, minutes = 00, hours = 22))) %>%
               # filter(diff_10 == as_hms("00:00:00") | diff_22 == as_hms("00:00:00")),
             buffer = 20,
             sea_start = sea_start, 
             sea_end = sea_end, 
             min_pts = 3,
             nest_cycle = 28, 
             min_d_fix = 1,
             min_consec = 1,
             min_top_att = 1,
             min_days_att = 1, discard_overlapping = TRUE)

nest_mapper(nestR_out, time_zone_local = "America/Mazatlan", tag_and_breeding_data, 
            bird_ring = bird_ring, map_year = map_year)

known_nests <-
  tag_and_breeding_data$nests %>%
  dplyr::filter(ring == bird_ring &
                  year(nest_initiation_date) == map_year &
                  min(nestR_out$visits$date) < end_date) %>%
  mutate(burst = paste(ring, year(nest_initiation_date), sep = "-")) %>%
  arrange(nest_initiation_date) %>% 
  dplyr::select(burst, lon, lat) %>%
  rename(long = lon,
         lat = lat) %>%
  as.data.frame() %>% 
  distinct()

# distance between nests
distGeo(nestR_out$nests %>% 
          arrange(attempt_start) %>% 
          dplyr::select(long, lat), 
        known_nests %>% dplyr::select(long, lat))

# time difference between nesting attempts (based on GPS data)
(nestR_out$nests %>% 
    arrange(attempt_start) %>% 
    slice(2) %>% 
    pull(attempt_start)) - 
  (nestR_out$nests %>% 
     arrange(attempt_start) %>% 
     slice(1) %>% 
     pull(attempt_end))

# time difference between nesting attempts (based on field data)
(tag_and_breeding_data$nests %>% 
    dplyr::filter(ring == bird_ring & 
                    year(nest_initiation_date) == map_year) %>% 
    arrange(nest_initiation_date) %>% 
    slice(1) %>% 
    pull(end_date)) - 
  (tag_and_breeding_data$nests %>% 
     dplyr::filter(ring == bird_ring & 
                     year(nest_initiation_date) == map_year) %>% 
     arrange(nest_initiation_date) %>% 
     slice(2) %>% 
     pull(nest_initiation_date))

# nest fates
(tag_and_breeding_data$nests %>% 
    dplyr::filter(ring == bird_ring & 
                    year(nest_initiation_date) == map_year) %>% 
    arrange(nest_initiation_date) %>% 
    slice(1) %>% 
    pull(fate))

(tag_and_breeding_data$nests %>% 
    dplyr::filter(ring == bird_ring & 
                    year(nest_initiation_date) == map_year) %>% 
    arrange(nest_initiation_date) %>% 
    slice(2) %>% 
    pull(fate))

#### CN0306 from 2019 (nanoFix 24-hour @ 0600) ----
# tag data doesn't cover the nesting period adequately 
bird_ring = "CN0306"
map_year = 2019

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

nestR_out <- 
  find_nests(nestR_data %>% 
               arrange(date) %>% 
               filter(str_detect(burst, bird_ring) & 
                        night_fix == 0),
             # mutate(diff_10 = as_hms(round(ymd_hms(date), "mins") - 
             #                           hms(seconds = 00, minutes = 00, hours = 10)),
             #        diff_22 = as_hms(round(ymd_hms(date), "mins") - 
             #                           hms(seconds = 00, minutes = 00, hours = 22))) %>%
             # filter(diff_10 == as_hms("00:00:00") | diff_22 == as_hms("00:00:00")),
             buffer = 20,
             sea_start = sea_start, 
             sea_end = sea_end, 
             min_pts = 3,
             nest_cycle = 28, 
             min_d_fix = 1,
             min_consec = 1,
             min_top_att = 1,
             min_days_att = 1, discard_overlapping = TRUE)

nest_mapper(nestR_out, time_zone_local = "America/Mazatlan", tag_and_breeding_data, 
            bird_ring = bird_ring, map_year = map_year)

known_nests <-
  tag_and_breeding_data$nests %>%
  dplyr::filter(ring == bird_ring &
                  year(nest_initiation_date) == map_year &
                  min(nestR_out$visits$date) < end_date) %>%
  mutate(burst = paste(ring, year(nest_initiation_date), sep = "-")) %>%
  arrange(nest_initiation_date) %>% 
  dplyr::select(burst, lon, lat) %>%
  rename(long = lon,
         lat = lat) %>%
  as.data.frame() %>% 
  distinct()

# distance between nests
distGeo(nestR_out$nests %>% 
          arrange(attempt_start) %>% 
          dplyr::select(long, lat), 
        known_nests %>% dplyr::select(long, lat))

# time difference between nesting attempts (based on GPS data)
(nestR_out$nests %>% 
    arrange(attempt_start) %>% 
    slice(2) %>% 
    pull(attempt_start)) - 
  (nestR_out$nests %>% 
     arrange(attempt_start) %>% 
     slice(1) %>% 
     pull(attempt_end))

# time difference between nesting attempts (based on field data)
(tag_and_breeding_data$nests %>% 
    dplyr::filter(ring == bird_ring & 
                    year(nest_initiation_date) == map_year) %>% 
    arrange(nest_initiation_date) %>% 
    slice(1) %>% 
    pull(end_date)) - 
  (tag_and_breeding_data$nests %>% 
     dplyr::filter(ring == bird_ring & 
                     year(nest_initiation_date) == map_year) %>% 
     arrange(nest_initiation_date) %>% 
     slice(2) %>% 
     pull(nest_initiation_date))

# nest fates
(tag_and_breeding_data$nests %>% 
    dplyr::filter(ring == bird_ring & 
                    year(nest_initiation_date) == map_year) %>% 
    arrange(nest_initiation_date) %>% 
    slice(1) %>% 
    pull(fate))

(tag_and_breeding_data$nests %>% 
    dplyr::filter(ring == bird_ring & 
                    year(nest_initiation_date) == map_year) %>% 
    arrange(nest_initiation_date) %>% 
    slice(2) %>% 
    pull(fate))

#### *CN0066 from 2022 (PinPoint 20-min) ----
# only 20-min data, finds nest and foraging location
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0066", map_year = 2022)
CN0066_nest_search <- 
  find_plover_nests(tagging_data = tag_breeding_data_ceuta$tagging,
                    longitude_col = "lon",
                    latitude_col = "lat",
                    local_UTM_zone = 13,
                    bird_ring = "CN0066",
                    circadian_search_period = "daytime",
                    local_time_zone = "America/Mazatlan",
                    recurse_radius_size = 5,
                    recurse_time_threshold = 1,
                    recurse_time_threshold_unit = "hours",
                    revisit_threshold = 1,
                    cluster_distance_threshold = 30,
                    nest_data = tag_breeding_data_ceuta$nests,
                    tag_year = 2022)

# evaluate nest site predictions
CN0066_nest_search$nest_search_plot

# determine the modal interval between fixes
CN0066_nest_search$tag_latlon_move %>% 
  timeLag(unit = "hours") %>% 
  getmode()

# determine the tag type ("PP" = PinPoint, "NF" = nanoFix)
CN0066_nest_search$bird_tagging_data$tag_ID %>% unique()

# determine the hours sampled
CN0066_nest_search$bird_tagging_data %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M")) %>% 
  pull(rounded_hour) %>% unique()

#### CN0520 from 2021 (nanoFix 4-hour @ 0600/1000/1400/1800) ----
# limited tag data overlapping nesting
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0520", map_year = 2021)
CN0520_nest_search <- 
  find_plover_nests(tagging_data = tag_breeding_data_ceuta$tagging,
                    longitude_col = "lon",
                    latitude_col = "lat",
                    local_UTM_zone = 13,
                    bird_ring = "CN0520",
                    circadian_search_period = "daytime",
                    local_time_zone = "America/Mazatlan",
                    recurse_radius_size = 3,
                    recurse_time_threshold = 3,
                    recurse_time_threshold_unit = "days",
                    revisit_threshold = 1,
                    cluster_distance_threshold = 30,
                    nest_data = tag_breeding_data_ceuta$nests,
                    tag_year = 2021)

# evaluate nest site predictions
CN0520_nest_search$nest_search_plot

# determine the modal interval between fixes
CN0520_nest_search$tag_latlon_move %>% 
  timeLag(unit = "hours") %>% 
  getmode()

# determine the tag type ("PP" = PinPoint, "NF" = nanoFix)
CN0520_nest_search$bird_tagging_data$tag_ID %>% unique()

# determine the hours sampled
CN0520_nest_search$bird_tagging_data %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M")) %>% 
  pull(rounded_hour) %>% unique()

#### *CN0517 from 2019 (nanoFix 24-hour @ 0600) ----
# found nest despite only a few fixes (cool!)
# 10m recurse radius, 1 hour revisit
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0517", map_year = 2019)
CN0517_nest_search <- 
  find_plover_nests(tagging_data = tag_breeding_data_ceuta$tagging,
                    longitude_col = "lon",
                    latitude_col = "lat",
                    local_UTM_zone = 13,
                    bird_ring = "CN0517",
                    circadian_search_period = "daytime",
                    local_time_zone = "America/Mazatlan",
                    recurse_radius_size = 10,
                    recurse_time_threshold = 1,
                    recurse_time_threshold_unit = "hours",
                    revisit_threshold = 1,
                    cluster_distance_threshold = 30,
                    nest_data = tag_breeding_data_ceuta$nests,
                    tag_year = 2019, end_md_of_breeding_season = "07-01")

# evaluate nest site predictions
CN0517_nest_search$nest_search_plot

# determine the modal interval between fixes
CN0517_nest_search$tag_latlon_move %>% 
  timeLag(unit = "hours") %>% 
  getmode()

# determine the tag type ("PP" = PinPoint, "NF" = nanoFix)
CN0517_nest_search$bird_tagging_data$tag_ID %>% unique()

# determine the hours sampled
CN0517_nest_search$bird_tagging_data %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M")) %>% 
  pull(rounded_hour) %>% unique()

#### **CA3224 from 2019 (nanoFix 24-hour @ 0600) ----
# locates all known nests nicely
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CA3224", map_year = 2019)
CA3224_nest_search <- 
  find_plover_nests(tagging_data = tag_breeding_data_ceuta$tagging,
                    longitude_col = "lon",
                    latitude_col = "lat",
                    local_UTM_zone = 13,
                    bird_ring = "CA3224",
                    circadian_search_period = "daytime",
                    local_time_zone = "America/Mazatlan",
                    recurse_radius_size = 3,
                    recurse_time_threshold = 2,
                    recurse_time_threshold_unit = "days",
                    revisit_threshold = 1,
                    cluster_distance_threshold = 30,
                    nest_data = tag_breeding_data_ceuta$nests,
                    tag_year = 2019)

# evaluate nest site predictions
CA3224_nest_search$nest_search_plot

# determine the modal interval between fixes
CA3224_nest_search$tag_latlon_move %>% 
  timeLag(unit = "hours") %>% 
  getmode()

# determine the tag type ("PP" = PinPoint, "NF" = nanoFix)
CA3224_nest_search$bird_tagging_data$tag_ID %>% unique()

# determine the hours sampled
CA3224_nest_search$bird_tagging_data %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M")) %>% 
  pull(rounded_hour) %>% unique()

min(CA3224_nest_search$nest_visits_list[[2]]$revisitStats$entranceTime) - 
  max(CA3224_nest_search$nest_visits_list[[1]]$revisitStats$exitTime)

#### *CN0318 from 2019 (nanoFix 24-hour @ 0600) ----
# locates second nest nicely
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0318", map_year = 2019)
CN0318_nest_search <- 
  find_plover_nests(tagging_data = tag_breeding_data_ceuta$tagging,
                    longitude_col = "lon",
                    latitude_col = "lat",
                    local_UTM_zone = 13,
                    bird_ring = "CN0318",
                    circadian_search_period = "daytime",
                    local_time_zone = "America/Mazatlan",
                    recurse_radius_size = 3,
                    recurse_time_threshold = 3,
                    recurse_time_threshold_unit = "days",
                    revisit_threshold = 1,
                    cluster_distance_threshold = 30,
                    nest_data = tag_breeding_data_ceuta$nests,
                    tag_year = 2019)

# evaluate nest site predictions
CN0318_nest_search$nest_search_plot

# determine the modal interval between fixes
CN0318_nest_search$tag_latlon_move %>% 
  timeLag(unit = "hours") %>% 
  getmode()

# determine the tag type ("PP" = PinPoint, "NF" = nanoFix)
CN0318_nest_search$bird_tagging_data$tag_ID %>% unique()

# determine the hours sampled
CN0318_nest_search$bird_tagging_data %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M")) %>% 
  pull(rounded_hour) %>% unique()

#### *CN0422 from 2019 (nanoFix 24-hour @ 0600) ----
# locates second nest nicely
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0422", map_year = 2019)
CN0422_nest_search <- 
  find_plover_nests(tagging_data = tag_breeding_data_ceuta$tagging,
                    longitude_col = "lon",
                    latitude_col = "lat",
                    local_UTM_zone = 13,
                    bird_ring = "CN0422",
                    circadian_search_period = "daytime",
                    local_time_zone = "America/Mazatlan",
                    recurse_radius_size = 3,
                    recurse_time_threshold = 3,
                    recurse_time_threshold_unit = "days",
                    revisit_threshold = 1,
                    cluster_distance_threshold = 30,
                    nest_data = tag_breeding_data_ceuta$nests,
                    tag_year = 2019)

# evaluate nest site predictions
CN0422_nest_search$nest_search_plot

# determine the modal interval between fixes
CN0422_nest_search$tag_latlon_move %>% 
  timeLag(unit = "hours") %>% 
  getmode()

# determine the tag type ("PP" = PinPoint, "NF" = nanoFix)
CN0422_nest_search$bird_tagging_data$tag_ID %>% unique()

# determine the hours sampled
CN0422_nest_search$bird_tagging_data %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M")) %>% 
  pull(rounded_hour) %>% unique()

#### *CN0130 from 2019 (nanoFix 24-hour @ 0600) ----
# locates second nest nicely
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0130", map_year = 2019)
CN0130_nest_search <- 
  find_plover_nests(tagging_data = tag_breeding_data_ceuta$tagging,
                    longitude_col = "lon",
                    latitude_col = "lat",
                    local_UTM_zone = 13,
                    bird_ring = "CN0130",
                    circadian_search_period = "daytime",
                    local_time_zone = "America/Mazatlan",
                    recurse_radius_size = 3,
                    recurse_time_threshold = 3,
                    recurse_time_threshold_unit = "days",
                    revisit_threshold = 1,
                    cluster_distance_threshold = 30,
                    nest_data = tag_breeding_data_ceuta$nests,
                    tag_year = 2019)

# evaluate nest site predictions
CN0130_nest_search$nest_search_plot

# determine the modal interval between fixes
CN0130_nest_search$tag_latlon_move %>% 
  timeLag(unit = "hours") %>% 
  getmode()

# determine the tag type ("PP" = PinPoint, "NF" = nanoFix)
CN0130_nest_search$bird_tagging_data$tag_ID %>% unique()

# determine the hours sampled
CN0130_nest_search$bird_tagging_data %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M")) %>% 
  pull(rounded_hour) %>% unique()

#### CN0155 from 2019 (nanoFix 24-hour @ 0600) ----
# not enough data during breeding season
# full migration though
# both nest search approaches don't work
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0155", map_year = 2019)
CN0155_nest_search <- # recurse_time_threshold = 1, recurse_time_threshold_unit = "hours"
  find_plover_nests(tagging_data = tag_breeding_data_ceuta$tagging,
                    longitude_col = "lon",
                    latitude_col = "lat",
                    local_UTM_zone = 13,
                    bird_ring = "CN0155",
                    circadian_search_period = "daytime",
                    local_time_zone = "America/Mazatlan",
                    recurse_radius_size = 10,
                    recurse_time_threshold = 1,
                    recurse_time_threshold_unit = "hours",
                    revisit_threshold = 1,
                    cluster_distance_threshold = 30,
                    nest_data = tag_breeding_data_ceuta$nests,
                    tag_year = 2019)

# evaluate nest site predictions
CN0155_nest_search$nest_search_plot

# determine the modal interval between fixes
CN0155_nest_search$tag_latlon_move %>% 
  timeLag(unit = "hours") %>% 
  getmode()

# determine the tag type ("PP" = PinPoint, "NF" = nanoFix)
CN0155_nest_search$bird_tagging_data$tag_ID %>% unique()

# determine the hours sampled
CN0155_nest_search$bird_tagging_data %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M")) %>% 
  pull(rounded_hour) %>% unique()

nestR_CN0155_out <- 
  find_nests(nestR_data %>% 
               filter(str_detect(burst, "CN0155") & 
                        night_fix == 0), 
             buffer = 30,
             sea_start = "04-01", 
             sea_end = "08-01", 
             min_pts = 2,
             nest_cycle = 28, 
             min_d_fix = 1,
             min_consec = 1,
             min_top_att = 1,
             min_days_att = 1, discard_overlapping = TRUE)

nest_mapper(nestR_out = nestR_CN0155_out, 
            plover_nest_finder_out = CN0155_nest_search,
            tag_and_breeding_data = tag_and_breeding_data,
            bird_ring = "CN0155")

#### Males ----
#### CA3314 from 2019 (nanoFix 24-hour @ 2300) ----
# not enough data during breeding season, but finds a spot close to nest
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CA3314", map_year = 2019)
CA3314_nest_search <- # recurse_time_threshold = 1, recurse_time_threshold_unit = "hours"
  find_plover_nests(tagging_data = tag_breeding_data_ceuta$tagging,
                    longitude_col = "lon",
                    latitude_col = "lat",
                    local_UTM_zone = 13,
                    bird_ring = "CA3314",
                    circadian_search_period = "nighttime",
                    local_time_zone = "America/Mazatlan",
                    recurse_radius_size = 10,
                    recurse_time_threshold = 1,
                    recurse_time_threshold_unit = "hours",
                    revisit_threshold = 1,
                    cluster_distance_threshold = 30,
                    nest_data = tag_breeding_data_ceuta$nests,
                    tag_year = 2019, end_md_of_breeding_season = "07-01")

# evaluate nest site predictions
CA3314_nest_search$nest_search_plot

# determine the modal interval between fixes
CA3314_nest_search$tag_latlon_move %>% 
  timeLag(unit = "hours") %>% 
  getmode()

# determine the tag type ("PP" = PinPoint, "NF" = nanoFix)
CA3314_nest_search$bird_tagging_data$tag_ID %>% unique()

# determine the hours sampled
CA3314_nest_search$bird_tagging_data %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M")) %>% 
  pull(rounded_hour) %>% unique()

#### *CA3340 from 2022 (PinPoint 20-min then 12-hour @ 1000/2200) ----
# picks up brood-care location
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CA3340", map_year = 2022)
CA3340_nest_search <- # recurse_time_threshold = 1, recurse_time_threshold_unit = "hours"
  find_plover_nests(tagging_data = tag_breeding_data_ceuta$tagging,
                    longitude_col = "lon",
                    latitude_col = "lat",
                    local_UTM_zone = 13,
                    bird_ring = "CA3340",
                    circadian_search_period = "nighttime",
                    local_time_zone = "America/Mazatlan",
                    recurse_radius_size = 3,
                    recurse_time_threshold = 1,
                    recurse_time_threshold_unit = "hours",
                    revisit_threshold = 1,
                    cluster_distance_threshold = 30,
                    nest_data = tag_breeding_data_ceuta$nests,
                    tag_year = 2022)

# evaluate nest site predictions
CA3340_nest_search$nest_search_plot

# determine the modal interval between fixes
CA3340_nest_search$tag_latlon_move %>% 
  timeLag(unit = "hours") %>% 
  getmode()

# determine the tag type ("PP" = PinPoint, "NF" = nanoFix)
CA3340_nest_search$bird_tagging_data$tag_ID %>% unique()

# determine the hours sampled
CA3340_nest_search$bird_tagging_data %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M")) %>% 
  pull(rounded_hour) %>% unique()

#### *CN0918 from 2022 (PinPoint 12-hour @ 1000/2200) ----
# finds second nest but also finds another cluster near the nest that is likely
# where the bird was foraging
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0918", map_year = 2022)
CN0918_nest_search <- 
  find_plover_nests(tagging_data = tag_breeding_data_ceuta$tagging,
                    longitude_col = "lon",
                    latitude_col = "lat",
                    local_UTM_zone = 13,
                    bird_ring = "CN0918",
                    circadian_search_period = "nighttime",
                    local_time_zone = "America/Mazatlan",
                    recurse_radius_size = 3,
                    recurse_time_threshold = 3,
                    recurse_time_threshold_unit = "days",
                    revisit_threshold = 1,
                    cluster_distance_threshold = 30,
                    nest_data = tag_breeding_data_ceuta$nests,
                    tag_year = 2022)

# evaluate nest site predictions
CN0918_nest_search$nest_search_plot

# determine the modal interval between fixes
CN0918_nest_search$tag_latlon_move %>% 
  timeLag(unit = "hours") %>% 
  getmode()

# determine the tag type ("PP" = PinPoint, "NF" = nanoFix)
CN0918_nest_search$bird_tagging_data$tag_ID %>% unique()

# determine the hours sampled
CN0918_nest_search$bird_tagging_data %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M")) %>% 
  pull(rounded_hour) %>% unique()

#### CA3315 from 2018 (PinPoint 96-hour @ 0000) ----
# limited tag data overlapping nesting
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CA3315", map_year = 2018)
# not adequate breeding season coverage
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CA3315", map_year = 2019)
CA3315_nest_search <- 
  find_plover_nests(tagging_data = tag_breeding_data_ceuta$tagging,
                    longitude_col = "lon",
                    latitude_col = "lat",
                    local_UTM_zone = 13,
                    bird_ring = "CA3315",
                    circadian_search_period = "nighttime",
                    local_time_zone = "America/Mazatlan",
                    recurse_radius_size = 10,
                    recurse_time_threshold = 1,
                    recurse_time_threshold_unit = "hours",
                    revisit_threshold = 1,
                    cluster_distance_threshold = 30,
                    nest_data = tag_breeding_data_ceuta$nests,
                    tag_year = 2018, end_md_of_breeding_season = "06-12")

# evaluate nest site predictions
CA3315_nest_search$nest_search_plot

# determine the modal interval between fixes
CA3315_nest_search$tag_latlon_move %>% 
  timeLag(unit = "hours") %>% 
  getmode()

# determine the tag type ("PP" = PinPoint, "NF" = nanoFix)
CA3315_nest_search$bird_tagging_data$tag_ID %>% unique()

# determine the hours sampled
CA3315_nest_search$bird_tagging_data %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M")) %>% 
  pull(rounded_hour) %>% unique()

#### CM1858 from 2019 (nanoFix 24-hour @ 2300) ----
# limited tag data overlapping nesting
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0138", map_year = 2019)
CN0138_nest_search <- 
  find_plover_nests(tagging_data = tag_breeding_data_ceuta$tagging,
                    longitude_col = "lon",
                    latitude_col = "lat",
                    local_UTM_zone = 13,
                    bird_ring = "CN0138",
                    circadian_search_period = "nighttime",
                    local_time_zone = "America/Mazatlan",
                    recurse_radius_size = 6,
                    recurse_time_threshold = 1,
                    recurse_time_threshold_unit = "hours",
                    revisit_threshold = 1,
                    cluster_distance_threshold = 30,
                    nest_data = tag_breeding_data_ceuta$nests,
                    tag_year = 2019)

# evaluate nest site predictions
CN0138_nest_search$nest_search_plot

# determine the modal interval between fixes
CN0138_nest_search$tag_latlon_move %>% 
  timeLag(unit = "hours") %>% 
  getmode()

# determine the tag type ("PP" = PinPoint, "NF" = nanoFix)
CN0138_nest_search$bird_tagging_data$tag_ID %>% unique()

# determine the hours sampled
CN0138_nest_search$bird_tagging_data %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M")) %>% 
  pull(rounded_hour) %>% unique()

#### CA2100 from 2018 (PinPoint 96-hour @ 0000) ----
# limited tag data overlapping nesting
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CA2100", map_year = 2018)
# not adequate breeding season coverage
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CA2100", map_year = 2019)
CA2100_nest_search <- 
  find_plover_nests(tagging_data = tag_breeding_data_ceuta$tagging,
                    longitude_col = "lon",
                    latitude_col = "lat",
                    local_UTM_zone = 13,
                    bird_ring = "CA2100",
                    circadian_search_period = "nighttime",
                    local_time_zone = "America/Mazatlan",
                    recurse_radius_size = 45,
                    recurse_time_threshold = 1,
                    recurse_time_threshold_unit = "hours",
                    revisit_threshold = 1,
                    cluster_distance_threshold = 30,
                    nest_data = tag_breeding_data_ceuta$nests,
                    tag_year = 2018, end_md_of_breeding_season = "06-04")

# evaluate nest site predictions
CA2100_nest_search$nest_search_plot

# determine the modal interval between fixes
CA2100_nest_search$tag_latlon_move %>% 
  timeLag(unit = "hours") %>% 
  getmode()

# determine the tag type ("PP" = PinPoint, "NF" = nanoFix)
CA2100_nest_search$bird_tagging_data$tag_ID %>% unique()

# determine the hours sampled
CA2100_nest_search$bird_tagging_data %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M")) %>% 
  pull(rounded_hour) %>% unique()

#### CN0311 from 2019 (nanoFix 24-hour @ 2300) ----
# locates nest but gives two clusters for it
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0311", map_year = 2019)
CN0311_nest_search <- 
  find_plover_nests(tagging_data = tag_breeding_data_ceuta$tagging,
                    longitude_col = "lon",
                    latitude_col = "lat",
                    local_UTM_zone = 13,
                    bird_ring = "CN0311",
                    circadian_search_period = "nighttime",
                    local_time_zone = "America/Mazatlan",
                    recurse_radius_size = 4,
                    recurse_time_threshold = 1,
                    recurse_time_threshold_unit = "hours",
                    revisit_threshold = 1,
                    cluster_distance_threshold = 30,
                    nest_data = tag_breeding_data_ceuta$nests,
                    tag_year = 2019, end_md_of_breeding_season = "06-11")

# evaluate nest site predictions
CN0311_nest_search$nest_search_plot

# determine the modal interval between fixes
CN0311_nest_search$tag_latlon_move %>% 
  timeLag(unit = "hours") %>% 
  getmode()

# determine the tag type ("PP" = PinPoint, "NF" = nanoFix)
CN0311_nest_search$bird_tagging_data$tag_ID %>% unique()

# determine the hours sampled
CN0311_nest_search$bird_tagging_data %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M")) %>% 
  pull(rounded_hour) %>% unique()

#### CN0118 from 2018 (PinPoint 96-hour @ 0000) ----
# not adequate breeding season coverage
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0118", map_year = 2018)
CN0118_nest_search <- 
  find_plover_nests(tagging_data = tag_breeding_data_ceuta$tagging,
                    longitude_col = "lon",
                    latitude_col = "lat",
                    local_UTM_zone = 13,
                    bird_ring = "CN0118",
                    circadian_search_period = "nighttime",
                    local_time_zone = "America/Mazatlan",
                    recurse_radius_size = 3,
                    recurse_time_threshold = 1,
                    recurse_time_threshold_unit = "hours",
                    revisit_threshold = 1,
                    cluster_distance_threshold = 30,
                    nest_data = tag_breeding_data_ceuta$nests,
                    tag_year = 2018)

# evaluate nest site predictions
CN0118_nest_search$nest_search_plot

# determine the modal interval between fixes
CN0118_nest_search$tag_latlon_move %>% 
  timeLag(unit = "hours") %>% 
  getmode()

# determine the tag type ("PP" = PinPoint, "NF" = nanoFix)
CN0118_nest_search$bird_tagging_data$tag_ID %>% unique()

# determine the hours sampled
CN0118_nest_search$bird_tagging_data %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M")) %>% 
  pull(rounded_hour) %>% unique()