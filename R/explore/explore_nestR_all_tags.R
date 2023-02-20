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

tag_breeding_data_ceuta$tagging %>% 
  dplyr::select(ring) %>% 
  distinct()

#### Females ----
#### CN0118 from 2018 (PinPoint 96-hour @ 0800) ----
# not adequate breeding season coverage
bird_ring = "CN0118"
map_year = 2018

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

nestR_out <- 
  find_nests(nestR_data %>% 
               arrange(date) %>% 
               filter(str_detect(burst, bird_ring) &
                        night_fix == 0 & str_detect(burst, as.character(map_year))),# %>% 
             # mutate(diff_10 = as_hms(round(ymd_hms(date), "mins") -
             #                           hms(seconds = 00, minutes = 00, hours = 10)),
             #        diff_22 = as_hms(round(ymd_hms(date), "mins") -
             #                           hms(seconds = 00, minutes = 00, hours = 22))) %>%
             # filter(diff_10 == as_hms("00:00:00") | diff_22 == as_hms("00:00:00")),
             buffer = 40,
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

#### **CN0161 from 2018 (PinPoint 9-hour schedule) ----
# perfectly locates the presumed second nesting attempt
bird_ring = "CN0161"
map_year = 2018

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

nestR_out <- 
  find_nests(nestR_data %>% 
               # arrange(date) %>% 
               # mutate(diff_10 = round(ymd_hms(date, tz = "America/Mazatlan"), "hours") - 
               #          hms(seconds = 00, minutes = 00, hours = 10),
               #        diff_22 = round(ymd_hms(date, tz = "America/Mazatlan"), "hours") - 
               #          hms(seconds = 00, minutes = 00, hours = 22))
               filter(str_detect(burst, bird_ring) &
                        night_fix == 0), #& hour(round(date, hours) %in% c(10, 22)),
             buffer = 20,
             sea_start = sea_start, 
             sea_end = sea_end, 
             min_pts = 3,
             nest_cycle = 28, 
             min_d_fix = 1,
             min_consec = 1,
             min_top_att = 1,
             min_days_att = 1, discard_overlapping = TRUE)

nest_mapper(nestR_out, tag_and_breeding_data, time_zone_local = "America/Mazatlan", 
            bird_ring = bird_ring, map_year = map_year)

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

nest_mapper(nestR_out, tag_and_breeding_data, time_zone_local = "America/Mazatlan",
            bird_ring = bird_ring, map_year = map_year)

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

#### *CN0930 from 2022 (PinPoint 20-min then 12-hour @ 1000/2200) ----
bird_ring = "CN0930"
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
    pull(nest_initiation_date)) - 
  (tag_and_breeding_data$nests %>% 
     dplyr::filter(ring == bird_ring & 
                     year(nest_initiation_date) == map_year) %>% 
     arrange(nest_initiation_date) %>% 
     slice(1) %>% 
     pull(end_date))

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

#### *CN0312 from 2019 (nanoFix 24-hour @ 0600)----
# identifies second nesting attempt nicely
bird_ring = "CN0312"
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
          arrange(long) %>% 
          dplyr::select(long, lat), 
        known_nests %>% dplyr::select(long, lat) %>% arrange(long))

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
bird_ring = "CN0066"
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
             buffer = 50,
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

#### CN0520 from 2021 (nanoFix 4-hour @ 0600/1000/1400/1800) ----
# limited tag data overlapping nesting
bird_ring = "CN0520"
map_year = 2021

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

nestR_out <- 
  find_nests(nestR_data %>% 
               arrange(date) %>% 
               filter(str_detect(burst, bird_ring) &
                        night_fix == 0 & str_detect(burst, as.character(map_year))),# %>% 
             # mutate(diff_10 = as_hms(round(ymd_hms(date), "mins") -
             #                           hms(seconds = 00, minutes = 00, hours = 10)),
             #        diff_22 = as_hms(round(ymd_hms(date), "mins") -
             #                           hms(seconds = 00, minutes = 00, hours = 22))) %>%
             # filter(diff_10 == as_hms("00:00:00") | diff_22 == as_hms("00:00:00")),
             buffer = 10,
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
                  year(nest_initiation_date) == map_year) %>% # &
  # min(nestR_out$visits$date) < end_date) %>%
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
    dplyr::select(family_ID, nest_initiation_date, end_date) %>% 
    distinct() %>% 
    slice(2) %>% 
    pull(nest_initiation_date)) -
  (tag_and_breeding_data$nests %>% 
     dplyr::filter(ring == bird_ring & 
                     year(nest_initiation_date) == map_year) %>% 
     arrange(nest_initiation_date) %>% 
     dplyr::select(family_ID, nest_initiation_date, end_date) %>% 
     distinct() %>% 
     slice(1) %>% 
     pull(end_date)) 

# nest fates
(tag_and_breeding_data$nests %>% 
    dplyr::filter(ring == bird_ring & 
                    year(nest_initiation_date) == map_year) %>% 
    arrange(nest_initiation_date) %>% 
    dplyr::select(family_ID, nest_initiation_date, end_date, fate) %>% 
    distinct() %>% 
    slice(1) %>% 
    pull(fate))

(tag_and_breeding_data$nests %>% 
    dplyr::filter(ring == bird_ring & 
                    year(nest_initiation_date) == map_year) %>% 
    arrange(nest_initiation_date) %>% 
    dplyr::select(family_ID, nest_initiation_date, end_date, fate) %>% 
    distinct() %>% 
    slice(2) %>% 
    pull(fate))


#### *CN0517 from 2019 (nanoFix 24-hour @ 0600) ----
# found nest despite only a few fixes (cool!)
# 10m recurse radius, 1 hour revisit
bird_ring = "CN0517"
map_year = 2019

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

nestR_out <- 
  find_nests(nestR_data %>% 
             arrange(date) %>% 
             filter(str_detect(burst, bird_ring) &
                      night_fix == 0),# %>% 
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

#### **CA3224 from 2019 (nanoFix 24-hour @ 0600) ----
# locates all known nests nicely
bird_ring = "CA3224"
map_year = 2019
map_year = 2022

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

nestR_out <- 
  find_nests(nestR_data %>% 
               arrange(date) %>% 
               filter(str_detect(burst, bird_ring) &
                        night_fix == 0 & str_detect(burst, as.character(map_year))),# %>% 
             # mutate(diff_10 = as_hms(round(ymd_hms(date), "mins") -
             #                           hms(seconds = 00, minutes = 00, hours = 10)),
             #        diff_22 = as_hms(round(ymd_hms(date), "mins") -
             #                           hms(seconds = 00, minutes = 00, hours = 22))) %>%
             # filter(diff_10 == as_hms("00:00:00") | diff_22 == as_hms("00:00:00")),
             buffer = 50,
             sea_start = sea_start, 
             sea_end = sea_end, 
             min_pts = 3,
             nest_cycle = 28, 
             min_d_fix = 10,
             min_consec = 1,
             min_top_att = 1,
             min_days_att = 1, discard_overlapping = TRUE)

nest_mapper(nestR_out, time_zone_local = "America/Mazatlan", tag_and_breeding_data, 
            bird_ring = bird_ring, map_year = map_year)

tag_and_breeding_data$nests %>%
  dplyr::filter(family_ID == "2019_I_2")

known_nests <-
  tag_and_breeding_data$nests %>%
  dplyr::filter(ring == bird_ring &
                  year(nest_initiation_date) == map_year) %>% # &
                  # min(nestR_out$visits$date) < end_date) %>%
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
     dplyr::select(family_ID, nest_initiation_date, end_date) %>% 
     distinct() %>% 
     slice(2) %>% 
     pull(nest_initiation_date)) -
  (tag_and_breeding_data$nests %>% 
     dplyr::filter(ring == bird_ring & 
                     year(nest_initiation_date) == map_year) %>% 
     arrange(nest_initiation_date) %>% 
     dplyr::select(family_ID, nest_initiation_date, end_date) %>% 
     distinct() %>% 
     slice(1) %>% 
     pull(end_date)) 

# nest fates
(tag_and_breeding_data$nests %>% 
    dplyr::filter(ring == bird_ring & 
                    year(nest_initiation_date) == map_year) %>% 
    arrange(nest_initiation_date) %>% 
    dplyr::select(family_ID, nest_initiation_date, end_date, fate) %>% 
    distinct() %>% 
    slice(1) %>% 
    pull(fate))

(tag_and_breeding_data$nests %>% 
    dplyr::filter(ring == bird_ring & 
                    year(nest_initiation_date) == map_year) %>% 
    arrange(nest_initiation_date) %>% 
    dplyr::select(family_ID, nest_initiation_date, end_date, fate) %>% 
    distinct() %>% 
    slice(2) %>% 
    pull(fate))

#### *CN0318 from 2019 (nanoFix 24-hour @ 0600) ----
# locates second nest nicely
bird_ring = "CN0318"
map_year = 2019

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

nestR_out <- 
  find_nests(nestR_data %>% 
               arrange(date) %>% 
               filter(str_detect(burst, bird_ring) &
                        night_fix == 0),# %>% 
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

#### *CN0422 from 2019 (nanoFix 24-hour @ 0600) ----
bird_ring = "CN0422"
map_year = 2019

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

nestR_out <- 
  find_nests(nestR_data %>% 
               arrange(date) %>% 
               filter(str_detect(burst, bird_ring) &
                        night_fix == 0 & str_detect(burst, as.character(map_year))),# %>% 
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
#### *CN0130 from 2019 (nanoFix 24-hour @ 0600) ----
# locates second nest nicely
bird_ring = "CN0130"
map_year = 2019

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

nestR_out <- 
  find_nests(nestR_data %>% 
               arrange(date) %>% 
               filter(str_detect(burst, bird_ring) &
                        night_fix == 0),# %>% 
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

#### CN0155 from 2019 (nanoFix 24-hour @ 0600) ----
# not enough data during breeding season
# full migration though
# both nest search approaches don't work
bird_ring = "CN0155"
map_year = 2019

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

nestR_out <- 
  find_nests(nestR_data %>% 
               arrange(date) %>% 
               filter(str_detect(burst, bird_ring) &
                        night_fix == 0),# %>% 
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

#### Males ----
#### CA3314 from 2019 (nanoFix 24-hour @ 2300) ----
# not enough data during breeding season, but finds a spot close to nest
bird_ring = "CA3314"
map_year = 2019

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

nestR_out <- 
  find_nests(nestR_data %>% 
               arrange(date) %>% 
               filter(str_detect(burst, bird_ring) &
                        night_fix == 1),# %>% 
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

#### *CA3340 from 2022 (PinPoint 20-min then 12-hour @ 1000/2200) ----
bird_ring = "CA3340"
map_year = 2022

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

nestR_out <- 
  find_nests(nestR_data %>% 
               arrange(date) %>% 
               filter(str_detect(burst, bird_ring) &
                        night_fix == 1) %>% 
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

#### *CN0918 from 2022 (PinPoint 12-hour @ 1000/2200) ----
# finds second nest but also finds another cluster near the nest that is likely
# where the bird was foraging
bird_ring = "CN0918"
map_year = 2022

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

nestR_out <- 
  find_nests(nestR_data %>% 
               arrange(date) %>% 
               filter(str_detect(burst, bird_ring) &
                        night_fix == 1),# %>% 
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

#### CA3315 from 2018 (PinPoint 96-hour @ 0000) ----
# limited tag data overlapping nesting
bird_ring = "CA3315"
map_year = 2018

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

nestR_out <- 
  find_nests(nestR_data %>% 
               arrange(date) %>% 
               filter(str_detect(burst, bird_ring) &
                        night_fix == 1 & str_detect(burst, as.character(map_year))),# %>% 
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

#### CN0138 from 2019 (nanoFix 24-hour @ 2300) ----
# limited tag data overlapping nesting
bird_ring = "CN0138"
map_year = 2019

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

nestR_out <- 
  find_nests(nestR_data %>% 
               arrange(date) %>% 
               filter(str_detect(burst, bird_ring) &
                        night_fix == 1 & str_detect(burst, as.character(map_year))),# %>% 
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

#### CA2100 from 2018 (PinPoint 96-hour @ 0000) ----
# limited tag data overlapping nesting
bird_ring = "CA2100"
map_year = 2018

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

nestR_out <- 
  find_nests(nestR_data %>% 
               arrange(date) %>% 
               filter(str_detect(burst, bird_ring) &
                        night_fix == 1 & str_detect(burst, as.character(map_year))),# %>% 
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

#### CN0311 from 2019 (nanoFix 24-hour @ 2300) ----
# locates nest but gives two clusters for it
bird_ring = "CN0311"
map_year = 2019

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

nestR_out <- 
  find_nests(nestR_data %>% 
               arrange(date) %>% 
               filter(str_detect(burst, bird_ring) &
                        night_fix == 1 & str_detect(burst, as.character(map_year))),# %>% 
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