# excellent example nests using low-resolution sampling to find nests (i.e., at 
# least one fix taken during known incubation shift)

#### Females with one daytime fix per day ----
# the following four females had fixes collected at times of day when they were
# expected to be on the nest during theit incubation shift. Each female had
# two known nests during the season and the GPS data nicely shows the time taken
# between breeding attempts

#### **CA3224 from 2019 (nanoFix 24-hour @ 0600) ----
# locates all known nests nicely
bird_ring = "CA3224"
map_year = 2019
# map_year = 2022

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

nestR_out <- 
  find_nests(nestR_data %>% 
               arrange(date) %>% 
               filter(str_detect(burst, bird_ring) &
                        night_fix == 0 & str_detect(burst, as.character(map_year))),
             buffer = 10,
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


#### Females with 20-min ----
# the following four females had fixes collected at times of day when they were
# expected to be on the nest during theit incubation shift. 

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

nestR_data %>% 
  # arrange(date) %>% 
  filter(str_detect(burst, bird_ring) &
           str_detect(burst, as.character(map_year))) %>% 
  st_as_sf(., 
           coords = c("long", "lat"),
           crs = projection) %>% 
  mapview(zcol = "night_fix", col.regions = c("yellow", "blue"))

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

# merge nest data with tagging data and make box plot
tag_breeding_data_ceuta$tagging %>% 
  mutate(timestamp_local = ymd_hms(timestamp_local, tz = "America/Mazatlan")) %>% 
  filter(ring == "CM1858" & 
           year(timestamp_local) == 2022) %>% 
  left_join(., tag_breeding_data_ceuta$nests %>% 
              filter(ring == "CM1858" & 
                       year(nest_initiation_date) == 2022) %>% 
              dplyr::select(ring, lon, lat, nest_initiation_date) %>% distinct(), by = "ring") %>% 
  filter(timestamp_local > nest_initiation_date) %>% 
  rename(bird_lat = lat.x,
         bird_lon = lon.x,
         nest_lat = lat.y,
         nest_lon = lon.y) %>% 
  mutate(dist_from_nest = distHaversine(p1 = matrix(c(bird_lon, bird_lat), ncol = 2),
                                        p2 = matrix(c(nest_lon, nest_lat), ncol = 2))) %>%
  mutate(rounded_hour = round(timestamp_local, "hours") %>%
           format(., format = "%H:%M:%S"),
         time_of_day = format(timestamp_local, format = "%H:%M:%S")) %>% 
  ggplot(.) +
  geom_boxplot(aes(x = hms::as_hms(rounded_hour),
                   y = dist_from_nest, group = rounded_hour))

#### **CA3224 from 2022 (PinPoint 20-min) ----
# locates all known nests nicely
bird_ring = "CA3224"
map_year = 2022

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

test <- 
  nestR_data %>% 
  arrange(date) %>% 
  filter(str_detect(burst, bird_ring) &
           night_fix == 0 & str_detect(burst, as.character(map_year))) %>% 
  mutate(rows_nums = as.numeric(row.names(.))) %>% 
  mutate(date_trans = ymd_hms(date, tz = "America/Mazatlan") + days(rows_nums))

nestR_out <- 
  find_nests(#test,
    nestR_data %>%
               arrange(date) %>%
               filter(str_detect(burst, bird_ring) &
                        night_fix == 0 &
                        str_detect(burst, as.character(map_year))),
             buffer = 20,
             sea_start = sea_start, 
             sea_end = sea_end, 
             min_pts = 10,
             nest_cycle = 28, 
             min_d_fix = 1,
             min_consec = 1,
             min_top_att = 1,
             min_days_att = 1, 
    discard_overlapping = TRUE)

nest_mapper(nestR_out, time_zone_local = "America/Mazatlan", tag_and_breeding_data, 
            bird_ring = bird_ring, map_year = map_year)

nestR_data %>% 
  # arrange(date) %>% 
  filter(str_detect(burst, bird_ring) &
           night_fix == 0 &
           str_detect(burst, as.character(map_year))) %>% 
  st_as_sf(., 
           coords = c("long", "lat"),
           crs = projection) %>% 
  mapview(zcol = "night_fix", col.regions = c("yellow", "blue"))

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

# merge nest data with tagging data and make box plot
tag_breeding_data_ceuta$tagging %>% 
  mutate(timestamp_local = ymd_hms(timestamp_local, tz = "America/Mazatlan")) %>% 
  filter(ring == "CA3224" & 
           year(timestamp_local) == 2022) %>% 
  left_join(., tag_breeding_data_ceuta$nests %>% 
              filter(ring == "CA3224" & 
                       year(nest_initiation_date) == 2022) %>% 
              dplyr::select(ring, lon, lat, nest_initiation_date) %>% distinct(), by = "ring") %>% 
  filter(timestamp_local > nest_initiation_date) %>% 
  rename(bird_lat = lat.x,
         bird_lon = lon.x,
         nest_lat = lat.y,
         nest_lon = lon.y) %>% 
  mutate(dist_from_nest = distHaversine(p1 = matrix(c(bird_lon, bird_lat), ncol = 2),
                                        p2 = matrix(c(nest_lon, nest_lat), ncol = 2))) %>%
  mutate(rounded_hour = round(timestamp_local, "hours") %>%
           format(., format = "%H:%M:%S"),
         time_of_day = format(timestamp_local, format = "%H:%M:%S")) %>% 
  ggplot(.) +
  geom_boxplot(aes(x = hms::as_hms(rounded_hour),
                   y = dist_from_nest, group = rounded_hour))


#### Males with 20-min ----
#### *CA3340 from 2022 (PinPoint 20-min then 12-hour @ 1000/2200) ----
# locates single known nests nicely
bird_ring = "CA3340"
map_year = 2022

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = bird_ring, map_year = map_year)

nestR_out <- 
  find_nests(nestR_data %>% 
               arrange(date) %>% 
               filter(str_detect(burst, bird_ring) & 
                        night_fix == 0 &
                        date < as.Date("2022-04-23")),
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

nestR_data %>% 
  arrange(date) %>% 
  filter(str_detect(burst, bird_ring) & 
           # night_fix == 0 &
           date < as.Date("2022-04-23")) %>% 
  st_as_sf(., 
           coords = c("long", "lat"),
           crs = projection) %>% 
  mapview(zcol = "night_fix", col.regions = c("yellow", "blue"))

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

# merge nest data with tagging data and make box plot
tag_breeding_data_ceuta$tagging %>% 
  mutate(timestamp_local = ymd_hms(timestamp_local, tz = "America/Mazatlan")) %>% 
  filter(ring == "CA3340" & 
           year(timestamp_local) == 2022) %>% 
  left_join(., tag_breeding_data_ceuta$nests %>% 
              filter(ring == "CA3340" & 
                       year(nest_initiation_date) == 2022) %>% 
              dplyr::select(ring, lon, lat, nest_initiation_date) %>% distinct(), by = "ring") %>% 
  filter(timestamp_local > nest_initiation_date) %>% 
  filter(timestamp_local < as.Date("2022-04-23")) %>% 
  rename(bird_lat = lat.x,
         bird_lon = lon.x,
         nest_lat = lat.y,
         nest_lon = lon.y) %>% 
  mutate(dist_from_nest = distHaversine(p1 = matrix(c(bird_lon, bird_lat), ncol = 2),
                                        p2 = matrix(c(nest_lon, nest_lat), ncol = 2))) %>%
  mutate(rounded_hour = round(timestamp_local, "hours") %>%
           format(., format = "%H:%M:%S"),
         time_of_day = format(timestamp_local, format = "%H:%M:%S")) %>% 
  ggplot(.) +
  geom_boxplot(aes(x = hms::as_hms(rounded_hour),
                   y = dist_from_nest, group = rounded_hour))
