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
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0161", map_year = 2018)
CN0161_nest_search <- 
  find_plover_nests(tagging_data = tag_breeding_data_ceuta$tagging,
                    longitude_col = "lon",
                    latitude_col = "lat",
                    local_UTM_zone = 13,
                    bird_ring = "CN0161",
                    circadian_search_period = "daytime",
                    local_time_zone = "America/Mazatlan",
                    recurse_radius_size = 3,
                    recurse_time_threshold = 3,
                    recurse_time_threshold_unit = "days",
                    revisit_threshold = 1,
                    cluster_distance_threshold = 30,
                    nest_data = tag_breeding_data_ceuta$nests,
                    tag_year = 2018)
CN0161_nest_search$nest_search_plot
CN0161_nest_search$tag_latlon_move %>% 
  timeLag(unit = "hours") %>% 
  getmode()

nestR_CN0161_out <- 
  find_nests(nestR_data %>% filter(str_detect(burst, "CN0161")), 
             buffer = 20,
             sea_start = "04-01", 
             sea_end = "08-01", 
             min_pts = 4,
             nest_cycle = 28, 
             min_d_fix = 1,
             min_consec = 1,
             min_top_att = 1,
             min_days_att = 1)

nest_mapper(nestR_out = nestR_CN0161_out, 
            plover_nest_finder_out = CN0161_nest_search,
            tag_and_breeding_data = tag_and_breeding_data,
            bird_ring = "CN0161")

known_nests <- 
  tag_and_breeding_data$nests %>% 
  dplyr::filter(ring == "CN0161" & 
                  year(nest_initiation_date) == "2018") %>% 
  mutate(burst = paste(ring, year(nest_initiation_date), sep = "-")) %>% 
  dplyr::select(burst, lon, lat) %>% 
  rename(long = lon,
         lat = lat) %>% 
  as.data.frame()

get_explodata(candidate_nests = nestR_CN0161_out$nests, 
              known_coords = known_nests,
              buffer = 40,
              pick_overlapping = FALSE)

explore_nests(nestR_data)

#### **CN0423 from 2022 (PinPoint 12-hour @ 1000/2200) ----
# perfectly locates both nesting attempts initiated post deployment
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0423", map_year = 2022)
CN0423_nest_search <- 
  find_plover_nests(tagging_data = tag_breeding_data_ceuta$tagging,
                    longitude_col = "lon",
                    latitude_col = "lat",
                    local_UTM_zone = 13,
                    bird_ring = "CN0423",
                    circadian_search_period = "daytime",
                    local_time_zone = "America/Mazatlan",
                    recurse_radius_size = 5,
                    recurse_time_threshold = 3,
                    recurse_time_threshold_unit = "days",
                    revisit_threshold = 1,
                    cluster_distance_threshold = 30,
                    nest_data = tag_breeding_data_ceuta$nests,
                    tag_year = 2022)
CN0423_nest_search$nest_search_plot
CN0423_nest_search$tag_latlon_move %>% 
  timeLag(unit = "hours") %>% 
  getmode()
CN0423_nest_search$bird_tagging_data$tag_ID %>% unique()

nestR_CN0423_out <- 
  find_nests(nestR_data %>% filter(str_detect(burst, "CN0423")), 
             buffer = 20,
             sea_start = "04-01", 
             sea_end = "08-01", 
             min_pts = 5,
             nest_cycle = 28, 
             min_d_fix = 1,
             min_consec = 1,
             min_top_att = 10,
             min_days_att = 2)

nest_mapper(nestR_out = nestR_CN0423_out, 
            plover_nest_finder_out = CN0423_nest_search,
            tag_and_breeding_data = tag_and_breeding_data,
            bird_ring = "CN0423")

#### *CN0937 from 2022 (PinPoint 20-min then 12-hour @ 1000/2200) ----
# locates second nesting attempt after first was predated 
# first nest had only 20-min sampling
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0937", map_year = 2022)
CN0937_nest_search <- 
  find_plover_nests(tagging_data = tag_breeding_data_ceuta$tagging,
                    longitude_col = "lon",
                    latitude_col = "lat",
                    local_UTM_zone = 13,
                    bird_ring = "CN0937",
                    circadian_search_period = "daytime",
                    local_time_zone = "America/Mazatlan",
                    recurse_radius_size = 3,
                    recurse_time_threshold = 3,
                    recurse_time_threshold_unit = "days",
                    revisit_threshold = 1,
                    cluster_distance_threshold = 30,
                    nest_data = tag_breeding_data_ceuta$nests,
                    tag_year = 2022)
# evaluate nest site predictions
CN0937_nest_search$nest_search_plot

# determine the modal interval between fixes
CN0937_nest_search$tag_latlon_move %>% 
  timeLag(unit = "hours") %>% 
  getmode()

# determine the tag type ("PP" = PinPoint, "NF" = nanoFix)
CN0937_nest_search$bird_tagging_data$tag_ID %>% unique()

# determine the hours sampled
CN0937_nest_search$bird_tagging_data %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M")) %>% 
  pull(rounded_hour) %>% unique()

nestR_CN0937_out <- 
  find_nests(nestR_data %>% filter(str_detect(burst, "CN0937") & date > as.Date("2022-05-06")), 
             buffer = 20,
             sea_start = "04-01", 
             sea_end = "08-01", 
             min_pts = 5,
             nest_cycle = 28, 
             min_d_fix = 1,
             min_consec = 1,
             min_top_att = 10,
             min_days_att = 2)

nest_mapper(nestR_out = nestR_CN0937_out, 
            plover_nest_finder_out = CN0937_nest_search,
            tag_and_breeding_data = tag_and_breeding_data,
            bird_ring = "CN0937")

#### *CN0930 from 2022 (PinPoint 20-min then 12-hour @ 1000/2200) ----
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0930", map_year = 2022)
CN0930_nest_search <- # recurse_radius_size = 10
  find_plover_nests(tagging_data = tag_breeding_data_ceuta$tagging,
                    longitude_col = "lon",
                    latitude_col = "lat",
                    local_UTM_zone = 13,
                    bird_ring = "CN0930",
                    circadian_search_period = "daytime",
                    local_time_zone = "America/Mazatlan",
                    recurse_radius_size = 10,
                    recurse_time_threshold = 3,
                    recurse_time_threshold_unit = "days",
                    revisit_threshold = 1,
                    cluster_distance_threshold = 30,
                    nest_data = tag_breeding_data_ceuta$nests,
                    tag_year = 2022)

# evaluate nest site predictions
CN0930_nest_search$nest_search_plot

# determine the modal interval between fixes
CN0930_nest_search$tag_latlon_move %>% 
  timeLag(unit = "hours") %>% 
  getmode()

# determine the tag type ("PP" = PinPoint, "NF" = nanoFix)
CN0930_nest_search$bird_tagging_data$tag_ID %>% unique()

# determine the hours sampled
CN0930_nest_search$bird_tagging_data %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M")) %>% 
  pull(rounded_hour) %>% unique()

nestR_CN0930_out <- 
  find_nests(nestR_data %>% filter(str_detect(burst, "CN0930") & date > as.Date("2022-04-30")), 
             buffer = 20,
             sea_start = "04-01", 
             sea_end = "08-01", 
             min_pts = 4,
             nest_cycle = 28, 
             min_d_fix = 1,
             min_consec = 1,
             min_top_att = 10,
             min_days_att = 2)

nest_mapper(nestR_out = nestR_CN0930_out, 
            plover_nest_finder_out = CN0930_nest_search,
            tag_and_breeding_data = tag_and_breeding_data,
            bird_ring = "CN0930")

#### CN0916 from 2022 (PinPoint 20-min then 12-hour @ 1000/2200) ----
# issues with temporal resolution (incorrectly found nests in cluster of 20-min fixes)
# nestR found second nest but location is a bit off
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0916", map_year = 2022)
CN0916_nest_search <- # recurse_time_threshold = 1, recurse_time_threshold_unit = "hours"
  find_plover_nests(tagging_data = tag_breeding_data_ceuta$tagging,
                    longitude_col = "lon",
                    latitude_col = "lat",
                    local_UTM_zone = 13,
                    bird_ring = "CN0916",
                    circadian_search_period = "daytime",
                    local_time_zone = "America/Mazatlan",
                    recurse_radius_size = 3,
                    recurse_time_threshold = 1,
                    recurse_time_threshold_unit = "hours",
                    revisit_threshold = 1,
                    cluster_distance_threshold = 30,
                    nest_data = tag_breeding_data_ceuta$nests,
                    tag_year = 2022)

# evaluate nest site predictions
CN0916_nest_search$nest_search_plot

# determine the modal interval between fixes
CN0916_nest_search$tag_latlon_move %>% 
  timeLag(unit = "hours") %>% 
  getmode()

# determine the tag type ("PP" = PinPoint, "NF" = nanoFix)
CN0916_nest_search$bird_tagging_data$tag_ID %>% unique()

# determine the hours sampled
CN0916_nest_search$bird_tagging_data %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M")) %>% 
  pull(rounded_hour) %>% unique()

nestR_CN0916_out <- 
  find_nests(nestR_data %>% 
               filter(str_detect(burst, "CN0916") & 
                        date > as.Date("2022-04-22") & 
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

nest_mapper(nestR_out = nestR_CN0916_out, 
            plover_nest_finder_out = CN0916_nest_search,
            tag_and_breeding_data = tag_and_breeding_data,
            bird_ring = "CN0916")

nestR_CN0916_out$nests

#### **CN0609 from 2022 (PinPoint 12-hour @ 1000/2200) ----
# finds second nest but cluster is slightly "off", could be issues with visit times
# nestR finds second nest
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0609", map_year = 2022)
nest_search <- 
  find_plover_nests(tagging_data = tag_breeding_data_ceuta$tagging,
                    longitude_col = "lon",
                    latitude_col = "lat",
                    local_UTM_zone = 13,
                    bird_ring = "CN0609",
                    circadian_search_period = "daytime",
                    local_time_zone = "America/Mazatlan",
                    recurse_radius_size = 4,
                    recurse_time_threshold = 1,
                    recurse_time_threshold_unit = "hours",
                    revisit_threshold = 1,
                    cluster_distance_threshold = 30,
                    nest_data = tag_breeding_data_ceuta$nests,
                    tag_year = 2022)

# evaluate nest site predictions
nest_search$nest_search_plot

# determine the modal interval between fixes
nest_search$tag_latlon_move %>% 
  timeLag(unit = "hours") %>% 
  getmode()

# determine the tag type ("PP" = PinPoint, "NF" = nanoFix)
nest_search$bird_tagging_data$tag_ID %>% unique()

# determine the hours sampled
nest_search$bird_tagging_data %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M")) %>% 
  pull(rounded_hour) %>% unique()

min(nest_search$nest_visits_list[[2]]$revisitStats$entranceTime) - 
  max(nest_search$nest_visits_list[[1]]$revisitStats$exitTime)

nestR_out <- 
  find_nests(nestR_data %>% 
               filter(str_detect(burst, "CN0609") & 
                        night_fix == 0), 
             buffer = 20,
             sea_start = sea_start, 
             sea_end = sea_end, 
             min_pts = 4,
             nest_cycle = 28, 
             min_d_fix = 1,
             min_consec = 1,
             min_top_att = 1,
             min_days_att = 1, discard_overlapping = TRUE)

nest_mapper(nestR_out = nestR_out, 
            plover_nest_finder_out = nest_search,
            tag_and_breeding_data = tag_and_breeding_data,
            bird_ring = "CN0609")

known_nests <- 
  tag_and_breeding_data$nests %>% 
  dplyr::filter(ring == "CN0609" & 
                  year(nest_initiation_date) == "2022") %>% 
  mutate(burst = paste(ring, year(nest_initiation_date), sep = "-")) %>% 
  dplyr::select(burst, lon, lat) %>% 
  rename(long = lon,
         lat = lat) %>% 
  as.data.frame()

get_explodata(candidate_nests = nestR_out$nests, 
              known_coords = known_nests,
              buffer = 40,
              pick_overlapping = FALSE)

distGeo(nestR_out$nests %>% dplyr::select(long, lat), 
        known_nests %>% dplyr::select(long, lat))

#### *CM1858 from 2022 (PinPoint 20-min) ----
# locates single known nests nicely
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CM1858", map_year = 2022)
CM1858_nest_search <- 
  find_plover_nests(tagging_data = tag_breeding_data_ceuta$tagging,
                    longitude_col = "lon",
                    latitude_col = "lat",
                    local_UTM_zone = 13,
                    bird_ring = "CM1858",
                    circadian_search_period = "daytime",
                    local_time_zone = "America/Mazatlan",
                    recurse_radius_size = 3,
                    recurse_time_threshold = 1,
                    recurse_time_threshold_unit = "hours",
                    revisit_threshold = 1,
                    cluster_distance_threshold = 30,
                    nest_data = tag_breeding_data_ceuta$nests,
                    tag_year = 2022)

# evaluate nest site predictions
CM1858_nest_search$nest_search_plot

# determine the modal interval between fixes
CM1858_nest_search$tag_latlon_move %>% 
  timeLag(unit = "hours") %>% 
  getmode()

# determine the tag type ("PP" = PinPoint, "NF" = nanoFix)
CM1858_nest_search$bird_tagging_data$tag_ID %>% unique()

# determine the hours sampled
CM1858_nest_search$bird_tagging_data %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M")) %>% 
  pull(rounded_hour) %>% unique()

#### CN0306 from 2019 (nanoFix 24-hour @ 0600) ----
# tag data doesn't cover the nesting period adequately 
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0306", map_year = 2019)
CN0306_nest_search <- 
  find_plover_nests(tagging_data = tag_breeding_data_ceuta$tagging,
                    longitude_col = "lon",
                    latitude_col = "lat",
                    local_UTM_zone = 13,
                    bird_ring = "CN0306",
                    circadian_search_period = "daytime",
                    local_time_zone = "America/Mazatlan",
                    recurse_radius_size = 3,
                    recurse_time_threshold = 1,
                    recurse_time_threshold_unit = "hours",
                    revisit_threshold = 1,
                    cluster_distance_threshold = 30,
                    nest_data = tag_breeding_data_ceuta$nests,
                    tag_year = 2019)

# evaluate nest site predictions
CN0306_nest_search$nest_search_plot

# determine the modal interval between fixes
CN0306_nest_search$tag_latlon_move %>% 
  timeLag(unit = "hours") %>% 
  getmode()

# determine the tag type ("PP" = PinPoint, "NF" = nanoFix)
CN0306_nest_search$bird_tagging_data$tag_ID %>% unique()

# determine the hours sampled
CN0306_nest_search$bird_tagging_data %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M")) %>% 
  pull(rounded_hour) %>% unique()

#### *CN0312 from 2019 (nanoFix 24-hour @ 0600)----
# identifies second nesting attempt nicely
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0312", map_year = 2019)
CN0312_nest_search <- 
  find_plover_nests(tagging_data = tag_breeding_data_ceuta$tagging,
                    longitude_col = "lon",
                    latitude_col = "lat",
                    local_UTM_zone = 13,
                    bird_ring = "CN0312",
                    circadian_search_period = "daytime",
                    local_time_zone = "America/Mazatlan",
                    recurse_radius_size = 3,
                    recurse_time_threshold = 1,
                    recurse_time_threshold_unit = "hours",
                    revisit_threshold = 1,
                    cluster_distance_threshold = 30,
                    nest_data = tag_breeding_data_ceuta$nests,
                    tag_year = 2019)

# evaluate nest site predictions
CN0312_nest_search$nest_search_plot

# determine the modal interval between fixes
CN0312_nest_search$tag_latlon_move %>% 
  timeLag(unit = "hours") %>% 
  getmode()

# determine the tag type ("PP" = PinPoint, "NF" = nanoFix)
CN0312_nest_search$bird_tagging_data$tag_ID %>% unique()

# determine the hours sampled
CN0312_nest_search$bird_tagging_data %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M")) %>% 
  pull(rounded_hour) %>% unique()

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


#### Tagus ----
#### D50218
tag_and_nest_data_mapper(tag_and_nest_data = tag_breeding_data_tagus,
                         bird_ring = "D50218", map_year = 2021, time_zone_local = "Europe/Lisbon")
#### D35644
tag_and_nest_data_mapper(tag_and_nest_data = tag_breeding_data_tagus,
                         bird_ring = "D35644", map_year = 2021, time_zone_local = "Europe/Lisbon")
#### D59946
tag_and_nest_data_mapper(tag_and_nest_data = tag_breeding_data_tagus,
                         bird_ring = "D59946", map_year = 2021, time_zone_local = "Europe/Lisbon")
#### D59933
tag_and_nest_data_mapper(tag_and_nest_data = tag_breeding_data_tagus,
                         bird_ring = "D59933", map_year = 2021, time_zone_local = "Europe/Lisbon")
#### P01903
tag_and_nest_data_mapper(tag_and_nest_data = tag_breeding_data_tagus,
                         bird_ring = "P01903", map_year = 2021, time_zone_local = "Europe/Lisbon")
#### D59182
tag_and_nest_data_mapper(tag_and_nest_data = tag_breeding_data_tagus,
                         bird_ring = "D59182", map_year = 2021, time_zone_local = "Europe/Lisbon")
#### D59932
tag_and_nest_data_mapper(tag_and_nest_data = tag_breeding_data_tagus,
                         bird_ring = "D59932", map_year = 2021, time_zone_local = "Europe/Lisbon")
#### P01902
tag_and_nest_data_mapper(tag_and_nest_data = tag_breeding_data_tagus,
                         bird_ring = "P01902", map_year = 2021, time_zone_local = "Europe/Lisbon")