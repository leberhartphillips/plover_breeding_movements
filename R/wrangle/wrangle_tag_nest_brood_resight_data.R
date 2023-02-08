# prepare R environment
source("R/project/project_libraries.R")
source("R/project/project_load_movement_data.R")
source("R/project/project_load_breeding_data.R")

# Determine the first fix for each tagging dataset
deploy_info <- 
  plover_tagging_df %>% 
  group_by(species, ring, tag_ID, code, sex) %>% 
  summarise(first_fix = min(timestamp_date),
            last_fix = max(timestamp_date)) %>% 
  arrange(ring, first_fix)

# extract the ring and tagIDs from the tagging dataset
rings_tags <- 
  plover_tagging_sf %>% 
  filter(species %in% c("SNPL", "WIPL","KEPL")) %>%
  select(ring, tag_ID) %>%
  distinct()

#### Ceuta breeding data ----
# wrangle the deployment dates and deployment nest IDs for each tagging dataset
tagging_data_ceuta <- 
  ceuta_list$Captures %>% 
  mutate(sex = ifelse(ring == "CA3340" & ID == "2019_D_203", "M", sex)) %>% 
  mutate(code = ifelse(ring == "CA3314", "GX.RM|BX.WX", code)) %>% 
  mutate(code = ifelse(ring == "CA3315", "GX.MR|GX.BX", code)) %>% 
  dplyr::select(ring, ID, code, date) %>% 
  left_join(deploy_info,., by = c("ring")) %>% 
  filter(code.x == code.y) %>% 
  select(-code.y) %>% 
  rename(code = code.x) %>% 
  mutate(time_diff = first_fix - date) %>% 
  filter(time_diff > -1) %>%
  arrange(ring, time_diff) %>% 
  group_by(ring, tag_ID) %>% 
  slice(1) %>% 
  arrange(desc(time_diff)) %>% 
  rename(deploy_nest_ID = ID) %>% 
  rename(cap_date = date)
  
# wrangle the nest information associated with each known nesting attempt of tagged birds
tag_nest_data_ceuta <- 
  ceuta_list$Nests %>% 
  mutate(female = ifelse(ID == "SNPL_2022_C_402", "OX.RM|OX.LX", female),
         fate = ifelse(fate == "Hatched", "Hatch", fate)) %>% 
  dplyr::select("ID", "female", "male", "easting", "northing", 
                "nest_initiation_date",  "end_date", 
                "last_observation_alive", "fate") %>% 
  pivot_longer(cols = c("female", "male"), names_to = "sex", values_to = "code") %>% 
  mutate(sex = ifelse(sex == "female", "F", ifelse(sex == "male", "M", NA))) %>% 
  left_join(tagging_data_ceuta, ., by = c("sex", "code")) %>% 
  distinct() %>% 
  st_as_sf(x = .,                         
           coords = c("easting", "northing"),
           crs = "+proj=utm +zone=13") %>% 
  st_transform(., crs = "+proj=longlat +datum=WGS84") %>% 
  sfc_as_cols(., names = c("lon", "lat")) %>% 
  st_drop_geometry() %>% 
  filter(end_date >= first_fix & nest_initiation_date <= last_fix) %>% 
  arrange(first_fix) %>% 
  rename(family_ID = ID)

# wrangle the brooding information associated with each known brood of tagged birds
tag_brood_data_ceuta <- 
  ceuta_list$Broods %>% 
  dplyr::select("ID", "female", "male", "easting", "northing", 
                "date",  "time", "chicks", "distance", "degree") %>% 
  pivot_longer(cols = c("female", "male"), names_to = "sex", values_to = "code") %>% 
  mutate(sex = ifelse(sex == "female", "F", ifelse(sex == "male", "M", NA))) %>% 
  left_join(tagging_data_ceuta, ., by = c("sex", "code")) %>% 
  distinct() %>% 
  rename(resight_date = date,
         family_ID = ID) %>%
  filter(!is.na(easting)) %>% 
  st_as_sf(x = .,                         
           coords = c("easting", "northing"),
           crs = "+proj=utm +zone=13") %>% 
  st_transform(., crs = "+proj=longlat +datum=WGS84") %>% 
  sfc_as_cols(., names = c("obs_lon", "obs_lat")) %>% 
  bind_cols(., destPoint(p = cbind(as.numeric(.$obs_lon), as.numeric(.$obs_lat)),
                         b = as.numeric(.$degree),
                         d = as.numeric(.$distance))) %>% 
  rename(brood_lat = lat,
         brood_lon = lon) %>% 
  filter(resight_date >= first_fix & resight_date <= last_fix) %>% 
  arrange(first_fix) %>% 
  mutate(timestamp_brood = paste(as.character(resight_date),
                                 paste(str_sub(time, 1, 2), 
                                       str_sub(time, 3, 4), 
                                       "00", sep = ":"), sep = "-") %>% 
           ymd_hms(., tz = "America/Mazatlan")) %>% 
  mutate(brood_lat = ifelse(is.na(brood_lat), obs_lat, brood_lat),
         brood_lon = ifelse(is.na(brood_lon), obs_lon, brood_lon)) %>% 
  select(-c(resight_date, time, obs_lon, obs_lat, distance, degree)) %>% 
  st_drop_geometry()

# wrangle the resighting information associated with each tagged bird
tag_resight_data_ceuta <-
  ceuta_list$Resights %>% 
  dplyr::select("code", "easting", "northing", "date", "time", "distance", "degree", "comments") %>% 
  left_join(tagging_data_ceuta, ., by = "code") %>% 
  distinct() %>% 
  rename(resight_date = date) %>%
  filter(!is.na(easting)) %>% 
  filter(resight_date >= first_fix & resight_date <= last_fix) %>% 
  arrange(first_fix) %>% 
  mutate(timestamp_resight = paste(as.character(resight_date),
                                 paste(str_sub(time, 1, 2), 
                                       str_sub(time, 3, 4), 
                                       "00", sep = ":"), sep = "-") %>% 
           ymd_hms(., tz = "America/Mazatlan")) %>% 
  st_as_sf(x = .,                         
           coords = c("easting", "northing"),
           crs = "+proj=utm +zone=13") %>% 
  st_transform(., crs = "+proj=longlat +datum=WGS84") %>% 
  sfc_as_cols(., names = c("obs_lon", "obs_lat")) %>% 
  bind_cols(., destPoint(p = cbind(as.numeric(.$obs_lon), as.numeric(.$obs_lat)),
                         b = as.numeric(.$degree),
                         d = as.numeric(.$distance))) %>% 
  rename(bird_lat = lat,
         bird_lon = lon) %>% 
  mutate(bird_lat = ifelse(is.na(bird_lat), obs_lat, bird_lat),
         bird_lon = ifelse(is.na(bird_lon), obs_lon, bird_lon)) %>% 
  select(-c(resight_date, time, obs_lon, obs_lat, distance, degree)) %>% 
  st_drop_geometry()

# bind all ceuta breeding and tagging data into a single list
tag_breeding_data_ceuta <- 
  list(nests = tag_nest_data_ceuta,
       broods = tag_brood_data_ceuta,
       resights = tag_resight_data_ceuta,
       tagging = plover_tagging_df %>% filter(population == "ceuta" & species == "SNPL"))


#### Tagus breeding data ----
# wrangle the nest information associated with each known nesting attempt of tagged birds
tag_nest_data_tagus <-
  tagus_list$Nests %>% 
  mutate(end_date = ifelse(is.na(`HATCH DATE`), as.character(`INACTIVE DATE`), 
                           as.character(`HATCH DATE`)) %>% 
           as.Date(., format = "%Y-%m-%d")) %>%
  select("NEST ID", "FEMALE", "MALE", "LAT", "LON", "LAY DATE", end_date, "FATE") %>% 
  rename(ID = "NEST ID",
         female = FEMALE,
         male = MALE,
         lat = LAT,
         lon = LON,
         nest_initiation_date = "LAY DATE",
         fate = "FATE") %>% 
  mutate(nest_initiation_date = as.Date(nest_initiation_date),
         fate = tolower(ifelse(str_detect(fate, "hatch"), "Hatch", fate))) %>% 
  pivot_longer(cols = c("female", "male"), names_to = "sex", values_to = "ring") %>% 
  mutate(sex = ifelse(sex == "female", "F", ifelse(sex == "male", "M", NA)),
         ring = str_replace_all(ring, regex("\\s*"), "")) %>% 
  left_join(deploy_info,., by = c("ring")) %>% 
  select(-sex.y) %>% 
  rename(sex = sex.x) %>% 
  filter(species == "KEPL" & !is.na(lat))

# # bind ceuta and tagus data
# tag_nest_data <- 
#   tag_nest_data_ceuta %>% 
#   dplyr::select(species, ring, tag_ID, code, sex, first_fix, last_fix, ID, lat, lon, nest_initiation_date, end_date, fate) %>% 
#   bind_rows(., tag_nest_data_tagus) %>% 
#   rename(nest_lat = lat,
#          nest_lon = lon,
#          nest_ID = ID)
# 
# # check that all ring and tag_ID data has been added
# tag_nest_data %>% 
#   select(ring, tag_ID) %>% 
#   distinct() %>% 
#   mutate(check = "X") %>% 
#   right_join(rings_tags,., by = c("ring", "tag_ID"))
# 
# # merge nest data with tagging data
# plover_tagging_sf_nest <- 
#   plover_tagging_sf %>% 
#   sfc_as_cols(., names = c("bird_lon", "bird_lat")) %>% 
#   filter(species %in% c("SNPL", "WIPL","KEPL")) %>%
#   left_join(., tag_nest_data, by = c("ring", "tag_ID", "sex", "species", "code")) %>% 
#   filter(timestamp_local <= end_date & timestamp_local >= nest_initiation_date) %>% 
#   mutate(days_before_nest_end = end_date - as.Date(str_sub(as.character(timestamp_local), 
#                                                     start = 1, end = 10), 
#                                             format = "%Y-%m-%d"),
#          dist_from_nest = distHaversine(p1 = matrix(c(bird_lon, bird_lat), ncol = 2),
#                                         p2 = matrix(c(nest_lon, nest_lat), ncol = 2))) %>% 
#   mutate(rounded_hour = round(timestamp_local, "hours") %>% 
#            format(., format = "%H:%M:%S"),
#          time_of_day = format(timestamp_local, format = "%H:%M:%S"))



# plover_tagging_sf_nest %>% 
#   # filter(fate != "Abandoned") %>% 
#   # filter(str_detect(tag_ID, "NF")) %>% 
#   filter(tag_ID == "PP48669") %>% 
#   ggplot(.) +
#   geom_boxplot(aes(x = hms::as_hms(rounded_hour),
#                    y = dist_from_nest, group = rounded_hour)) +
#   # geom_boxplot(aes(x = six_hour_bin,
#   #                  y = dist_from_nest, group = six_hour_bin)) +
#   facet_grid(sex ~ species)
# 
# as.ltraj(xy = coordinates(tag_df),
#          date = tag_df@data$timestamp,
#          id = tag_df@data$tag_ID)
# 
# #### Fit GAM to mass data ----
# daily_dist_mod <- 
#   gam(dist_from_nest ~ s(time_of_day, bs = "cc"), 
#       data = st_drop_geometry(plover_tagging_sf_nest))
# 
# #### estimate model predictions ----
# newdata_jul_days <- 
#   data.frame(JulianDay = seq(1, 365))
# 
# annual_mass_mod_fits <- 
#   predict(annual_mass_mod, 
#           newdata = newdata_jul_days, 
#           type = 'response', se = TRUE)
# 
# annual_mass_mod_predicts <-  
#   data.frame(newdata_jul_days, annual_mass_mod_fits) %>% 
#   mutate(lower = fit - 1.96 * se.fit,
#          upper = fit + 1.96 * se.fit,
#          JulianDay2 = as.Date(JulianDay, 
#                               format = "%j", 
#                               origin = as.Date("2018-01-01")))
# 
# plover_tagging_sf_nest_summary <- 
#   plover_tagging_sf_nest %>%
#   st_drop_geometry() %>% 
#   mutate(rounded_hour = round(timestamp_local, "hours") %>% 
#            format(., format = "%H:%M:%S")) %>% 
#   mutate(six_hour_bin = ifelse(rounded_hour >= format("00:00:00", format = "%H:%M:%S") & rounded_hour < format("06:00:01", format = "%H:%M:%S"), "midnight to 0600",
#                                ifelse(rounded_hour > format("06:00:00", format = "%H:%M:%S") & rounded_hour < format("12:00:01", format = "%H:%M:%S"), "0600 to noon",
#                                       ifelse(rounded_hour > format("12:00:00", format = "%H:%M:%S") & rounded_hour < format("18:00:01", format = "%H:%M:%S"), "noon to 1800",
#                                              ifelse(rounded_hour > format("18:00:00", format = "%H:%M:%S") & rounded_hour <= format("23:59:59", format = "%H:%M:%S"), "1800 to midnight", "XXX"))))) %>% 
#   mutate(six_hour_bin = factor(six_hour_bin, levels = c("midnight to 0600", "0600 to noon", "noon to 1800", "1800 to midnight")))
#   # group_by(species, sex, rounded_hour) %>% 
#   # summarise(mean_dist = mean(dist_from_nest, na.rm = TRUE),
#   #           sd_dist = sd(dist_from_nest, na.rm = TRUE))
# 
# plover_tagging_sf_nest_summary %>% 
#   # filter(fate != "Abandoned") %>% 
#   # filter(str_detect(tag_ID, "NF")) %>% 
#   filter(tag_ID == "PP48669") %>% 
#   ggplot(.) +
#   geom_boxplot(aes(x = hms::as_hms(rounded_hour),
#                    y = log(dist_from_nest), group = rounded_hour)) +
#   # geom_boxplot(aes(x = six_hour_bin,
#   #                  y = dist_from_nest, group = six_hour_bin)) +
#   facet_grid(sex ~ species)
# 
# plover_tagging_sf_nest_summary %>% 
#   filter(species == "SNPL" & sex == "M" & as.character(rounded_hour) %in% c("01:00:00", "02:00:00", "03:00:00"))
# 
# v41_pair_ODBA <-
#   behav_data %>%
#   filter(UUID %in% c("14000002d5", "14000002ea")) %>%
#   ggplot() +
#   geom_rect(data = behav_data_sun_times, 
#             aes(xmin = 0, xmax = hms::as_hms(nauticalDawn), ymin = 0, ymax = 7000), 
#             fill = "#023858", alpha = 0.15) +
#   geom_rect(data = behav_data_sun_times, 
#             aes(xmin = hms::as_hms(nauticalDawn), xmax = hms::as_hms(dawn), ymin = 0, ymax = 7000),
#             fill = "#045a8d", alpha = 0.15) +
#   geom_rect(data = behav_data_sun_times, 
#             aes(xmin = hms::as_hms(dawn), xmax = hms::as_hms(sunrise), ymin = 0, ymax = 7000), 
#             fill = "#0570b0", alpha = 0.15) +
#   geom_rect(data = behav_data_sun_times, 
#             aes(xmin = hms::as_hms(sunrise), xmax = hms::as_hms(goldenHourEnd), ymin = 0, ymax = 7000), 
#             fill = "#3690c0", alpha = 0.15) +
#   geom_rect(data = behav_data_sun_times, 
#             aes(xmin = hms::as_hms(dusk), xmax = hms::as_hms(nauticalDusk), ymin = 0, ymax = 7000), 
#             fill = "#045a8d", alpha = 0.15) +
#   geom_rect(data = behav_data_sun_times, 
#             aes(xmin = hms::as_hms(sunset), xmax = hms::as_hms(dusk), ymin = 0, ymax = 7000), 
#             fill = "#0570b0", alpha = 0.15) +
#   geom_rect(data = behav_data_sun_times, 
#             aes(xmin = hms::as_hms(goldenHour), xmax = hms::as_hms(sunset), ymin = 0, ymax = 7000), 
#             fill = "#3690c0", alpha = 0.15) +
#   geom_rect(data = behav_data_sun_times, 
#             aes(xmin = hms::as_hms(nauticalDusk), xmax = Inf, ymin = 0, ymax = 7000), 
#             fill = "#023858", alpha = 0.15) +
#   geom_line(aes(x = hms, y = ODBA, color = UUID)) +
#   # geom_line(data = filter(temp_data_HUB, UUID == "2a2000026b"),
#   #           aes(x = hms::as_hms(timestamp),
#   #               y = as.numeric(Temperature) / coef), color = "green", linetype = "longdash") +
#   geom_rect(data = behav_data_sun_times, 
#             aes(xmin = hms::as_hms(moonrise), xmax = hms::as_hms(moonset), 
#                 ymin = 0, ymax = fraction / coef), 
#             fill = "white", alpha = 0.15) +
#   facet_wrap(. ~ format(as.Date(timestamp, tz = "America/Santiago"), "%d-%B"), ncol = 1) +
#   luke_theme +
#   theme(legend.position = "none",
#         strip.background = element_blank(),
#         strip.text = element_text(size = 8, face = "italic"),
#         plot.title = element_text(hjust = 0.5, size = 11),
#         plot.subtitle = element_text(hjust = 0.5, size = 9, face = "italic")) +
#   ylab("overall dynamic body acceleration index\n (30-min intervals)") +
#   xlab("local time of day") +
#   scale_x_time(expand = c(0, 0)) +
#   scale_color_manual(values = c("#ff7f00", "#000000")) +
#   ggtitle(label = "Circadian activity of Vega 41 breeding pair (YG|GR and YG|BO)", 
#           subtitle = "Pair was caught tending two fledglings on 1 December.\n(blue regions show the local transition between night and day)") +
#   scale_y_continuous(sec.axis = sec_axis(~.*coef, name = "temperature (°C, dashed green)"))