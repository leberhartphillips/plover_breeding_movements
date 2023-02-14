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

# breeding pair both tagged with nesting and brooding
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0423", map_year = 2022)
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CA3340", map_year = 2022)

CN0423_move <- 
  tag_data_move_wrangle(formatted_tag_data = CN0423$tagging, 
                        temporal_res = 1, 
                        temporal_unit = "hours", 
                        longitude_name = "lon", 
                        latitude_name = "lat", 
                        timestamp_name = "timestamp_local", 
                        ind_name = "ring",
                        projection = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

CA3340_move <- 
  tag_data_move_wrangle(formatted_tag_data = CA3340$tagging, 
                        temporal_res = 1, 
                        temporal_unit = "hours", 
                        longitude_name = "lon", 
                        latitude_name = "lat", 
                        timestamp_name = "timestamp_local", 
                        ind_name = "ring",
                        projection = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#### proximity between birds ----
# extract data for each focal bird
snpl_pair <- 
  lapply(tag_breeding_data_ceuta, function(x) 
    subset(x, ring %in% c("CN0423", "CA3340")))
  
round(median(CA3340_move$sampling_time_lags))
round(median(CN0423_move$sampling_time_lags))

snpl_pair$tagging %>% 
  group_by(ring) %>% 
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

CA3340_move_aligned_round <- 
  CA3340_move$aligned_move_object %>% 
  as.data.frame() %>% 
  mutate(rounded_time = round(time, "hours")) %>% 
  filter(as_hms(rounded_time) %in% c(as_hms("22:00:00"), as_hms("10:00:00"))) %>% 
  rename(x_CA3340 = x,
         y_CA3340 = y)

snpl_pair$nests

snpl_pair_dist_season_events <- 
  full_join(CN0423_move_aligned_round, CA3340_move_aligned_round, by = "rounded_time") %>%
  dplyr::select(rounded_time, x_CN0423, y_CN0423, x_CA3340, y_CA3340) %>% 
  drop_na() %>% 
  mutate(proximity = distHaversine(p1 = matrix(c(x_CN0423, y_CN0423), ncol = 2),
                                   p2 = matrix(c(x_CA3340, y_CA3340), ncol = 2))) %>% 
  mutate(hms = hms::as_hms(rounded_time),
         ymd = as.Date(rounded_time, tz = "America/Mazatlan")) %>% 
  left_join(., dplyr::select(snpl_pair$nests, nest_initiation_date, family_ID), 
            by = c("ymd" = "nest_initiation_date"), multiple = "all") %>% 
  rename(nest_initiation = family_ID) %>% 
  distinct() %>% 
  left_join(., dplyr::select(snpl_pair$nests, end_date, fate, family_ID), 
            by = c("ymd" = "end_date"), multiple = "all") %>% 
  rename(nest_end = family_ID) %>% 
  distinct() %>% 
  left_join(., snpl_pair$broods %>% 
              filter(sex == "M") %>% 
              mutate(brood_ymd = as.Date(timestamp_brood, tz = "America/Mazatlan")) %>% 
              dplyr::select(brood_ymd, family_ID),
            by = c("ymd" = "brood_ymd"), multiple = "all") %>% 
  rename(male_with_brood = family_ID) %>% 
  distinct() %>% 
  left_join(., snpl_pair$broods %>% 
              filter(sex == "F") %>% 
              mutate(brood_ymd = as.Date(timestamp_brood, tz = "America/Mazatlan")) %>% 
              dplyr::select(brood_ymd, family_ID),
            by = c("ymd" = "brood_ymd"), multiple = "all") %>% 
  rename(female_with_brood = family_ID) %>% 
  distinct() 
  # filter(UUID %in% c("14000002d5", "14000002ea")) %>%
  ggplot() +
  # geom_rect(data = behav_data_sun_times,
  #           aes(xmin = 0, xmax = hms::as_hms(nauticalDawn), ymin = 0, ymax = 1100),
  #           fill = "#023858", alpha = 0.15) +
  # geom_rect(data = behav_data_sun_times, 
  #           aes(xmin = hms::as_hms(nauticalDawn), xmax = hms::as_hms(dawn), ymin = 0, ymax = 1100),
  #           fill = "#045a8d", alpha = 0.15) +
  # geom_rect(data = behav_data_sun_times, 
  #           aes(xmin = hms::as_hms(dawn), xmax = hms::as_hms(sunrise), ymin = 0, ymax = 1100), 
  #           fill = "#0570b0", alpha = 0.15) +
  # geom_rect(data = behav_data_sun_times, 
  #           aes(xmin = hms::as_hms(sunrise), xmax = hms::as_hms(goldenHourEnd), ymin = 0, ymax = 1100), 
  #           fill = "#3690c0", alpha = 0.15) +
  # geom_rect(data = behav_data_sun_times, 
  #           aes(xmin = hms::as_hms(goldenHour), xmax = hms::as_hms(sunset), ymin = 0, ymax = 1100), 
  #           fill = "#3690c0", alpha = 0.15) +
  # geom_rect(data = behav_data_sun_times, 
  #           aes(xmin = hms::as_hms(sunset), xmax = hms::as_hms(dusk), ymin = 0, ymax = 1100), 
  #           fill = "#0570b0", alpha = 0.15) +
  # geom_rect(data = behav_data_sun_times, 
  #           aes(xmin = hms::as_hms(dusk), xmax = hms::as_hms(nauticalDusk), ymin = 0, ymax = 1100), 
  #           fill = "#045a8d", alpha = 0.15) +
  # geom_rect(data = behav_data_sun_times, 
  #           aes(xmin = hms::as_hms(nauticalDusk), xmax = Inf, ymin = 0, ymax = 1100), 
  #           fill = "#023858", alpha = 0.15) +
  # geom_line(data = temp_data,
  #           aes(x = hms::as.hms(timestamp), y = as.numeric(Temperature)/coef)) +
  geom_line(aes(x = rounded_time, y = proximity)) +#, 
                # group = hms,
                # color = hms)) +
  # facet_grid(. ~ hms) +
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
  labs(color = "time of day")#+
# scale_y_continuous(sec.axis = sec_axis(~.*coef, name="Second Axis"))


move_align_object = CA3340_move
ggplot() +
  geom_point(data = data.frame(times = as.Date(move_align_object$sampling_times[1:length(move_align_object$sampling_times) - 1]),
                               lags = move_align_object$sampling_time_lags),
             aes(x = times, y = lags)) +
  geom_line(data = data.frame(times = as.Date(move_align_object$sampling_times[1:length(move_align_object$sampling_times) - 1]),
                              lags = move_align_object$sampling_time_lags), 
            aes(x = times, y = lags), 
            color = brewer.pal(8, "Dark2")[c(6)]) +
  scale_x_date(date_labels = "%B", expand = c(0.01, 0.01), date_breaks = "1 months") +
  ylab("Time lag between\nsampling occasions (hours)") +
  theme_bw() +
  theme(
    # text = element_text(family = "Franklin Gothic Book"),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    panel.grid.major = element_line(colour = "grey70", size = 0.1),
    panel.grid.minor = element_line(colour = "grey70", size = 0.1),
    axis.ticks = element_line(size = 0.25, lineend = "round", colour = "grey60"),
    axis.ticks.length = unit(0.1, "cm"),
    panel.border = element_rect(linetype = "solid", colour = "grey"),
    plot.margin = unit(c(5, 1, 5, 1), "cm")
  ) +
  ggtitle(label = paste0(move_align_object$number_of_fixes, 
                         " fixes collected over ", 
                         round(move_align_object$timespan_of_sampling), " days"),
          subtitle = paste(paste("First sample = ", 
                                 move_align_object$sampling_start, 
                                 attr(as.POSIXlt(move_align_object$sampling_start),
                                      "tzone")[3], sep = " "),
                           paste("Last sample = ", 
                                 move_align_object$sampling_end, 
                                 attr(as.POSIXlt(move_align_object$sampling_start),
                                      "tzone")[3], sep = " "), sep = "\n"))

move_align_object = CN0423_move
ggplot() +
  geom_point(data = data.frame(times = as.Date(move_align_object$sampling_times[1:length(move_align_object$sampling_times) - 1]),
                               lags = move_align_object$sampling_time_lags),
             aes(x = times, y = lags)) +
  geom_line(data = data.frame(times = as.Date(move_align_object$sampling_times[1:length(move_align_object$sampling_times) - 1]),
                              lags = move_align_object$sampling_time_lags), 
            aes(x = times, y = lags), 
            color = brewer.pal(8, "Dark2")[c(6)]) +
  scale_x_date(date_labels = "%B", expand = c(0.01, 0.01), date_breaks = "1 months") +
  ylab("Time lag between\nsampling occasions (hours)") +
  theme_bw() +
  theme(
    # text = element_text(family = "Franklin Gothic Book"),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    panel.grid.major = element_line(colour = "grey70", size = 0.1),
    panel.grid.minor = element_line(colour = "grey70", size = 0.1),
    axis.ticks = element_line(size = 0.25, lineend = "round", colour = "grey60"),
    axis.ticks.length = unit(0.1, "cm"),
    panel.border = element_rect(linetype = "solid", colour = "grey"),
    plot.margin = unit(c(5, 1, 5, 1), "cm")
  ) +
  ggtitle(label = paste0(move_align_object$number_of_fixes, 
                         " fixes collected over ", 
                         round(move_align_object$timespan_of_sampling), " days"),
          subtitle = paste(paste("First sample = ", 
                                 move_align_object$sampling_start, 
                                 attr(as.POSIXlt(move_align_object$sampling_start),
                                      "tzone")[3], sep = " "),
                           paste("Last sample = ", 
                                 move_align_object$sampling_end, 
                                 attr(as.POSIXlt(move_align_object$sampling_start),
                                      "tzone")[3], sep = " "), sep = "\n"))


lapply(tag_breeding_data_ceuta$nests, function(x) 
  subset(x, family_ID %in% c("SNPL_2022_C_4")))

lapply(tag_breeding_data_ceuta$nests, function(x) 
  subset(x, ring %in% c("SNPL_2022_C_4")))

tag_breeding_data_ceuta$nests %>% 
  filter(family_ID == "SNPL_2022_C_4")

tag_breeding_data_ceuta$broods %>% 
  filter(family_ID %in% tagged_broods_summary$family_ID) %>% 
  select(ring, sex) %>% distinct()

# broods with both parents tagged
# SNPL_2022_C_2
# subset tagging and breeding data to "CN0423" and "CA3340"
snpl_pair <- 
  lapply(tag_breeding_data_ceuta, function(x) 
    subset(x, ring %in% c("CN0423", "CA3340")))

tagged_broods_summary <- 
  tag_breeding_data_ceuta$broods %>% 
  group_by(family_ID) %>% 
  summarise(min_date = min(timestamp_brood, na.rm = TRUE),
            max_date = max(timestamp_brood, na.rm = TRUE)) %>% 
  rename(ID = family_ID) %>% 
  left_join(., ceuta_list$Nests %>% 
              select(ID, nest_initiation_date, end_date, fate), 
            by = "ID") %>% 
  mutate(max_age_brood_obs = as.Date(max_date) - end_date) %>% 
  rename(family_ID = ID)

snpl_pair$broods %>% 
  group_by(brood_ID) %>% 
  summarise(min_date = min(timestamp_brood, na.rm = TRUE),
            max_date = max(timestamp_brood, na.rm = TRUE)) %>% 
  mutate()

snpl_pair$nests %>% 
  select(nest_ID, nest_initiation_date, end_date, fate) %>% 
  distinct()

# wrangle deployment information
deploy_info <- 
  plover_tagging_df %>% 
  group_by(species, ring, tag_ID, code, sex) %>% 
  summarise(first_fix = min(timestamp_date),
            last_fix = max(timestamp_date)) %>% 
  arrange(ring, first_fix)

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
  rename(deploy_nest_ID = ID)

# check the temporal coverage of the two focal birds and the deployment nest ID
tagging_data_ceuta %>% filter(ring %in% c("CN0423", "CA3340"))

# wrangle nest info for each tagged bird
tag_nest_data_ceuta <-
  ceuta_list$Nests %>% 
  mutate(female = ifelse(ID == "SNPL_2022_C_402", "OX.RM|OX.LX", female)) %>% 
  dplyr::select("ID", "female", "male", "easting", "northing", 
                "nest_initiation_date",  "end_date", 
                "last_observation_alive", "fate") %>% 
  pivot_longer(cols = c("female", "male"), names_to = "sex", values_to = "code") %>% 
  mutate(sex = ifelse(sex == "female", "F", ifelse(sex == "male", "M", NA))) %>% 
  left_join(tagging_data_ceuta, ., by = c("sex", "code")) %>% 
  distinct() %>% 
  rename(cap_date = date) %>% 
  st_as_sf(x = .,                         
           coords = c("easting", "northing"),
           crs = "+proj=utm +zone=13") %>% 
  st_transform(., crs = "+proj=longlat +datum=WGS84") %>% 
  sfc_as_cols(., names = c("lon", "lat")) %>% 
  st_drop_geometry() %>% 
  filter(end_date >= first_fix & nest_initiation_date <= last_fix) %>%
  arrange(first_fix) 

# check how many nests were the coverage of the two focal birds and the deployment nest ID
tag_nest_data_ceuta %>%
  filter(ring %in% c("CN0423", "CA3340")) %>% select(ID, code, nest_initiation_date) %>% arrange(nest_initiation_date)

plover_tagging_sf_nest <- 
  plover_tagging_sf %>% 
  sfc_as_cols(., names = c("bird_lon", "bird_lat")) %>% 
  filter(species %in% c("SNPL", "WIPL","KEPL")) %>%
  left_join(., tag_nest_data, by = c("ring", "tag_ID", "sex", "species", "code")) %>% 
  filter(timestamp_local <= end_date & timestamp_local >= nest_initiation_date) %>% 
  mutate(days_before_nest_end = end_date - as.Date(str_sub(as.character(timestamp_local), 
                                                           start = 1, end = 10), 
                                                   format = "%Y-%m-%d"),
         dist_from_nest = distHaversine(p1 = matrix(c(bird_lon, bird_lat), ncol = 2),
                                        p2 = matrix(c(nest_lon, nest_lat), ncol = 2))) %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M:%S"),
         time_of_day = format(timestamp_local, format = "%H:%M:%S"))

plover_tagging_sf_nest %>% 
  filter(ring %in% c("CN0423", "CA3340")) %>% 
  group_by(nest_ID) %>% 
  summarise(n_fixes = n())
