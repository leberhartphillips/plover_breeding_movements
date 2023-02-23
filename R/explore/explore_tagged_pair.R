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

# subset data to focal birds
CN0423 <- 
  lapply(tag_breeding_data_ceuta, function(x) 
    subset(x, ring %in% c("CN0423")))

CA3340 <- 
  lapply(tag_breeding_data_ceuta, function(x) 
    subset(x, ring %in% c("CA3340")))

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

#### summarise breeding events for pair ----
# extract data for each focal bird
snpl_pair <- 
  lapply(tag_breeding_data_ceuta, function(x) 
    subset(x, ring %in% c("CN0423", "CA3340")))

snpl_pair_nest_info <- 
  snpl_pair$nests %>% 
  filter(year(nest_initiation_date) == 2022 & cap_date > as.Date("2022-04-19")) %>% 
  dplyr::select(family_ID, nest_initiation_date, end_date, fate) %>% 
  distinct()

snpl_pair_brood_info <- 
  snpl_pair$broods %>% 
  group_by(ring, family_ID) %>% 
  summarise(min_date = min(timestamp_brood),
            max_date = max(timestamp_brood))

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
  labs(color = "time of day") #+
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
         dataset = "full") 

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
         dataset = "split") 

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
         dataset = "split")

# bind together
dftraj_snpl_pair <- 
  bind_rows(dftraj_snpl_pair_full, dftraj_snpl_pair_AM, dftraj_snpl_pair_PM)

# plot spatiotemporal repeatability 
ring_sex_labels <- c(
  "CA3340" = "Male",
  "CN0423" = "Female")

timestamp_labels <- c(
  "10:00:00" = "Morning (1000)",
  "22:00:00" = "Evening (2200)")

ggplot() +
  geom_point(data = dftraj_snpl_pair %>% filter(dataset == "full"),
             aes(x = rounded_time_ymd, y = distance_m, 
                 group = ring, color = ring), alpha = 0.5) +
  geom_line(data = dftraj_snpl_pair %>% filter(dataset == "full"),
            aes(x = rounded_time_ymd, y = distance_m, 
                group = ring, color = ring), alpha = 0.5) +
  geom_point(data = dftraj_snpl_pair %>% filter(dataset == "split"),
             aes(x = rounded_time_ymd, y = distance_m, 
                 group = ring, color = ring)) +
  geom_line(data = dftraj_snpl_pair %>% filter(dataset == "split"),
            aes(x = rounded_time_ymd, y = distance_m, 
                group = ring, color = ring)) +
  facet_grid(rounded_time_hms ~ ., 
             # ring ~ ., 
             labeller = labeller(.rows = timestamp_labels)) +
  scale_color_manual(labels = c("male", "female"), values = c("blue", "red")) +
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

# make a spatial trajectory object of each animal's encounter history
CA3340_step_lengths <- 
  step_length_calculator(df = snpl_move_aligned_round %>% filter(ring == "CA3340"), 
                         longitude_col = "lon", 
                         latitude_col = "lat", 
                         ind_col = "ring")

CN0423_step_lengths <- 
  step_length_calculator(df = snpl_move_aligned_round %>% filter(ring == "CN0423"), 
                         longitude_col = "lon", 
                         latitude_col = "lat", 
                         ind_col = "ring")

extract_sampling_times(df = snpl_move_aligned_round %>% filter(ring == "CA3340"),
                       timestamp_col = "timestamp_local")





# plot(dftraj_snpl_pair %>% filter(ring == "CN0423") %>% pull(distance_m), CN0423_step_lengths$distance)



dftraj_snpl_pair %>% 
  arrange(desc(distance_m))




#%>% 
  left_join(., dplyr::select(snpl_pair$nests, nest_initiation_date, family_ID), 
            by = c("rounded_time_ymd" = "nest_initiation_date"), multiple = "all") %>% 
  rename(nest_initiation = family_ID) %>% 
  distinct() %>% 
  left_join(., dplyr::select(snpl_pair$nests, end_date, fate, family_ID), 
            by = c("rounded_time_ymd" = "end_date"), multiple = "all") %>% 
  rename(nest_end = family_ID) %>% 
  distinct() %>% 
  left_join(., snpl_pair$broods %>% 
              filter(sex == "M") %>% 
              mutate(brood_ymd = as.Date(timestamp_brood, tz = "America/Mazatlan")) %>% 
              dplyr::select(brood_ymd, family_ID),
            by = c("rounded_time_ymd" = "brood_ymd"), multiple = "all") %>% 
  rename(male_with_brood = family_ID) %>% 
  distinct() %>% 
  left_join(., snpl_pair$broods %>% 
              filter(sex == "F") %>% 
              mutate(brood_ymd = as.Date(timestamp_brood, tz = "America/Mazatlan")) %>% 
              dplyr::select(brood_ymd, family_ID),
            by = c("rounded_time_ymd" = "brood_ymd"), multiple = "all") %>% 
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
