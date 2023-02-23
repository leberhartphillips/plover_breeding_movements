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
  summarise(min_date = min(timestamp_brood),
            max_date = max(timestamp_brood)) %>% 
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

# dftraj_snpl_pair_AM <- 
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
         dataset = "split") %>% 
  left_join(., snpl_pair_brood_info, by = "ring") %>% 
  left_join(., snpl_pair_nest_info, by = "ring") %>% 
  mutate(nesting = ifelse(rounded_time_ymd >= nest_initiation_date & rounded_time_ymd <= end_date, 1, 0),
         brooding = ifelse(rounded_time_ymd >= min_date & rounded_time_ymd <= max_date, 1, 0)) %>% 
  arrange(ring, rounded_time_ymd, desc(nesting), desc(brooding)) %>%
  dplyr::select(ring, rounded_time_ymd, family_ID.x, family_ID.y, 
                min_date, max_date, nest_initiation_date, 
                end_date, fate, nesting, brooding) %>% 
    distinct() %>% 
    print(n = 100)

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
  # geom_point(data = dftraj_snpl_pair %>% filter(dataset == "full"),
  #            aes(x = rounded_time_ymd, y = distance_m, 
  #                group = ring, color = ring), alpha = 0.5) +
  # geom_line(data = dftraj_snpl_pair %>% filter(dataset == "full"),
  #           aes(x = rounded_time_ymd, y = distance_m, 
  #               group = ring, color = ring), alpha = 0.5) +
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