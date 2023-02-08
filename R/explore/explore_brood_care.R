# prepare R environment
source("R/project/project_libraries.R")
source("R/project/project_load_movement_data.R")
source("R/project/project_load_breeding_data.R")
source("R/wrangle/wrangle_tag_nest_brood_resight_data.R")

# summary of observed broods that have at least one of the parents tagged
tagged_broods_summary <-
  tag_breeding_data_ceuta$broods %>% 
  group_by(family_ID, ring, code) %>% 
  summarise(min_date = min(timestamp_brood, na.rm = TRUE),
            max_date = max(timestamp_brood, na.rm = TRUE)) %>% 
  rename(ID = family_ID) %>% 
  left_join(., ceuta_list$Nests %>% 
              select(ID, nest_initiation_date, end_date, fate), 
            by = "ID") %>% 
  mutate(post_hatch_days_with_tag_data = as.Date(max_date) - end_date) %>% 
  rename(family_ID = ID) %>% 
  select(family_ID, post_hatch_days_with_tag_data, ring, code, end_date) %>% 
  left_join(., tag_breeding_data_ceuta$broods %>% select(ring, family_ID, sex)) %>% 
  distinct() %>% 
  arrange(post_hatch_days_with_tag_data) %>% 
  mutate(status = "brooding")

ceuta_list$Nests %>% 
  filter(ID %in% c("2019_D_2", "2019_D_3"))

ceuta_list$Broods %>% 
  filter(ID %in% c("2019_D_2", "2019_D_3")) %>% 
  select(ID, male) %>% 
  distinct()

#### error checks ----
# check if there are obvious errors in the color combos reported in brood data
broods_with_multiple_males <- 
  ceuta_list$Broods %>% 
  mutate(male = ifelse(ID == "2018_C_301" & male == "GX.RM|GX.BX", "GX.MR|GX.BX", male)) %>% 
  filter(!is.na(male)) %>% 
  select(ID, male) %>% 
  distinct() %>% group_by(ID) %>% summarise(n_males = n_distinct(male)) %>%
  filter(str_detect(ID, "NA", negate = TRUE)) %>% 
  arrange(desc(n_males)) %>% 
  filter(n_males > 1) 

broods_with_multiple_females <- 
  ceuta_list$Broods %>% 
  filter(!is.na(female)) %>% 
  select(ID, female) %>% 
  distinct() %>% group_by(ID) %>% summarise(n_females = n_distinct(female)) %>%
  filter(str_detect(ID, "NA", negate = TRUE)) %>% 
  arrange(desc(n_females)) %>% 
  filter(n_females > 1)

tagged_brooding_summary %>% 
  filter(family_ID %in% broods_with_multiple_males$ID & family_ID %in% broods_with_multiple_females$ID)

# check if some combos are reported to have more than one nest at a time
codes_with_multiple_nests_in_tagging_dataset <-
  ceuta_list$Nests %>% 
  mutate(female = ifelse(ID == "SNPL_2022_C_402", "OX.RM|OX.LX", female),
         fate = ifelse(fate == "Hatched", "Hatch", fate)) %>% 
  pivot_longer(cols = c("female", "male"), names_to = "sex", values_to = "code") %>% 
  mutate(sex = ifelse(sex == "female", "F", ifelse(sex == "male", "M", NA))) %>% 
  filter(!is.na(code) & code != "XX.XX|XX.XX") %>% 
  group_by(species, code, year) %>% 
  summarise(n_nests = n_distinct(ID)) %>% 
  arrange(desc(n_nests)) %>% 
  filter(n_nests > 1) %>% 
  mutate(code_year = paste(code, year, sep = "_")) %>% 
  filter(code_year %in% tagged_brooding_summary$code_year)

tagged_brooding_summary$code_year = paste(tagged_brooding_summary$code, year(tagged_brooding_summary$end_date), sep = "_")

ceuta_list$Nests %>% 
  mutate(female = ifelse(ID == "SNPL_2022_C_402", "OX.RM|OX.LX", female),
         fate = ifelse(fate == "Hatched", "Hatch", fate)) %>% 
  pivot_longer(cols = c("female", "male"), names_to = "sex", values_to = "code") %>% 
  mutate(sex = ifelse(sex == "female", "F", ifelse(sex == "male", "M", NA))) %>% 
  filter(!is.na(code) & code != "XX.XX|XX.XX") %>% 
  filter(species == "SNPL") %>% 
  select(ID, found_date, nest_initiation_date, end_date, code, sex, observer, comments) %>% 
  arrange(found_date) %>% 
  mutate(code_year = paste(code, year(found_date), sep = "_")) %>% 
  filter(code_year %in% codes_with_multiple_nests_in_tagging_dataset$code_year) %>% 
  arrange(code, found_date)
  

ceuta_list$Captures %>% 
  # mutate(code = ifelse(ID == "SNPL_2022_E_3" & code == "OX.RM|WX.GX", "OX.RM|WX.GX_male", code)) %>%
  filter(ID == "SNPL_2022_E_3")

ceuta_list$Nests %>% 
  mutate(male = ifelse(ID == "SNPL_2022_E_3" & male == "OX.RM|WX.GX", "OX.RM|WX.GX_male", male)) %>%
  filter(ID %in% c("SNPL_2022_E_3"))

ceuta_list$Nests %>% 
  filter(ID %in% c("SNPL_2022_C_201", "SNPL_2022_D_205"))

ceuta_list$Nests %>% 
  filter(ID == "2018_C_301")

MX.RW|GX.GX

ceuta_list$Captures %>% 
  filter(ring == "CN0138")
  # filter(code == "GX.MR|GX.BX")

# summary of hatched nests that have at least one of the parents tagged but no
# known brooding information
tagged_hatched_nests_summary <- 
  tag_breeding_data_ceuta$nests %>% 
  group_by(family_ID) %>% 
  filter(fate == "Hatch") %>% 
  mutate(post_hatch_days_with_tag_data = last_fix - end_date) %>% 
  filter(post_hatch_days_with_tag_data > 0) %>% 
  select(family_ID, post_hatch_days_with_tag_data, ring, code, sex, end_date) %>% 
  filter(family_ID %!in% tagged_broods_summary$family_ID) %>% 
  mutate(status = "potential")

# bind two data summaries together
tagged_brooding_summary <- 
  bind_rows(tagged_broods_summary, tagged_hatched_nests_summary)

tagged_brooding_summary %>% 
  arrange(ring)

# subset tagging data to birds and time-periods of interest
tagged_brooding_subset <- 
  tag_breeding_data_ceuta$tagging %>% 
  left_join(tagged_brooding_summary, ., by = c("ring", "sex")) %>% 
  filter(end_date <= as.Date(timestamp_local) & (end_date + 30) >= as.Date(timestamp_local)) %>% 
  ungroup()

tagged_brooding_subset %>% 
  ungroup() %>% 
  select(ring, tag_ID) %>% 
  distinct() %>% 
  arrange(ring)

tagged_brooding_subset %>% 
  filter(ring == "CA3340") %>% 
  group_by(tag_ID) %>% 
  summarise(n())

CN0918_telemetry <- 
  df2move(df = arrange(filter(tagged_brooding_subset, ring == "CN0918"), timestamp_local), track_id = "ring", 
          proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
          x = "lon", y = "lat", time = "timestamp_local") %>% 
  as.telemetry()

known_brooders_telemetry <- 
  df2move(df = arrange(filter(tagged_brooding_subset, status == "brooding"), timestamp_local), track_id = "ring", 
          proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
          x = "lon", y = "lat", time = "timestamp_local") %>% 
  as.telemetry()

arrange(filter(tagged_brooding_subset, status == "brooding"), timestamp_local) %>% 
filter(timestamp_local == ymd_hms("2019-05-12 23:00:27", tz = "America/Mazatlan")) %>% 
  select(ring, family_ID, code.x, code.y, tag_ID, end_date)

zoom(SVF)

plot(CN0918_telemetry)
SVF <- variogram(CN0918_telemetry)
variogram.fit(SVF)
level <- c(0.5,0.95) # 50% and 95% CIs
xlim <- c(0,12 %#% "hour") # 0-12 hour window
plot(SVF,xlim=xlim,level=level)
title("zoomed in")
plot(SVF,fraction=0.65,level=level)
title("zoomed out")
  

tagged_brooding_subset %>% 
  filter(ring == "CN0918") %>%
  ungroup() %>% 
  select(timestamp_local, lat, lon) %>% 
  as.telemetry()

data("buffalo")
Cilla <- buffalo$Cilla


tagged_brooding_subset %>% 
  # filter(str_detect(tag_ID, "NF") & species == "KEPL") %>%
  # filter(year(timestamp_local) == 2020) %>% 
  # filter(month(timestamp_local) %in% c(4, 5, 6, 7)) %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M:%S") %>% hms::as_hms(.)) %>%
  select(ring, sex, rounded_hour) %>% 
  distinct() %>% 
  # arrange(rounded_hour)
  # mutate(time_hms = hms::as_hms(format(timestamp_local, format = "%H:%M:%S"))) %>% 
  # str()
  ggplot() +
  geom_histogram(aes(x = rounded_hour), stat = "count") +
  facet_grid(sex ~ .)



######### Junk ############
lapply(tag_breeding_data_ceuta$nests, function(x) 
  subset(x, family_ID %in% c("SNPL_2022_C_4")))

tag_breeding_data_ceuta$nests %>% 
  filter(family_ID == "SNPL_2022_C_4")

tag_breeding_data_ceuta$broods %>% 
  filter(family_ID == "2018_C_301") %>% 
  filter(sex == "F")

ceuta_list$Broods %>% 
  filter(ID == "2018_C_301")
  
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
