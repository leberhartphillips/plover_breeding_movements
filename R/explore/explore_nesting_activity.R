# prepare R environment
source("R/project/project_libraries.R")
source("R/project/project_load_movement_data.R")
source("R/project/project_load_breeding_data.R")

# Determine the first fix for each tagging dataset
deploy_info <- 
  plover_tagging_df %>% 
  group_by(species, ring, tag_ID, code, sex) %>% 
  summarise(first_fix = min(timestamp_simple),
            last_fix = max(timestamp_simple)) %>% 
  arrange(ring, first_fix)

# extract the ring and tagIDs from the tagging dataset
rings_tags <- 
  plover_tagging_df %>% 
  filter(species %in% c("SNPL", "WIPL","KEPL")) %>%
  select(ring, tag_ID) %>%
  distinct()

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
  rename(deploy_nest_ID = ID)
  
# wrangle the nest information associated with each deployment nest
tag_nest_data_ceuta <- 
  ceuta_list$Nests %>% 
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

# wrangle the nest information associated with each deployment nest
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

# bind ceuta and tagus data
tag_nest_data <- 
  tag_nest_data_ceuta %>% 
  dplyr::select(species, ring, tag_ID, code, sex, first_fix, last_fix, ID, lat, lon, nest_initiation_date, end_date, fate) %>% 
  bind_rows(., tag_nest_data_tagus) %>% 
  rename(nest_lat = lat,
         nest_lon = lon,
         nest_ID = ID)

# check that all ring and tag_ID data has been added
tag_nest_data %>% 
  select(ring, tag_ID) %>% 
  distinct() %>% 
  mutate(check = "X") %>% 
  right_join(rings_tags,., by = c("ring", "tag_ID"))

# merge nest data with tagging data
plover_tagging_df %>% 
  filter(species %in% c("SNPL", "WIPL","KEPL")) %>%
  left_join(., tag_nest_data, by = c("ring", "tag_ID", "sex", "species", "code")) %>% 
  filter(timestamp_local <= end_date & timestamp_local >= nest_initiation_date) %>% 
  mutate(days_recorded = end_date - as.Date(str_sub(as.character(timestamp_local), 
                                                    start = 1, end = 10), 
                                            format = "%Y-%m-%d"))