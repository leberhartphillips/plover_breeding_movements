# prepare R environment
source("R/project/project_libraries.R")
source("R/project/project_load_movement_data.R")
source("R/project/project_load_breeding_data.R")

deploy_info <- 
  plover_tagging_df %>% 
  group_by(species, ring, tag_ID, code, sex) %>% 
  summarise(first_fix = min(timestamp_simple)) %>% 
  arrange(ring, first_fix)

rings_tags <- 
  plover_tagging_df %>% 
  filter(species == "SNPL") %>%
  select(ring, tag_ID) %>%
  distinct()
  # filter(ring == "CN0520")

ceuta_list$Captures %>% 
  filter(ring == "CN0423") %>%
  # filter(code == "OX.RM|OX.LX")
  arrange(date)

ceuta_list$Captures %>% 
  filter(str_detect(comments, "55650"))
  # filter(lubridate::year(date) == "2021")#date == as.Date("2021-05-16", format = "%Y-%m-%d"))

tagging_data <- 
  ceuta_list$Captures %>% 
  mutate(sex = ifelse(ring == "CA3340" & ID == "2019_D_203", "M", sex)) %>% 
  mutate(code = ifelse(ring == "CA3314", "GX.RM|BX.WX", code)) %>% 
  mutate(code = ifelse(ring == "CA3315", "GX.MR|GX.BX", code)) %>% 
  dplyr::select(ring, ID, code, date) %>% 
  left_join(deploy_info,., by = c("ring")) %>% 
  # filter(ring == "CN0161") %>%
  filter(code.x == code.y & species == "SNPL") %>% 
  select(-code.y) %>% 
  rename(code = code.x) %>% 
  mutate(time_diff = first_fix - date) %>% 
  filter(time_diff > -1) %>%
  # filter(time_diff > -100 & time_diff < 100) %>%
  arrange(ring, time_diff) %>% 
  group_by(ring, tag_ID) %>% 
  # filter(ring == "CN0161")
  slice(1) %>% 
  # arrange(time_diff)
  arrange(desc(time_diff))
  
ceuta_list$Nests %>% 
  dplyr::select("ID", "female", "male", "easting", "northing", 
                "nest_initiation_date",  "end_date", 
                "last_observation_alive", "fate") %>% 
  pivot_longer(cols = c("female", "male"), names_to = "sex", values_to = "code") %>% 
  left_join(dplyr::select(ceuta_list$Captures, ID, ring, code, date), ., by = c("code", "ID")) %>% 
  distinct() %>% 
  rename(cap_date = date) %>% 
  left_join(deploy_info, ., by = "ring") %>% 
  mutate(time_diff = nest_initiation_date - first_fix) %>%
  arrange(abs(time_diff)) %>% 
  group_by(ring) %>% 
  slice(1)

# tagus_nest_data <- 
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
         nest_intiation_date = "LAY DATE",
         fate = "FATE") %>% 
    mutate(nest_intiation_date = as.Date(nest_intiation_date),
           fate = tolower(ifelse(str_detect(fate, "hatch"), "Hatch", fate)))
  