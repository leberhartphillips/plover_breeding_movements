# prepare R environment
source("R/project/project_libraries.R")
source("R/project/project_load_movement_data.R")
source("R/project/project_load_breeding_data.R")

# PP51075 seems to stay away from the nest during the deployment
# PP51076a male is missing data from the early hours of the sampling day, but shows clear male circadian variation
# PP51064 female clear circadian pattern (but some daytime variation)
# PP51067b female very clear circadian pattern
# PP51069b male away from nest during early hours of the morning
# PP51073 female very clear circadian pattern
short_term_tags_SNPL <- c("PP51064", "PP51067b", "PP51069b", "PP51073", "PP51075", "PP51076a") 

# PP51065b male looks awesome, male is clearly away from nest during daylight and is on the nest during the night
# PP51065a female is close to nest during day and leaves at night, but daytime distance is remarkably varied
# PP51070b male has no temporal pattern in proximity of movements around nest
short_term_tags_WIPL <- c("PP51065b", "PP51065a", "PP51070b") 

# determine the sampling distribution across the day
plover_tagging_sf %>% 
  filter(str_detect(tag_ID, "NF") & species == "KEPL") %>%
  # filter(year(timestamp_local) == 2020) %>% 
  filter(month(timestamp_local) %in% c(4, 5, 6, 7)) %>% 
  mutate(rounded_hour = round(timestamp_local, "hours") %>% 
           format(., format = "%H:%M:%S") %>% hms::as_hms(.)) %>%
  # arrange(rounded_hour)
  # mutate(time_hms = hms::as_hms(format(timestamp_local, format = "%H:%M:%S"))) %>% 
  # str()
  ggplot() +
  geom_histogram(aes(x = rounded_hour), stat = "count") +
  facet_grid(sex ~ species)
