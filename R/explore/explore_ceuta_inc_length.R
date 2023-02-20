# find average incubation length from CeutaCLOSED nests

ceuta_list$Nests %>% 
  dplyr::mutate(female = ifelse(ID == "SNPL_2022_C_402", "OX.RM|OX.LX", female),
                fate = ifelse(fate == "Hatched", "Hatch", fate)) %>% 
  dplyr::select("ID", "nest_initiation_date",  "end_date", "fate", 
                "float1", "float2", "float3") %>% 
  filter(str_detect(fate, "atch")) %>%
  filter(str_detect(fate, "[Uu]n", negate = TRUE)) %>% 
  filter(float1 != "F" & float2 != "F" & float3 != "F") %>% 
  mutate(inc_days = end_date - nest_initiation_date) %>%
  summarise(mean(inc_days, na.rm = TRUE),
            min(inc_days, na.rm = TRUE), 
            max(inc_days, na.rm = TRUE),
            median(inc_days, na.rm = TRUE))
