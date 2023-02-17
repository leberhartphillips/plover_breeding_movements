# prepare R environment
source("R/project/project_libraries.R")
source("R/project/project_load_breeding_data.R")
source("R/project/project_load_movement_data.R")
source("R/wrangle/wrangle_tag_nest_brood_resight_data.R")

# summary of observed broods that have at least one of the parents tagged
tagged_broods_summary <-
  tag_breeding_data_ceuta$broods %>% 
  dplyr::group_by(family_ID, ring, code) %>% 
  dplyr::summarise(min_date = min(timestamp_brood, na.rm = TRUE),
            max_date = max(timestamp_brood, na.rm = TRUE)) %>% 
  dplyr::rename(ID = family_ID) %>% 
  dplyr::left_join(., ceuta_list$Nests %>% 
                     dplyr::select(ID, nest_initiation_date, end_date, fate), 
            by = "ID",
            multiple = "all") %>% 
  dplyr::mutate(post_hatch_days_with_tag_data = as.Date(max_date) - end_date) %>% 
  dplyr::rename(family_ID = ID) %>% 
  dplyr::select(family_ID, post_hatch_days_with_tag_data, ring, code, end_date) %>% 
  dplyr::left_join(., tag_breeding_data_ceuta$broods %>% 
                     dplyr::select(ring, family_ID, sex),
                   multiple = "all") %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(post_hatch_days_with_tag_data) %>% 
  dplyr::mutate(status = "brooding")

# summary of hatched nests that have at least one of the parents tagged but no
# known brooding information
tagged_hatched_nests_summary <- 
  tag_breeding_data_ceuta$nests %>% 
  dplyr::group_by(family_ID) %>% 
  dplyr::filter(fate == "Hatch") %>% 
  dplyr::mutate(post_hatch_days_with_tag_data = last_fix - end_date) %>% 
  dplyr::filter(post_hatch_days_with_tag_data > 0) %>% 
  dplyr::select(family_ID, post_hatch_days_with_tag_data, ring, code, sex, end_date) %>% 
  dplyr::filter(family_ID %!in% tagged_broods_summary$family_ID) %>% 
  dplyr::mutate(status = "potential")

# bind two data summaries together
tagged_brooding_summary <- 
  dplyr::bind_rows(tagged_broods_summary, tagged_hatched_nests_summary)

# subset tagging data to birds and time-periods of interest
tagged_brooding_subset <- 
  tag_breeding_data_ceuta$tagging %>% 
  dplyr::left_join(tagged_brooding_summary, ., 
                   by = c("ring", "sex"),
                   multiple = "all") %>% 
  dplyr::filter(end_date <= as.Date(timestamp_local) & (end_date + 30) >= as.Date(timestamp_local)) %>% 
  dplyr::ungroup()

#### inspect spatial data of tags and breeding information ----
tag_breeding_data_ceuta$tagging %>% 
  dplyr::group_by(ring, year(timestamp_local)) %>% 
  dplyr::summarise(fixes = dplyr::n()) %>% 
  dplyr::arrange(desc(fixes)) %>% 
  View()

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CA3340", map_year = 2022) #

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0161", map_year = 2018) #

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0937", map_year = 2022) #

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0423", map_year = 2022) #

# confirmed that the bird was foraging in I away from nest, 
# remarkable foraging dispersal south to a big bay by Playa las labradas
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0916", map_year = 2022) #

# migration north near Culiacan, resightings back in Ceuta in winter
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0155", map_year = 2019) #

# migration north near Culiacan and back to Ceuta in winter
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CA3314", map_year = 2019) #

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0930", map_year = 2022) #

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0609", map_year = 2022) #

# lots of good brood resightings
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0918", map_year = 2022) #

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CA3224", map_year = 2022) # 

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CM1858", map_year = 2022) #

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0306", map_year = 2019) #

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0312", map_year = 2019) #

# need to remove data from La Cruz
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0066", map_year = 2022) #

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0520", map_year = 2021) #

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0517", map_year = 2019) #

# full migration
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CA3315", map_year = 2018) #

# good brood resightings
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0138", map_year = 2019) #

# migration north to Culiacan and then south to Mazatlan
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CA2100", map_year = 2018) #

# two nests with good tagging coverage
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CA3224", map_year = 2019) #

# good brood resightings, desertion captured in tagging data
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0318", map_year = 2019) #

# desertion captured in tagging data
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0422", map_year = 2019) #

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0130", map_year = 2019) #

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CA2100", map_year = 2018) #

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0311", map_year = 2019) #

tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CA3315", map_year = 2019) #

# need to remove data from La Cruz
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0118", map_year = 2018)

# breeding pair both tagged with nesting and brooding
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CN0423", map_year = 2022)
tag_and_breeding_data_mapper(tag_and_breeding_data = tag_breeding_data_ceuta,
                             bird_ring = "CA3340", map_year = 2022)

# tagged_brooding_subset %>% 
#   ungroup() %>% 
#   select(ring, tag_ID) %>% 
#   distinct() %>% 
#   arrange(ring)
# 
# tagged_brooding_subset %>% 
#   filter(ring == "CA3340") %>% 
#   group_by(tag_ID) %>% 
#   summarise(n())

#### AKDE ----
CN0918_telemetry <- 
  df2move(df = arrange(filter(tagged_brooding_subset, ring == "CN0918"), timestamp_local), track_id = "ring", 
          proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
          x = "lon", y = "lat", time = "timestamp_local") %>% 
  as.telemetry()

# RX.RM|RX.WX

CN0318_telemetry <- 
  df2move(df = arrange(filter(tagged_brooding_subset, ring == "CN0318"), timestamp_local), track_id = "ring", 
          proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
          x = "lon", y = "lat", time = "timestamp_local") %>% 
  as.telemetry()

CN0138_telemetry <- 
  df2move(df = arrange(filter(tagged_brooding_subset, ring == "CN0138"), timestamp_local), track_id = "ring", 
          proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
          x = "lon", y = "lat", time = "timestamp_local") %>% 
  as.telemetry()

CN0138_sf <- 
  arrange(filter(tagged_brooding_subset, ring == "CN0138"), timestamp_local) %>% 
  st_as_sf(., 
           coords = c("lon", "lat"),
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

mapviewOptions(basemaps = c("Esri.WorldImagery"),
               # raster.palette = colorRampPalette(brewer.pal(9, "Greys")),
               # vector.palette = colorRampPalette(brewer.pal(9, "BrBG")[9:1]),
               # na.color = "magenta",
               layers.control.pos = "topright")

tagged_brooding_subset %>% 
  st_as_sf(., 
           coords = c("lon", "lat"),
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  filter(ring == "CN0318") %>% 
  mutate(timestamp_simp = as.character(timestamp_local)) %>% 
  mapview(zcol = "timestamp_simp", layer.name = "CN0318")

mapview(filter(plover_tagging_sf, ring == "CN0138") %>% mutate(), zcol = "timestamp_date", layer.name = "CN0138")

telemetry_object <- CN0138_telemetry
M.IID <- ctmm.fit(telemetry_object)
GUESS <- ctmm.guess(telemetry_object, interactive = FALSE)
M.OUF <- ctmm.fit(telemetry_object, GUESS)
KDE <- akde(telemetry_object, M.IID) # KDE
AKDE <- akde(telemetry_object, M.OUF) # AKDE
wAKDE <- akde(telemetry_object, M.OUF, weights = TRUE)
EXT <- extent(list(KDE, AKDE, wAKDE), level = 0.95)
plot(telemetry_object, UD = KDE, xlim = EXT$x, ylim = EXT$y)
title(expression("IID KDE"["C"]))
plot(telemetry_object, UD = AKDE, xlim = EXT$x, ylim = EXT$y)
title(expression("OUF AKDE"["C"]))
plot(telemetry_object, UD = wAKDE, xlim = EXT$x, ylim = EXT$y)
title(expression("weighted OUF AKDE"["C"]))
summary(KDE)
summary(wAKDE)

known_brooders_telemetry <- 
  df2move(df = arrange(filter(tagged_brooding_subset, status == "brooding"), timestamp_local), track_id = "ring", 
          proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
          x = "lon", y = "lat", time = "timestamp_local") %>% 
  as.telemetry()

SVF_all <- lapply(known_brooders_telemetry, variogram)
SVF_all <- mean(SVF_all)
plot(SVF_all, fraction = 0.35 , level = level)
title("Population variogram")


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

# other checks
ceuta_list$Nests %>% 
  filter(ID %in% c("2019_D_2", "2019_D_3"))

ceuta_list$Captures %>% 
  filter(ID %in% c("2019_D_2", "2019_D_3") & age == "A")

ceuta_list$Captures %>% 
  filter(code == "OX.RM|LX.BX" & year == "2019")

ceuta_list$Captures %>% 
  filter(ID %in% c("2019_D_2", "2019_D_3") & age == "J") %>% 
  select(ID, date, time, code, observer, comments) %>% 
  mutate(chick_codes = str_extract(code, "[BGROWLY]")) %>% 
  filter(!is.na(chick_codes)) %>% 
  arrange(ID, date)

ceuta_list$BirdRef %>% 
  filter(ID %in% c("2019_D_2", "2019_D_3"))

ceuta_list$Broods %>% 
  filter(ID %in% c("2019_D_2", "2019_D_3")) %>% 
  select(ID, date, time, male, chicks, chick_codes, observer, comments) %>% 
  mutate(chick_codes_simp = str_extract_all(chick_codes, "[BGROWLY]")) %>% 
  arrange(ID, date) %>% 
  select(ID, male) %>%
  distinct()

ceuta_list$Broods %>% 
  filter(ID == "SNPL_2022_C_4") %>% 
  mutate(chick_codes_simp = str_extract_all(chick_codes, "[BGROWLY]")) %>% 
  select(ID, date, male, female, chick_codes_simp) %>% 
  distinct() %>% 
  arrange(date)

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

# MX.RW|GX.GX

ceuta_list$Captures %>% 
  filter(ring == "CN0138")
# filter(code == "GX.MR|GX.BX")

# check that female CN0318 YX.RM|BX.GX has 21 days of brood care for 2019_D_14
# Looks good: 2 chicks, is indeed a female
ceuta_list$Nests %>%
  filter(ID == "2019_D_14") %>% 
  select(ID, easting, northing, nest_initiation_date, 
         end_date, fate, male, female, no_chicks, comments)

ceuta_list$Nests %>%
  filter(female == "YX.RM|BX.GX") %>% 
  select(ID, easting, northing, nest_initiation_date, 
         end_date, fate, male, female, no_chicks, comments)

ceuta_list$Broods %>%
  filter(ID == "2019_D_14") %>% 
  select(ID, easting, northing, date, time, distance, degree,
         male, female, chicks, comments, observer)

ceuta_list$Captures %>%
  filter(code == "YX.RM|BX.GX" | ring == "CN0318") %>% 
  select(ID, easting, northing, nest_initiation_date, 
         end_date, fate, male, female, no_chicks, comments)

CN0318_2019_D_14_brood_sf <- 
  tag_breeding_data_ceuta$broods %>% 
  filter(ring == "CN0318" & family_ID == "2019_D_14") %>% 
  st_as_sf(., 
           coords = c("brood_lon", "brood_lat"),
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  mutate(timestamp_simp = as.character(timestamp_brood))

CN0318_2019_D_14_tag_sf <- 
  tagged_brooding_subset %>% 
  filter(ring == "CN0318") %>% 
  st_as_sf(., 
           coords = c("lon", "lat"),
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  mutate(timestamp_simp = as.character(timestamp_local)) 

mapviewOptions(basemaps = c("Esri.WorldImagery"),
               # raster.palette = colorRampPalette(brewer.pal(9, "Greys")),
               # vector.palette = colorRampPalette(brewer.pal(9, "BrBG")[9:1]),
               # na.color = "magenta",
               layers.control.pos = "topright")

mapview(list(CN0318_2019_D_14_brood_sf, CN0318_2019_D_14_tag_sf),
        layer.name = (c("brood_sightings", "tag_data")),
        zcol = c("timestamp_simp", "timestamp_simp"))

lapply(tag_breeding_data_ceuta, function(x) 
  subset(x, ring %in% c("CN0318"))) 

tag_data = tag_breeding_data_ceuta$tagging
nest_data = tag_breeding_data_ceuta$nests
brood_data = tag_breeding_data_ceuta$broods
resight_data = tag_breeding_data_ceuta$resights
tag = TRUE
nest = TRUE
brood = TRUE
resight = TRUE
bird_ring = "CA3224"
map_year = 2018

tag_breeding_data_ceuta

tag_and_breeding_data_mapper <- 
  function(tag_data, nest_data, brood_data, resight_data, 
           # tag = FALSE, nest = FALSE, brood = FALSE, resight = FALSE,
           bird_ring, map_year = NULL, 
           breeding = TRUE){
    
    if(is.null(map_year)){
      map_year = tag_data %>% 
        filter(ring == bird_ring) %>% 
        pull(timestamp_local) %>% 
        min(.) %>% year()
    } else {map_year = map_year}

    
    if (nrow(tag_data) > nrow(brood_data)){
      pal <- 
        colorNumeric(
          palette = "Blues", 
          domain = tag_data %>% 
            filter(ring == bird_ring & 
                     year(timestamp_local) == map_year) %>% pull(timestamp_local))
    } else if (nrow(brood_data) > nrow(resight_data)){
      pal <- 
        colorNumeric(
          palette = "Blues", 
          domain = brood_data %>% 
            filter(ring == bird_ring & 
                     year(timestamp_brood) == map_year) %>% pull(timestamp_brood))
    } else if (nrow(resight_data) > nrow(nest_data)){
      pal <- 
        colorNumeric(
          palette = "Blues", 
          domain = resight_data %>% 
            filter(ring == bird_ring & 
                     year(timestamp_resight) == map_year) %>% pull(timestamp_resight))
    } else {
      pal <- 
        colorNumeric(
          palette = "Blues", 
          domain = nest_data %>% 
            filter(ring == bird_ring & 
                     year(nest_initiation_date) == map_year) %>% pull(nest_initiation_date) %>% 
            ymd_hms(paste(as.character(.), "00:00:00"), tz = "America/Mazatlan") %>% 
            subset(!is.na(.)))
    } 
    
    title = paste0("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">", 
                   tag_data %>% 
                     filter(ring == bird_ring & 
                              year(timestamp_local) == map_year) %>% pull(species) %>% unique(), ", ",
                   tag_data %>% 
                     filter(ring == bird_ring & 
                              year(timestamp_local) == map_year) %>% pull(sex) %>% unique(), ", ",
                   bird_ring, ", ", 
                   map_year, 
                   "</label>');
        }
    ")
    
    # if(tag == TRUE & nest = TRUE & brood = TRUE & resight = TRUE){
      leaflet() %>% 
        addProviderTiles('Esri.WorldImagery') %>% 
        # tag data
        addCircleMarkers(data = tag_data %>% 
                           filter(ring == bird_ring & 
                                    year(timestamp_local) == map_year), 
                         ~lon, ~lat,
                         radius = ~6,
                         color = "white",
                         fillColor = ~pal(timestamp_local),
                         stroke = FALSE, fillOpacity = 0.75,
                         popup = ~as.character(timestamp_local),
                         label = ~as.character(timestamp_local), 
                         group = "Tag locations") %>% 
        
        # nest data
        addCircleMarkers(data = nest_data %>% 
                           filter(ring == bird_ring & 
                                    year(nest_initiation_date) == map_year), 
                         ~lon, ~lat, 
                         radius = ~10,
                         color = "red",
                         fillColor = ~pal(ymd_hms(paste(as.character(nest_initiation_date), 
                                                        "00:00:00"), 
                                                  tz = "America/Mazatlan") %>% 
                                            subset(!is.na(.))),
                         stroke = TRUE, fillOpacity = 0.75,
                         popup = ~paste(sep = "<br/>", paste0("<br>", as.character(family_ID)),
                                        paste0("<br>lay date = ", as.character(nest_initiation_date)), 
                                         paste0("<br>end date = ", as.character(end_date)),
                                         paste0("<br>fate = ", as.character(fate))), 
                         group = "Nests") %>% 
        
        # brood data
        addCircleMarkers(data = brood_data %>% 
                           filter(ring == bird_ring & 
                                    year(timestamp_brood) == map_year), 
                         ~lon, ~lat, 
                         radius = ~10,
                         color = "black",
                         fillColor = ~pal(timestamp_brood),
                         stroke = TRUE, fillOpacity = 0.75,
                         popup = ~paste(sep = "<br/>", paste0("<br>", as.character(family_ID)),
                                        paste0("no. chicks = ", as.character(chicks))), 
                         group = "Brood observations") %>%
        
        # resight data
        addCircleMarkers(data = resight_data %>% 
                           filter(ring == bird_ring & 
                                    year(timestamp_resight) == map_year), 
                         ~lon, ~lat, 
                         radius = ~10,
                         color = "green",
                         fillColor = ~pal(timestamp_resight),
                         stroke = TRUE, fillOpacity = 0.75,
                         popup = ~paste0(as.character(timestamp_resight), 
                                         ", comments = ", as.character(comments)), 
                         group = "Resight observations") %>% 
        # Layers control
        addLayersControl(
          overlayGroups = c("Tag locations", "Nests", 
                            "Brood observations", "Resight observations"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        htmlwidgets::onRender(title) %>%
        addMeasure(primaryLengthUnit = "meters")
      }

tag_and_breeding_data_mapper(tag_data = tag_breeding_data_ceuta$tagging,
                             nest_data = tag_breeding_data_ceuta$nests,
                             brood_data = tag_breeding_data_ceuta$broods,
                             resight_data = tag_breeding_data_ceuta$resights,
                             bird_ring = "CA3224")
    
# pal_tag <- 
#   colorNumeric(
#     palette = "Blues", 
#     domain = CN0318_2019_D_14_tag_sf$timestamp_local)
# 
# pal_brood <- 
#   colorNumeric(
#     palette = "Blues", 
#     domain = CN0318_2019_D_14_tag_sf$timestamp_local)

leaflet() %>% 
  addProviderTiles('Esri.WorldImagery') %>% 
  addCircleMarkers(data = tagged_brooding_subset %>% filter(ring == "CN0318"), 
                   ~lon, ~lat,
                   radius = ~6,
                   color = "white",
                   fillColor = ~pal_tag(timestamp_local),
                   stroke = FALSE, fillOpacity = 0.75,
                   popup = ~as.character(timestamp_local),
                   label = ~as.character(timestamp_local), group = "Tag locations") %>%  
  addCircleMarkers(data = tag_breeding_data_ceuta$broods %>% 
                   filter(ring == "CN0318" & family_ID == "2019_D_14"), 
                   ~brood_lon, ~brood_lat, 
                   radius = ~10,
                   color = "black",
                   fillColor = ~pal_brood(timestamp_brood),
                   stroke = TRUE, fillOpacity = 0.75,
                   popup = ~paste0(as.character(timestamp_brood), 
                                   ", no. chicks = ", as.character(chicks)), 
                   label = ~paste0(as.character(timestamp_brood), 
                                   ", no. chicks = ", as.character(chicks)), 
                   group = "Brood observations") %>% 
  # Layers control
  addLayersControl(
    overlayGroups = c("Tag locations", "Brood observations"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  addMeasure(primaryLengthUnit = "meters")

  addLegend("topright", pal = pal_tag, values = ~timestamp_local)#,
            title = "Brood care",
            # labFormat = labelFormat(prefix = "$"),
            opacity = 1)

# check that female CN0517 BX.RM|RX.OX has 13 days of brood care for 2019_D_201
ceuta_list$Nests %>%
  filter(ID == "2019_D_201") %>% 
  select(ID, easting, northing, nest_initiation_date, 
         end_date, fate, male, female, no_chicks, comments)

ceuta_list$Nests %>%
  filter(female == "BX.RM|RX.OX") %>% 
  select(ID, easting, northing, nest_initiation_date, 
         end_date, fate, male, female, no_chicks, comments)

ceuta_list$Broods %>%
  filter(ID == "2019_D_201") %>% 
  select(ID, easting, northing, date, time, distance, degree,
         male, female, chicks, comments, observer)

ceuta_list$Captures %>%
  filter(ID == "2019_D_201")

tagged_brooding_subset %>% 
  st_as_sf(., 
           coords = c("lon", "lat"),
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  filter(ring == "CN0517") %>% 
  mutate(timestamp_simp = as.character(timestamp_local)) %>% 
  mapview(zcol = "timestamp_simp", layer.name = "CN0517")
  
  # filter(code == "BX.RM|RX.OX" | ring == "CN0517") %>% 
  # select(ID, easting, northing, nest_initiation_date, 
  #        end_date, fate, male, female, no_chicks, comments)

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
