# R script to import, wrangle, analyze, and visualize sex-specific diel 
# movements of Common Ringed Plovers and Kentish Plovers breeding at the 
# Beltringharder Koog, Germany

# Data collected by Dominic Cimiotti (Dominic.Cimiotti@NABU.de)
# Script written by Luke Eberhart-Hertel (luke.eberhart@bi.mpg.de)

# March 10, 2023

#### R environment prep ----
packages_required_in_project <- 
  c("lubridate", "tidyverse", "sf", "readxl", "hms", "mgcv", "geosphere", 
    "scales", "suncalc", "gratia", "ggthemes")

# of the required packages, check if some need to be installed
new.packages <- 
  packages_required_in_project[!(packages_required_in_project %in% 
                                   installed.packages()[,"Package"])]

# install all packages that are not locally available
if(length(new.packages)) install.packages(new.packages)

# load all the packages into the current R session
lapply(packages_required_in_project, require, character.only = TRUE)

#### functions ----
# function that reads PinPoint or Nanofix data into R and formats
import_plover_tag_spatial <- 
  function(data_loc, tag_ID, projection, collect_time_zone, local_time_zone,
           tag_model, bird_ID, bird_sex, bird_code, 
           species, population, n_slice){
    
    if(tag_model == "nanoFix-mini")
    {
      # read data file and extract data
      Tag <- 
        read.table(sep = ",", skip = 5, file = data_loc) %>% 
        
        # assign column names
        `colnames<-` (c("day", "month", "year", "hour", "minute", "second", "unknown_col", 
                        "satellites", "latitude", "longitude", "elevation", 
                        "clock_offset", "accuracy_indicator", "battery")) %>% 
        
        dplyr::mutate(
          # make a fix number column
          fix_number = stringr::str_pad(row.names(.), 2, pad = "0"),
          
          # make a timestamp column
          timestamp = paste(paste(year, month, 
                                  day, sep = "-"), 
                            paste(hour, minute, 
                                  second, sep = ":"), sep = " ")) %>% 
        dplyr::mutate(
          # convert to a useable POSIX time/date string
          timestamp_collect = 
            ymd_hms(as.character(timestamp), tz = collect_time_zone),
          
          # assign name of tag
          tag_ID = tag_ID,
          
          # make a simplified version of the time sting
          timestamp_date = as.Date(paste0("20", year, "-", month, "-", day), 
                                   format = "%Y-%m-%d")) %>% 
        
        # make a GMT timestamp
        dplyr::mutate(timestamp_utc = with_tz(timestamp_collect, "UTC")) %>% 
        
        # remove observations without a reliable location
        dplyr::filter(latitude != 0 & !is.na(latitude)) %>%
        
        # remove observations without a reliable location
        dplyr::filter(timestamp != 0 & !is.na(timestamp)) %>%
        
        # names() %>% 
        # as.data.frame()
        
        dplyr::select(tag_ID, timestamp_date, fix_number, timestamp_collect, 
                      timestamp_utc, satellites, latitude, longitude, 
                      elevation, battery) %>% 
        
        dplyr::mutate(# assign bird ring
          ring = bird_ID,
          
          # assign sex of bird
          sex = bird_sex, 
          
          # assign bird color combo
          code = bird_code,
          
          # assign bird species
          species = species,
          
          # assign bird population
          population = population,
          
          satellites = as.character(satellites)) %>% 
        
        # make a local timestamp and force all temporal cols to character
        dplyr::mutate(timestamp_local = as.character(with_tz(timestamp_utc, local_time_zone)),
                      timestamp_collect = as.character(timestamp_collect),
                      timestamp_date = as.character(timestamp_date),
                      timestamp_utc = as.character(timestamp_utc)) %>% 
        
        # make the dataframe a simple feature, define coordinates and projection
        st_as_sf(., 
                 coords = c("longitude", "latitude"),
                 crs = projection)
      
      
      # output result
      Tag
    }
    else if(tag_model == "PinPoint-10"){
      if(n_slice == 0){
        # read data file and extract data
        Tag <- 
          read.table(sep = "", na.strings = "", file = data_loc, fill = TRUE,
                     stringsAsFactors = FALSE, header = TRUE) %>% 
          
          dplyr::mutate(
            # extract the temporal information from the string
            year = paste0("20", stringr::str_sub(string = RTC.date, start = 1, end = 2)),
            month = paste0(stringr::str_sub(string = RTC.date, start = 4, end = 5)),
            day = paste0(stringr::str_sub(string = RTC.date, start = 7, end = 8)),
            hour = paste0(stringr::str_sub(string = RTC.time, start = 1, end = 2)),
            minute = paste0(stringr::str_sub(string = RTC.time, start = 4, end = 5)),
            second = paste0(stringr::str_sub(string = RTC.time, start = 7, end = 8))
          ) %>% 
        
        dplyr::mutate(
          # make a fix number column
          fix_number = stringr::str_pad(row.names(.), 2, pad = "0"),
          
          # make a timestamp column
          timestamp = paste(paste(year, month, 
                                  day, sep = "-"), 
                            paste(hour, minute, 
                                  second, sep = ":"), sep = " ")) %>% 
          dplyr::mutate(
            # convert to a useable POSIX time/date string
            timestamp_collect = 
              ymd_hms(as.character(timestamp), tz = collect_time_zone),
            
            # assign name of tag
            tag_ID = tag_ID,
            
            # make a simplified version of the time sting
            timestamp_date = as.Date(paste0(year, "-", month, "-", day), 
                                     format = "%Y-%m-%d")) %>% 
          
          # make a GMT timestamp
          dplyr::mutate(timestamp_utc = with_tz(timestamp_collect, "UTC")) %>% 
          
          # remove observations without a reliable location
          dplyr::select(tag_ID, timestamp_date, fix_number, timestamp_collect, timestamp_utc,
                        Sats, Latitude, Longitude, Altitude.m.) %>% 
          
          `colnames<-` (tolower(names(.))) %>% 
          
          dplyr::rename(satellites = sats,
                        tag_ID = tag_id,
                        elevation = altitude.m.) %>% 
          
          dplyr::mutate(battery = NA,
                        # assign bird ring
                        ring = bird_ID,
                        
                        # assign sex of bird
                        sex = bird_sex, 
                        
                        # assign bird color combo
                        code = bird_code,
                        
                        # assign bird species
                        species = species,
                        
                        # assign bird population
                        population = population,
                        
                        satellites = as.character(satellites)) %>% 
          
          # make a local timestamp and force all temporal cols to character
          dplyr::mutate(timestamp_local = as.character(with_tz(timestamp_utc, local_time_zone)),
                        timestamp_collect = as.character(timestamp_collect),
                        timestamp_date = as.character(timestamp_date),
                        timestamp_utc = as.character(timestamp_utc)) %>% 
          
          # remove observations without a reliable location
          dplyr::filter(latitude != 0 | !is.na(latitude)) %>% 
          
          # make the dataframe a simple feature, define coordinates and projection
          st_as_sf(., 
                   coords = c("longitude", "latitude"),
                   crs = projection)
        
        # output result
        Tag
      }
      else{
        # read data file and extract data
        Tag <- 
          read.table(sep = "", na.strings = "", file = data_loc, fill = TRUE,
                     stringsAsFactors = FALSE, header = TRUE) %>% 
          
          # remove first observation (calibration fix)
          dplyr::slice(-(n_slice/n_slice):(n_slice * -1)) %>%
          
          dplyr::mutate(
            # extract the temporal information from the string
            year = paste0("20", stringr::str_sub(string = RTC.date, start = 1, end = 2)),
            month = paste0(stringr::str_sub(string = RTC.date, start = 4, end = 5)),
            day = paste0(stringr::str_sub(string = RTC.date, start = 7, end = 8)),
            hour = paste0(stringr::str_sub(string = RTC.time, start = 1, end = 2)),
            minute = paste0(stringr::str_sub(string = RTC.time, start = 4, end = 5)),
            second = paste0(stringr::str_sub(string = RTC.time, start = 7, end = 8))
          ) %>% 
          
          dplyr::mutate(
            # make a fix number column
            fix_number = stringr::str_pad(row.names(.), 2, pad = "0"),
            
            # make a timestamp column
            timestamp = paste(paste(year, month, 
                                    day, sep = "-"), 
                              paste(hour, minute, 
                                    second, sep = ":"), sep = " ")) %>% 
          dplyr::mutate(
            # convert to a useable POSIX time/date string
            timestamp_collect = 
              ymd_hms(as.character(timestamp), tz = collect_time_zone),
            
            # assign name of tag
            tag_ID = tag_ID,
            
            # make a simplified version of the time sting
            timestamp_date = as.Date(paste0(year, "-", month, "-", day), 
                                     format = "%Y-%m-%d")) %>% 
          
          # make a GMT timestamp
          dplyr::mutate(timestamp_utc = with_tz(timestamp_collect, "UTC")) %>% 
          
          # remove observations without a reliable location
          dplyr::select(tag_ID, timestamp_date, fix_number, timestamp_collect, timestamp_utc,
                        Sats, Latitude, Longitude, Altitude.m.) %>% 
          
          `colnames<-` (tolower(names(.))) %>% 
          
          dplyr::rename(satellites = sats,
                        tag_ID = tag_id,
                        elevation = altitude.m.) %>% 
          
          dplyr::mutate(battery = NA,
                        # assign bird ring
                        ring = bird_ID,
                        
                        # assign sex of bird
                        sex = bird_sex, 
                        
                        # assign bird color combo
                        code = bird_code,
                        
                        # assign bird species
                        species = species,
                        
                        # assign bird population
                        population = population,
                        
                        satellites = as.character(satellites)) %>% 
          
          # make a local timestamp and force all temporal cols to character
          dplyr::mutate(timestamp_local = as.character(with_tz(timestamp_utc, local_time_zone)),
                        timestamp_collect = as.character(timestamp_collect),
                        timestamp_date = as.character(timestamp_date),
                        timestamp_utc = as.character(timestamp_utc)) %>% 
          
          # remove observations without a reliable location
          dplyr::filter(latitude != 0 | !is.na(latitude)) %>% 
          
          # make the dataframe a simple feature, define coordinates and projection
          st_as_sf(., 
                   coords = c("longitude", "latitude"),
                   crs = projection)
        
        # output result
        Tag
      }
    }
    
    else{
      print("Unknown tag_model. Options are 'PinPoint-10' or 'nanoFix-mini'.")
    }
  }

##### tag data import and wrangle ----
# KEPL husum
PP50635_husum_T009761 <- 
  import_plover_tag_spatial(data_loc = "data/KEPL/PinPoint 50635 KP m my_nry 2021-06-30 21-44-30.txt",
                            tag_ID = "PP50635_T009761", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "UTC", tag_model = "PinPoint-10", n_slice = 0, local_time_zone = "Europe/Berlin",
                            bird_ID = "T009761", bird_code = "MY-NRY", bird_sex = "M", species = "KEPL", population = "husum")

PP50640_husum_T009763 <- 
  import_plover_tag_spatial(data_loc = "data/KEPL/PinPoint 50640 KP f my_pry 2021-06-30 20-49-54.txt",
                            tag_ID = "PP50640_T009763", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "UTC", tag_model = "PinPoint-10", n_slice = 0, local_time_zone = "Europe/Berlin",
                            bird_ID = "T009763", bird_code = "MY-PRY", bird_sex = "F", species = "KEPL", population = "husum")

PP50639_husum_T009764 <- 
  import_plover_tag_spatial(data_loc = "data/KEPL/PinPoint 50639 KP f my_gry 2021-06-30 18-53-20.txt",
                            tag_ID = "PP50639_T009764", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "UTC", tag_model = "PinPoint-10", n_slice = 0, local_time_zone = "Europe/Berlin",
                            bird_ID = "T009764", bird_code = "MY-GRY", bird_sex = "F", species = "KEPL", population = "husum")

PP50641_husum_T009787 <- 
  import_plover_tag_spatial(data_loc = "data/KEPL/PinPoint 50641 KP f mr_gny 2022-07-09 11-55-38.txt",
                            tag_ID = "PP50641_T009787", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "UTC", tag_model = "PinPoint-10", n_slice = 0, local_time_zone = "Europe/Berlin",
                            bird_ID = "T009787", bird_code = "MR-GNY", bird_sex = "F", species = "KEPL", population = "husum")

PP51262_husum_T009785 <- 
  import_plover_tag_spatial(data_loc = "data/KEPL/PinPoint 51262 KP f mr_ggy 2022-07-01 22-40-44.txt",
                            tag_ID = "PP51262_T009785", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "UTC", tag_model = "PinPoint-10", n_slice = 0, local_time_zone = "Europe/Berlin",
                            bird_ID = "T009785", bird_code = "MR-GGY", bird_sex = "F", species = "KEPL", population = "husum")

PP50641_husum_T009784 <- 
  import_plover_tag_spatial(data_loc = "data/KEPL/PinPoint 50641 KP m mp_ryy 2022-06-10 07-50-49.txt",
                            tag_ID = "PP50641_T009784", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "UTC", tag_model = "PinPoint-10", n_slice = 0, local_time_zone = "Europe/Berlin",
                            bird_ID = "T009784", bird_code = "MP-RYY", bird_sex = "M", species = "KEPL", population = "husum")

PP50641_husum_T009778 <- 
  import_plover_tag_spatial(data_loc = "data/KEPL/PinPoint 50641 KP m mp_rpy 2022-06-01 22-37-48.txt",
                            tag_ID = "PP50641_T009778", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "UTC", tag_model = "PinPoint-10", n_slice = 0, local_time_zone = "Europe/Berlin",
                            bird_ID = "T009778", bird_code = "MP-RPY", bird_sex = "M", species = "KEPL", population = "husum")

PP50635_husum_T009779 <- 
  import_plover_tag_spatial(data_loc = "data/KEPL/PinPoint 50635 KP m mp_rry 2022-06-01 18-09-31.txt",
                            tag_ID = "PP50635_T009779", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "UTC", tag_model = "PinPoint-10", n_slice = 0, local_time_zone = "Europe/Berlin",
                            bird_ID = "T009779", bird_code = "MP-RRY", bird_sex = "M", species = "KEPL", population = "husum")

PP51007_husum_T009777 <- 
  import_plover_tag_spatial(data_loc = "data/KEPL/PinPoint 51007 KP f mp_rny 2022-05-31 12-49-05.txt",
                            tag_ID = "PP51007_T009777", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "UTC", tag_model = "PinPoint-10", n_slice = 0, local_time_zone = "Europe/Berlin",
                            bird_ID = "T009777", bird_code = "MP-RNY", bird_sex = "F", species = "KEPL", population = "husum")

PP51007_husum_T009773 <- 
  import_plover_tag_spatial(data_loc = "data/KEPL/PinPoint 51007 KP m mp_rgy 2022-05-24 05-59-41.txt",
                            tag_ID = "PP51007_T009773", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "UTC", tag_model = "PinPoint-10", n_slice = 0, local_time_zone = "Europe/Berlin",
                            bird_ID = "T009773", bird_code = "MP-RGY", bird_sex = "M", species = "KEPL", population = "husum")

PP51004_husum_T009770 <- 
  import_plover_tag_spatial(data_loc = "data/KEPL/PinPoint 51004 KP m mp_nry 2022-05-21 05-35-57.txt",
                            tag_ID = "PP51004_T009770", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "UTC", tag_model = "PinPoint-10", n_slice = 0, local_time_zone = "Europe/Berlin",
                            bird_ID = "T009770", bird_code = "MP-NRY", bird_sex = "M", species = "KEPL", population = "husum")

PP51009_husum_T009772 <- 
  import_plover_tag_spatial(data_loc = "data/KEPL/PinPoint 51009 KP f mp_pry 2022-05-20 12-54-16.txt",
                            tag_ID = "PP51009_T009772", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "UTC", tag_model = "PinPoint-10", n_slice = 0, local_time_zone = "Europe/Berlin",
                            bird_ID = "T009772", bird_code = "MP-PRY", bird_sex = "F", species = "KEPL", population = "husum")

PP50637_husum_T009768 <- 
  import_plover_tag_spatial(data_loc = "data/KEPL/PinPoint 50637 KP f mn_rny 2022-05-18 16-07-49.txt",
                            tag_ID = "PP50637_T009768", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "UTC", tag_model = "PinPoint-10", n_slice = 0, local_time_zone = "Europe/Berlin",
                            bird_ID = "T009768", bird_code = "MN-RNY", bird_sex = "F", species = "KEPL", population = "husum")

PP50634_husum_T009766 <- 
  import_plover_tag_spatial(data_loc = "data/KEPL/PinPoint 50634 KP f mn_pry 2022-05-16 13-23-43.txt",
                            tag_ID = "PP50634_T009766", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "UTC", tag_model = "PinPoint-10", n_slice = 0, local_time_zone = "Europe/Berlin",
                            bird_ID = "T009766", bird_code = "MN-PRY", bird_sex = "F", species = "KEPL", population = "husum")

PP50635_husum_T009769 <- 
  import_plover_tag_spatial(data_loc = "data/KEPL/PinPoint 50635 KP m mn_rpy 2022-05-15 15-34-13.txt",
                            tag_ID = "PP50634_T009769", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "UTC", tag_model = "PinPoint-10", n_slice = 0, local_time_zone = "Europe/Berlin",
                            bird_ID = "T009769", bird_code = "MN-RPY", bird_sex = "M", species = "KEPL", population = "husum")

# CRPL husum
PP50641_husum_T009083 <- 
  import_plover_tag_spatial(data_loc = "data/CRPL/PinPoint 50641 RP f gnn_grm 2022-07-03 11-04-01.txt",
                            tag_ID = "PP50641_T009083", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "UTC", tag_model = "PinPoint-10", n_slice = 0, local_time_zone = "Europe/Berlin",
                            bird_ID = "T009083", bird_code = "GNN-GRM", bird_sex = "F", species = "CRPL", population = "husum")

PP50641_husum_T009714 <- 
  import_plover_tag_spatial(data_loc = "data/CRPL/PinPoint 50641 RP f ggr_wnm 2022-06-06 09-08-30.txt",
                            tag_ID = "PP50641_T009714", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "UTC", tag_model = "PinPoint-10", n_slice = 0, local_time_zone = "Europe/Berlin",
                            bird_ID = "T009714", bird_code = "GGR-WNM", bird_sex = "F", species = "CRPL", population = "husum")

PP50641_husum_T009158 <- 
  import_plover_tag_spatial(data_loc = "data/CRPL/PinPoint 50641 RP f nrn_wwm 2022-05-10 14-05-21.txt",
                            tag_ID = "PP50641_T009158", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "UTC", tag_model = "PinPoint-10", n_slice = 0, local_time_zone = "Europe/Berlin",
                            bird_ID = "T009158", bird_code = "NRN-WWM", bird_sex = "F", species = "CRPL", population = "husum")

PP50641_husum_T009077 <- 
  import_plover_tag_spatial(data_loc = "data/CRPL/PinPoint 50641 RP m ggn_nwm 2021-06-30 15-45-27.txt",
                            tag_ID = "PP50641_T009077", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "UTC", tag_model = "PinPoint-10", n_slice = 0, local_time_zone = "Europe/Berlin",
                            bird_ID = "T009077", bird_code = "GGN-NWM", bird_sex = "M", species = "CRPL", population = "husum")

PP50641_husum_T009671 <- 
  import_plover_tag_spatial(data_loc = "data/CRPL/PinPoint 50641 RP m wgn_gnm 2022-05-24 05-43-10.txt",
                            tag_ID = "PP50641_T009671", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "UTC", tag_model = "PinPoint-10", n_slice = 0, local_time_zone = "Europe/Berlin",
                            bird_ID = "T009671", bird_code = "WGN-GNM", bird_sex = "M", species = "CRPL", population = "husum")

PP50637_husum_T009765 <- 
  import_plover_tag_spatial(data_loc = "data/CRPL/PinPoint 50637 RP f wwr_gnm 2021-06-30 17-26-50.txt",
                            tag_ID = "PP50637_T009765", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "UTC", tag_model = "PinPoint-10", n_slice = 0, local_time_zone = "Europe/Berlin",
                            bird_ID = "T009765", bird_code = "WWR-GNM", bird_sex = "F", species = "CRPL", population = "husum")

# bind all tagging data together
husum_tagging_df <- 
  dplyr::bind_rows(PP50637_husum_T009765, PP50641_husum_T009671, PP50641_husum_T009077,
            PP50641_husum_T009158, PP50641_husum_T009714, PP50641_husum_T009083,
            PP50635_husum_T009769, PP50634_husum_T009766, PP50637_husum_T009768,
            PP51009_husum_T009772, PP51004_husum_T009770, PP51007_husum_T009773,
            PP51007_husum_T009777, PP50635_husum_T009779, PP50641_husum_T009778,
            PP50641_husum_T009784, PP51262_husum_T009785, PP50641_husum_T009787,
            PP50639_husum_T009764, PP50640_husum_T009763, PP50635_husum_T009761) %>% 
  sfc_as_cols(., names = c("lon", "lat")) %>% 
  st_drop_geometry()

# extract sunrise, sunset, dawn, and dusk times for each fix
husum_tagging_df <- 
  husum_tagging_df %>% 
  mutate(date = as.Date(timestamp_local, tz = "Europe/Berlin")) %>%
  getSunlightTimes(data = ., keep = c("nightEnd", "night", 
                                      "sunrise", "sunset", 
                                      "dawn", "dusk", 
                                      "nauticalDawn", "nauticalDusk"), 
                   tz = "Europe/Berlin") %>% 
  rename(lat_fun = lat,
         lon_fun = lon) %>% 
  bind_cols(husum_tagging_df) %>% 
  mutate(timestamp_local = ymd_hms(timestamp_local, tz = "Europe/Berlin")) %>% 
  filter(lat_fun == lat | lon_fun == lon) %>% 
  dplyr::select(-c(lat_fun, lon_fun)) %>% 
  mutate(night_fix = ifelse(timestamp_local < nightEnd | timestamp_local > night, 1, 0),
         timestamp_local = as.character(timestamp_local),
         date = as.character(date),
         nightEnd = as.character(nightEnd),
         night = as.character(night),
         sunset = as.character(sunset),
         sunrise = as.character(sunrise),
         dawn = as.character(dawn),
         dusk = as.character(dusk),
         nauticalDawn = as.character(nauticalDawn),
         nauticalDusk = as.character(nauticalDusk))
  
#### nest data import and wrangle ----
# wrangle excel sheet of nest information for each tag into R
nest_data_husum <-
  read_xlsx(path = "data/KEPL/Short term deployments 2021 and 2022 basic data BHK for Luke.xlsx", col_types = "guess")  %>% 
  `colnames<-` (tolower(names(.))) %>% 
  rename(code = `bird id`,
         ring = `ring no`,
         tag_ID = `tag id`,
         family_ID = `nest id`,
         lat = nest_north,
         lon = nest_east) %>% 
  dplyr::select(species, code, ring, tag_ID, family_ID, lat, lon, 
                start_date, start_timeutc, end_date_real, end_time_real_utc) %>% 
  mutate(start_date = as.Date(start_date),
         start_timeutc = as_hms(start_timeutc),
         end_date_real = as.Date(as.numeric(end_date_real), origin = "1900-01-01"),
         end_time_real_utc = seconds_to_period(as.numeric(end_time_real_utc) * 24 * 60 * 60),
         species = ifelse(str_detect(species, "R"), "CRPL", "KEPL"),
         code = toupper(code)) %>%
  mutate(end_time_real_utc = paste(str_pad(end_time_real_utc@hour, width = 2, side = "left", pad = "0"), 
                                   str_pad(minute(end_time_real_utc), width = 2, side = "left", pad = "0"), sep = ":")) %>% 
  mutate(end_time_real_utc = ifelse(str_detect(end_time_real_utc, "NA"), NA, end_time_real_utc)) %>% 
  mutate(end_timestamp_utc = paste(as.character(end_date_real), paste(as.character(end_time_real_utc), "00", sep = ":"), sep = " "),
         start_timestamp_utc = paste(as.character(start_date), as.character(start_timeutc), sep = " ")) %>% 
  mutate(end_timestamp_utc = ifelse(str_detect(end_timestamp_utc, "NA"), NA, end_timestamp_utc),
         start_timestamp_utc = ifelse(str_detect(start_timestamp_utc, "NA"), NA, start_timestamp_utc)) %>% 
  mutate(end_timestamp_utc = ymd_hms(as.character(end_timestamp_utc), 
                                     tz = "Europe/Berlin"),
         start_timestamp_utc = ymd_hms(as.character(start_timestamp_utc), 
                                       tz = "Europe/Berlin")) %>% 
  dplyr::select(-c(start_date, start_timeutc, end_date_real, end_time_real_utc))

# consolidate nest data and tag data into a single list
tag_breeding_data_husum <- 
  list(nests = nest_data_husum,
       tagging = husum_tagging_df)

# join nest data with tag data and calculate the distance between the nest and each fix
husum_tag_nest_dist <-
  left_join(tag_breeding_data_husum$tagging,
          tag_breeding_data_husum$nests,
          by = c("ring", "species", "code")) %>% 
  rename(bird_lat = lat.x,
         bird_lon = lon.x,
         nest_lat = lat.y,
         nest_lon = lon.y) %>% 
  mutate(timestamp_local = ymd_hms(timestamp_local, tz = "Europe/Berlin"),
         dist_from_nest = distHaversine(p1 = matrix(c(bird_lon, bird_lat), ncol = 2),
                                        p2 = matrix(c(nest_lon, nest_lat), ncol = 2))) %>%
  mutate(rounded_hour = round(timestamp_local, "hours") %>%
           format(., format = "%H:%M:%S"),
         time_of_day = format(timestamp_local, format = "%H:%M:%S"),
         timestamp_local = as.character(timestamp_local)) %>% 
  filter(timestamp_utc >= start_timestamp_utc) %>% 
  mutate(sex = as.factor(sex),
         ring = as.factor(ring)) %>% 
  mutate(across(c("nightEnd", "night", 
                  "sunrise", "sunset", 
                  "dawn", "dusk", 
                  "nauticalDawn", "nauticalDusk"), ~ as.character(as_hms(ymd_hms(.x, tz = "Europe/Berlin"))))) %>% 
  dplyr::select(ring, sex, species, population, nightEnd, night, sunrise, 
                sunset, dawn, dusk, nauticalDawn, nauticalDusk, 
                dist_from_nest, time_of_day) %>% 
  mutate(seconds_of_day = lubridate::hms(time_of_day) %>% period_to_seconds(.)) %>% 
  mutate(across(c("nightEnd", "night", 
                  "sunrise", "sunset", 
                  "dawn", "dusk", 
                  "nauticalDawn", "nauticalDusk"), ~ period_to_seconds(lubridate::hms(.x))))

#### modelling ----
# run a generalized additive model with a circular spline to determine 
# sex-specific diel movements in relation to the nest
husum_dist_to_nest_mod <- 
  gam(log(dist_from_nest) ~ sex + species +
        s(seconds_of_day, by = interaction(sex, species), bs = "cc") + 
        s(ring, bs = 're'), 
      data = husum_tag_nest_dist)

# evaluate temporal autocorrelation
acf(resid(husum_dist_to_nest_mod), type ="p")

# evaluate raw model results
draw(husum_dist_to_nest_mod, parametric = FALSE)

# evaluate the spread of log-transformed distances (should be two peaks: one
# shows the cluster around the nest during incubation and the other shows a foraging
# patch on incubation recess
ggplot(data = husum_tag_nest_dist) +
  geom_histogram(aes(log(dist_from_nest)), bins = 15)

# make newdata for the predicted trend
newdata_seconds_sex <- 
  expand.grid(seconds_of_day = seq(1, 60*60*24),
              sex = as.factor(c("F", "M")),
              species = as.factor(c("KEPL", "CRPL")))

# estimate model predictions of the newdata
dist_to_nest_mod_fits <- 
  predict(husum_dist_to_nest_mod, 
          newdata = newdata_seconds_sex, 
          type = 'response', se = TRUE,
          newdata.guaranteed = TRUE, exclude = "s(ring)")

# summarise predictions and confidence levels
dist_to_nest_mod_predicts <-  
  data.frame(newdata_seconds_sex, dist_to_nest_mod_fits) %>% 
  mutate(lower = fit - 1.96 * se.fit,
         upper = fit + 1.96 * se.fit,
         time_of_day2 = as_hms(seconds_of_day)) %>% 
  mutate(fit_trans = exp(fit),
         lower_trans = exp(lower),
         upper_trans = exp(upper)) %>% 
  arrange(sex)

# specify species lables for the plot facets
species_pop_labels <- c(
  'KEPL' = "Kentish Plover\nBeltringharder Koog, Germany",
  'CRPL' = "Common Ringed Plover\nBeltringharder Koog, Germany"
)

# summarise celestial times for husum
husum_sunlighttimes <- 
  husum_tag_nest_dist %>% 
  summarise(mean_nightEnd = mean(nightEnd), 
            mean_night = mean(night), 
            mean_sunrise = mean(sunrise), 
            mean_sunset = mean(sunset), 
            mean_dawn = mean(dawn), 
            mean_dusk = mean(dusk), 
            mean_nauticalDawn = mean(nauticalDawn), 
            mean_nauticalDusk = mean(nauticalDusk))

#### plotting ----
# specify personal plotting theme
luke_theme <- 
  theme_bw() +
  theme(
    text = element_text(family = "Franklin Gothic Book"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.text.x  = element_text(size = 8), 
    axis.title.y = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(size = 0.25, lineend = "round", colour = "grey60"),
    axis.ticks.length = unit(0.1, "cm"),
    panel.border = element_rect(linetype = "solid", colour = "grey")
  )

# specify color palete for sexes
sex_pal <- 
  c(pull(ggthemes_data$wsj$palettes$colors6[3,2]), 
    pull(ggthemes_data$wsj$palettes$colors6[2,2]))

# draw linear plot of results, facetted by species
ggplot() +
  geom_rect(data = husum_sunlighttimes,
            aes(xmin = 0, xmax = hms::as_hms(mean_dawn), ymin = 0, ymax = Inf),
            fill = "grey50", alpha = 0.75) +
  geom_rect(data = husum_sunlighttimes,
            aes(xmin = hms::as_hms(mean_dawn), xmax = hms::as_hms(mean_sunrise), ymin = 0, ymax = Inf),
            fill = "grey60", alpha = 0.75) +
  geom_rect(data = husum_sunlighttimes,
            aes(xmin = hms::as_hms(mean_sunset), xmax = hms::as_hms(mean_dusk), ymin = 0, ymax = Inf),
            fill = "grey60", alpha = 0.75) +
  geom_rect(data = husum_sunlighttimes,
            aes(xmin = hms::as_hms(mean_dusk), xmax = Inf, ymin = 0, ymax = Inf),
            fill = "grey50", alpha = 0.75) +
  geom_ribbon(data = dist_to_nest_mod_predicts,
              aes(x = time_of_day2, ymin = lower_trans, ymax = upper_trans, fill = sex, color = sex),
              alpha = 0.8) +
  geom_line(data = dist_to_nest_mod_predicts,
            aes(x = time_of_day2, y = fit_trans, color = sex)) +
  geom_jitter(data = husum_tag_nest_dist,
              aes(x = as_hms(time_of_day),
                  y = dist_from_nest + 0.00001, fill = sex),
              position = position_jitter(width = 60*20, seed = 12356),
              alpha = 0.5,
              shape = 21, color = "white", size = 3) +
  scale_x_time(labels = label_time(format = "%H"), 
               name = "hour of day", 
               expand = c(-0.01, 0.0),
               breaks = as_hms(c('01:00:00', '06:00:00', '12:00:00', '18:00:00', '23:00:00'))) +
  scale_y_continuous(trans = 'log10', ) +
  ylab("distance from nest (log10, m)") +
  facet_grid(species ~ ., labeller = labeller(.rows = species_pop_labels)) +
  luke_theme +
  theme(legend.position = c(0.5, 0.95),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        strip.background = element_blank()) +
  scale_colour_manual(values = c("white", "white"), 
                      labels = c("Female", "Male")) +
  scale_fill_manual(values = sex_pal, 
                    labels = c("Female", "Male"))
