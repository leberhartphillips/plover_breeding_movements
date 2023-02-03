import_plover_tag_spatial <- 
  function(data_loc, tag_ID, projection,
           time_zone = "America/Mazatlan",
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
        
        mutate(
          # make a fix number column
          fix_number = str_pad(row.names(.), 2, pad = "0"),
          
          # make a timestamp column
          timestamp = paste(paste(year, month, 
                                  day, sep = "-"), 
                            paste(hour, minute, 
                                  second, sep = ":"), sep = " ")) %>% 
        mutate(
          # convert to a useable POSIX time/date string
          timestamp = as.POSIXct(strptime(as.character(timestamp), 
                                          format = "%y-%m-%d %H:%M:%S"), 
                                 tz = time_zone),
          
          # assign name of tag
          tag_ID = tag_ID,
          
          # make a simplified version of the time sting
          timestamp_simple = as.Date(str_sub(as.character(timestamp), 
                                             start = 1, end = 10), 
                                     format = "%Y-%m-%d")) %>% 
        
        # make a GMT timestamp
        mutate(timestamp_gmt = with_tz(timestamp, "GMT")) %>% 
        
        # remove observations without a reliable location
        dplyr::filter(latitude != 0 & !is.na(latitude)) %>%
        
        # remove observations without a reliable location
        dplyr::filter(timestamp != 0 & !is.na(timestamp)) %>%
        
        # names() %>% 
        # as.data.frame()
        
        dplyr::select(tag_ID, timestamp_simple, fix_number, timestamp, timestamp_gmt,
                      satellites, latitude, longitude, elevation, battery) %>% 
        
        mutate(# assign bird ring
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
          
          mutate(
            # extract the temporal information from the string
            year = paste0("20", str_sub(string = RTC.date, start = 1, end = 2)),
            month = paste0(str_sub(string = RTC.date, start = 4, end = 5)),
            day = paste0(str_sub(string = RTC.date, start = 7, end = 8)),
            hour = paste0(str_sub(string = RTC.time, start = 1, end = 2)),
            minute = paste0(str_sub(string = RTC.time, start = 4, end = 5)),
            second = paste0(str_sub(string = RTC.time, start = 7, end = 8))
          ) %>% 
          
          mutate(
            # Merge the time and date columns together to formulate the time stamp for a given row
            timestamp = ISOdate(year = year, month = month, day = day, 
                                hour = hour, min = minute, sec = second, 
                                tz = time_zone),
            
            # Tag$datetime[nrow(Tag)] = Tag$datetime[nrow(Tag)] + 8 * 60 * 60,
            # 
            # Tag$hour[nrow(Tag)] = as.character(as.numeric(Tag$hour[nrow(Tag)]) + 8),
            # 
            # timestamp = paste0(year, "-", month, "-", day, " ", hour, ":", minute, ":", second)
            
            timestamp_simple = as.Date(str_sub(as.character(timestamp), 
                                               start = 1, end = 10), 
                                       format = "%Y-%m-%d"),          
            # make a fix number column
            fix_number = str_pad(row.names(.), 2, pad = "0"),
            
            # assign name of tag
            tag_ID = tag_ID
          ) %>% 
          
          # make a GMT timestamp
          mutate(timestamp_gmt = with_tz(timestamp, "GMT")) %>% 
          
          # remove observations without a reliable location
          dplyr::select(tag_ID, timestamp_simple, fix_number, timestamp, timestamp_gmt,
                        Sats, Latitude, Longitude, Altitude.m.) %>% 
          
          `colnames<-` (tolower(names(.))) %>% 
          
          rename(satellites = sats,
                 tag_ID = tag_id,
                 elevation = altitude.m.) %>% 
          
          mutate(battery = NA,
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
          
          # remove observations without a reliable location
          filter(latitude != 0 | !is.na(latitude)) %>% 
          
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
          slice(-(n_slice/n_slice):(n_slice * -1)) %>%
          
          mutate(
            # extract the temporal information from the string
            year = paste0("20", str_sub(string = RTC.date, start = 1, end = 2)),
            month = paste0(str_sub(string = RTC.date, start = 4, end = 5)),
            day = paste0(str_sub(string = RTC.date, start = 7, end = 8)),
            hour = paste0(str_sub(string = RTC.time, start = 1, end = 2)),
            minute = paste0(str_sub(string = RTC.time, start = 4, end = 5)),
            second = paste0(str_sub(string = RTC.time, start = 7, end = 8))
          ) %>% 
          
          mutate(
            # Merge the time and date columns together to formulate the time stamp for a given row
            timestamp = ISOdate(year = year, month = month, day = day, 
                                hour = hour, min = minute, sec = second, 
                                tz = time_zone),
            
            # Tag$datetime[nrow(Tag)] = Tag$datetime[nrow(Tag)] + 8 * 60 * 60,
            # 
            # Tag$hour[nrow(Tag)] = as.character(as.numeric(Tag$hour[nrow(Tag)]) + 8),
            # 
            # timestamp = paste0(year, "-", month, "-", day, " ", hour, ":", minute, ":", second)
            
            timestamp_simple = as.Date(str_sub(as.character(timestamp), 
                                               start = 1, end = 10), 
                                       format = "%Y-%m-%d"),          
            # make a fix number column
            fix_number = str_pad(row.names(.), 2, pad = "0"),
            
            # assign name of tag
            tag_ID = tag_ID
          ) %>% 
          
          # make a GMT timestamp
          mutate(timestamp_gmt = with_tz(timestamp, "GMT")) %>% 
          
          # remove observations without a reliable location
          dplyr::select(tag_ID, timestamp_simple, fix_number, timestamp, timestamp_gmt,
                        Sats, Latitude, Longitude, Altitude.m.) %>% 
          
          `colnames<-` (tolower(names(.))) %>% 
          
          rename(satellites = sats,
                 tag_ID = tag_id,
                 elevation = altitude.m.) %>% 
          
          mutate(battery = NA,
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
          
          # remove observations without a reliable location
          filter(latitude != 0 | !is.na(latitude)) %>% 
          
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