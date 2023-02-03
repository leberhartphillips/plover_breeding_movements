# prepare R environment
source("R/project_libraries.R")

# load movement data function to read tagging data
import_plover_tag_spatial_fun <- 
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

##### data import ----
NF21050_tagus <- 
  import_plover_tag_spatial_fun(data_loc = "data/KEPL/Tag21050/Obs160721_062557_Tag21050.pos",
                                tag_ID = "NF21050", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "Europe/Lisbon", tag_model = "nanoFix-mini", n_slice = 0,
                                bird_ID = "D50218", bird_code = "NA", bird_sex = "F", species = "KEPL", population = "tagus")

NF55584a_tagus <- 
  import_plover_tag_spatial_fun(data_loc = "data/KEPL/Tag55584/Obs040621_133158_Tag55584.pos",
                                tag_ID = "NF55584a", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "Europe/Lisbon", tag_model = "nanoFix-mini", n_slice = 0,
                                bird_ID = "D35644", bird_code = "NA", bird_sex = "M", species = "KEPL", population = "tagus")

NF55584b_tagus <- 
  import_plover_tag_spatial_fun(data_loc = "data/KEPL/Tag55584_2/Obs130722_222151_Tag55584.pos",
                                tag_ID = "NF55584b", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "Europe/Lisbon", tag_model = "nanoFix-mini", n_slice = 0,
                                bird_ID = "D59946", bird_code = "NA", bird_sex = "M", species = "KEPL", population = "tagus")

NF21200_tagus <- 
  import_plover_tag_spatial_fun(data_loc = "data/KEPL/Tag21200/Obs060721_152942_Tag21200.pos",
                                tag_ID = "NF21200", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "Europe/Lisbon", tag_model = "nanoFix-mini", n_slice = 0,
                                bird_ID = "D59933", bird_code = "NA", bird_sex = "F", species = "KEPL", population = "tagus")

NF55719_tagus <- 
  import_plover_tag_spatial_fun(data_loc = "data/KEPL/Tag55719/Obs050721_232954_Tag55719.pos",
                                tag_ID = "NF55719", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "Europe/Lisbon", tag_model = "nanoFix-mini", n_slice = 0,
                                bird_ID = "P01903", bird_code = "NA", bird_sex = "M", species = "KEPL", population = "tagus")

NF55808_tagus <- 
  import_plover_tag_spatial_fun(data_loc = "data/KEPL/Tag55808/Obs300522_210831_Tag55808.pos",
                                tag_ID = "NF55808", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "Europe/Lisbon", tag_model = "nanoFix-mini", n_slice = 0,
                                bird_ID = "D59182", bird_code = "NA", bird_sex = "F", species = "KEPL", population = "tagus")

NF55831_tagus <- 
  import_plover_tag_spatial_fun(data_loc = "data/KEPL/Tag55831/Obs300522_213604_Tag55831.pos",
                                tag_ID = "NF55808", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "Europe/Lisbon", tag_model = "nanoFix-mini", n_slice = 0,
                                bird_ID = "D59932", bird_code = "NA", bird_sex = "F", species = "KEPL", population = "tagus")

NF55833_tagus <- 
  import_plover_tag_spatial_fun(data_loc = "data/KEPL/Tag55833/Obs130722_152954_Tag55833.pos",
                                tag_ID = "NF55833", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "Europe/Lisbon", tag_model = "nanoFix-mini", n_slice = 0,
                                bird_ID = "P01902", bird_code = "NA", bird_sex = "F", species = "KEPL", population = "tagus")

# SNPL nanofix ----
NF20805_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/pathtrack/Tag20805/Obs010719_110510_Tag20805.pos",
                                tag_ID = "NF20805", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0,
                                bird_ID = "CN0311", bird_code = "YX.RM|RX.YX", bird_sex = "M", species = "SNPL", population = "ceuta")

NF20849_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/pathtrack/Tag20849/Obs080619_103237_Tag20849.pos",
                                tag_ID = "NF20849", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0,
                                bird_ID = "CA3224", bird_code = "MX.RB|LX.OX", bird_sex = "F", species = "SNPL", population = "ceuta")

NF20996_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/pathtrack/Tag20996/Obs150520_132636_Tag20996.pos",
                                tag_ID = "NF20996", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0,
                                bird_ID = "CN0306", bird_code = "YX.RM|OX.GX", bird_sex = "F", species = "SNPL", population = "ceuta")

NF21065_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/pathtrack/Tag21065/Obs040520_150543_Tag21065.pos",
                                tag_ID = "NF55650", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0,
                                bird_ID = "CA3314", bird_code = "GX.RM|BX.WX", bird_sex = "M", species = "SNPL", population = "ceuta")

NF21094_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/pathtrack/Tag21094/Obs120619_091545_Tag21094.pos",
                                tag_ID = "NF21094", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0,
                                bird_ID = "CN0318", bird_code = "YX.RM|BX.GX", bird_sex = "F", species = "SNPL", population = "ceuta")

NF55650_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/pathtrack/Tag55650/Obs251022_150625_Tag55650.pos",
                            tag_ID = "NF55650", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0,
                            bird_ID = "CN0520", bird_code = "BX.RM|RX.RX", bird_sex = "F", species = "SNPL", population = "ceuta")

NF21117_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/pathtrack/Tag21117/Obs250820_104847_Tag21117.pos",
                                tag_ID = "NF21117", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0,
                                bird_ID = "CN0517", bird_code = "BX.RM|RX.OX", bird_sex = "F", species = "SNPL", population = "ceuta")

NF21263_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/pathtrack/Tag21263/Obs190619_111532_Tag21263.pos",
                                tag_ID = "NF21263", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0,
                                bird_ID = "CN0422", bird_code = "OX.RM|GX.GX", bird_sex = "F", species = "SNPL", population = "ceuta")

NF21166_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/pathtrack/Tag21166/Obs250820_105128_Tag21166.pos",
                                tag_ID = "NF21166", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0,
                                bird_ID = "CN0312", bird_code = "YX.RM|BX.BX", bird_sex = "F", species = "SNPL", population = "ceuta")

NF21176_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/pathtrack/Tag21176/Obs010719_205116_Tag21176.pos",
                                tag_ID = "NF21176", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0,
                                bird_ID = "CN0138", bird_code = "MX.RW|GX.GX", bird_sex = "M", species = "SNPL", population = "ceuta")

NF21261_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/pathtrack/Tag21261/Obs020619_093140_Tag21261.pos",
                            tag_ID = "NF21261", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0,
                            bird_ID = "CN0130", bird_code = "WX.RM|RX.RX", bird_sex = "F", species = "SNPL", population = "ceuta")

NF21163_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/pathtrack/Tag21163/Obs250820_104449_Tag21163.pos",
                            tag_ID = "NF21163", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0,
                            bird_ID = "CA3315", bird_code = "WX.RM|YX.WX", bird_sex = "F", species = "SNPL", population = "ceuta")

## SNPL PinPoint ----
PP48669_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/lotek/PinPoint_Tag_48669/Swift GPS Data Files/PinPoint 48669 2018-06-27 11-26-37_female_A001.txt",
                                tag_ID = "PP48669", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "America/Mazatlan", tag_model = "PinPoint-10", n_slice = 0,
                                bird_ID = "CN0161", bird_code = "MX.RW|YX.LX", bird_sex = "F", species = "SNPL", population = "ceuta")

PP48670_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/lotek/PinPoint_Tag_48670/PinPoint 48670 2018-05-24 09-07-12.txt",
                            tag_ID = "PP48670", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            time_zone = "America/Mazatlan", tag_model = "PinPoint-10", n_slice = 0,
                            bird_ID = "CN0118", bird_code = "MX.RW|LX.RX", bird_sex = "F", species = "SNPL", population = "ceuta")

PP48671_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/lotek/PinPoint_Tag_48671/Swift GPS Data Files/PinPoint 48671 2019-05-09 12-07-57.txt",
                            tag_ID = "PP48671", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            time_zone = "America/Mazatlan", tag_model = "PinPoint-10", n_slice = 0,
                            bird_ID = "CA3315", bird_code = "GX.MR|GX.BX", bird_sex = "M", species = "SNPL", population = "ceuta")

PP48672_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/lotek/PinPoint_Tag_48672/Swift GPS Data FIles/PinPoint 48672 2019-06-02 23-57-11.txt",
                            tag_ID = "PP48672", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            time_zone = "America/Mazatlan", tag_model = "PinPoint-10", n_slice = 0,
                            bird_ID = "CA2100", bird_code = "MX.OX|BX.YX", bird_sex = "M", species = "SNPL", population = "ceuta")

PP51060_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/lotek/PinPoint_Tag_51060 /Swift GPS Data Files/PinPoint 51060 2022-06-10 20-56-10_NestC402_Male_CA3340.txt",
                                tag_ID = "PP51060", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1,
                                bird_ID = "CA3340", bird_code = "GX.RM|WX.GX", bird_sex = "M", species = "SNPL", population = "ceuta")

PP51063_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/lotek/PinPoint_Tag_51063/PinPoint 51063 2022-06-02 20-33-23_NestD104MaleCN0918.txt",
                                tag_ID = "PP51063", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1,
                                bird_ID = "CN0918", bird_code = "UNK", bird_sex = "M", species = "SNPL", population = "ceuta")

PP51064_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/lotek/PinPoint_Tag_51064/Swift GPS Data Files/PinPoint 51064 2022-04-22 21-18-02_NestC1_FemaleCA3224.txt",
                            tag_ID = "PP51064", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 2,
                            bird_ID = "CA3224", bird_code = "MX.RB|LX.OX", bird_sex = "F", species = "SNPL", population = "ceuta")

PP51067a_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/lotek/PinPoint_Tag_51067a/Swift GPS Data Files/PinPoint 51067 2022-05-26 22-52-04_NestD205_FemaleCN0930.txt",
                                tag_ID = "PP51067a", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1,
                                bird_ID = "CN0930", bird_code = "UNK", bird_sex = "F", species = "SNPL", population = "ceuta")

PP51067b_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/lotek/PinPoint_Tag_51067b/Swift GPS Data Files/PinPoint 51067 2022-06-14 19-16-51_NestD212_FemaleCM1858.txt",
                                tag_ID = "PP51067b", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1,
                                bird_ID = "CM1858", bird_code = "KX.RM|WX.OX", bird_sex = "F", species = "SNPL", population = "ceuta")

PP51069a_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/lotek/PinPoint_Tag_51069a/PinPoint 51069 2022-06-02 19-42-58_NestD103FemaleCN0937.txt",
                                tag_ID = "PP51069a", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1,
                                bird_ID = "CN0937", bird_code = "UNK", bird_sex = "F", species = "SNPL", population = "ceuta")

PP51069b_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/lotek/PinPoint_Tag_51069b/Swift GPS Data Files/PinPoint 51069 2022-05-04 01-45-14_NestC301_MaleCN0066.txt",
                            tag_ID = "PP51069b", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1,
                            bird_ID = "CN0066", bird_code = "GX.RM|YX.OX", bird_sex = "M", species = "SNPL", population = "ceuta")

PP51070a_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/lotek/PinPoint_Tag_51070a/Swift GPS Data Files/PinPoint 51070 2022-06-14 19-19-55_NestC402_FemaleCN0423.txt",
                                tag_ID = "PP51070a", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1,
                                bird_ID = "CM0423", bird_code = "OX.RM|OX.LX", bird_sex = "F", species = "SNPL", population = "ceuta")

PP51073_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/lotek/PinPoint_Tag_51073/Swift GPS Data Files/PinPoint 51073 2022-05-06 22-39-50_NestC301_FemaleCN0937.txt",
                            tag_ID = "PP51073", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1,
                            bird_ID = "CN0937", bird_code = "GX.RM|YX.OX", bird_sex = "F", species = "SNPL", population = "ceuta")

PP51075_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/lotek/PinPoint_Tag_51075/Swift GPS Data Files/PinPoint 51075 2022-06-14 19-30-21_Nestd211_FemaleCN0916.txt",
                                tag_ID = "PP51075", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1,
                                bird_ID = "CN0916", bird_code = "BX.RM|OX.YX", bird_sex = "F", species = "SNPL", population = "ceuta")

PP51076a_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/lotek/PinPoint_Tag_51076a/Swift GPS Data Files/PinPoint 51076 2022-04-22 21-34-26_NestC2_MaleCA3440.txt",
                                tag_ID = "PP51076a", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1,
                                bird_ID = "CA3340", bird_code = "GX.RM|WX.GX", bird_sex = "M", species = "SNPL", population = "ceuta")

PP51076b_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/SNPL/lotek/PinPoint_Tag_51076b/Swift GPS Data Files/PinPoint 51076 2022-06-14 19-26-22_NestC403_FemaleCN0609.txt",
                            tag_ID = "PP51076b", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1,
                            bird_ID = "CN0609", bird_code = "GX.RM|OX.RX", bird_sex = "F", species = "SNPL", population = "ceuta")

# WIPL Pinpoint ----
PP51070b_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/WIPL/lotek/PinPoint_Tag_51070b/PinPoint 51070 2022-07-02 05-29-16_NestW401_Male_CV0253.txt",
                            tag_ID = "PP51070b", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1,
                            bird_ID = "CV0253", bird_code = "YX.RM|BX.YX", bird_sex = "M", species = "WIPL", population = "ceuta")

PP51065a_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/WIPL/lotek/PinPoint_Tag_51065a/Swift GPS Data Files/PinPoint 51065 2022-05-04 02-02-18_NestWD1FemaleCV0195.txt",
                            tag_ID = "PP51065a", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1,
                            bird_ID = "CV0195", bird_code = "UNK", bird_sex = "F", species = "WIPL", population = "ceuta")

PP51065b_ceuta <- 
  import_plover_tag_spatial_fun(data_loc = "data/WIPL/lotek/PinPoint_Tag_51065b/Swift GPS Data Files/PinPoint 51065 2022-05-09 06-00-18_NestWD2_MaleCV0266.txt",
                                tag_ID = "PP51065b", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1,
                                bird_ID = "CV0266", bird_code = "GX.RM|YX.OX", bird_sex = "M", species = "WIPL", population = "ceuta")

# BADO ----
NFTag55843_nz <- 
  import_plover_tag_spatial_fun(data_loc = "data/BADO/Tag55843/Obs161122_192740_Tag55843.pos",
                                tag_ID = "NF55843", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0,
                                bird_ID = "CP9042", bird_code = "FWX.RR|MX.YY", bird_sex = "F", species = "BADO", population = "kaikoura")

NFTag20865_nz <- 
  import_plover_tag_spatial_fun(data_loc = "data/BADO/Tag20865/Obs261022_231825_Tag20865.pos",
                                tag_ID = "NF20865", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0,
                                bird_ID = "CP9055", bird_code = "FWX.RR|MX.LR", bird_sex = "M", species = "BADO", population = "kaikoura")

NFTag21146_nz <- 
  import_plover_tag_spatial_fun(data_loc = "data/BADO/Tag21146/Obs261022_234604_Tag21146.pos",
                                tag_ID = "NF21146", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0,
                                bird_ID = "CP9098", bird_code = "XX.RW|MX.LB", bird_sex = "F", species = "BADO", population = "kaikoura")

NFTag55687_nz <- 
  import_plover_tag_spatial_fun(data_loc = "data/BADO/Tag55687/Obs261022_232953_Tag55687.pos",
                                tag_ID = "NF55687", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0,
                                bird_ID = "CP9032", bird_code = "XX.RB|MX.BR", bird_sex = "M", species = "BADO", population = "kaikoura")

NFTag55660_nz <- 
  import_plover_tag_spatial_fun(data_loc = "data/BADO/Tag55660/Obs031222_180904_Tag55660.pos",
                                tag_ID = "NF55660", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0,
                                bird_ID = "CP9066", bird_code = "XX.RB|MX.BY", bird_sex = "F", species = "BADO", population = "kaikoura")

NFTag55795_nz <- 
  import_plover_tag_spatial_fun(data_loc = "data/BADO/Tag55795/Obs261022_233831_Tag55795.pos",
                                tag_ID = "NF55795", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0,
                                bird_ID = "CP9096", bird_code = "XX.RW|MX.BY", bird_sex = "F", species = "BADO", population = "kaikoura")

# merge ----
plover_tagging_data <- 
  bind_rows(as.data.frame(NF21163_ceuta), as.data.frame(NF21065_ceuta), 
            as.data.frame(NF20996_ceuta), as.data.frame(NF21117_ceuta), 
            as.data.frame(NF21166_ceuta), as.data.frame(NF21176_ceuta),
            as.data.frame(NF21094_ceuta), as.data.frame(NF20849_ceuta), 
            as.data.frame(NF21261_ceuta), as.data.frame(NF20805_ceuta), 
            as.data.frame(NF55650_ceuta), as.data.frame(NF21263_ceuta),
            as.data.frame(PP51070b_ceuta), as.data.frame(PP51065a_ceuta),
            as.data.frame(PP51065b_ceuta), as.data.frame(PP51076a_ceuta),
            as.data.frame(PP51076b_ceuta), as.data.frame(PP51075_ceuta),
            as.data.frame(PP51073_ceuta), as.data.frame(PP51070a_ceuta),
            as.data.frame(PP51069b_ceuta), as.data.frame(PP51069a_ceuta),
            as.data.frame(PP51067b_ceuta), as.data.frame(PP51067a_ceuta),
            as.data.frame(PP51064_ceuta), as.data.frame(PP51063_ceuta),
            as.data.frame(PP51060_ceuta), as.data.frame(PP48672_ceuta),
            as.data.frame(PP48671_ceuta), as.data.frame(PP48670_ceuta),
            as.data.frame(PP48669_ceuta), as.data.frame(NF55833_tagus),
            as.data.frame(NF55831_tagus), as.data.frame(NF55808_tagus),
            as.data.frame(NF55719_tagus), as.data.frame(NF21200_tagus),
            as.data.frame(NF55584b_tagus), as.data.frame(NF55584a_tagus),
            as.data.frame(NF21050_tagus), as.data.frame(NFTag55843_nz),
            as.data.frame(NFTag20865_nz), as.data.frame(NFTag21146_nz),
            as.data.frame(NFTag55687_nz), as.data.frame(NFTag55660_nz),
            as.data.frame(NFTag55795_nz))

plover_tagging_sf <- 
  rbind(NF21163_ceuta, NF21065_ceuta, NF20996_ceuta, NF21117_ceuta, 
            NF21166_ceuta, NF21176_ceuta,
            NF21094_ceuta, NF20849_ceuta, 
            NF21261_ceuta, NF20805_ceuta, 
            NF55650_ceuta, NF21263_ceuta,
            PP51070b_ceuta, PP51065a_ceuta,
            PP51065b_ceuta, PP51076a_ceuta,
            PP51076b_ceuta, PP51075_ceuta,
            PP51073_ceuta, PP51070a_ceuta,
            PP51069b_ceuta, PP51069a_ceuta,
            PP51067b_ceuta, PP51067a_ceuta,
            PP51064_ceuta, PP51063_ceuta,
            PP51060_ceuta, PP48672_ceuta,
            PP48671_ceuta, PP48670_ceuta,
            PP48669_ceuta, NF55833_tagus,
            NF55831_tagus, NF55808_tagus,
            NF55719_tagus, NF21200_tagus,
            NF55584b_tagus, NF55584a_tagus,
            NF21050_tagus, NFTag55843_nz,
            NFTag20865_nz, NFTag21146_nz,
            NFTag55687_nz, NFTag55660_nz,
            NFTag55795_nz)
