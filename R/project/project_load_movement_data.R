# prepare R environment
source("R/project/project_libraries.R")

# load functions
function.sources = list.files(path = "R/functions/", 
                              pattern = "*\\().R$", full.names = TRUE, 
                              ignore.case = TRUE)
sapply(function.sources, source, .GlobalEnv)

##### data import ----
NF21050_tagus <- 
  import_plover_tag_spatial(data_loc = "data/KEPL/Tag21050/Obs160721_062557_Tag21050.pos",
                                tag_ID = "NF21050", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "UTC", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "Europe/Lisbon",
                                bird_ID = "D50218", bird_code = "BYYMP", bird_sex = "F", species = "KEPL", population = "tagus")

NF55584a_tagus <- 
  import_plover_tag_spatial(data_loc = "data/KEPL/Tag55584/Obs040621_133158_Tag55584.pos",
                                tag_ID = "NF55584a", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "UTC", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "Europe/Lisbon",
                                bird_ID = "D35644", bird_code = "NA", bird_sex = "M", species = "KEPL", population = "tagus")

NF55584b_tagus <- 
  import_plover_tag_spatial(data_loc = "data/KEPL/Tag55584_2/Obs130722_222151_Tag55584.pos",
                                tag_ID = "NF55584b", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "UTC", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "Europe/Lisbon",
                                bird_ID = "D59946", bird_code = "WWYOM", bird_sex = "M", species = "KEPL", population = "tagus")

NF21200_tagus <- 
  import_plover_tag_spatial(data_loc = "data/KEPL/Tag21200/Obs060721_152942_Tag21200.pos",
                                tag_ID = "NF21200", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "UTC", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "Europe/Lisbon",
                                bird_ID = "D59933", bird_code = "OGYOM", bird_sex = "F", species = "KEPL", population = "tagus")

NF55719_tagus <- 
  import_plover_tag_spatial(data_loc = "data/KEPL/Tag55719/Obs050721_232954_Tag55719.pos",
                                tag_ID = "NF55719", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "UTC", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "Europe/Lisbon",
                                bird_ID = "P01903", bird_code = "ROYOM", bird_sex = "M", species = "KEPL", population = "tagus")

NF55808_tagus <- 
  import_plover_tag_spatial(data_loc = "data/KEPL/Tag55808/Obs300522_210831_Tag55808.pos",
                                tag_ID = "NF55808", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "UTC", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "Europe/Lisbon",
                                bird_ID = "D59182", bird_code = "OWYPM", bird_sex = "F", species = "KEPL", population = "tagus")

NF55831_tagus <- 
  import_plover_tag_spatial(data_loc = "data/KEPL/Tag55831/Obs300522_213604_Tag55831.pos",
                                tag_ID = "NF55808", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "UTC", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "Europe/Lisbon",
                                bird_ID = "D59932", bird_code = "RWYOM", bird_sex = "M", species = "KEPL", population = "tagus")

NF55833_tagus <- 
  import_plover_tag_spatial(data_loc = "data/KEPL/Tag55833/Obs130722_152954_Tag55833.pos",
                                tag_ID = "NF55833", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "UTC", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "Europe/Lisbon",
                                bird_ID = "P01902", bird_code = "RRYOM", bird_sex = "F", species = "KEPL", population = "tagus")

# SNPL nanofix ----
NF20805_ceuta <- 
  import_plover_tag_spatial(data_loc = "data/SNPL/pathtrack/Tag20805/Obs010719_110510_Tag20805.pos",
                                tag_ID = "NF20805", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "America/Mazatlan",
                                bird_ID = "CN0311", bird_code = "YX.RM|RX.YX", bird_sex = "M", species = "SNPL", population = "ceuta")

NF20849_ceuta <- 
  import_plover_tag_spatial(data_loc = "data/SNPL/pathtrack/Tag20849/Obs080619_103237_Tag20849.pos",
                                tag_ID = "NF20849", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "America/Mazatlan",
                                bird_ID = "CA3224", bird_code = "MX.RB|LX.OX", bird_sex = "F", species = "SNPL", population = "ceuta")

NF20996_ceuta <- 
  import_plover_tag_spatial(data_loc = "data/SNPL/pathtrack/Tag20996/Obs150520_132636_Tag20996.pos",
                                tag_ID = "NF20996", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "America/Mazatlan",
                                bird_ID = "CN0306", bird_code = "YX.RM|OX.GX", bird_sex = "F", species = "SNPL", population = "ceuta")

NF21065_ceuta <- 
  import_plover_tag_spatial(data_loc = "data/SNPL/pathtrack/Tag21065/Obs040520_150543_Tag21065.pos",
                                tag_ID = "NF55650", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "America/Mazatlan",
                                bird_ID = "CA3314", bird_code = "GX.RM|BX.WX", bird_sex = "M", species = "SNPL", population = "ceuta")

NF21094_ceuta <- 
  import_plover_tag_spatial(data_loc = "data/SNPL/pathtrack/Tag21094/Obs120619_091545_Tag21094.pos",
                                tag_ID = "NF21094", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "America/Mazatlan",
                                bird_ID = "CN0318", bird_code = "YX.RM|BX.GX", bird_sex = "F", species = "SNPL", population = "ceuta")

NF55650_ceuta <- 
  import_plover_tag_spatial(data_loc = "data/SNPL/pathtrack/Tag55650/Obs251022_150625_Tag55650.pos",
                            tag_ID = "NF55650", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "America/Mazatlan", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "America/Mazatlan",
                            bird_ID = "CN0520", bird_code = "BX.RM|RX.RX", bird_sex = "F", species = "SNPL", population = "ceuta")

NF21117_ceuta <- 
  import_plover_tag_spatial(data_loc = "data/SNPL/pathtrack/Tag21117/Obs250820_104847_Tag21117.pos",
                                tag_ID = "NF21117", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "America/Mazatlan",
                                bird_ID = "CN0517", bird_code = "BX.RM|RX.OX", bird_sex = "F", species = "SNPL", population = "ceuta")

NF21263_ceuta <- 
  import_plover_tag_spatial(data_loc = "data/SNPL/pathtrack/Tag21263/Obs190619_111532_Tag21263.pos",
                                tag_ID = "NF21263", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "America/Mazatlan",
                                bird_ID = "CN0422", bird_code = "OX.RM|GX.GX", bird_sex = "F", species = "SNPL", population = "ceuta")

NF21166_ceuta <- 
  import_plover_tag_spatial(data_loc = "data/SNPL/pathtrack/Tag21166/Obs250820_105128_Tag21166.pos",
                                tag_ID = "NF21166", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "America/Mazatlan",
                                bird_ID = "CN0312", bird_code = "YX.RM|BX.BX", bird_sex = "F", species = "SNPL", population = "ceuta")

NF21176_ceuta <- 
  import_plover_tag_spatial(data_loc = "data/SNPL/pathtrack/Tag21176/Obs010719_205116_Tag21176.pos",
                                tag_ID = "NF21176", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "America/Mazatlan",
                                bird_ID = "CN0138", bird_code = "MX.RW|GX.GX", bird_sex = "M", species = "SNPL", population = "ceuta")

NF21261_ceuta <- 
  import_plover_tag_spatial(data_loc = "data/SNPL/pathtrack/Tag21261/Obs020619_093140_Tag21261.pos",
                            tag_ID = "NF21261", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "America/Mazatlan",
                            bird_ID = "CN0130", bird_code = "WX.RM|RX.RX", bird_sex = "F", species = "SNPL", population = "ceuta")

NF21163_ceuta <- 
  import_plover_tag_spatial(data_loc = "data/SNPL/pathtrack/Tag21163/Obs250820_104449_Tag21163.pos",
                            tag_ID = "NF21163", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "America/Mazatlan",
                            bird_ID = "CN0155", bird_code = "WX.RM|YX.WX", bird_sex = "F", species = "SNPL", population = "ceuta")

## SNPL PinPoint ----
PP48669_ceuta <- # every 9-hours
  import_plover_tag_spatial(data_loc = "data/SNPL/lotek/PinPoint_Tag_48669/Swift GPS Data Files/PinPoint 48669 2018-06-27 11-26-37_female_A001.txt",
                                tag_ID = "PP48669", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "America/Mazatlan", tag_model = "PinPoint-10", n_slice = 0, local_time_zone = "America/Mazatlan",
                                bird_ID = "CN0161", bird_code = "MX.RW|YX.LX", bird_sex = "F", species = "SNPL", population = "ceuta")

PP48670_ceuta <- # once-per-day (0000)
  import_plover_tag_spatial(data_loc = "data/SNPL/lotek/PinPoint_Tag_48670/PinPoint 48670 2018-05-24 09-07-12.txt",
                            tag_ID = "PP48670", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "America/Mazatlan", tag_model = "PinPoint-10", n_slice = 0, local_time_zone = "America/Mazatlan",
                            bird_ID = "CN0118", bird_code = "MX.RW|LX.RX", bird_sex = "F", species = "SNPL", population = "ceuta") %>% 
  filter(ymd_hms(timestamp_local, tz = "America/Mazatlan") < ymd_hms("2018-05-24 08:00:00", tz = "America/Mazatlan")) %>%
  arrange(desc(timestamp_local))

PP48671_ceuta <- # no nesting data
  import_plover_tag_spatial(data_loc = "data/SNPL/lotek/PinPoint_Tag_48671/Swift GPS Data Files/PinPoint 48671 2019-05-09 12-07-57.txt",
                            tag_ID = "PP48671", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "America/Mazatlan", tag_model = "PinPoint-10", n_slice = 0, local_time_zone = "America/Mazatlan",
                            bird_ID = "CA3315", bird_code = "GX.MR|GX.BX", bird_sex = "M", species = "SNPL", population = "ceuta")

PP48672_ceuta <- # once-per-day (0000)
  import_plover_tag_spatial(data_loc = "data/SNPL/lotek/PinPoint_Tag_48672/Swift GPS Data FIles/PinPoint 48672 2019-06-02 23-57-11.txt",
                            tag_ID = "PP48672", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "America/Mazatlan", tag_model = "PinPoint-10", n_slice = 0, local_time_zone = "America/Mazatlan",
                            bird_ID = "CA2100", bird_code = "MX.OX|BX.YX", bird_sex = "M", species = "SNPL", population = "ceuta")

PP51060_ceuta <- # mid-term (1000 and 2200)
  import_plover_tag_spatial(data_loc = "data/SNPL/lotek/PinPoint_Tag_51060 /Swift GPS Data Files/PinPoint 51060 2022-06-10 20-56-10_NestC402_Male_CA3340.txt",
                                tag_ID = "PP51060", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1, local_time_zone = "America/Mazatlan",
                                bird_ID = "CA3340", bird_code = "GX.RM|WX.GX", bird_sex = "M", species = "SNPL", population = "ceuta")

PP51063_ceuta <- # mid-term (1000 and 2200)
  import_plover_tag_spatial(data_loc = "data/SNPL/lotek/PinPoint_Tag_51063/PinPoint 51063 2022-06-02 20-33-23_NestD104MaleCN0918.txt",
                                tag_ID = "PP51063", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1, local_time_zone = "America/Mazatlan",
                                bird_ID = "CN0918", bird_code = "RX.RM|RX.WX", bird_sex = "M", species = "SNPL", population = "ceuta")

PP51064_ceuta <- # short-term (every 20 mins)
  import_plover_tag_spatial(data_loc = "data/SNPL/lotek/PinPoint_Tag_51064/Swift GPS Data Files/PinPoint 51064 2022-04-22 21-18-02_NestC1_FemaleCA3224.txt",
                            tag_ID = "PP51064", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 2, local_time_zone = "America/Mazatlan",
                            bird_ID = "CA3224", bird_code = "MX.RB|LX.OX", bird_sex = "F", species = "SNPL", population = "ceuta")

PP51067a_ceuta <- # mid-term (1000 and 2200)
  import_plover_tag_spatial(data_loc = "data/SNPL/lotek/PinPoint_Tag_51067a/Swift GPS Data Files/PinPoint 51067 2022-05-26 22-52-04_NestD205_FemaleCN0930.txt",
                                tag_ID = "PP51067a", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1, local_time_zone = "America/Mazatlan",
                                bird_ID = "CN0930", bird_code = "OX.RM|WX.GX", bird_sex = "F", species = "SNPL", population = "ceuta")

PP51067b_ceuta <- # short-term (every 20 mins)
  import_plover_tag_spatial(data_loc = "data/SNPL/lotek/PinPoint_Tag_51067b/Swift GPS Data Files/PinPoint 51067 2022-06-14 19-16-51_NestD212_FemaleCM1858.txt",
                                tag_ID = "PP51067b", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1, local_time_zone = "America/Mazatlan",
                                bird_ID = "CM1858", bird_code = "KX.RM|WX.OX", bird_sex = "F", species = "SNPL", population = "ceuta")

PP51069a_ceuta <- # mid-term (1000 and 2200)
  import_plover_tag_spatial(data_loc = "data/SNPL/lotek/PinPoint_Tag_51069a/PinPoint 51069 2022-06-02 19-42-58_NestD103FemaleCN0937.txt",
                                tag_ID = "PP51069a", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1, local_time_zone = "America/Mazatlan",
                                bird_ID = "CN0937", bird_code = "RX.RM|GX.WX", bird_sex = "F", species = "SNPL", population = "ceuta")

PP51069b_ceuta <- # short-term (every 20 mins)
  import_plover_tag_spatial(data_loc = "data/SNPL/lotek/PinPoint_Tag_51069b/Swift GPS Data Files/PinPoint 51069 2022-05-04 01-45-14_NestC301_MaleCN0066.txt",
                            tag_ID = "PP51069b", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1, local_time_zone = "America/Mazatlan",
                            bird_ID = "CN0066", bird_code = "GX.RM|YX.OX", bird_sex = "M", species = "SNPL", population = "ceuta") %>% 
  filter(ymd_hms(timestamp_local, tz = "America/Mazatlan") < ymd_hms("2022-05-02 20:50:00", tz = "America/Mazatlan")) %>%
  arrange(desc(timestamp_local))

PP51070a_ceuta <- # mid-term (1000 and 2200)
  import_plover_tag_spatial(data_loc = "data/SNPL/lotek/PinPoint_Tag_51070a/Swift GPS Data Files/PinPoint 51070 2022-06-14 19-19-55_NestC402_FemaleCN0423.txt",
                                tag_ID = "PP51070a", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1, local_time_zone = "America/Mazatlan",
                                bird_ID = "CN0423", bird_code = "OX.RM|OX.LX", bird_sex = "F", species = "SNPL", population = "ceuta")

PP51073_ceuta <- # short-term (every 20 mins)
  import_plover_tag_spatial(data_loc = "data/SNPL/lotek/PinPoint_Tag_51073/Swift GPS Data Files/PinPoint 51073 2022-05-06 22-39-50_NestC301_FemaleCN0937.txt",
                            tag_ID = "PP51073", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1, local_time_zone = "America/Mazatlan",
                            bird_ID = "CN0937", bird_code = "RX.RM|GX.WX", bird_sex = "F", species = "SNPL", population = "ceuta")

PP51075_ceuta <- # short-term (every 20 mins)
  import_plover_tag_spatial(data_loc = "data/SNPL/lotek/PinPoint_Tag_51075/Swift GPS Data Files/PinPoint 51075 2022-06-14 19-30-21_Nestd211_FemaleCN0916.txt",
                                tag_ID = "PP51075", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1, local_time_zone = "America/Mazatlan",
                                bird_ID = "CN0916", bird_code = "BX.RM|OX.YX", bird_sex = "F", species = "SNPL", population = "ceuta")

PP51076a_ceuta <- # short-term (every 20 mins)
  import_plover_tag_spatial(data_loc = "data/SNPL/lotek/PinPoint_Tag_51076a/Swift GPS Data Files/PinPoint 51076 2022-04-22 21-34-26_NestC2_MaleCA3440.txt",
                                tag_ID = "PP51076a", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1, local_time_zone = "America/Mazatlan",
                                bird_ID = "CA3340", bird_code = "GX.RM|WX.GX", bird_sex = "M", species = "SNPL", population = "ceuta")

PP51076b_ceuta <- # mid-term (1000 and 2200)
  import_plover_tag_spatial(data_loc = "data/SNPL/lotek/PinPoint_Tag_51076b/Swift GPS Data Files/PinPoint 51076 2022-06-14 19-26-22_NestC403_FemaleCN0609.txt",
                            tag_ID = "PP51076b", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1, local_time_zone = "America/Mazatlan",
                            bird_ID = "CN0609", bird_code = "GX.RM|OX.RX", bird_sex = "F", species = "SNPL", population = "ceuta")

# WIPL Pinpoint ----
PP51070b_ceuta <- 
  import_plover_tag_spatial(data_loc = "data/WIPL/lotek/PinPoint_Tag_51070b/PinPoint 51070 2022-07-02 05-29-16_NestW401_Male_CV0253.txt",
                            tag_ID = "PP51070b", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1, local_time_zone = "America/Mazatlan",
                            bird_ID = "CV0253", bird_code = "YX.RM|BX.YX", bird_sex = "M", species = "WIPL", population = "ceuta")

PP51065a_ceuta <- 
  import_plover_tag_spatial(data_loc = "data/WIPL/lotek/PinPoint_Tag_51065a/Swift GPS Data Files/PinPoint 51065 2022-05-04 02-02-18_NestWD1FemaleCV0195.txt",
                            tag_ID = "PP51065a", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                            collect_time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1, local_time_zone = "America/Mazatlan",
                            bird_ID = "CV0195", bird_code = "LX.RM|BX.YX", bird_sex = "F", species = "WIPL", population = "ceuta")

PP51065b_ceuta <- 
  import_plover_tag_spatial(data_loc = "data/WIPL/lotek/PinPoint_Tag_51065b/Swift GPS Data Files/PinPoint 51065 2022-05-09 06-00-18_NestWD2_MaleCV0266.txt",
                                tag_ID = "PP51065b", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1, local_time_zone = "America/Mazatlan",
                                bird_ID = "CV0266", bird_code = "BX.RM|LX.LX", bird_sex = "M", species = "WIPL", population = "ceuta")

# BADO ----
NFTag55843_nz <- 
  import_plover_tag_spatial(data_loc = "data/BADO/Tag55843/Obs161122_192740_Tag55843.pos",
                                tag_ID = "NF55843", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "Pacific/Auckland",
                                bird_ID = "CP9042", bird_code = "FWX.RR|MX.YY", bird_sex = "F", species = "BADO", population = "kaikoura")

NFTag20865_nz <- 
  import_plover_tag_spatial(data_loc = "data/BADO/Tag20865/Obs261022_231825_Tag20865.pos",
                                tag_ID = "NF20865", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "Pacific/Auckland",
                                bird_ID = "CP9055", bird_code = "FWX.RR|MX.LR", bird_sex = "M", species = "BADO", population = "kaikoura")

NFTag21146_nz <- 
  import_plover_tag_spatial(data_loc = "data/BADO/Tag21146/Obs261022_234604_Tag21146.pos",
                                tag_ID = "NF21146", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "Pacific/Auckland",
                                bird_ID = "CP9098", bird_code = "XX.RW|MX.LB", bird_sex = "F", species = "BADO", population = "kaikoura")

NFTag55687_nz <- 
  import_plover_tag_spatial(data_loc = "data/BADO/Tag55687/Obs261022_232953_Tag55687.pos",
                                tag_ID = "NF55687", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "Pacific/Auckland",
                                bird_ID = "CP9032", bird_code = "XX.RB|MX.BR", bird_sex = "M", species = "BADO", population = "kaikoura")

NFTag55660_nz <- 
  import_plover_tag_spatial(data_loc = "data/BADO/Tag55660/Obs031222_180904_Tag55660.pos",
                                tag_ID = "NF55660", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "Pacific/Auckland",
                                bird_ID = "CP9066", bird_code = "XX.RB|MX.BY", bird_sex = "F", species = "BADO", population = "kaikoura")

NFTag55795_nz <- 
  import_plover_tag_spatial(data_loc = "data/BADO/Tag55795/Obs261022_233831_Tag55795.pos",
                                tag_ID = "NF55795", projection = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                collect_time_zone = "GMT", tag_model = "nanoFix-mini", n_slice = 0, local_time_zone = "Pacific/Auckland",
                                bird_ID = "CP9096", bird_code = "XX.RW|MX.BY", bird_sex = "F", species = "BADO", population = "kaikoura")

# merge ----
plover_tagging_df <- 
  dplyr::bind_rows(NF21163_ceuta, NF21065_ceuta, NF20996_ceuta, NF21117_ceuta, 
            NF21166_ceuta, NF21176_ceuta, NF21094_ceuta, NF20849_ceuta, 
            NF21261_ceuta, NF20805_ceuta, NF55650_ceuta, NF21263_ceuta,
            PP51070b_ceuta, PP51065a_ceuta, PP51065b_ceuta, PP51076a_ceuta,
            PP51076b_ceuta, PP51075_ceuta, PP51073_ceuta, PP51070a_ceuta,
            PP51069b_ceuta, PP51069a_ceuta, PP51067b_ceuta, PP51067a_ceuta,
            PP51064_ceuta, PP51063_ceuta, PP51060_ceuta, PP48672_ceuta,
            PP48671_ceuta, PP48670_ceuta, PP48669_ceuta, NF55833_tagus,
            NF55831_tagus, NF55808_tagus, NF55719_tagus, NF21200_tagus,
            NF55584b_tagus, NF55584a_tagus, NF21050_tagus, NFTag55843_nz,
            NFTag20865_nz, NFTag21146_nz, NFTag55687_nz, NFTag55660_nz,
            NFTag55795_nz) %>% 
  sfc_as_cols(., names = c("lon", "lat")) %>% 
  st_drop_geometry()

ceuta <- 
  plover_tagging_df %>% 
  filter(population == "ceuta") %>% 
  mutate(date = as.Date(timestamp_local, tz = "America/Mazatlan")) %>%
  getSunlightTimes(data = ., keep = c("nightEnd", "night", 
                                      "sunrise", "sunset", 
                                      "dawn", "dusk", 
                                      "nauticalDawn", "nauticalDusk"), 
                   tz = "America/Mazatlan") %>% 
  rename(lat_fun = lat,
         lon_fun = lon) %>% 
  bind_cols(plover_tagging_df %>% filter(population == "ceuta"), .) %>% 
  mutate(timestamp_local = ymd_hms(timestamp_local, tz = "America/Mazatlan")) %>% 
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

kaikoura <- 
  plover_tagging_df %>% 
  filter(population == "kaikoura") %>% 
  mutate(date = as.Date(timestamp_local, tz = "Pacific/Auckland")) %>%
  getSunlightTimes(data = ., keep = c("nightEnd", "night", 
                                      "sunrise", "sunset", 
                                      "dawn", "dusk", 
                                      "nauticalDawn", "nauticalDusk"), 
                   tz = "Pacific/Auckland") %>% 
  rename(lat_fun = lat,
         lon_fun = lon) %>% 
  bind_cols(plover_tagging_df %>% filter(population == "kaikoura"), .) %>% 
  mutate(timestamp_local = ymd_hms(timestamp_local, tz = "Pacific/Auckland")) %>% 
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

tagus <- 
  plover_tagging_df %>% 
  filter(population == "tagus") %>% 
  mutate(date = as.Date(timestamp_local, tz = "Europe/Lisbon")) %>%
  getSunlightTimes(data = ., keep = c("nightEnd", "night", 
                                      "sunrise", "sunset", 
                                      "dawn", "dusk", 
                                      "nauticalDawn", "nauticalDusk"), 
                   tz = "Europe/Lisbon") %>% 
  rename(lat_fun = lat,
         lon_fun = lon) %>% 
  bind_cols(plover_tagging_df %>% filter(population == "tagus"), .) %>% 
  mutate(timestamp_local = ymd_hms(timestamp_local, tz = "Europe/Lisbon")) %>% 
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

plover_tagging_df <- 
  bind_rows(ceuta, kaikoura, tagus)

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

