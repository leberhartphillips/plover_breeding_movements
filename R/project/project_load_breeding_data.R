# prepare R environment
source("R/project/project_libraries.R")

# load functions
function.sources = list.files(path = "R/functions/", 
                              pattern = "*\\().R$", full.names = TRUE, 
                              ignore.case = TRUE)
sapply(function.sources, source, .GlobalEnv)

# connect to CeutaCLOSED and wrangle
CeutaCLOSED <- 
  dbConnect(SQLite(), 
            dbname = "/Users/luketheduke2/Documents/Academic/Mexico/Ceuta_CLOSED/data/Ceuta_CLOSED_version_releases/Ceuta_CLOSED_mdd_v2-0-2.sqlite")

ceuta_list <- list(Nests = dbReadTable(CeutaCLOSED, "Nests") %>% 
                     # corrections that need to be done to the next version of CeutaCLOSED
                     dplyr::mutate(female = ifelse(ID == "SNPL_2022_C_402", "OX.RM|OX.LX", female),
                            fate = ifelse(fate == "Hatched", "Hatch", fate),
                            nest_initiation_date = ifelse(ID == "SNPL_2022_C_201" &
                                                            nest_initiation_date == "514",
                                                          "414", nest_initiation_date)) %>%
                     dplyr::mutate(female = ifelse(ID == "SNPL_2022_A_2" & female == "BX.RM|OX.YX", "BX.RM|OX.YXx", female)) %>% 
                     dplyr::mutate(male = ifelse(ID == "SNPL_2022_E_3" & male == "OX.RM|WX.GX", "OX.RM|WX.GX_male", male)) %>%
                     plover_date_convert(input = "mdd"),
                     
                   Captures = dbReadTable(CeutaCLOSED, "Captures") %>% 
                     # corrections that need to be done to the next version of CeutaCLOSED
                     dplyr::mutate(sex = ifelse(ring == "CA3340" & ID == "2019_D_203", "M", sex)) %>% 
                     dplyr::mutate(code = ifelse(ring == "CA3314", "GX.RM|BX.WX", code)) %>% 
                     dplyr::mutate(code = ifelse(ring == "CA3315", "GX.MR|GX.BX", code)) %>% 
                     dplyr::mutate(code = ifelse(ID == "SNPL_2022_A_2" & code == "BX.RM|OX.YX", "BX.RM|OX.YXx", code)) %>% 
                     dplyr::mutate(code = ifelse(ID == "SNPL_2022_E_3" & code == "OX.RM|WX.GX", "OX.RM|WX.GX_male", code)) %>%
                     plover_date_convert(input = "mdd"),
                   
                   Broods = dbReadTable(CeutaCLOSED, "Broods") %>% 
                     # corrections that need to be done to the next version of CeutaCLOSED
                     dplyr::mutate(male = ifelse(ID == "2018_C_301" & male == "GX.RM|GX.BX", "GX.MR|GX.BX", male)) %>% 
                     dplyr::mutate(ID = ifelse(ID == "2019_D_3" & male == "OX.RM|LX.BX", "2019_C_102", ID)) %>% 
                     dplyr::mutate(male = ifelse(ID == "2019_D_3" & male == "MX.RW|GX.GX", "BX.RM|WX.GX", male)) %>% 
                     dplyr::mutate(male = ifelse(ID == "2019_D_3" & male == "BX.RM|WX.LX", "BX.RM|WX.GX", male)) %>% 
                     dplyr::mutate(male = ifelse(ID == "2019_D_2" & male == "MX.RW|GX.LX", "MX.RW|GX.GX", male)) %>% 
                     plover_date_convert(input = "mdd"),
                   
                   Resights = dbReadTable(CeutaCLOSED, "Resights") %>% 
                     plover_date_convert(input = "mdd"),
                   
                   BirdRef = dbReadTable(CeutaCLOSED, "BirdRef") %>% 
                     plover_date_convert(input = "mdd"))

# ceuta_list$Captures %>% 
#   group_by(year) %>% 
#   summarise(n())

# import data from Afonso and wrangle
tagus_list <- list(Nests = read_xlsx(path = "data/breeding_data/Tagus/Tagged_KP_breeding.xlsx", sheet = "nests"),
                   Broods = read_xlsx(path = "data/breeding_data/Tagus/Broodcare_AR.xlsx", sheet = "summary"))

husum_list <- list(Nests = read_xlsx(path = "data/KEPL/Short term deployments 2021 and 2022 basic data BHK for Luke.xlsx", col_types = "guess"))
