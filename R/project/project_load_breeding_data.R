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

ceuta_list <- list(Nests = dbReadTable(CeutaCLOSED, "Nests") %>% plover_date_convert(input = "mdd"),
                   Captures = dbReadTable(CeutaCLOSED, "Captures") %>% plover_date_convert(input = "mdd"),
                   Broods = dbReadTable(CeutaCLOSED, "Broods") %>% plover_date_convert(input = "mdd"),
                   Resights = dbReadTable(CeutaCLOSED, "Resights") %>% plover_date_convert(input = "mdd"),
                   BirdRef = dbReadTable(CeutaCLOSED, "BirdRef") %>% plover_date_convert(input = "mdd"))

ceuta_list$Captures %>% 
  group_by(year) %>% 
  summarise(n())

# import data from Afonso and wrangle
tagus_list <- list(Nests = read_xlsx(path = "data/breeding_data/Tagus/Tagged_KP_breeding.xlsx", sheet = "nests"),
                   Broods = read_xlsx(path = "data/breeding_data/Tagus/Broodcare_AR.xlsx", sheet = "summary"))