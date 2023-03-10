## install dependent packages

# a vector of all the packages needed in the project's scripts
packages_required_in_project <- 
  c(#"arm", "aspace","BaSTA","bayestestR","broom.mixed", "coefplot2", "cowplot",
    #"data.table", "doParallel", "effects", "extrafont", "flexmix", "fGarch", 
    #"ggmap", "ggpubr", "ggridges", "ggthemes", "grid", "gt", "kableExtra", 
    #"leaflet", "lme4", "magick", "mapview", "RMark", "merDeriv", "mgcv", "moveVis", 
    #"multcomp", "multipanelfigure", "parallel", "parameters", "partR2", 
    #"patchwork", "RColorBrewer", "rptR", "RSQLite", "scales", "snowfall", "sp", 
    #"standardize", "tidybayes", 
    "leaflet.extras2", "leaflet", "RColorBrewer", "adehabitatLT", 
    "lubridate", "RSQLite", "tidyverse", "sf", "ctmm", "readxl", 
    "geosphere", "hms", "mgcv", "mapview", "geosphere", "ctmm", "moveVis", "broom.mixed", "ggthemes", "astroFns",
    "move", "scales", "recurse", "suncalc", "dismo", "rgeos", "nestR", "activity", "bpnreg", "gratia", "circlize", "lme4", "parameters"#, "Actigraphy"
    #, "rmdformats", "webshot2"
    )

# of the required packages, check if some need to be installed
new.packages <- 
  packages_required_in_project[!(packages_required_in_project %in% 
                                   installed.packages()[,"Package"])]

# install all packages that are not locally available
if(length(new.packages)) install.packages(new.packages)

# load all the packages into the current R session
lapply(packages_required_in_project, require, character.only = TRUE)

# remotes::install_github("picardis/nestR", build_vignettes = TRUE)
# devtools::install_github("junruidi/actigraphy")
