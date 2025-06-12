## setup renv

# > renv::status()
# The following package(s) are in an inconsistent state:
#   
# The following package(s) are used in this project, but are not installed:
# - doMC
# - maptools
# - rgdal
# - rgeos
# - whomapper

  
# define packages
install.packages("arcgis", repos = c("https://r-arcgis.r-universe.dev", "https://cloud.r-project.org"))
install.packages(
  "arcgisbinding", 
  repos = "https://r.esri.com", 
  type = "win.binary"
)

install.packages("C:/Users/lopezg/Downloads/arcgisbinding_1.0.1.311.zip", repos = NULL)



library(arcgis)
library(arcgisbinding)
library(ggbump)
library(readxl)
library(gtsummary)
library(haven)
library(tidyverse)
library(maps)
library(countrycode)
library(ggplot2)
library(openxlsx)
library(ggrepel)
library(ggforce)
library(arrow)
library(ggh4x)
library(patchwork)
library(tidytext)
#library(raster)
#library(rgdal)
library(broom)
library(RColorBrewer)
#library(rgeos)
#library(maptools)
library(jsonlite)
library(httr)
library(cowplot)

# use renv::init() to initialise the the renv
# use renv::snapshot() to save the state of the art project library
# use renv::restore() to update your project library to the last saved
# renv.lock file. ATTENTION: this overrides any changes you made locally
# renv::init()
# renv::snapshot()
# renv::restore()

# At the top of all scripts run
# source("requirements.R")
