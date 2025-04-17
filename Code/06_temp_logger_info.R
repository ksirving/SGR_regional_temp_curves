### checking logger info

library(tidylog)
library(tidyverse)
library(sf)
library(mapview)
library(nhdplusTools)


# logger data -------------------------------------------------------------

sites <- st_read("/Users/katieirving/Library/CloudStorage/OneDrive-SharedLibraries-SCCWRP/San Gabriel and Santa Clara Temp Effects - Documents/General/Data/Maps/shapefiles and map objects/watertempdata_all_merged.shp")
sites

### list sites and join 

setwd("/Users/katieirving/Library/CloudStorage/OneDrive-SharedLibraries-SCCWRP/San Gabriel and Santa Clara Temp Effects - Documents/General/Data/Maps/LatLong csv files")

# h <- list.files(pattern="watertempdata")
# length(h) ## 10
# 
# 
# ## get one file for format
# 
# TempDatax <- read.csv(h[3], sep="")
# TempDatax
# 
# ## remove file
# 
# h <- h[-c(3,4)]
# h
# n
# 
# h[n]
# 
# for(n in 1:length(h)) {
#   
#   TempData <- read.csv(h[n], sep="") 
#     # mutate(access.comments = as.character(access.comments))
#   head(TempData)
#   
#   TempDatax <- bind_rows(TempDatax, TempData)
#   
# }

data1 <- read.csv("watertempdata_CEDEC.csv")
data2 <- read.csv("watertempdata_FishAndWildlife.csv")
data3 <- read.csv("watertempdata_RB9.csv")
data4 <- read.csv("watertempdata_SDAM.csv")
data5 <- read.csv("watertempdata_SOCHistorical.csv")
data6 <- read.csv("watertempdata_StreamTemp_ALL.csv")
data7 <- read.csv("watertempdata_USCR.csv")
data8 <- read.csv("watertempdata_USGS.csv") %>%
  mutate(start.year = as.integer(start.year),
         end.year = as.integer(end.year))

df <- bind_rows(data1,data2,data3,data4,data5,data6,data7,data8)

head(df)

# Map ---------------------------------------------------------------------

## map done externally and lives - https://sccwrp.sharepoint.com/:f:/s/TempModelling/Erqgm8fSBJZHh78Qdd2klu8BCHfdjS3tC01TzULkhBGt2g?e=JBc4Ux
# temperature modelling teams channel