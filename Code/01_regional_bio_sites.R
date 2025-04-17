### bioassessment sites

##workflow
## 1 - upload csci and asci
## subset to region of interest - RB4?
## plot
## explore data in region
## format alternative response metrics
## - component metrics
## - traits and capture probability

library(tidyverse)
library(sf)
library(mapview)
library(nhdplusTools)
# install.packages("devtools")
library(devtools)
# install_github("SCCWRP/CSCI", force=T)
# library(CSCI)
library(lubridate)
library(tidylog)


# CSCI --------------------------------------------------------------------

load("ignore/SMC_bmi_cali.Rdata") ## update
load("ignore/SMC_algae_cali.Rdata") ## update
head(bug_tax_ca)
class(bug_tax_ca)
head(alg_tax_ca)

load(file="/Users/katieirving/Library/CloudStorage/OneDrive-SharedLibraries-SCCWRP/Cannabis E-Flows - Documents/General/Data/RawData/Bio/ForJazzmyn/SMC_csci_cali_Mar2023_v2.RData")

## GET DISTINCT SITES
bug_tax_ca <- bug_tax_ca %>%
  dplyr::select(masterid, longitude, latitude) %>%
  distinct(masterid, .keep_all = TRUE) %>%
  mutate(Bio = "BMI")

## GET DISTINCT SITES ASCI
alg_tax_ca <- alg_tax_ca %>%
  dplyr::select(masterid, longitude, latitude) %>%
  distinct(masterid, .keep_all = TRUE) %>%
  mutate(Bio = "Algae")

## JOIN

allBioSites <- bind_rows(bug_tax_ca, alg_tax_ca)

##SAVE
allBioSites

write.csv(allBioSites, "ignore/01_all_bio_sites_updated_Feb2023.csv")

## make spatial
# Create dataframe for looking up COMIDS (here use all stations)
bug_segs <- bug_tax_ca %>%
  dplyr::select(masterid, longitude, latitude, comid) %>%
  distinct(masterid, .keep_all = TRUE) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=F) %>%
  rename(COMID1 = comid)  %>%
  arrange(masterid)

# use nhdtools to get comids
bug_all_coms <- bug_segs %>%
  group_split(masterid) %>%
  set_names(., bug_segs$masterid) %>%
  map(~discover_nhdplus_id(.x$geometry))

bug_all_coms

# flatten into single dataframe instead of list
bug_segs_df <-bug_all_coms %>% flatten_dfc() %>% t() %>%
  as.data.frame() %>%
  rename("COMID"=V1) %>% rownames_to_column(var = "masterid")

bug_segs_df

bugs <- full_join(bug_segs, bug_segs_df, by = "masterid")
head(bugs)

## add county to filter
county <- bug_tax_ca %>%
  select(masterid, county) %>%
  
  distinct()

bugs2 <- left_join(bugs, county, by = "masterid")

class(bugs2)
dim(bugs2)

st_write(bugs2, "ignore/output/01_bio_sites_all.shp", append=F)

bugs2 <- st_read("ignore/output/01_bio_sites_all.shp")
head(bugs2)

### filter to only surrounding counties and join to get bugs/algae cat
bugs2 <- bugs2 %>%
  filter(county %in% c("Kern", "Imperial", "Los Angeles", "Orange", "Riverside", "San Diego", 
                       "San Bernardino", "Santa Barbara", "San Luis Obispo", "Ventura")) %>%
  right_join(allBioSites, by =c("masterid", "latitude", "longitude"), relationship =
               "many-to-many") %>%
  drop_na(county)

## subset to surrounding counties - update this!!! do by HUC
sort(unique(bugs2$county))
names(bugs2)

bugsSites <- bugs2 %>%
  filter(Bio == "BMI")

length(unique(bugsSites$masterid)) # 1938

algaeSites <- bugs2 %>%
  filter(Bio == "Algae")

length(unique(algaeSites$masterid)) # 1113

## total sites
length(unique(bugs2$masterid)) # 1938

## map of all sites in state
# set background basemaps:
basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery",
                  "Esri.NatGeoWorldMap",
                  "OpenTopoMap", "OpenStreetMap", 
                  "CartoDB.Positron", "Stamen.TopOSMFeatures")

mapviewOptions(basemaps=basemapsList, fgb = FALSE)



# this map of all sites 
# m1 <- mapview(bugsSites, cex=2, col.regions=c("orange"),
#               layer.name="BMI Sites") +
#   mapview(algaeSites, cex=2, col.regions=c("green"),
#           layer.name="Algae Sites") 

# m1
# # m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")
# 
# mapshot(m1, url = paste0(getwd(), "/ignore/output/01_bio_sites_socal_counties_mapview.html"),
#         file = paste0(getwd(), "/ignore/output/01_bio_sites_socal_counties_mapview.png"))
# 

## define counties to select
counties <- c("Los Angeles", "Ventura", "Orange", "Riverside", "San Bernardino")

## filter to selelted counties
bug_sp_sub <- bugs2 %>%
  filter(county %in% counties) %>%
  select(masterid, latitude, longitude, county, COMID)

dim(bug_sp_sub)

st_write(bug_sp_sub, "ignore/output/01_bio_sites_surrounding_counties.shp", append=F)

length(unique(bug_sp_sub$county)) ## 5
length(unique(bug_sp_sub$COMID)) ## 831
length(unique(bug_sp_sub$masterid)) ## 1265

### plot

## select only soatial columns

# set background basemaps:
# basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery",
#                   "Esri.NatGeoWorldMap",
#                   "OpenTopoMap", "OpenStreetMap", 
#                   "CartoDB.Positron", "Stamen.TopOSMFeatures")
# 
# mapviewOptions(basemaps=basemapsList, fgb = FALSE)
# 
# 
# 
# # this map of all sites in same HUC 12
# m1 <- mapview(bug_sp_sub, cex=2, col.regions="orange",
#               layer.name="Bugs Stations") 
#   
# 
# m1
# # m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")
# 
# mapshot(m1, url = paste0(getwd(), "/output_data/01_bio_sites_surrounding_counties_mapview.html"),
#         file = paste0(getwd(), "/ignore/01_bio_sites_surrounding_counties_mapview.png"))
# getwd()
# 
# mapview()


# CSCI component metrics --------------------------------------------------

compMets <- read.csv("ignore/csci_comps.csv")

# load(file="com_mets_ca.RData")
head(compMets)
names(compMets)

## for Jazzmyn
compMetsx <- compMets %>%
  select(masterid, sampledate, sampleid, csci)

## save
write.csv(compMetsx, "ignore/output/01_csci_scores_ca.csv")

## make long 
## keep only metrics with "score"
compMets <- compMets %>%
  dplyr::select(masterid:mmi_score, ends_with("_score")) %>%
  pivot_longer(csci:intolerant_percent_score, names_to="Metric", values_to = "MetricValue")

unique(compMets$Metric) 

## add comid

bugs2 <- st_read("ignore/output/01_bio_sites_all.shp")
head(bugs2)

bugs2 <- as.data.frame(bugs2) %>%
  select(-geometry)


compMetsComs <- inner_join(compMets, bugs2, by="masterid")
head(compMetsComs)

## save out

write.csv(compMetsComs, "ignore/output/01_csci_comp_mets_comid_socal.csv")


# ASCI component metrics --------------------------------------------------

compMets <- read.csv("ignore/asci_comps.csv")
head(compMetsx)
names(compMets)

## for Jazzmyn
compMetsx <- compMets %>%
  select(masterid, sampledate, sampleid, ASCI_Hybrid)

## save
write.csv(compMetsx, "ignore/output/01_asci_scores_ca.csv")

## make long 
## keep only metrics with "score"
compMets <- compMets %>%
  dplyr::select(masterid:sampleid, ends_with("_Hybrid")) %>%
  dplyr::select(-contains(c("Raw", "Pred", "Pct.Att"))) %>%
  pivot_longer(NumberTaxa_Hybrid:Salinity.BF.richness_Score_Hybrid, names_to="Metric", values_to = "MetricValue")

unique(compMets$Metric) 

## add comid

bugs2 <- st_read("ignore/output/01_bio_sites_all.shp")
head(bugs2)

bugs2 <- as.data.frame(bugs2) %>%
  select(-geometry)
## join
compMetsComs <- inner_join(compMets, bugs2, by="masterid")
head(compMetsComs)

## save out

write.csv(compMetsComs, "ignore/output/01_asci_comp_mets_comid_socal.csv")


# SCAPE values ------------------------------------------------------------
library(sf)
## upload data from SCAPE git repo
load(file = "/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/SCAPE/data/spat_San Gabriel.RData")
head(`spat_San Gabriel`)
dim(`spat_San Gabriel`)
st_crs(`spat_San Gabriel`)
mean(na.omit(`spat_San Gabriel`$core0.50))

## load temp data

load(file = "ignore/baseline_stream_temp.RData")

baseline_stream_temp <- baseline_stream_temp %>% ungroup() %>%
  pivot_longer(Max_Wkly_Mean_StreamT:Mean_Wkl_Rng_StreamT, names_to = "Mets", values_to = "Vals") %>%
  mutate(ValsF = (Vals*1.8)+32) %>% 
  select(-Vals) %>%
  pivot_wider(names_from = Mets, values_from = ValsF)


## map of all sites in state
# set background basemaps:
# basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery",
#                   "Esri.NatGeoWorldMap",
#                   "OpenTopoMap", "OpenStreetMap", 
#                   "CartoDB.Positron", "Stamen.TopOSMFeatures")
# 
# mapviewOptions(basemaps=basemapsList, fgb = FALSE)


### coord taken from google maps - rough
#34.15777020930686, -118.00101763245898
# 34.11496952007641, -117.6469782510806
#33.77127898678574, -118.18478806195667
# 33.66408409118295, -117.9984203885485

## make df with coords
y_coord <- c(34.15777020930686,  34.11496952007641,33.66408409118295,  33.77127898678574)
x_coord <- c(-118.00101763245898, -117.6469782510806,  -117.9984203885485,-118.18478806195667)
xym <- as.data.frame(cbind(x_coord, y_coord))
xym

## make polygon
poly <- xym %>% 
  st_as_sf(coords = c("x_coord", "y_coord"), 
           crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

## map
m1 <- mapview(poly, 
              layer.name="poly") +
  mapview(`spat_San Gabriel`, layer.name="reaches")


m1


# which points fall inside a polygon?
(mat = st_intersects(`spat_San Gabriel`, poly, sparse = FALSE))

`spat_San Gabriel`$inPoly <- mat
`spat_San Gabriel`

## subset to lower SGR

SGRLower <- `spat_San Gabriel` %>%
  filter(inPoly == "TRUE")

SGRLower

## get mean csci

mean(na.omit(SGRLower$core0.50)) ## 0.59
mean(na.omit(SGRLower$core0.75)) ## 0.72
