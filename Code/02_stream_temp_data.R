## stream temp la county
## from Jenny's model - https://ftp.sccwrp.org/pub/download/DOCUMENTS/TechnicalReports/1084_ClimateChangeVulnerability.pdf

library(tidyverse)
library(sf)
library(tidylog)

# modelled data -------------------------------------------------------------

load(file = "ignore/baseline_stream_temp.RData")

baseline_stream_temp <- baseline_stream_temp %>% ungroup() %>%
  pivot_longer(Max_Wkly_Mean_StreamT:Mean_Wkl_Rng_StreamT, names_to = "Mets", values_to = "Vals") %>%
  mutate(ValsF = (Vals*1.8)+32) %>% 
  select(-Vals) %>%
  pivot_wider(names_from = Mets, values_from = ValsF)

# test <-inner_join(`spat_San Gabriel`, baseline_stream_temp, by = "COMID")
# test
head(baseline_stream_temp) 
range(baseline_stream_temp$Max_Wkl_Max_StreamT_grt_30_)
names(baseline_stream_temp)

length(unique(baseline_stream_temp$COMID)) ## 5428

LARTemp <- baseline_stream_temp %>%
  select(COMID, year)

write.csv(LARTemp, "/Users/katieirving/Library/CloudStorage/OneDrive-SharedLibraries-SCCWRP/Temp Modelling - General/Data/Raw/LAR/Mod_data_comids_years.csv")

# Map temperature data ----------------------------------------------------

nhd <- st_read("ignore/SpatialData/NHDPlus/All_Ca_NHDPlus.shp")
nhd
## simplify
# nhd <- nhd %>%
#   st_as_sf %>%
#   st_simplify(dTolerance = 0.5, preserveTopology = T)
baseline_stream_temp <- select(baseline_stream_temp, COMID, year) %>%
  mutate(HasData = "Yes")

### join to temp

nhd_temp <- right_join(nhd, baseline_stream_temp, by = "COMID") %>%
  select(COMID, HasData) %>%
  distinct()

head(nhd_temp)
class(nhd_temp)


## save out
st_write(nhd_temp, "ignore/output/nhd_baseline_temp.shp", append = F)

## map 
library(mapview)
library(sf)
library(RColorBrewer)
# webshot::install_phantomjs()

## map
# set background basemaps:
# basemapsList <- c("Esri.WorldTopoMap", "Esri.WorldImagery",
#                   "Esri.NatGeoWorldMap",
#                   "OpenTopoMap", "OpenStreetMap", 
#                   "CartoDB.Positron", "Stamen.TopOSMFeatures")
# 
# mapviewOptions(basemaps=basemapsList, fgb = FALSE)
# 
# 
# m1 <- mapview(nhd_temp) #+
# 
# m1 <- mapview(nhd_temp,
#               layer.name="Temperature reaches", legend=T) 
# 
# 
# m1
# # ?mapview
# 
# m1@map %>% leaflet::addMeasure(primaryLengthUnit = "meters")
# 
# mapshot(m1, url = paste0(getwd(), "/ignore/output/baseline_temp_extent.html"),
#         file = paste0(getwd(), "/ignore/output/baseline_temp_extent.png"))
# getwd()


# Temperature alteration --------------------------------------------------

## workflow
## match streamcat data to Jenny data
## calculate alteration
## eventually compare ref streamcat to temp prefs of historical distribution
## model with o/e csci metrics

## upload ref temp data - streamcat

refTemps <- read.csv("ignore/RefStreamTempPred_CA.csv")
head(refTemps)

## make long and separate metric and year
refTempsLong <- refTemps %>%
  pivot_longer(MAST_2008:MWST_2014, names_to = "MetricYear", values_to = "Values") %>%
  separate(MetricYear, into = c("RefMetric", "Year")) %>%
  mutate(Values = (Values*1.8)+32) 

head(refTempsLong)

## how many comids match?

sum(unique(baseline_stream_temp$COMID)  %in%  unique(refTempsLong$COMID)) ## 4885, where are the others?


## summarise by year, comid and variable, filter by modelled data comids

SumTempsLong <- refTempsLong %>%
  filter(COMID %in% baseline_stream_temp$COMID) %>%
  group_by(COMID, RefMetric) %>%
  summarise(MedRefTemp = median(na.omit(Values)))


## temperature aren't comparable right now, join but replace with annual, summaer and winter current temp

load(file = "ignore/baseline_stream_temp.RData")

baseline_stream_temp <- baseline_stream_temp %>% ungroup() %>%
  pivot_longer(Max_Wkly_Mean_StreamT:Mean_Wkl_Rng_StreamT, names_to = "Mets", values_to = "Vals") %>%
  mutate(ValsF = (Vals*1.8)+32) %>% 
  select(-Vals) %>%
  pivot_wider(names_from = Mets, values_from = ValsF)


CurTempLong <- baseline_stream_temp %>%
  ungroup() %>%
  pivot_longer(Max_Wkl_Max_StreamT_grt_30_:Mean_Wkl_Rng_StreamT, names_to = "Mets", values_to = "Vals")# %>%
 
head(CurTempLong)

## join current and ref temps

AllTemp <- inner_join(CurTempLong, SumTempsLong, by = "COMID")

head(AllTemp)
# View(AllTemp)
## save out 
save(AllTemp, file="ignore/output/02_cur_ref_temps_not_matching_metrics.RData")

### calculate alteration, remove NAs 

AllTempAlt <- CurTempLong %>%
  # mutate(AltTemp = CurTemp-MedRefTemp) %>%
  # pivot_wider(names_from=CurMetric, values_from = CurTemp) %>%
  # pivot_longer(c(Max_Wkl_Max_StreamT_grt_30_,Max_Wkly_Mean_StreamT:Mean_Wkl_Rng_StreamT), 
  #              names_to = "CurMetric", values_to = "CurTemp") %>%
  rename(CurTemp = Vals, CurMetric = Mets) %>%
  mutate(AltTempMinus80 = CurTemp-80, 
         AltTempMinus86 = CurTemp-86,
         AltTempDivide80 = CurTemp/80) %>%
  # pivot_wider(names_from =CurMetric, values_from = CurTemp) %>%
  # pivot_longer(c(Max_Wkl_Max_StreamT_grt_30_,Max_Wkly_Mean_StreamT:Mean_Wkl_Rng_StreamT), names_to="CurMetric", values_to = "CurTemp") #%>%
  pivot_longer(AltTempMinus80:AltTempDivide80, names_to = "AltMetric", values_to = "AltTemp")
  # drop_na()

unique(AllTempAlt$CurMetric)
names(AllTempAlt)
# View(AllTempAlt)

## save out 
# save(AllTempAlt, file="ignore/output/02_cur_ref_temps_not_matching_metrics_alteration.RData")
save(AllTempAlt, file = "ignore/output/02_cur_temp_alt_mets.RData")


# Add stream channel ------------------------------------------------------

## upload channel engineering data from Rafi
BioEng <- read.csv("ignore/Channel_Eng_02152023.csv")
head(BioEng)

## put all enginneered in same class
BioEng <- BioEng %>%
  mutate(Class2 = ifelse(channel_engineering_class =="NAT", "Natural", "Hard")) %>%
  mutate(Class2 = ifelse(channel_engineering_class %in% c("SB0", "SB1", "SB2"), "Soft",Class2))

View(BioEng)

write.csv(BioEng, "ignore/output/02_chan_eng.csv")




         