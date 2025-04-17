### validation

library(tidylog)
library(tidyverse)
library(sf)
library(zoo)
library(scales)

getwd()
setwd("/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/SGR_regional_temp_curves/")
## workflow
## get temp data
## make metrics
## pair temp data with bio - same comid
## find bio sites for heal the bay data
## pair heal the bay data
## check if can pair by years, if not use all years in temp data
## test bio score with temp - what probability of good and modified score with temp data

## bio sites near Bear Creek
# Downstream
# 405BH2Bxx 
# SMC00464
# SMC00464_SGRRMP
# upstream
# 405SGBWFK 

## Bio sites near rainbow ranch
## downstream
# SMC06496
# SGUT505

## Big Dalton
## downstream
# 405SGB022


# Upper Walnut Creek - San Dimas
# SMC02656 far away


# Temp data ---------------------------------------------------------------

### sccwrp SGR data
sgrData <- read.csv("ignore/Temperature/SCCWRP_loggers/master_SGR_temp_data.csv")

head(sgrDatax)

## get year from datetime
str(sgrData)

sgrDatax <- sgrData %>%
  separate(datetime, into = c("Date", "Time"), sep = " ", remove = F) %>% ## separate dateime
  separate(Date, into = c("Month", "Day", "Year"), sep = "/") %>% ## separate date
  mutate(Year = paste0("20", Year))  %>%## change date from 23 to 2023
  mutate(Source = "SCCWRP")

names(sgrDatax)



## summarise daily statistics
dfSCCWRP <- sgrDatax  %>%
  group_by(sitename, Year, Month, Day) %>%
  summarise(MeanTemp = mean(temperature),
            MinTemp = min(temperature),
            MaxTemp = max(temperature))
dfSCCWRP

## calculate rolling min and max
df <- dfSCCWRP %>% 
  ungroup() %>%
  group_by(sitename, Year) %>%
  mutate(max_07da = zoo::rollmean(MaxTemp, k=7, fill = NA)) %>% ## rolling daily max
  mutate(mean_07da = zoo::rollmean(MeanTemp, k=7, fill = NA)) %>% ## rolling daily mean
  mutate(min_07da = zoo::rollmean(MinTemp, k=7, fill = NA)) %>% ## rolling daily min
  mutate(DTR = MaxTemp - MinTemp) ## diurnal temp rate

df

df_sums <- df %>%
  # mutate(SiteName = factor(siteName, levels = c("Burbank", "Rattlesnake", "Benedict", "Steelhead", "Riverfront",
  #                                               "Compton", "Willow"))) %>%
  pivot_longer(MeanTemp:DTR, names_to = "Metric", values_to= "Values")%>%
  group_by(sitename, Metric, Year) %>%
  summarise(Temp = mean(na.omit(Values))) %>%
  mutate(Metric = factor(Metric, levels = c("max_07da", "mean_07da", "min_07da", "MeanTemp", "MinTemp", "MaxTemp", "DTR"))) %>%
  mutate(TempF =  (Temp*1.8)+32)
  # pivot_wider(names_from = Metric, values_from = meanvals)

unique(df_sums$Metric)

write.csv(df_sums, "output_data/07_temp_means_per_site_year_SCCWRP.csv")

## plot temps

library(ggthemes)
df_sums
## plots don't work with just year!
## plot by site
T1 <- ggplot(df_sums, aes(y = TempF, x = Year, group = Metric, color = Metric)) +
  # geom_line(aes(y=AirTemp, col = "gray")) +
  geom_line() +
  # geom_line(aes(x=Date, y=AirTemp, color = "Max Daily Air Temp")) +
  facet_wrap(~sitename) + ## , scales = "free_x"
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(name="Water Temp (°F)") +
  scale_color_brewer(palette= "Set2",
                     labels = c("7 Day Max", "7 Day Mean", "7 Day Min","Mean Daily Temp", "Min Daily Temp",
                                "Max Daily Temp", "Diurnal Temp Rate", "Max Daily Air Temp")) +
  # scale_x_date(date_labels = "%b %Y") +
  theme_linedraw() 


T1

file.name1 <- paste0( "Figures/07_SCCWRP_Temp_by_Site.jpg")
ggsave(T1, filename=file.name1, dpi=300, height=5, width=6)


## plot by metric
## metric levels
levels(df_sums$Metric) <- c("7 Day Max", "7 Day Mean","7 Day Min", "Mean Daily Temp", "Min Daily Temp",
                             "Max Daily Temp", "Diurnal Temp Rate")

T2 <- ggplot(df_sums, aes(y = TempF, x = Month, group = sitename, color = sitename)) +
  # geom_line(aes(y=AirTemp, col = "gray")) +
  geom_line() +
  # geom_line(aes(x=Date, y=AirTemp, color = "Max Daily Air Temp")) +
  facet_wrap(~Metric, scales = "free_x") + ## , scales = "free_x"
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(name="Water Temp (°F)") +
  scale_color_brewer(palette= "Set2") +
  # scale_x_date(date_labels = "%b %Y") +
  theme_linedraw() 


T2

file.name1 <- paste0( "Figures/07_SCCWRP_Temp_by_Metric.jpg")
ggsave(T2, filename=file.name1, dpi=300, height=5, width=6)


# Heal The Bay Data -------------------------------------------------------

## upload data and make longer - from SGR temp script  temp_data_v4
HLBData <- read.csv("ignore/Temperature/temp/means_per_sites_month.csv") %>%
  select(-c(X)) %>%
  pivot_longer(DTR:min_07da, names_to = "Metric", values_to = "Temp") %>%
  group_by(SiteName, Metric, Year, Month) %>%
  summarise(Temp = mean(na.omit(Temp))) %>%
  ## make factors of month and metric
  mutate(Metric = factor(Metric, levels = c("max_07da", "mean_07da", "min_07da", "MeanTemp", "MinTemp", "MaxTemp", "DTR"))) %>%
  ## convert to F
  mutate(TempF =  (Temp*1.8)+32)

head(HLBData)
## plots don't work with just year!
## plot by site
T3 <- ggplot(HLBData, aes(y = TempF, x = Month, group = Metric, color = Metric)) +
  # geom_line(aes(y=AirTemp, col = "gray")) +
  geom_line() +
  # geom_line(aes(x=Date, y=AirTemp, color = "Max Daily Air Temp")) +
  facet_wrap(~SiteName) + ## , scales = "free_x"
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(name="Water Temp (°F)") +
  scale_color_brewer(palette= "Set2",
                     labels = c("7 Day Max", "7 Day Mean", "7 Day Min","Mean Daily Temp", "Min Daily Temp",
                                "Max Daily Temp", "Diurnal Temp Rate", "Max Daily Air Temp")) +
  # scale_x_date(date_labels = "%b %Y") +
  theme_linedraw() 


T3

file.name1 <- paste0( "Figures/07_HTB_Temp_by_Site.jpg")
ggsave(T3, filename=file.name1, dpi=300, height=5, width=6)


## plot by metric
## metric levels
levels(HLBData$Metric) <- c("7 Day Max", "7 Day Mean","7 Day Min", "Mean Daily Temp", "Min Daily Temp",
                            "Max Daily Temp", "Diurnal Temp Rate")

T4 <- ggplot(HLBData, aes(y = TempF, x = Month, group = SiteName, color = SiteName)) +
  # geom_line(aes(y=AirTemp, col = "gray")) +
  geom_line() +
  # geom_line(aes(x=Date, y=AirTemp, color = "Max Daily Air Temp")) +
  facet_wrap(~Metric, scales = "free_x") + ## , scales = "free_x"
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(name="Water Temp (°F)") +
  scale_color_brewer(palette= "Set2") +
  # scale_x_date(date_labels = "%b %Y") +
  theme_linedraw() 


T4

file.name1 <- paste0( "Figures/07_HTB_Temp_by_Metric.jpg")
ggsave(T4, filename=file.name1, dpi=300, height=5, width=6)


# SMC biosite continuous data ---------------------------------------------

## upload continuous data for socal from smc
bios <- read.csv("ignore/Temperature/socal_temp_timeseries_data_comid.csv") %>%
  select(masterid, latitude, longitude, county, result, sampledate, comid) %>% ## only needed columns
  rename(TempC = result, Comid = comid) %>%
  filter(county %in% c("Los Angeles")) %>% ## filter to LA
  separate(sampledate, into = c("Date", "Time"), sep = " ", remove = F) %>% ## separate dateime
  separate(Date, into = c("Month", "Day", "Year" ), sep = "/") %>% ## separate date
  # mutate(Year = paste0("20", Year))  %>%## change date from 23 to 2023
  mutate(Source = "SMC")

head(bios)

unique(bios$Month)

## years of data

years <- bios %>%
  select(masterid, Month, Year) %>%
  distinct() 


unique(bios$county)

## summarise daily statistics
dfSMC <- bios  %>%
  group_by(masterid, Year, Month, Day, Comid, latitude, longitude) %>%
  summarise(MeanTemp = mean(TempC),
            MinTemp = min(TempC),
            MaxTemp = max(TempC))
dfSMC


## calculate rolling min and max
dfs <- dfSMC %>% 
  ungroup() %>%
  group_by(masterid, Year, Comid,latitude, longitude) %>%
  mutate(max_07da = zoo::rollmean(MaxTemp, k=7, fill = NA)) %>% ## rolling daily max
  mutate(mean_07da = zoo::rollmean(MeanTemp, k=7, fill = NA)) %>% ## rolling daily mean
  mutate(min_07da = zoo::rollmean(MinTemp, k=7, fill = NA)) %>% ## rolling daily min
  mutate(DTR = MaxTemp - MinTemp) ## diurnal temp rate

dfs

df_sumsSMC <- dfs %>%
  # mutate(SiteName = factor(siteName, levels = c("Burbank", "Rattlesnake", "Benedict", "Steelhead", "Riverfront",
  #                                               "Compton", "Willow"))) %>%
  pivot_longer(MeanTemp:DTR, names_to = "Metric", values_to= "Values")%>%
  group_by(masterid, Metric, Year, Comid,latitude, longitude, Month) %>%
  summarise(Temp = mean(na.omit(Values))) %>%
  # mutate(Month = factor(Month, levels = c(01,02,03,04,05,06,07,08,09,10,11)),
  #        Metric = factor(Metric, levels = c("max_07da", "mean_07da", "min_07da", "MeanTemp", "MinTemp", "MaxTemp", "DTR"))) %>%
  mutate(TempF =  (Temp*1.8)+32) %>%
  drop_na(TempF) %>%
  rename(SiteName = masterid) %>%
  mutate(Source = "SMC")
# pivot_wider(names_from = Metric, values_from = meanvals)

df_sumsSMC
str(df_sumsSMC)



## plot by site
T5 <- ggplot(df_sumsSMC, aes(y = TempF, x = Month, group = Metric, color = Metric)) +
  # geom_line(aes(y=AirTemp, col = "gray")) +
  geom_line() +
  # geom_line(aes(x=Date, y=AirTemp, color = "Max Daily Air Temp")) +
  facet_wrap(~SiteName) + ## , scales = "free_x"
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(name="Water Temp (°F)") +
  scale_color_brewer(palette= "Set2",
                     labels = c("7 Day Max", "7 Day Mean", "7 Day Min","Mean Daily Temp", "Min Daily Temp",
                                "Max Daily Temp", "Diurnal Temp Rate", "Max Daily Air Temp")) +
  # scale_x_date(date_labels = "%b %Y") +
  theme_linedraw() 


T5

file.name1 <- paste0( "Figures/07_SMC_Temp_by_Site.jpg")
ggsave(T5, filename=file.name1, dpi=300, height=5, width=6)


# Join Temp data together -------------------------------------------------

## join sccwrp and HTB data together 

names(HLBData)
names(df_sums)
names(df_sumsSMC)

## change sitename to match HTB, and add source
df_sums <- df_sums %>%
  rename(SiteName = sitename) %>%
  mutate(Source = "SCCWRP")

## add source to HLB, change year to charater to match sccwrp data
HLBData <- HLBData %>% mutate(Source = "HealTheBay") %>%
  mutate(Year = as.character(Year))

## join rows
allTemp <- bind_rows(df_sums, HLBData)

head(allTemp)

## check sites
unique(allTemp$SiteName)

## check metrics
unique(allTemp$Metric)

## upload comids

comids <- read.csv("ignore/Temperature/sites_need_comid.csv") %>%
  ## change names to match data
  mutate(SiteName = case_when(Site.Name == "Willow Street" ~ "Willow",
                              Site.Name == "Riverfront Park" ~ "Riverfront",
                              Site.Name == "Lower Compton Creek Site" ~ "Compton",
                              Site.Name == "Steelhead Park" ~ "Steelhead",
                              Site.Name == "Benedict Street" ~ "Benedict",
                              Site.Name == "Rattlesnake Park" ~ "Rattlesnake",
                              Site.Name == "Burbank Blvd" ~ "Burbank",
                              Site.Name == "Big Dalton " ~ "Big Dalton ",
                              Site.Name == "Rainbow Canyon Ranch " ~ "Rainbow Canyon Ranch ",
                              Site.Name == "San Dimas Walnut Creek " ~ "San Dimas Walnut Creek ",
                              Site.Name == "West Fork" ~ "West Fork")) %>%
  drop_na(SiteName)
comids

## join with temp data

allTempComs <- full_join(allTemp, comids, by = c( "SiteName"), relationship = "many-to-many") %>%
  select(-Site.Name) %>%
  rename(latitude = Latitude, longitude = Longitude)

allTempComs

## join with SMC data
names(allTempComs)
names(df_sumsSMC)

allTempComs <- bind_rows(allTempComs, df_sumsSMC)


# Bio Data ----------------------------------------------------------------

## bugs - sites only

bugSites <- st_read("ignore/output/01_bio_sites_all.shp")
head(bugSites)

## bugs data

csciScores <- read.csv("ignore/output/01_csci_comp_mets_comid_socal.csv")
head(csciScores)

### algae scores

asciScores <- read.csv("ignore/output/01_asci_comp_mets_comid_socal.csv")
head(asciScores)

## how many csci sites match comids with temp data

sum(unique(csciScores$COMID1) %in% allTempComs$Comid) # 8

## which ones?

indC <- which(csciScores$COMID1 %in% allTempComs$Comid)
indC

## extract from df

comscscsi <- csciScores[indC,] ## filter to rows with matching comids
sitesC <- unique(comscscsi$masterid) ## define sites to filter
sitesC
### match sites with smc data
# sum(unique(df_sumsSMC$SiteName) %in% unique(csciScores$masterid)) ## 13
indca <- which(csciScores$masterid %in% unique(df_sumsSMC$SiteName))
comscscsiSMC <- csciScores[indca,]
sitesCa <- unique(comscscsiSMC$masterid)

## join with other sites

sitesC <- c(sitesC, sitesCa)
sitesC

## filter csci scores for matching sites
names(csciScores)
matchedSitesC <- csciScores %>%
  filter(masterid %in% sitesC, Metric == "csci") %>%
  select(masterid, sampledate, fieldreplicate, Metric, MetricValue, COMID1, county) %>% ## remove some columns
  separate(sampledate, into = c("sampledate", "Time"), sep= "T", remove = F) %>% ## get year etc from date
  separate(sampledate, into = c("year", "Month", "Day"), sep= "-", remove = F) %>%
  rename(replicate = fieldreplicate)

matchedSitesC

## how many asci sites match comids with temp data

sum(unique(asciScores$COMID1) %in% allTempComs$Comid) # 5

## which ones?

indA <- which(asciScores$COMID1 %in% allTempComs$Comid)
indA

## extract from df

comsascsi <- asciScores[indA,] ## filter to rows with matching comids
sitesA <- unique(comsascsi$masterid) ## define sites to filter

### match sites with smc data
# sum(unique(df_sumsSMC$SiteName) %in% unique(csciScores$masterid)) ## 13
indAa <- which(asciScores$masterid %in% unique(df_sumsSMC$SiteName))
comsascsiSMC <- asciScores[indAa,]
sitesAa <- unique(comsascsiSMC$masterid)

## join sites
sitesA <- c(sitesA, sitesAa)
sitesA

## filter csci scores for matching sites
names(asciScores)

matchedSitesA <- asciScores %>%
  filter(masterid %in% sitesA, Metric == "ASCI_Hybrid") %>%
  select(masterid, sampledate, replicate, Metric, MetricValue, COMID1, county) %>% ## remove some columns
  separate(sampledate, into = c("sampledate", "Time"), sep= "T", remove = F) %>% ## get year etc from date
  separate(sampledate, into = c("year", "Month", "Day"), sep= "-", remove = F)

matchedSitesA

## bind csci and asci together

matchedSites <- bind_rows(matchedSitesC, matchedSitesA)

matchedSites

## save
write.csv(matchedSites, "output_data/07_matched_bio_sites_with_validation_temp_data.csv")


# Join Temp to Bio --------------------------------------------------------

head(allTempComs)
head(matchedSites)
length(unique(allTempComs$SiteName)) ##25
length(unique(matchedSites$masterid)) ##44

## join data by comid
data <- full_join(allTempComs, matchedSites, by = c("Comid" = "COMID1"), relationship =
                    "many-to-many") %>%
  drop_na(masterid)

## check if years can be matched
sum(data$Year %in% data$year) ## 0 no years can be matched

## reduce to lookn uop table
names(data)
tempBioLookUp <- data %>%
  ungroup() %>%
  select(SiteName, Source, Comid, year, masterid, Metric.y, MetricValue) %>%
  rename(Metric = Metric.y) %>%
  distinct()
  
tempBioLookUp

# Temp-ecology models -----------------------------------------------------

## filter temp data to Max temp
unique(allTempComs$Metric)
names(allTempComs)

maxTemp <- allTempComs %>%
  select(-c(latitude:longitude)) %>%
  # filter(Metric == "max_07da") %>%
  select(-Metric) %>%
  distinct() %>%
  rename(Max_Wkl_Max_StreamT = TempF) %>% ## change to match model
  drop_na(Max_Wkl_Max_StreamT) 

maxTemp
## define values to predict on
tempvalues <- maxTemp$Max_Wkl_Max_StreamT
tempvalues

### predict probabilities 
## CSCI

## look up table
load(file="ignore/models/03_csci_glm_rsqds.RData")
bio_h_summary

## models
load(file = "ignore/models/03_csci_glm_currentTemp.RData")
log.lm

## which models are max weekly temp?
## modified = 3
## standard = 9

## modified threshold
mod1 <- log.lm[[3]] ## modified model
summary(mod1)

modResults <- predict.glm(mod1, list(CurTemp = tempvalues),  type = "response")

## standard threshold
mod2 <- log.lm[[9]] ## modified model
summary(mod2)

stanResults <- predict.glm(mod2, list(CurTemp = tempvalues),  type = "response")
stanResults

## join with temp

maxTempC <- maxTemp

maxTempC$Modified <- modResults
maxTempC$Standard <- stanResults

## make long and add Metric for csci column

maxTempC <- maxTempC %>%
  pivot_longer(Modified:Standard, names_to = "Type", values_to = "CSCI") #%>%
  # mutate(BioMetric = "CSCI") %>%
  # pivot_wider(names_from = CSCI, values_from = Probability)
maxTempC

### ASCI

## look up table
load(file="ignore/models/03_asci_glm_rsqds.RData")
bio_h_summary

## models
load(file = "ignore/models/03_asci_glm_currentTemp.RData")
log.lm

## modified threshold
mod3 <- log.lm[[3]] ## modified model
summary(mod3)

modResults <- predict.glm(mod3, list(CurTemp = tempvalues),  type = "response")

## standard threshold
mod4 <- log.lm[[9]] ## modified model
summary(mod4)

stanResults <- predict.glm(mod4, list(CurTemp = tempvalues),  type = "response")
stanResults

## join with temp

maxTempA <- maxTemp

maxTempA$Modified <- modResults
maxTempA$Standard <- stanResults

## make long and add Metric for csci column

maxTempA <- maxTempA %>%
  pivot_longer(Modified:Standard, names_to = "Type", values_to = "ASCI") #%>%
  # mutate(BioMetric = "ASCI")
names(maxTempA)

## join csci and asci together

allBio <- full_join(maxTempA, maxTempC, by = c("Metric", "SiteName", "Year",  "Temp", "Max_Wkl_Max_StreamT", "Source", "Comid", "Type"),
                    relationship = "many-to-many")

## join bio scores
tempBioLookUp

allResults <- full_join(allBio, tempBioLookUp, by = c("Comid", "SiteName", "Source"), relationship =
                          "many-to-many") %>%
  rename(BioYear = year, TempYear = Year) %>%
  drop_na(masterid, Max_Wkl_Max_StreamT)

allResults

## save

write.csv(allResults, "output_data/07_predictions_csci_asci_observed_temp.csv")


# Comparison between predictions and obs ----------------------------------

## upload probability and score data, add season, 
allResults <- read.csv("output_data/07_predictions_csci_asci_observed_temp.csv") %>%
  rename(FlowMetric = Metric.x, BioMetric = Metric.y) %>%
  select(-X) %>%
  # mutate(Season = ifelse(Month %in% c(10, 11,12,1,2,3), "Winter", "Summer")) %>%
  group_by(Type, BioMetric) %>%
  mutate(BioThresh = case_when(BioMetric == "csci" & Type == "Modified" ~ 0.6,
                                BioMetric == "csci" & Type == "Standard" ~ 0.79,
                                  BioMetric == "ASCI_Hybrid" & Type == "Modified" ~ 0.75,
                                    BioMetric == "ASCI_Hybrid" & Type == "Standard" ~ 0.86))
allResultsx
## scale the metric values to 1 to compare with probability
## also match probability with bio metric score
allResultsx <- allResults %>% 
  mutate(ScaledScore = ifelse(MetricValue >= BioThresh, BioThresh, MetricValue)) %>%
  group_by(BioMetric, Type) %>%
  mutate(ResultScaled = rescale(ScaledScore, to = c(min(ScaledScore), 1))) %>%
  pivot_longer(ASCI:CSCI, names_to = "Bio", values_to = "Probability") %>% ## make probability wider
  mutate(ProbabilityScaled = rescale(Probability, to = c(0, 1))) %>%
  mutate(BioMetric = ifelse(BioMetric == "csci", "CSCI", "ASCI")) %>%  ## change biometric to match prob names
  mutate(matched = ifelse(BioMetric == Bio, "Yes", "No")) %>% ## check they match
  filter(!matched == "No") ## remove one that don't match

### match years as much as possible. + or - 3 years
range(allResultsx$TempYear) ## 2020 2023
range(allResultsx$BioYear) ## 2005 2020

allResultsSub <- allResultsx %>%
  mutate(YearDiff = TempYear - BioYear) %>% ## get difference between years
  # filter(!YearDiff > 0) %>% ## remove any with greater than 3 years difference
  mutate(YearDiff = as.factor(YearDiff))

allResultsSub

### plot

V1 <- ggplot(allResultsSub, aes(x=ResultScaled, y = Probability, group = Source, colour = Source)) +
  geom_point(size = 2) +
  geom_abline(aes(intercept=0, slope=1)) +
  facet_grid(rows=vars(BioMetric), cols = vars(Type)) +
  scale_x_continuous(name = "Bio Score (Scaled)", limits = c(0,1)) +
  scale_y_continuous(name = "Probability of acheiving bio threshold", limits = c(0,1)) +
  guides(color = "none")
V1

file.name1 <- paste0("Figures/07_annual_observed_v_exp_probs_bio_score_validation.jpg")
ggsave(V1, filename=file.name1, dpi=300, height=6, width=6)

unique(allResultsSub$masterid)

# Spatial  ----------------------------------------------------------------

## bug sites with coords
bugSites <- st_read("ignore/output/01_bio_sites_all.shp")
head(bugSites)

sitesSub <- inner_join(bugSites, allResultsSub, by = "masterid") %>%
  select(masterid:county, SiteName) %>%
  distinct()

sitesSub

st_write(sitesSub, "output_data/07_validation_sites.shp", append = F)

## use ggmap to get google 
library(ggmap)
library("ggsci")

## register google key 
## in notes
imps_sf 
# register_google("")

## get google basemap
basemap <- ggmap(get_googlemap(center = c(lon = -118.5589, lat = 34.35769), zoom=10, maptype = "terrain", color = "color" ))
# ## c("terrain", "satellite", "roadmap", "hybrid")
# ?get_googlemap
print(basemap)

save(basemap, file = "Figures/08_basemap_LA_ggplot.RData")

# Plot per flow metric
## define metrics to loop through


m1 <- basemap + 
  geom_point(data = sitesSub, aes(x = longitude, y = latitude, color = "red", size = 3)) +
  # scale_color_jco(name = "Site") +
  # facet_wrap(~Type) +
  guides(size = "none", color = "none")
m1

file.name1 <- paste0("Figures/07_map_validation_sites.jpg")
ggsave(m1, filename=file.name1, dpi=1000, height=10, width=15)

