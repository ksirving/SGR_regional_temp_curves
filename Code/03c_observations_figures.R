## observation data

library(tidylog)
library(tidyverse)
library(sf)


# Bio data ----------------------------------------------------------------

out.dir <- "Figures/"
## workflow
## join csci with temp data, match years
## models with tolerant/senstivie taxa
## csci scores
## functional traits
## component metrics

## LA county temp data

load(file = "ignore/output/02_cur_temp_alt_mets.RData")

head(AllTempAlt)

## get comids
TempSites <- unique(AllTempAlt$COMID)
length(TempSites)

## bugs - sites only

bugSites <- st_read("ignore/output/01_bio_sites_all.shp")
head(bugSites)

## bugs data

csciScores <- read.csv("ignore/output/01_csci_comp_mets_comid_socal.csv")
head(csciScores)

### algae scores

asciScores <- read.csv("ignore/output/01_asci_comp_mets_comid_socal.csv")
head(asciScores)


# Filter sites to LA temp region --------------------------------------------------

## how many temp in bug sites
sum(TempSites %in% unique(bugSites$COMID)) ## 478

## filter bug sites to temp sites
bugTempSites <- bugSites %>%
  filter(COMID %in% TempSites)
bugTempSites

## how many sites with scores?
sum(unique(csciScores$masterid) %in% unique(bugTempSites$masterid)) ## 705

## format scores
# csciScores <- csciScores %>%
#   select(-X) %>%
#   rename(masterid = stationcode)

head(csciScores)

## filter bug data using masterid ### remove reps - remove 2nd rep for now, change later!!!!
csciScoresLA <- csciScores %>%
  select(-X, -stationcode) %>%
  filter(masterid %in% bugTempSites$masterid, fieldreplicate == 1 ) %>%
  separate(sampledate, into = c("sampledate", "Time"), sep= "T", remove = F) %>%
  separate(sampledate, into = c("year", "Month", "Day"), sep= "-", remove = F) %>%
  mutate(year = as.numeric(year))

length(unique(csciScoresLA$masterid)) ## 705 sites in LA region with temp



# Bioassessment data ------------------------------------------------------

tempObs <- read.csv("ignore/smc_temp_loggers.csv")
head(tempObs)

## convert to fahrenheit
tempObs <- tempObs %>%
  mutate(temperature_degF = (temperature_degc*1.8)+32) 


## summary stats per site, separate datetime to create sampledate to match csci scores
tempObsAv <- tempObs %>%
  # rename(sampledate = sampledatetime) %>%
  separate(sampledatetime, into = c("sampledate", "Time"), sep= "T", remove = F) %>%
  # mutate(year = as.numeric(year)) %>%
  group_by(masterid, sampledate) %>%
  summarise(MedTemp = median(temperature_degF),
            MinTemp = min(temperature_degF),
            MaxTemp = max(temperature_degF),
            MeanTemp = mean(temperature_degF)) %>%
  mutate(RgTemp = MaxTemp-MinTemp)

head(tempObsAv)
head(csciScoresLA)

## check matching sampledates

sum(unique(csciScoresLA$sampledate) %in% unique(tempObsAv$sampledate)) ## 288

## join with bug data, join by date and masterid. no sites match with date, . wtf??

obsData <- inner_join(tempObsAv, csciScoresLA, by = c("masterid", "sampledate"))
## join only working for one site

## make temp metrics long for figures
obsDataLong <- obsData %>%
  pivot_longer(MedTemp:RgTemp, names_to = "VariableObs", values_to = "ValueObs") %>%
  filter(!Metric == "count")

mets <- unique(obsDataLong$Metric)
mets
names(obsDataLong)
m=1
## figures of temp metric by component metric

for(m in 1:length(mets)) {
  
  T1 <- ggplot(subset(obsDataLong, Metric == mets[m]), aes(y=MetricValue, x=ValueObs, group = VariableObs, color = VariableObs)) +
    geom_point(size=0.2) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1)+
    # geom_vline(xintercept = 86, linetype="dashed", 
    #            color = "red", linewidth=0.5, show.legend = T) +
    geom_vline(xintercept = 80, linetype="dashed",
               color = "gray", linewidth=0.5, show.legend = T) +
    # geom_hline(yintercept = 0.79) +
    facet_wrap(~VariableObs,
               scales = "free_x") +
    scale_x_continuous(name="Water Temp (°F)") +
    scale_y_continuous(name = paste(mets[m])) +
    theme(legend.position = "none")
  
  T1
  
  file.name1 <- paste0(out.dir, "03_", mets[m], "_csci_temp_response_GAMs_observed.jpg")
  ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)
}


## figures of component metric by temp metric

mets <- unique(obsDataLong$VariableObs)
mets
m=1
names(obsDataLong)

for(m in 1:length(mets)) {
  
  T1 <- ggplot(subset(obsDataLong, VariableObs == mets[m]), aes(y=MetricValue, x=ValueObs, group = Metric, color = Metric)) +
    geom_point(size=0.2) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1)+
    geom_vline(xintercept = 86, linetype="dashed", 
               color = "red", linewidth=0.5, show.legend = T) +
    geom_vline(xintercept = 80, linetype="dashed",
               color = "blue", linewidth=0.5, show.legend = T) +
    # geom_hline(yintercept = 0.79) +
    facet_wrap(~Metric,
               scales = "free") +
    scale_x_continuous(name=paste(mets[m])) +
    scale_y_continuous(name = "Metric Value") +
    theme(legend.position = "none")
  
  T1
  
  file.name1 <- paste0(out.dir, "03_", mets[m], "_csci_temp_response_GAMs_observed.jpg")
  ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)
}


# Individual figures ------------------------------------------------------

## get only CSCI
obsDataLong <- obsDataLong %>%
  filter(Metric == "csci")

unique(obsDataLong$Metric)

mets <- unique(obsDataLong$VariableObs)
mets
m=1
names(obsDataLong)

for(m in 1:length(mets)) {
  
  T1 <- ggplot(subset(obsDataLong, VariableObs == mets[m]), aes(y=MetricValue, x=ValueObs, group = Metric, color = Metric)) +
    geom_point(size=0.2) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1)+
    # geom_vline(xintercept = 86, linetype="dashed", 
    #            color = "red", linewidth=0.5, show.legend = T) +
    geom_vline(xintercept = 80, linetype="dashed",
               color = "grey", linewidth=0.5, show.legend = T) +
    # geom_hline(yintercept = 0.79) +
    # facet_wrap(~Metric,
    #            scales = "free") +
    scale_x_continuous(name=paste(mets[m])) +
    scale_y_continuous(name = "CSCI Score") +
    theme(legend.position = "none")
  
  T1
  
  file.name1 <- paste0(out.dir, "03a_", mets[m], "_csci_ONLY_temp_response_observed.jpg")
  ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)
}

length(unique(obsDataLong$masterid)) #22


# ASCI: Observed Data -----------------------------------------------------

## filter asci data using masterid ### remove reps - remove 2nd rep for now, change later!!!!
asciScoresLA <- asciScores %>%
  select(-X, -stationcode) %>%
  filter(masterid %in% bugTempSites$masterid,replicate == 1 ) %>%
  separate(sampledate, into = c("sampledate", "Time"), sep= "T", remove = F) %>%
  separate(sampledate, into = c("year", "Month", "Day"), sep= "-", remove = F) %>%
  mutate(year = as.numeric(year))

length(unique(asciScoresLA$masterid)) ## 466 sites in LA region with temp

## join with bug data, join by date and masterid. no sites match with date, . wtf??

obsData <- inner_join(tempObsAv, asciScoresLA, by = c("masterid", "sampledate"))
## join only working for one site

## make temp metrics long for figures
obsDataLong <- obsData %>%
  pivot_longer(MedTemp:RgTemp, names_to = "VariableObs", values_to = "ValueObs") %>%
  filter(!Metric == "count")

mets <- unique(obsDataLong$Metric)
mets
names(obsDataLong)

## figures of temp metric by component metric

for(m in 1:length(mets)) {
  
  T1 <- ggplot(subset(obsDataLong, Metric == mets[m]), aes(y=MetricValue, x=ValueObs, group = VariableObs, color = VariableObs)) +
    geom_point(size=0.2) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1)+
    geom_vline(xintercept = 86, linetype="dashed", 
               color = "red", linewidth=0.5, show.legend = T) +
    geom_vline(xintercept = 80, linetype="dashed",
               color = "blue", linewidth=0.5, show.legend = T) +
    # geom_hline(yintercept = 0.79) +
    facet_wrap(~VariableObs,
               scales = "free_x") +
    scale_x_continuous(name="Water Temp (°F)") +
    scale_y_continuous(name = paste(mets[m])) +
    theme(legend.position = "none")
  
  T1
  
  file.name1 <- paste0(out.dir, "03_", mets[m], "_asci_temp_response_GAMs_observed.jpg")
  ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)
}


## figures of component metric by temp metric

mets <- unique(obsDataLong$VariableObs)
mets
m=1
names(obsDataLong)

for(m in 1:length(mets)) {
  
  T1 <- ggplot(subset(obsDataLong, VariableObs == mets[m]), aes(y=MetricValue, x=ValueObs, group = Metric, color = Metric)) +
    geom_point(size=0.2) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1)+
    geom_vline(xintercept = 86, linetype="dashed", 
               color = "red", linewidth=0.5, show.legend = T) +
    geom_vline(xintercept = 80, linetype="dashed",
               color = "blue", linewidth=0.5, show.legend = T) +
    # geom_hline(yintercept = 0.79) +
    facet_wrap(~Metric,
               scales = "free") +
    scale_x_continuous(name=paste(mets[m])) +
    scale_y_continuous(name = "Metric Value") +
    theme(legend.position = "none")
  
  T1
  
  file.name1 <- paste0(out.dir, "03_", mets[m], "_asci_temp_response_GAMs_observed.jpg")
  ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)
}

# Individual figures ------------------------------------------------------

## get only ASCI_Hybrid
obsDataLong <- obsDataLong %>%
  filter(Metric == "ASCI_Hybrid")

unique(obsDataLong$Metric)

mets <- unique(obsDataLong$VariableObs)
mets
m=1
names(obsDataLong)

for(m in 1:length(mets)) {
  
  T1 <- ggplot(subset(obsDataLong, VariableObs == mets[m]), aes(y=MetricValue, x=ValueObs, group = Metric, color = Metric)) +
    geom_point(size=0.2) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1)+
    # geom_vline(xintercept = 86, linetype="dashed", 
    #            color = "red", linewidth=0.5, show.legend = T) +
    geom_vline(xintercept = 80, linetype="dashed",
               color = "grey", linewidth=0.5, show.legend = T) +
    # geom_hline(yintercept = 0.79) +
    # facet_wrap(~Metric,
    #            scales = "free") +
    scale_x_continuous(name=paste(mets[m])) +
    scale_y_continuous(name = "ASCI Score") +
    theme(legend.position = "none")
  
  T1
  
  file.name1 <- paste0(out.dir, "03_", mets[m], "_asci_ONLY_temp_response_observed.jpg")
  ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)
}

length(unique(obsDataLong$masterid)) #22




