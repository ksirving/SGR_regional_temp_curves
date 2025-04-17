## model exploration


library(tidyverse)
library(sf)

library(tidylog)

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


st_write(bugTempSites, "ignore/output/bio_sites_temp_area.shp", append = F)

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

## join comids
# scoresTempSites <- full_join(csciScoresLA, bugTempSites, by = "masterid")
# head(scoresTempSites)

# Join bug sites to temp data ---------------------------------------------

## join by comid and year

scoresTempSites <- csciScoresLA 

AllData <- left_join(scoresTempSites, AllTempAlt, by = c("COMID", "year")) %>% drop_na()
head(AllData)
dim(AllData) ## 6894

test <- AllData %>%
  drop_na(MetricValue)

range(test$year)

## how many sites with temp data
length(unique(test$masterid)) ## 575

## how many comids with sites
length(unique(test$COMID)) ## 397

## save out
write.csv(AllData, "ignore/output/03_bugs_temp_joined_by_year_for_score_figures.csv")


# Models: CSCI ------------------------------------------------------------------
head(AllData)

AllDataLong <- AllData %>%
  filter(!Metric == "count")

## make quadratic terms
AllDataLong2 <- AllDataLong %>%
  # group_by(Variable, Metric) %>%
  mutate(CurTemp2 = CurTemp^2)


## create df of model configurations

## bio 
biol.endpoints<-unique(AllDataLong2$Metric)

## temp
temp.endpoints<- unique(AllDataLong2$CurMetric)

bio_h_summary<-  expand.grid(biol.endpoints=biol.endpoints,temp.endpoints=temp.endpoints, stringsAsFactors = F)

# bio_h_summary <- bio_h_summary[-c(1:9),]
bio_h_summary
i=1
## model of each configuration
quad.lm <-lapply(1:nrow(bio_h_summary), function(i)
{
  
  tmet<-as.character(bio_h_summary[i,"temp.endpoints"])
  bmet<-as.character(bio_h_summary[i,"biol.endpoints"])

  mydat<-AllDataLong2 %>%
    filter(Metric == bmet,
           CurMetric == tmet) %>%
    select(CurTemp,CurTemp2, MetricValue, COMID) %>% ## only metrics needed
    drop_na(CurTemp, MetricValue) %>%
    filter_all(all_vars(!is.infinite(.))) %>% ## remove all missing values
    distinct()
  
  mydat<-mydat[order(mydat$MetricValue),]
  
  lm(MetricValue~CurTemp+CurTemp2,data=mydat)

  #family=Gamma(link = "log")
})
bio_h_summary
quad.lm 
## save models
save(quad.lm, file = "ignore/models/03_csci_component_mods_lm_quad.RData")

## function to get p value from lm 
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

### get rsqds and pvals
for(i in 1:length(quad.lm)) {
  
  mod <- summary(quad.lm[[i]])
  bio_h_summary$RSquared[i] <- mod$r.squared ##1-mod$deviance/mod$null.deviance ## mcfaddens r2
  bio_h_summary$PValue[i] <- lmp(quad.lm[[i]])

}
## save configs and r sqds
save(bio_h_summary, file="ignore/models/03_csci_component_mods_quadLM_rsqds.RData")

## make df of predicted values to predict on - need to be different for each temp metric

## blank df
DF <- NULL
DF <- as.data.frame(DF)

bio_h_summary
i
### get predictions and fitted values
for(i in 1:length(quad.lm)) {
  
  bio <- bio_h_summary[i,"biol.endpoints"]
  temp <- bio_h_summary[i,"temp.endpoints"]
  
  data <- AllDataLong2 %>%
    filter(Metric == bio,
           CurMetric == temp) %>%
    select(CurTemp, MetricValue, COMID) %>% ## only metrics needed
    drop_na(CurTemp, MetricValue) %>%
    filter_all(all_vars(!is.infinite(.))) %>% ## remove all missing values
    distinct()
  
  
  ## new data - plus and minus 10% as a start
  tempvalues <- seq((range(data$CurTemp)[1])*0.9,(range(data$CurTemp)[2])*1.1,0.05)

  ## get model, predict, extract all data and categories
  mod <- quad.lm[[i]]
  predictedVals <- predict.glm(mod,list(CurTemp = tempvalues, CurTemp2 = tempvalues^2),  type = "response")
  DFX <- as.data.frame(predictedVals)
  DFX$Value <- tempvalues
  DFX$Bio <- bio
  DFX$Variable <- temp
  DFX$MinVal <-  range(data$CurTemp)[1]
  DFX$MaxVal <-  range(data$CurTemp)[2]

  DF <- bind_rows(DF, DFX)
  
}
DF
# plot(DF$predictedVals, DF$BioVals)

### predicted figures

m=1
mets <- unique(DF$Bio)

## facet labels
supp.labs <- c(
  "Max_Wkl_Max_StreamT_grt_30_"="Weeks greater than 86F",
  "Max_Wkly_Mean_StreamT" = "Max Weekly Mean",
  "Max_Wkl_Max_StreamT" = "Weekly Maximum",
  "Min_Wkl_Min_StreamT" = "Weekly Minimum",
  "Max_Wkl_Rng_StreamT" = "Max Weekly Range",
  "Mean_Wkl_Rng_StreamT" =  "Av Weekly Range" 
)

for(m in 1:length(mets)) {
  
  T1 <- ggplot(subset(DF, Bio == mets[m]), aes(y=predictedVals, x=Value, group = Variable, color = Variable)) +
    geom_line(linewidth=0.5) +
    # stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1)+
    # geom_vline(xintercept = 86, linetype="dashed", 
    #            color = "red", linewidth=0.5, show.legend = T) +
    geom_vline(xintercept = 80, linetype="dashed",
               color = "blue", linewidth=0.5, show.legend = T) +
    # geom_hline(yintercept = 0.79) +
    facet_wrap(~Variable, labeller = labeller(supp.labs),
               scales = "free") +
    scale_x_continuous(name="Water Temp (°F)") +
    scale_y_continuous(name = paste(mets[m])) +
    theme(legend.position = "none")
  
  T1
  
  file.name1 <- paste0(out.dir, "03_", mets[m], "_csci_temp_response_predicted.jpg")
  ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)
}


# Figures -----------------------------------------------------------------
head( AllDataLong2)
## format for figures, make temp vars long

AllDataLong <- AllDataLong2 %>%
  rename(Variable = CurMetric, Value = CurTemp) %>%
  filter(!Metric == "count")

## facet labels
supp.labs <- c(
  "Max_Wkl_Max_StreamT_grt_30_"="Weeks greater than 86F",
  "Max_Wkly_Mean_StreamT" = "Max Weekly Mean",
  "Max_Wkl_Max_StreamT" = "Weekly Maximum",
  "Min_Wkl_Min_StreamT" = "Weekly Minimum",
  "Max_Wkl_Rng_StreamT" = "Max Weekly Range",
  "Mean_Wkl_Rng_StreamT" =  "Av Weekly Range" 
)

mets <- unique(AllDataLong$Metric)
mets

for(m in 1:length(mets)) {
  
  T1 <- (ggplot(subset(AllDataLong, Metric == mets[m]), aes(y=MetricValue, x=Value, group = Variable, color = Variable)) +
    # geom_point(size=0.2) +
    geom_line(data=subset(DF, Bio == mets[m]), aes(y=predictedVals, x=Value, group = Variable, color = Variable), linewidth = 1)+
    # stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1)+
    # geom_hline(yintercept = 0.79,  linetype="dashed", linewidth=0.5, color = "blue") +
    #   geom_hline(yintercept = 0.6,  linetype="dashed", linewidth=0.5, color = "red") +
    facet_wrap(~Variable, labeller = as_labeller(supp.labs),
               scales = "free_x") +
      # geom_vline(data=filter(AllDataLong, !Variable %in% c("Max_Wkl_Max_StreamT_grt_30_","Max_Wkl_Rng_StreamT", "Mean_Wkl_Rng_StreamT")), 
      #            aes(xintercept = 86), linetype="dashed", color = "red", linewidth=0.5, show.legend = T) +
      # geom_vline(data=filter(AllDataLong, !Variable %in% c("Max_Wkl_Max_StreamT_grt_30_","Max_Wkl_Rng_StreamT", "Mean_Wkl_Rng_StreamT")), 
      #            aes(xintercept = 80), linetype="dashed", color = "grey50", linewidth=0.5, show.legend = T) +
    scale_x_continuous(name="Water Temp (°F)") +
    scale_y_continuous(name = paste(mets[m])) +
    theme(legend.position = "none"))
  
  T1
  
  file.name1 <- paste0(out.dir, "03_", mets[m], "_csci_temp_response_quadLM_modelled_simple.jpg")
  ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)
}

## figures of component metric by temp metric

mets <- unique(AllDataLong$Variable)
mets
m=1
names(AllDataLong)

for(m in 1:length(mets)) {
  
  T1 <- ggplot(subset(AllDataLong, Variable == mets[m]), aes(y=MetricValue, x=Value, group = Metric, color = Metric)) +
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
  
  file.name1 <- paste0(out.dir, "03_", mets[m], "_csci_temp_response_GAMs_modelled.jpg")
  ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)
}

### issues - 
## only one temp value per year, need seasonal
## can we get other metrics?


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


# ASCI component metrics figures -------------------------------------------

head(asciScoresLA)
## filter bug data using masterid ### remove reps - remove 2nd rep for now, change later!!!!
asciScoresLA <- asciScores %>%
  select(-X, -stationcode) %>%
  filter(masterid %in% bugTempSites$masterid,replicate == 1 ) %>%
  separate(sampledate, into = c("sampledate", "Time"), sep= "T", remove = F) %>%
  separate(sampledate, into = c("year", "Month", "Day"), sep= "-", remove = F) %>%
  mutate(year = as.numeric(year))

length(unique(asciScoresLA$masterid)) ## 466 sites in LA region with temp

## join comids
# scoresTempSites <- full_join(csciScoresLA, bugTempSites, by = "masterid")
# head(scoresTempSites)

# Join algae sites to temp data ---------------------------------------------

## join by comid and year

scoresTempSites <- asciScoresLA 

str(baseline_stream_temp)

AllData <- left_join(scoresTempSites, baseline_stream_temp, by = c("COMID", "year")) %>% drop_na()
head(AllData)
dim(AllData) 

## save out
write.csv(AllData, "ignore/output/03_algae_temp_joined_by_year.csv")


# Models: ASCI ------------------------------------------------------------

AllData <- read.csv("ignore/output/03_algae_temp_joined_by_year.csv")
head(AllData)

test1 <- AllData %>%
  drop_na(MetricValue)

## how many sites with temp data?

length(unique(test1$masterid))

AllDataLong <- AllData %>%
  pivot_longer(Max_Wkl_Max_StreamT_grt_30_:Mean_Wkl_Rng_StreamT, names_to = "Variable", values_to = "Value") %>%
  # filter(!Variable == "Max_Wkl_Max_StreamT_grt_30_") %>%
  filter(!Metric == "count")

## make quadratic terms
AllDataLong2 <- AllDataLong %>%
  group_by(Variable, Metric) %>%
  mutate(Value2 = Value^2)

## create df of model configurations

## bio 
biol.endpoints<-unique(AllDataLong2$Metric)

## hydro
temp.endpoints<- unique(AllDataLong2$Variable)

bio_h_summary<-  expand.grid(biol.endpoints=biol.endpoints,temp.endpoints=temp.endpoints, stringsAsFactors = F)

# bio_h_summary <- bio_h_summary[-c(1:9),]
bio_h_summary

## model of each configuration
quad.lm <-lapply(1:nrow(bio_h_summary), function(i)
{
  
  tmet<-as.character(bio_h_summary[i,"temp.endpoints"])
  bmet<-as.character(bio_h_summary[i,"biol.endpoints"])
  
  mydat<-AllDataLong2 %>%
    filter(Metric == bmet,
           Variable == tmet)
  
  # mydat <- mydat[which(mydat$hydro<0 ),]
  
  # mydat$Condition<-ifelse(mydat$bio< bmet.thresh,0,1)
  mydat<-mydat[order(mydat$MetricValue),]
  
  lm(MetricValue~Value+Value2,data=mydat)
  
  #family=Gamma(link = "log")
})

bio_h_summary
quad.lm 
## save models
save(quad.lm, file = "ignore/models/03_asci_component_mods_lm_quad.RData")

## function to get p value from lm 
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

### get rsqds and pvals
for(i in 1:length(quad.lm)) {
  
  mod <- summary(quad.lm[[i]])
  bio_h_summary$RSquared[i] <- mod$r.squared ##1-mod$deviance/mod$null.deviance ## mcfaddens r2
  bio_h_summary$PValue[i] <- lmp(quad.lm[[i]])
  
}
## save configs and r sqds
save(bio_h_summary, file="ignore/models/03_asci_component_mods_quadLM_rsqds.RData")

## make df of predicted values to predict on - need to be different for each temp metric

## blank df
DF <- NULL
DF <- as.data.frame(DF)

bio_h_summary
i
### get predictions and fitted values
for(i in 1:length(quad.lm)) {
  
  bio <- bio_h_summary[i,"biol.endpoints"]
  temp <- bio_h_summary[i,"temp.endpoints"]
  
  data <- AllDataLong2 %>%
    filter(Metric == bio,
           Variable == temp)
  
  ## new data - plus and minus 10% as a start
  tempvalues <- seq((range(data$Value)[1])*0.9,(range(data$Value)[2])*1.1,0.05)
  
  ## get model, predict, extract all data and categories
  mod <- quad.lm[[i]]
  predictedVals <- predict.glm(mod,list(Value = tempvalues, Value2 = tempvalues^2),  type = "response")
  DFX <- as.data.frame(predictedVals)
  DFX$Value <- tempvalues
  DFX$Bio <- bio
  DFX$Variable <- temp
  DFX$MinVal <-  range(data$Value)[1]
  DFX$MaxVal <-  range(data$Value)[2]
  
  DF <- bind_rows(DF, DFX)
  
}

# plot(DF$predictedVals, DF$BioVals)

### predicted figures

m=1
mets <- unique(DF$Bio)
mets
## facet labels
supp.labs <- c(
  "Max_Wkl_Max_StreamT_grt_30_"="Weeks greater than 86F",
  "Max_Wkly_Mean_StreamT" = "Max Weekly Mean",
  "Max_Wkl_Max_StreamT" = "Weekly Maximum",
  "Min_Wkl_Min_StreamT" = "Weekly Minimum",
  "Max_Wkl_Rng_StreamT" = "Max Weekly Range",
  "Mean_Wkl_Rng_StreamT" =  "Av Weekly Range" 
)

for(m in 1:length(mets)) {
  
  T1 <- ggplot(subset(DF, Bio == mets[m]), aes(y=predictedVals, x=Value, group = Variable, color = Variable)) +
    geom_line(linewidth=0.5) +
    # stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1)+
    geom_vline(xintercept = 86, linetype="dashed", 
               color = "red", linewidth=0.5, show.legend = T) +
    geom_vline(xintercept = 80, linetype="dashed",
               color = "blue", linewidth=0.5, show.legend = T) +
    # geom_hline(yintercept = 0.79) +
    facet_wrap(~Variable, labeller = labeller(supp.labs),
               scales = "free") +
    scale_x_continuous(name="Water Temp (°F)") +
    scale_y_continuous(name = paste(mets[m])) +
    theme(legend.position = "none")
  
  T1
  
  file.name1 <- paste0(out.dir, "03_", mets[m], "_asci_temp_response_predicted.jpg")
  ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)
}


# Figures -----------------------------------------------------------------

## format for figures, make temp vars long

AllDataLong <- AllData %>%
  pivot_longer(Max_Wkl_Max_StreamT_grt_30_:Mean_Wkl_Rng_StreamT, names_to = "Variable", values_to = "Value") %>%
  # filter(!Variable == "Max_Wkl_Max_StreamT_grt_30_") %>%
  filter(!Metric == "NumberTaxa_Hybrid")

## facet labels
supp.labs <- c(
  "Max_Wkl_Max_StreamT_grt_30_"="Weeks greater than 86F",
  "Max_Wkly_Mean_StreamT" = "Max Weekly Mean",
  "Max_Wkl_Max_StreamT" = "Weekly Maximum",
  "Min_Wkl_Min_StreamT" = "Weekly Minimum",
  "Max_Wkl_Rng_StreamT" = "Max Weekly Range",
  "Mean_Wkl_Rng_StreamT" =  "Av Weekly Range" 
)

mets <- unique(AllDataLong$Metric)
mets

for(m in 1:length(mets)) {
  
  T1 <- (ggplot(subset(AllDataLong, Metric == mets[m]), aes(y=MetricValue, x=Value, group = Variable, color = Variable)) +
           geom_point(size=0.2) +
           geom_line(data=subset(DF, Bio == mets[m]), aes(y=predictedVals, x=Value, group = Variable, color = Variable), linewidth = 1)+
           # stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1)+
           # geom_hline(yintercept = 0.86,  linetype="dashed", linewidth=0.5, color = "grey50") +
           facet_wrap(~Variable, labeller = as_labeller(supp.labs),
                      scales = "free") +
           geom_hline(yintercept = 0.86,  linetype="dashed", linewidth=0.5, color = "blue") +
           geom_hline(yintercept = 0.75,  linetype="dashed", linewidth=0.5, color = "red") +
           # geom_vline(data=filter(AllDataLong, !Variable %in% c("Max_Wkl_Max_StreamT_grt_30_","Max_Wkl_Rng_StreamT", "Mean_Wkl_Rng_StreamT")), 
           #            aes(xintercept = 86), linetype="dashed", color = "red", linewidth=0.5, show.legend = T) +
           geom_vline(data=filter(AllDataLong, !Variable %in% c("Max_Wkl_Max_StreamT_grt_30_","Max_Wkl_Rng_StreamT", "Mean_Wkl_Rng_StreamT")), 
                      aes(xintercept = 80), linetype="dashed", color = "grey50", linewidth=0.5, show.legend = T) +
           scale_x_continuous(name="Water Temp (°F)") +
           scale_y_continuous(name = paste(mets[m])) +
           theme(legend.position = "none"))
  
  T1
  
  file.name1 <- paste0(out.dir, "03_", mets[m], "_asci_temp_response_quadLM_modelled.jpg")
  ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)
}


## figures of component metric by temp metric

mets <- unique(AllDataLong$Variable)
mets
m=1
names(AllDataLong)

for(m in 1:length(mets)) {
  
  T1 <- ggplot(subset(AllDataLong, Variable == mets[m]), aes(y=MetricValue, x=Value, group = Metric, color = Metric)) +
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
  
  file.name1 <- paste0(out.dir, "03_", mets[m], "_asci_temp_response_GAMs_modelled.jpg")
  ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)
}

### issues - 
## only one temp value per year, need seasonal
## can we get other metrics?


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
head(asciScoresLA)

## check matching sampledates

sum(unique(asciScoresLA$sampledate) %in% unique(tempObsAv$sampledate)) ## 246

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


