## model exploration

library(tidylog)
library(tidyverse)
library(sf)

out.dir <- "/Users/katieirving/OneDrive - SCCWRP/Documents - Katie’s MacBook Pro/git/SGR_Temp_Benthic_v2/Figures/"

# Temp data ---------------------------------------------------------------

## modelled current data
load("/Users/katieirving/OneDrive - SCCWRP/Documents - Katie’s MacBook Pro/Projects/San_Gabriel_Temp/Data/AirTemp/Modeling/baseline_stream_temp.RData")

# baseline_stream_tempx <- baseline_stream_temp %>% 
#   ungroup() %>%
#   pivot_longer(Max_Wkl_Max_StreamT:Min_Wkl_Min_StreamT, names_to = "Mets", values_to = "Vals") %>%
#   group_by(COMID, year) %>%
#   mutate(MedAnn = median(Vals)) %>%
#   pivot_wider(names_from = Mets, values_from = Vals )

## ungroup as old object, convert to fahrenheit
baseline_stream_temp <- baseline_stream_temp %>% ungroup() %>%
  pivot_longer(Max_Wkly_Mean_StreamT:Mean_Wkl_Rng_StreamT, names_to = "Mets", values_to = "Vals") %>%
  mutate(ValsF = (Vals*1.8)+32) %>% 
  select(-Vals) %>%
  pivot_wider(names_from = Mets, values_from = ValsF)
head(baseline_stream_temp)

# filter(baseline_stream_temp, COMID == "410CE0284")

# alteration

load(file = "ignore/02_cur_ref_temps_not_matching_metrics_alteration.RData")
head(AllTempAlt)

## convert to fahrenheit
AllTempAlt <- AllTempAlt %>%
  mutate(CurTemp = (CurTemp*1.8)+32,
         MedRefTemp = (MedRefTemp*1.8)+32,
         AltTemp = (AltTemp*1.8)+32) 

head(AllTempAlt)

# Capture probabilities ---------------------------------------------------

## capture probs
load(file="ignore/SMC_cap_prob_cali.csv")
head(oe_ca)
dim(oe_ca)

speciesList <- unique(oe_ca$otu)

write.csv(speciesList, "output_data/03_csci_species_list.csv")
## bug data

bugs2 <- st_read("output_data/01_bio_sites_all.shp")
head(bugs2)
dim(bugs2)

## format capture probability

capLAC <- oe_ca %>%
  # filter(masterid %in% bugs2$masterid) %>%
  rename(TAXON = otu) %>%
  drop_na(captureprob)
dim(capLAC)

# Traits ------------------------------------------------------------------

## uplaod traits EPA

traitsUSA <- read.csv("/Users/katieirving/OneDrive - SCCWRP/Documents - Katie’s MacBook Pro/git/SGR_Temp_Benthic/ignore/FreshwaterBioTraits_20100927.csv")
head(traitsUSA)
length(unique(traitsUSA$TAXON))

## define traits of interest
unique(traitsUSA$TRAITS_NAME)

## continuous traits
trtsV <- c("Thermal tolerance value", 
          "Thermal optima value",  "Body length")

## categorical traits
trtsC <- c("Fecundity", "Rheophily", "Swimming ability", "Voltinism",
          "Thermal Indicator","Life span", "Observed maximum letal temperature",
          "Diapause", "Development speed", "Maximal body size")
trtsC

##"Eurythermal", "Euthermal", 

## filter to species in capture prob df
length(unique(capLAC$TAXON)) ## 304

TraitsCA <- traitsUSA %>%
  filter(TAXON %in% capLAC$TAXON )
length(unique(TraitsCA$TAXON)) ## 266

## get tolerance values to start with
names(TraitsCA)

### convert preferences to fahtenhet
tolsVals <- TraitsCA %>%
  # filter(CATEGORY_NAME == "Tolerance") %>%
  dplyr::select(TAXON:TAXON_ORDER,STUDY_LOCATION_REGION,TRAITS_NAME, VALUE_NUMBER) %>%
  filter(TRAITS_NAME %in% trtsV)%>%
  group_by(TAXON, TRAITS_NAME) %>% 
  mutate(MeanValue = mean(VALUE_NUMBER)) %>% ## quick fix, change later
  select(-VALUE_NUMBER) %>%
  distinct()

### get categorical traits and make wider to colour points in plot

tolsCats <- TraitsCA %>%
  dplyr::select(TAXON:TAXON_ORDER,STUDY_LOCATION_REGION, CATEGORY_NAME:VALUE_TEXT) %>%
  filter(TRAITS_NAME %in% trtsC) %>%
  group_by(TAXON, TRAITS_NAME) %>% 
  mutate(AvText = ifelse(unique(length(VALUE_TEXT)) > 1, VALUE_TEXT[1], VALUE_TEXT)) %>% ## when there's >1 classes, take the 1st
  select(TAXON:TAXON_ORDER,  AvText) %>%
  distinct() %>%
  rename(CatTraits = TRAITS_NAME) %>%
  mutate(AvText = ifelse(AvText == "fast seasonal", "Fast seasonal", AvText )) %>%
  mutate(AvText = ifelse(AvText == "slow seasonal", "Slow seasonal", AvText ))
  # filter(!TRAITS_NAME %in% c("Thermal optima value", "Thermal tolerance value")) %>%
  # pivot_wider(names_from = TRAITS_NAME, values_from = AvText) 

names(tolsCats)

tolsAll <- right_join(tolsVals, tolsCats, by = c("TAXON","ASSEMBLAGE","TSN", "GENUS","FAMILY","TAXON_ORDER"))
head(tolsAll)

# %>%
  # pivot_longer(c(Eurythermal,Euthermal), names_to = "Trait", values_to = "Value") %>%
  # select(-Value)

length(unique(tolsVals$TAXON)) ## 224 - find more!!!!

### join with capture probability

capTols <- inner_join(capLAC, tolsAll, by = "TAXON") #%>%
# dplyr::select(-c(sampleid)) %>% distinct()
names(capTols)

head(capTols)

sum(is.na(capTols$MeanValue))
sum(is.na(capTols$captureprob))

## calculted means of trait per sites, weighrd on capture probability. 
## o/e calcuated by weightmean oberseved/weighted mean expected
## calcuated for each level within different trait categories

oeCats <- capTols %>% ungroup() %>% group_by(masterid, latitude, longitude, sampleid, TRAITS_NAME, CatTraits,  AvText) %>%
  summarise(wgtValueEx = weighted.mean(MeanValue, captureprob),
            wgtValueEx_obs = weighted.mean(MeanValue[meanobserved>0], captureprob[meanobserved>0])) %>%
  # ungroup() %>%
  mutate(oeTemp = wgtValueEx_obs/wgtValueEx) 

### calcuate a total o/e without trait categories
oe <- capTols %>% ungroup() %>% group_by(masterid,latitude, longitude, sampleid, TRAITS_NAME) %>%
  summarise(wgtValueEx = weighted.mean(MeanValue, captureprob),
            wgtValueEx_obs = weighted.mean(MeanValue[meanobserved>0], captureprob[meanobserved>0])) %>%
  # ungroup() %>%
  mutate(oeTemp = wgtValueEx_obs/wgtValueEx) 

## expected values coming out the same, why? Fixed!!! na.omit in weighted mean 

names(oeCats)
names(oe)

## plot observed/expected by trait categories

siteOE <- oeCats %>%
  # select(masterid, sampleid, TRAITS_NAME, wgtValueEx:oeTemp) %>%
  distinct() %>% drop_na()

## define traits
cats <- unique(siteOE$CatTraits)

for(c in 1:length(cats)) {

c1 <- ggplot(subset(siteOE, CatTraits == cats[c]), aes(x=wgtValueEx_obs, y = wgtValueEx, group = AvText)) +
  geom_point(size=0.2, aes(color = AvText)) +
  guides(color = guide_legend(title = paste(cats[c]))) +
  facet_wrap(~TRAITS_NAME, scales = "free") 

file.name1 <- paste0(out.dir, "03_", cats[c], "_bmi_cat_traits_obs_exp_points.jpg")
ggsave(c1, filename=file.name1, dpi=300, height=5, width=7.5)
  
}

## plot overal obs/exp by thermal traits

c2 <- ggplot(oe, aes(x=wgtValueEx_obs, y = wgtValueEx, colour = oeTemp)) +
  geom_point(size=0.2) +
  # guides(color = guide_legend(title = paste(cats[c]))) +
  facet_wrap(~TRAITS_NAME, scales = "free") + 
  scale_color_viridis_c()

c2

file.name1 <- paste0(out.dir, "03_", cats[c], "_bmi_overall_traits_obs_exp_points.jpg")
ggsave(c2, filename=file.name1, dpi=300, height=5, width=7.5)


## save out
write.csv(oe, "output_data/03_ObsExp_all_CA.csv")
write.csv(oeCats, "output_data/03_ObsExp_traits_all_CA.csv")

# Join with Temp ----------------------------------------------------------

## join o/e with bugs to get comid
head(bugs2)
unique(oe$sampleid)

bugs_oe <- inner_join(oe, bugs2, by=c("masterid", "longitude", "latitude"))  ## difficult to get the year from the sampleid, match with masterid for now

## difficult to get 
  # separate(sampleid, into= c("Site", "Date", "Type", "Agency", "Version"), sep =c("_"))# %>%
  # separate(Date, into = c("Month", "Day", "Year")) %>% dplyr::select(-Agency, -Version, -Type, -Site) %>% ## get sample year

head(bugs_oe)

sum(unique(bugs_oe$COMID) %in% unique(baseline_stream_temp$COMID)) ## 461

## join with modelled temp data, by comid for now, year not possible to extarct yet

head(AllTempAlt)

AllData <- left_join(bugs_oe, AllTempAlt, by = c("COMID")) #%>%
  # pivot_longer(Max_Wkly_Mean_StreamT:Max_Wkl_Max_StreamT_grt_30_, names_to = "CurMetric", values_to = "CurTemp")

head(AllData)
dim(AllData) ## 40656

range(na.omit(AllData$oeTemp))


# Plot --------------------------------------------------------------------

## statewide

m1 <- ggplot(oe, aes(x=longitude, y = latitude, color = oeTemp)) +
  geom_point() + 
  scale_color_viridis_c()

m1

file.name1 <- paste0(out.dir, "03_bmi_traits_oe_map.jpg")
ggsave(m1, filename=file.name1, dpi=300, height=5, width=7.5)

## labels
supp.labs <- unique(AllData$CurMetric)
names(supp.labs) <- c("Max Weekly Mean","Max Weekly Max", "Max Weekly Min", "Max Weekly Range", "Av Weekly Range", "Greater Than 30")


## plot quad lm

T1 <- ggplot(AllData, aes(y=oeTemp, x=CurTemp, group = TRAITS_NAME, color = TRAITS_NAME)) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1)+
  geom_vline(xintercept = 86, linetype="dashed", 
             color = "red", linewidth=0.5, show.legend = T) +
  geom_vline(xintercept = 80, linetype="dashed",
             color = "blue", linewidth=0.5, show.legend = T) +
  # geom_hline(yintercept = 0.79) +
  facet_wrap(~CurMetric, labeller =labeller(supp.labs),
             scales = "free_x") +
  scale_x_continuous(name="Water Temp (°F)") +
  scale_y_continuous(name = "Temp Preference (o/e)")

T1

file.name1 <- paste0(out.dir, "03_bmi_traits_temp_response_modelled.jpg")
ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)

## plot points
names(AllData)
m=1
mets <- unique(na.omit(AllData$TRAITS_NAME))
mets

for(m in 1:length(mets)) {
  
  T2 <- ggplot(subset(AllData,TRAITS_NAME == mets[m]), aes(y=oeTemp, x=CurTemp)) +
    geom_point(size=0.2) +
    # stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)+
    geom_vline(xintercept = 86, linetype="dashed", 
               color = "red", linewidth=0.5, show.legend = T) +
    geom_vline(xintercept = 80, linetype="dashed",
               color = "blue", linewidth=0.5, show.legend = T) +
    # geom_hline(yintercept = 0.79) +
    facet_wrap(~CurMetric, labeller = labeller(supp.labs),
               scales = "free_x") +
    scale_x_continuous(name="Water Temp (°F)") +
    scale_y_continuous(name = paste(mets[m])) 
    # theme(legend.position = "none")
  
  T2
  
  file.name1 <- paste0(out.dir, "03_", mets[m], "_csci_temp_response_points_modelled.jpg")
  ggsave(T2, filename=file.name1, dpi=300, height=5, width=7.5)
}


# Join and plot oe by traits ----------------------------------------------

bugs_oec <- inner_join(oeCats, bugs2, by=c("masterid"))  ## difficult to get the year from the sampleid, match with masterid for now

## difficult to get 
# separate(sampleid, into= c("Site", "Date", "Type", "Agency", "Version"), sep =c("_"))# %>%
# separate(Date, into = c("Month", "Day", "Year")) %>% dplyr::select(-Agency, -Version, -Type, -Site) %>% ## get sample year

names(bugs_oec)

sum(unique(bugs_oec$COMID) %in% unique(AllTempAlt$COMID)) ## 461

## join with modelled temp data, by comid for now, year not possible to extarct yet

head(AllTempAlt)

AllDatac <- left_join(bugs_oec, AllTempAlt, by = c("COMID")) #%>%
  # pivot_longer(Max_Wkly_Mean_StreamT:Max_Wkl_Max_StreamT_grt_30_, names_to = "CurMetric", values_to = "CurTemp")

head(AllDatac)
dim(AllDatac) ## 40656
names(AllDatac)

range(na.omit(AllDatac$oeTemp))


# Model: CSCI traits ------------------------------------------------------

names(AllDataLong2)
## make quadratic terms
AllDataLong2 <- AllDatac %>%
  group_by(CurMetric, TRAITS_NAME, CatTraits) %>%
  mutate(CurTemp2 = CurTemp^2)

## create df of model configurations

## thermal index 
biol.endpoints<-unique(na.omit(AllDataLong2$TRAITS_NAME))
biol.endpoints

## categrical traits
biol.endpoints2<-unique(na.omit(AllDataLong2$AvText))
biol.endpoints2

## temp
temp.endpoints<- unique(AllDataLong2$CurMetric)

bio_h_summary<-  expand.grid(thermal.endpoints=biol.endpoints,trait.endpoints=biol.endpoints2, temp.endpoints=temp.endpoints, stringsAsFactors = F)

## extract categories to add the summary
cats <- AllDataLong2 %>% ungroup() %>%
  select(CatTraits, AvText) %>% distinct() %>%
  rename(trait.endpoints=AvText)

bio_h_summary <- na.omit(bio_h_summary)
bio_h_summary <- full_join(bio_h_summary, cats, by = "trait.endpoints")
bio_h_summary <- bio_h_summary %>% filter(!trait.endpoints == "No")

## take subset to test

bio_h_summary <- bio_h_summary %>% filter(CatTraits == "Thermal Indicator")
bio_h_summary

i=1
## model of each configuration
quad.lm <-lapply(1:nrow(bio_h_summary), function(i)
{
  
  tmet<-as.character(bio_h_summary[i,"temp.endpoints"])
  bmet<-as.character(bio_h_summary[i,"thermal.endpoints"])
  bmet2<-as.character(bio_h_summary[i,"trait.endpoints"])
  
  mydat<-AllDataLong2 %>%
    filter(CurMetric == tmet,
           TRAITS_NAME == bmet,
           AvText == bmet2) %>%
    select(oeTemp, CurTemp, CurTemp2, CurMetric, TRAITS_NAME, AvText, CatTraits) %>%
    distinct() %>% drop_na() %>% 
    filter_all(all_vars(!is.infinite(.)))
  
  head(mydat)

  mydat<-mydat[order(mydat$oeTemp),]
  
  lm(oeTemp~CurTemp+CurTemp2,data=mydat)
  
  #family=Gamma(link = "log")
})

bio_h_summary
quad.lm 
## save models
save(quad.lm, file = "output_data/03_csci_traits_mods_lm_quad.RData")

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
save(bio_h_summary, file="output_data/03_asci_component_mods_quadLM_rsqds.RData")

## make df of predicted values to predict on - need to be different for each temp metric

## blank df
DF <- NULL
DF <- as.data.frame(DF)

bio_h_summary
i
### get predictions and fitted values
for(i in 1:length(quad.lm)) {
  
  tmet<-as.character(bio_h_summary[i,"temp.endpoints"])
  bmet<-as.character(bio_h_summary[i,"thermal.endpoints"])
  bmet2<-as.character(bio_h_summary[i,"trait.endpoints"])
  
  data<-AllDataLong2 %>%
    filter(CurMetric == tmet,
           TRAITS_NAME == bmet,
           AvText == bmet2) %>%
    select(oeTemp, CurTemp, CurTemp2, CurMetric, TRAITS_NAME, AvText, CatTraits) %>%
    distinct() %>% drop_na() %>% 
    filter_all(all_vars(!is.infinite(.)))
  
  ## new data - plus and minus 10% as a start
  tempvalues <- seq((range(data$CurTemp)[1])*0.9,(range(data$CurTemp)[2])*1.1,0.05)
  
  ## get model, predict, extract all data and categories
  mod <- quad.lm[[i]]
  predictedVals <- predict.glm(mod,list(CurTemp = tempvalues, CurTemp2 = tempvalues^2),  type = "response")
  DFX <- as.data.frame(predictedVals)
  DFX$Value <- tempvalues
  DFX$BioTrait <- bmet
  DFX$BioCat <- unique(data$CatTraits)
  DFX$AvText <- bmet2
  DFX$Variable <- tmet
  DFX$MinVal <-  range(data$CurTemp)[1]
  DFX$MaxVal <-  range(data$CurTemp)[2]
  
  DF <- bind_rows(DF, DFX)

}
DF
## save predicted values
save(DF, file="output_data/03_csci_traits_QuadLM_predVals.RData")

### predicted figures

m=1
mets <- unique(DF$BioTrait)
mets
names(DF)
## facet labels
supp.labs <- c(
  "Max_Wkl_Max_StreamT_grt_30_"="Weeks greater than 86F",
  "Max_Wkly_Mean_StreamT" = "Max Weekly Mean",
  "Max_Wkl_Max_StreamT" = "Max Weekly Max",
  "Min_Wkl_Min_StreamT" = "Min Weekly Min",
  "Max_Wkl_Rng_StreamT" = " Max Weekly Range",
  "Mean_Wkl_Rng_StreamT" =  "Av Weekly Range",
  "MedAnn" =  "Median Annual Temp"
)
?recode_factor
DF$AvText <- recode_factor(DF$AvText, warm = "Warm", cold = "Cold")

for(m in 1:length(mets)) {
  
  AllDataWide <- DF %>%
    filter(BioTrait == mets[m]) 
  
  cats <- unique(AllDataWide$BioCat)
  cats
  
  head(AllDataWide)
  
  for(c in 1:length(cats)) {
    
  
  T1 <- ggplot(subset(DF, BioTrait == mets[m]), aes(y=predictedVals, x=Value, group = AvText, color = AvText)) +
    geom_line(linewidth=0.7) +
    facet_wrap(~Variable, labeller = as_labeller(supp.labs),
               scales = "free") +
    guides(color = guide_legend(title = paste(cats[c]))) +
    geom_vline(data=filter(DF, !Variable %in% c("Max_Wkl_Max_StreamT_grt_30_","Max_Wkl_Rng_StreamT", "Mean_Wkl_Rng_StreamT")), 
               aes(xintercept = 86), linetype="dashed", color = "red", linewidth=0.4) +
    geom_vline(data=filter(DF, !Variable %in% c("Max_Wkl_Max_StreamT_grt_30_","Max_Wkl_Rng_StreamT", "Mean_Wkl_Rng_StreamT")), 
               aes(xintercept = 80), linetype="dashed", color = "blue", linewidth=0.4) +
    scale_x_continuous(name="Water Temp (°F)") +
    scale_y_continuous(name = paste(mets[m])) 
    
    # theme(legend.position = "none")
  
  T1
  
  file.name1 <- paste0(out.dir, "03_",  mets[m],"_", cats[c], "_csci_temp_traits_predicted.jpg")
  ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)
  
  }
}


# Other figures -----------------------------------------------------------


### plots per trait: curves

for(m in 1:length(mets)) {
  
  AllDataWide <- AllDatac %>%
    filter(TRAITS_NAME == mets[m]) 
  
  cats <- unique(AllDataWide$CatTraits)
  cats
  
  head(AllDataWide)
  
  for(c in 1:length(cats)) {
    
    # AllDataWideC <- AllDataWide %>%
    #   filter(CatTraits == cats[c]) 
    
    T4 <- ggplot(subset(AllDataWide,CatTraits == cats[c]), aes(y=oeTemp, x=CurTemp, group = AvText)) +
      # geom_point(size=0.2, aes(color = AvText)) +
      stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1, aes(color = AvText))+
      geom_vline(xintercept = 86, linetype="dashed", 
                 color = "red", linewidth=0.5, show.legend = T) +
      geom_vline(xintercept = 80, linetype="dashed",
                 color = "blue", linewidth=0.5, show.legend = T) +
      # geom_hline(yintercept = 0.79) +
      facet_wrap(~CurMetric, labeller = labeller(supp.labs),
                 scales = "free_x") +
      scale_x_continuous(name="Water Temp (°F)") +
      scale_y_continuous(name = paste(mets[m])) +
      guides(color = guide_legend(title = paste(cats[c]))) 
    # theme(legend.position = "none")
    
    T4
    
    file.name1 <- paste0(out.dir, "03_", mets[m],"_", cats[c], "_bmi_temp_by_traits_qlm.jpg")
    ggsave(T4, filename=file.name1, dpi=300, height=5, width=7.5)
  }
  
}


### plot by trait: points

names(AllDatac)
# names(AllDataWide)
m=1
mets <- unique(na.omit(AllDatac$TRAITS_NAME))
mets

for(m in 1:length(mets)) {
  
  AllDataWide <- AllDatac %>%
    filter(TRAITS_NAME == mets[m]) 
    
  cats <- unique(AllDataWide$CatTraits)
  cats
  
  head(AllDataWide)
  
  for(c in 1:length(cats)) {
    
    # AllDataWideC <- AllDataWide %>%
    #   filter(CatTraits == cats[c]) 
    
    T3 <- ggplot(subset(AllDataWide,CatTraits == cats[c]), aes(y=oeTemp, x=CurTemp, group = AvText)) +
      geom_point(size=0.2, aes(color = AvText)) +
      # stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)+
      geom_vline(xintercept = 86, linetype="dashed", 
                 color = "red", linewidth=0.5, show.legend = T) +
      geom_vline(xintercept = 80, linetype="dashed",
                 color = "blue", linewidth=0.5, show.legend = T) +
      # geom_hline(yintercept = 0.79) +
      facet_wrap(~CurMetric, labeller = labeller(supp.labs),
                 scales = "free_x") +
      scale_x_continuous(name="Water Temp (°F)") +
      scale_y_continuous(name = paste(mets[m])) +
      guides(color = guide_legend(title = paste(cats[c]))) 
    # theme(legend.position = "none")
    
    T3
    
    file.name1 <- paste0(out.dir, "03_", mets[m],"_", cats[c], "_bmi_temp_by_traits_points.jpg")
    ggsave(T3, filename=file.name1, dpi=300, height=5, width=7.5)
  }
    
  }
  


# Temperature alteration --------------------------------------------------


### get only mean weekly temp and median annual temp - the closest match, but needs improved
AllDataAltSub <- AllDatac %>%
  filter(CurMetric == "MedAnn", 
         RefMetric == "MAST")


## plot quad lm

T1 <- ggplot(AllDataAltSub, aes(y=oeTemp, x=AltTemp, group = TRAITS_NAME, color = TRAITS_NAME)) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1)+
  # geom_vline(xintercept = 30, linetype="dashed", 
  #            color = "red", linewidth=0.5, show.legend = T) +
  # geom_vline(xintercept = 26.667, linetype="dashed",
  #            color = "blue", linewidth=0.5, show.legend = T) +
  # geom_hline(yintercept = 0.79) +
  facet_wrap(~CurMetric, labeller =labeller(supp.labs),
             scales = "free_x") +
  scale_x_continuous(name="Temp Alteration (°F)") +
  scale_y_continuous(name = "Temp Index (o/e)")

T1

file.name1 <- paste0(out.dir, "03_bmi_traits_temp_response_alteration.jpg")
ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)

## plot points by traits
names(AllData)
m=1
mets <- unique(na.omit(AllDataAltSub$TRAITS_NAME))
mets

for(m in 1:length(mets)) {
  
  T2 <- ggplot(subset(AllDataAltSub,TRAITS_NAME == mets[m]), aes(y=oeTemp, x=AltTemp)) +
    geom_point(size=0.2) +
    # stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)+
    geom_vline(xintercept = 86, linetype="dashed", 
               color = "red", linewidth=0.5, show.legend = T) +
    geom_vline(xintercept = 80, linetype="dashed",
               color = "blue", linewidth=0.5, show.legend = T) +
    # geom_hline(yintercept = 0.79) +
    facet_wrap(~CurMetric, labeller = labeller(supp.labs),
               scales = "free_x") +
    scale_x_continuous(name="Water Temp (°F)") +
    scale_y_continuous(name = paste(mets[m])) 
  # theme(legend.position = "none")
  
  T2
  
  file.name1 <- paste0(out.dir, "03_", mets[m], "_csci_temp_response_points_alteration.jpg")
  ggsave(T2, filename=file.name1, dpi=300, height=5, width=7.5)
}


# Join and plot oe by traits and alteration----------------------------------------------
### plot by trait: points

names(AllDatac)
names(AllDataWide)

### get only mean weekly temp and median annual temp - the closest match, but needs improved
AllDataAltSubc <- AllDatac %>%
  filter(CurMetric == "MedAnn", 
         RefMetric == "MAST")
m=1
mets <- unique(na.omit(AllDataAltSubc$TRAITS_NAME))
mets

for(m in 1:length(mets)) {
  
  AllDataWide <- AllDataAltSubc %>%
    filter(TRAITS_NAME == mets[m]) 
  
  cats <- unique(AllDataWide$CatTraits)
  cats
  
  head(AllDataWide)
  
  for(c in 1:length(cats)) {
    
    # AllDataWideC <- AllDataWide %>%
    #   filter(CatTraits == cats[c]) 
    
    T3 <- ggplot(subset(AllDataWide,CatTraits == cats[c]), aes(y=oeTemp, x=AltTemp, group = AvText)) +
      geom_point(size=0.2, aes(color = AvText)) +
      # stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)+
      # geom_vline(xintercept = 30, linetype="dashed", 
      #            color = "red", linewidth=0.5, show.legend = T) +
      # geom_vline(xintercept = 26.667, linetype="dashed",
      #            color = "blue", linewidth=0.5, show.legend = T) +
      # geom_hline(yintercept = 0.79) +
      facet_wrap(~CurMetric, labeller = labeller(supp.labs),
                 scales = "free_x") +
      scale_x_continuous(name="Water Temp (°F)") +
      scale_y_continuous(name = paste(mets[m])) +
      guides(color = guide_legend(title = paste(cats[c]))) 
    # theme(legend.position = "none")
    
    T3
    
    file.name1 <- paste0(out.dir, "03_", mets[m],"_", cats[c], "_bmi_temp_by_traits_points_alteration.jpg")
    ggsave(T3, filename=file.name1, dpi=300, height=5, width=7.5)
  }
  
}

### plots per trait: curves

for(m in 1:length(mets)) {
  
  AllDataWide <- AllDataAltSubc %>%
    filter(TRAITS_NAME == mets[m]) 
  
  cats <- unique(AllDataWide$CatTraits)
  cats
  
  head(AllDataWide)
  
  for(c in 1:length(cats)) {
    
    # AllDataWideC <- AllDataWide %>%
    #   filter(CatTraits == cats[c]) 
    
    T4 <- ggplot(subset(AllDataWide,CatTraits == cats[c]), aes(y=oeTemp, x=AltTemp, group = AvText)) +
      # geom_point(size=0.2, aes(color = AvText)) +
      stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1, aes(color = AvText))+
      # geom_vline(xintercept = 30, linetype="dashed", 
      #            color = "red", linewidth=0.5, show.legend = T) +
      # geom_vline(xintercept = 26.667, linetype="dashed",
      #            color = "blue", linewidth=0.5, show.legend = T) +
      # geom_hline(yintercept = 0.79) +
      facet_wrap(~CurMetric, labeller = labeller(supp.labs),
                 scales = "free_x") +
      scale_x_continuous(name="Water Temp (°F)") +
      scale_y_continuous(name = paste(mets[m])) +
      guides(color = guide_legend(title = paste(cats[c]))) 
    # theme(legend.position = "none")
    
    T4
    
    file.name1 <- paste0(out.dir, "03_", mets[m],"_", cats[c], "_bmi_temp_by_traits_qlm_alteration.jpg")
    ggsave(T4, filename=file.name1, dpi=300, height=5, width=7.5)
  }
  
}


