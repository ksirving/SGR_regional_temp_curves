## testing Lawrence et al. (2010) Long-term macroinvertebrate responses to climate change: 
## implications for biological assessment in Mediterranean-climate streams?
## climate change indicator

library(tidylog)
library(tidyverse)
library(sf)

out.dir <- "Figures/"

# Temp data ---------------------------------------------------------------

## modelled current data
load(file = "ignore/baseline_stream_temp.RData")

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

load(file = "ignore/02_cur_temp_alt_mets.RData")
head(AllTempAlt)

## convert to fahrenheit
# AllTempAlt <- AllTempAlt %>%
#   mutate(CurTemp = (CurTemp*1.8)+32,
#          MedRefTemp = (MedRefTemp*1.8)+32,
#          AltTemp = (AltTemp*1.8)+32) 
# 
# head(AllTempAlt)

# Capture probabilities ---------------------------------------------------

## capture probs
load(file="ignore/SMC_cap_prob_cali.csv")
head(oe_ca)
length(unique(oe_ca$otu))
dim(oe_ca)

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


# Join with Temp ----------------------------------------------------------

## distinct masterid 
## join o/e with bugs to get comid
head(bugs2)
unique(oe_ca$sampleid)

bugs_oe <- inner_join(oe_ca, bugs2, by=c("masterid", "longitude", "latitude", "county"), relationship = "many-to-many")  ## difficult to get the year from the sampleid, match with masterid for now

## difficult to get 
# separate(sampleid, into= c("Site", "Date", "Type", "Agency", "Version"), sep =c("_"))# %>%
# separate(Date, into = c("Month", "Day", "Year")) %>% dplyr::select(-Agency, -Version, -Type, -Site) %>% ## get sample year

head(bugs_oe)

sum(unique(bugs_oe$COMID) %in% unique(baseline_stream_temp$COMID)) ## 466

## join with modelled temp data, by comid for now, year not possible to extarct yet

head(AllTempAlt)

AllData <- left_join(bugs_oe, baseline_stream_temp, by = c("COMID"), relationship = "many-to-many") #%>%
# pivot_longer(Max_Wkly_Mean_StreamT:Max_Wkl_Max_StreamT_grt_30_, names_to = "CurMetric", values_to = "CurTemp")

head(AllData)
dim(AllData) ## 40656

# range(na.omit(AllData$oeTemp))


# Filter to species in climate change indicator ---------------------------


species <- c("Ambrysus", "Chironomidae", "Dixa", "Euparyphus", "Hydropsyche",
             "Hydroptilia", "Lepidostoma", "Ochrotrichia", "Trichocorixa")
# 
# AllDataSub <- AllData %>%
#   filter(otu %in% species)
# 
# dim(AllDataSub)
# head(AllDataSub)
# 
# Temp <- AllDataSub %>%
#   select(COMID:Mean_Wkl_Rng_StreamT)

# Proportion observed -----------------------------------------------------

## calculate proportion of 9 species at each site
## change observed to presence/absence, then % of species per site
Props <- AllData %>%
  drop_na(Max_Wkl_Max_StreamT) %>%
  mutate(PresenceAbsence = ifelse(meanobserved ==0, 0,1)) %>%
  select(masterid, latitude, longitude, county, huc, sampleid, COMID, meanobserved, otu,PresenceAbsence) %>%
  # filter(!captureprob == 0) %>% ## remove capture prob at 0
  mutate(IndSpeciesPresence = ifelse(otu %in% species, PresenceAbsence, NA)) %>%
  distinct()

head(Props)
Propsx <- Props %>%
  group_by(masterid, latitude, longitude, county, huc, sampleid, COMID) %>%
  mutate(TotalatSiteEx = length(otu), TotalObserved = sum(PresenceAbsence),
         TotalIndObserved = sum(na.omit(IndSpeciesPresence))) %>%
  select(-otu, -meanobserved, -PresenceAbsence) %>%
  distinct() %>%
  mutate(ProportionIndTot = na.omit(TotalIndObserved/TotalObserved), ## proportion of indicator species observed compared to total species observed
         ProportionIndOnly = na.omit(TotalIndObserved/5)) ## proportion of indictator species observed compared to all indicator species
 
# summarise()
unique(Props$otu) ## only 5 species from the nine in the dataset, some not at all sites

head(Props)

## join back to temp data

PropsTemp <- inner_join(Propsx, Temp, by=c("COMID"), relationship = "many-to-many")  ## difficult to get the year from the sampleid, match with masterid for now
head(PropsTemp)


# Plot --------------------------------------------------------------------
unique(PropsTempLong$Metric)
PropsTempLong <- PropsTemp %>%
  pivot_longer(Max_Wkl_Max_StreamT_grt_30_:Mean_Wkl_Rng_StreamT, names_to = "Metric", values_to = "Value") %>%
  filter(Metric %in% c("Max_Wkl_Max_StreamT", "Min_Wkl_Min_StreamT", "Max_Wkl_Rng_StreamT", "Mean_Wkl_Rng_StreamT")) %>%
  mutate(Metric = as.factor(Metric))

head(PropsTempLong)
## labels
supp.labs <- unique(PropsTempLong$Metric)
names(supp.labs) <- c("Weekly Max", "Weekly Min", "Max Weekly Range", "Av Weekly Range")
supp.labs

## plot quad lm
?facet_wrap
T1 <- ggplot(PropsTempLong, aes(y=ProportionIndOnly, x=Value, group = Metric, color = Metric)) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1)+
  facet_wrap(~Metric, labeller =as_labeller(supp.labs),
             scales = "free_x") +
  scale_x_continuous(name="Water Temp (°F)") +
  scale_y_continuous(name = "Proportion Species Present")

T1


file.name1 <- paste0(out.dir, "05_test_climate_indicator.jpg")
ggsave(T1, filename=file.name1, dpi=300, height=5, width=7.5)

T2 <- ggplot(PropsTempLong, aes(y=ProportionIndTot, x=Value, group = Metric, color = Metric)) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1)+
  facet_wrap(~Metric, labeller =as_labeller(supp.labs),
             scales = "free_x") +
  scale_x_continuous(name="Water Temp (°F)") +
  scale_y_continuous(name = "Proportion Species Present (from total species)")

T2


file.name1 <- paste0(out.dir, "05_test_climate_indicator_total_species_proportions.jpg")
ggsave(T2, filename=file.name1, dpi=300, height=5, width=7.5)



# - proportions of species and number of sites --------------------
?tally
propstal <- Props %>% ungroup() %>% group_by(Proportion) %>%
  summarise(length(unique(masterid)))

propstal
tallyx
