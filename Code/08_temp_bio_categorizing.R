## test SGR bio sites and categorize

library(tidylog)
library(tidyverse)
library(sf)
library(zoo)
library(scales)

## workflow
## all bio sites in SGR
## modelled Temp in SGR
## match up
## use target temps - 

# Index	Threshold Value	Threshold Type	Maximum weekly temperature (F)
# CSCI	0.79	Standard	87
# CSCI	0.6	Modified	93
# ASCI	0.86	Standard	82
# ASCI	0.75	Modified	89

## categories in 4 classes


# Upload and format Bio data ----------------------------------------------

## csci
csci <- read.csv("ignore/sites_with_CSCI_SGR.csv") %>%
  select(sampledate, csci, masterid, latitude, longitude, comid) %>% ## select columns
  separate(sampledate, into = c("Year", "Month", "Day"), sep = "-", remove = F) %>% ## separate date
  rename(Biovalue = csci) %>%
  mutate(Year = as.numeric(Year)) %>% # change date to number to match temp
  mutate(metric = "CSCI") %>%
  mutate(Modified = 0.6, Standard = 0.79) %>% ## add coloumns of thresholds
  pivot_longer(Modified:Standard, names_to = "Type", values_to = "BioThresh") %>% ## make longer
  mutate(TempThresh = case_when(Type == "Modified" ~ 93,
                                Type == "Standard" ~ 87))

head(csci)

## asci
asci <- read.csv("ignore/asci_scores_comid_SGR.csv") %>%
  filter(assemblage == "Hybrid") %>%
  filter(replicate == 1) %>% ## take 1st rep only
  select(sampledate, metric, result, masterid, comid, latitude, longitude) %>%
  rename(Biovalue = result) %>%
  separate(sampledate, into = c("Year", "Month", "Day"), sep = "-", remove = F) %>% ## separate date
  mutate(Year = as.numeric(Year)) %>% ## change date to number to match temp
  drop_na(Biovalue) %>%
  mutate(Modified = 0.75, Standard = 0.86) %>% ## add coloumns of thresholds
  pivot_longer(Modified:Standard, names_to = "Type", values_to = "BioThresh") %>% ## make longer
  mutate(TempThresh = case_when(Type == "Modified" ~ 89,
                                Type == "Standard" ~ 82))

  
head(asci)

dim(asci)

## join csci and asci together
names(csci)
names(asci)

allbio <- bind_rows(csci, asci)



# Upload modelled stream temp ---------------------------------------------

load("/Users/katieirving/OneDrive - SCCWRP/Documents - Katie’s MacBook Pro/Projects/San_Gabriel_Temp/Data/AirTemp/Modeling/baseline_stream_temp.RData")

baseline_stream_temp <- baseline_stream_temp %>% ungroup() %>%
  pivot_longer(Max_Wkly_Mean_StreamT:Mean_Wkl_Rng_StreamT, names_to = "Mets", values_to = "Vals") %>%
  mutate(TempF = (Vals*1.8)+32) %>% 
  select(-Vals, -Max_Wkl_Max_StreamT_grt_30_) %>%
  filter(Mets == "Max_Wkl_Max_StreamT") %>%
  filter(!TempF == "-Inf")


# Join  -------------------------------------------------------------------

allDataThresh <- inner_join(allbio, baseline_stream_temp, by = c("comid" = "COMID", "Year" = "year") , relationship = "many-to-many")
head(allDataThresh)

length(unique(allDataThresh$masterid)) ## 80
length(unique(allDataThresh$comid)) ## 57

## count number of asci and csci sites
tallySites <- allDataThresh %>%
  group_by(metric) %>%
  select(masterid:metric) %>%
  distinct() %>%
  tally()

tallySites

# 1 ASCI      42
# 2 CSCI      81

## get coords to count number bewlo Santa Fe Dam

df <- allDataThresh %>%
  select(masterid, metric, longitude, latitude) %>%
  distinct() %>%
  st_as_sf(coords=c( "longitude", "latitude"), crs=4326, remove=F)

head(df)

## save

st_write(df, "output_data/08_SGR_sites.shp", append = F)

## santa fe dam is 37.0719
## filter latitude

lowerSGR  <- df %>%
  filter(latitude <= 34.0719)

## count number of asci and csci sites
tallySites <- lowerSGR %>%
  group_by(metric) %>%
  # select(masterid:metric) %>%
  distinct() %>%
  tally()

tallySites

## save

st_write(lowerSGR, "output_data/08_Lower_SGR_sites.shp", append = F)



# Within thresholds -------------------------------------------------------

## calculte 4 categoeies of stres, bio, temp, both or none
datalimits <- allDataThresh %>%
  mutate(WithinTempLimits = ifelse(TempF <= TempThresh, "Within", "NotWithin")) %>%  ## within hydro limits
  mutate(WithinBioLimits = ifelse(Biovalue >= BioThresh, "Within", "NotWithin")) %>%## above bio thresholds
  mutate(Result = case_when((WithinTempLimits == "Within" & WithinBioLimits == "Within") ~ "NoImpact", ## add impact
                            (WithinTempLimits == "Within" & WithinBioLimits == "NotWithin") ~ "BioImpact", ##
                            (WithinTempLimits == "NotWithin" & WithinBioLimits == "Within") ~ "TempImpact",
                            (WithinTempLimits == "NotWithin" & WithinBioLimits == "NotWithin") ~ "BothImpact")) %>%
  mutate(Result = factor(Result, levels = c("NoImpact", "BioImpact", "TempImpact", "BothImpact"))) # %>%
  
## tally results
names(datalimits)

## csci
tallyImpactC <- datalimits %>%
  filter(metric =="CSCI") %>%
  group_by(Type, Result) %>%
  distinct() %>%
  tally() %>%
  drop_na(Result) %>%
  mutate(PercChans = (n/sum(n)*100)) %>%
  mutate(metric ="CSCI")

## asci
tallyImpactA <- datalimits %>%
  filter(metric =="ASCI") %>%
  group_by(Type, Result) %>%
  distinct() %>%
  tally() %>%
  drop_na(Result) %>%
  mutate(PercChans = (n/sum(n)*100)) %>%
  mutate(metric ="ASCI")

tallyImpactA

## join 
tallyImpact <- bind_rows(tallyImpactC, tallyImpactA)

write.csv(tallyImpact, "output_data/08_count_impact.csv")

tallyImpact
## revise table for report

tallyImpactWide <- tallyImpact %>%
  select(-n) %>%
  pivot_wider(names_from = Result, values_from = PercChans)

tallyImpactWide

write.csv(tallyImpactWide, "output_data/08_count_impact_wide.csv")

# Plot spatially ----------------------------------------------------------

imps_sf <- datalimits %>% 
  # st_as_sf(coords=c( "longitude", "latitude"), crs=4326, remove=F) %>%
  # st_transform(crs = st_crs(socal)) %>%
  # select(Index, Hydro_endpoint, Threshold, BioThresh, masterid, COMID, Flow.Metric.Name, Flow.Component, Result, longitude, latitude)  %>%
  mutate(Type = as.factor(Type), Result = as.factor(Result), metric = as.factor(metric)) %>%
  mutate(Result = factor(Result, levels = c("NoImpact", "BioImpact", "TempImpact", "BothImpact"),
         labels = c("Targets Met", "Impacted Biology", "Impacted Temperature", "Both Biology and Temperature Impacted")))

## use ggmap to get google 
library(ggmap)
library("ggsci")

## register google key 
## in notes
imps_sf 
# register_google("")

# get google basemap
basemap <- ggmap(get_googlemap(center = c(lon = -118.0582, lat = 34.01036), zoom=9, maptype = c( "terrain"), color = "color" ))
 # c("terrain", "satellite", "roadmap", "hybrid")
# # ?get_googlemap
# print(basemap)
# 
# save(basemap, file = "Figures/08_basemap_LA_ggplot.RData")

load(file = "Figures/08_basemap_LA_ggplot.RData")
print(basemap)
# Plot per flow metric
## define metrics to loop through

  
  m1 <- basemap + 
    geom_point(data = imps_sf, aes(x = longitude, y = latitude, colour = Result, size = 1)) +
    scale_color_jco(name = "Impact Category") +
    # scale_fill_discrete(name = "Impact Category") +
    facet_grid(rows = vars(metric), cols = vars(Type)) +
    theme(legend.title = element_blank(), 
          # legend.position = "bottom",
          legend.text=element_text(size=15),
          # axis.text = element_text(size = 15),
          # axis.title = element_text(size = 15),
          strip.text.x = element_text(size = 15),
          strip.text.y = element_text(size = 15)) +
    guides(size = "none")
  m1
  
  file.name1 <- paste0("Figures/08_map_4_cats_per_impact.jpg")
  ggsave(m1, filename=file.name1, dpi=1000, height=10, width=15)
 

