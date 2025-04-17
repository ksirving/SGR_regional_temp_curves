## BRTs for relative importance

### CSCI brts

library(gbm)
library(dismo)
library(ggplot2)
library(purrr)
library(dplyr)
library(tidyverse)

set.seed(321) # reproducibility

## brt function - Ryan Peek
source("Code/functions/My.gbm.step.R")

## upload data

load(file="ignore/output/03_bugs_temp_joined_by_year.RData")
head(AllData)
## format for model
names(data)
data <- AllData %>%
  filter(Metric =="csci") %>%
  drop_na(CurMetric, CurTemp) %>%
  select(masterid, year, COMID, Metric, MetricValue, CurMetric, CurTemp) %>%
  distinct() %>%
  pivot_wider(names_from = CurMetric, values_from = CurTemp) %>%
  select(-Max_Wkl_Max_StreamT_grt_30_) %>%
  drop_na()

data <- as.data.frame(data)
sum(is.na(data))
head(data)
class(data)
## gbm functions in brt.functions script

## CSCI

## define criteria

# set up tuning params
hyper_grid <- expand.grid(
  shrinkage = c(0.001, 0.003, 0.005), 
  interaction.depth = c(5), 
  n.minobsinnode = c(3, 5, 10), 
  bag.fraction = c(0.75, 0.8) 
)


# double check and view
hyper_grid

# load the GBM.step function (requires dismo and function loaded)
gbm_fit_step <- function(
    shrinkage, interaction.depth, n.minobsinnode, bag.fraction, data) {
  set.seed(123) # make it reproducible
  m_step <- My.gbm.step(
    gbm.y = 5, # response in training data
    gbm.x = 6:10, # temp dat
    family = "gaussian",
    data = data,
    #max.trees = 8000, # can specify but don't for now
    learning.rate = shrinkage,
    tree.complexity = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    bag.fraction = bag.fraction,
    plot.main = FALSE,
    verbose = FALSE
  )
  
  # Compute the Deviance Explained: (total dev - cv dev) / total dev
  if(!is.null(m_step)){ # this helps if there's an error above
    (m_step$self.statistics$mean.null - m_step$cv.statistics$deviance.mean) /
      m_step$self.statistics$mean.null
  } else { 
    return(NA)
  }
}

# use PURRR: this part can take awhile...get some coffee
hyper_grid$dev_explained <-purrr::pmap_dbl(
  hyper_grid,
  ~ gbm_fit_step(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = data) # CHECK AND CHANGE!!
)

# look at results:
hyper_grid %>% 
  dplyr::arrange(desc(dev_explained)) %>%
  head(5) # top 5 models

# pick the best solution
(hyper_best <- hyper_grid %>% 
    dplyr::arrange(desc(dev_explained)) %>% #
    head(n=1))

# based on above, run final BRT and save:
gbm_final_step <- function(
    shrinkage, interaction.depth, n.minobsinnode, bag.fraction, data) {
  set.seed(123) # make it reproducible
  m_final <- My.gbm.step(
    gbm.y = 5, # response in training data
    gbm.x = 6:10, # hydro dat
    family = "gaussian",
    data = data,
    learning.rate = shrinkage,
    tree.complexity = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    bag.fraction = bag.fraction,
    plot.main = TRUE,
    verbose = TRUE
  )
}

gbm_final_step
# set up filename for best model outputs
(gbm_best_file <- paste0("ignore/models/04_gbm_final_csci_model_output.txt"))

# run best option with PURR
capture.output(gbm_fin_out <- purrr::pmap(
  hyper_best,
  ~ gbm_final_step(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = data # CHECK AND CHANGE!!
  )
), file=gbm_best_file, append=T)

#strip off a list layer to view data
(gbm_fin_out <- gbm_fin_out[[1]])

# add hyperbest to capture output file:
cat("\nBest parameters for GBM.STEP:\n\n", 
    file = gbm_best_file, append=TRUE)

# add the parameters used to run the model
write.csv(hyper_best, "ignore/models/04_best_model_csci_output.csv")

# % percent explained
(gbm_fin_out$self.statistics$mean.null - gbm_fin_out$cv.statistics$deviance.mean) / gbm_fin_out$self.statistics$mean.null 
# 0.59


# 10. SAVE FINAL GBM AND DATA ---------------------------------------------------------------

# reassign names for RI outputs and save:
assign(x = tolower(paste0("gbm_final_csci")), value=gbm_fin_out)

# get file name
(fileToSave <- ls(pattern = paste0("gbm_final_csci")))

# save to RDS
write_rds(x = get(fileToSave), path = paste0("ignore/models/04_",fileToSave, "_model.rds"), compress = "gz")

# Save all the datasets used in the model:
save(list = ls(pattern="data_"), file = tolower(paste0("ignore/models/04_",fileToSave,"_model_data.rda")))

gbm_final <- read_rds("ignore/models/04_gbm_final_csci_model.rds")
class(gbm_final)

gbm_fin_RI<-as.data.frame(summary(gbm_final, plotit = F, method=relative.influence)) 
gbm_fin_RI  


# var   rel.inf
# Max_Wkl_Max_StreamT     Max_Wkl_Max_StreamT 34.063806
# Min_Wkl_Min_StreamT     Min_Wkl_Min_StreamT 25.261261
# Mean_Wkl_Rng_StreamT   Mean_Wkl_Rng_StreamT 17.614401
# Max_Wkly_Mean_StreamT Max_Wkly_Mean_StreamT 13.132074
# Max_Wkl_Rng_StreamT     Max_Wkl_Rng_StreamT  9.928458


# Plots and metrics-------------------------------------------------------------------


write.csv(gbm_fin_RI, "ignore/output/04_rel_imp_csci_labels.csv")


ggplot(data=gbm_fin_RI, aes(x=reorder(var,-rel.inf), y=rel.inf, fill = var)) +
  geom_bar(stat="identity") +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 75, vjust = 1, hjust=1))+
  # scale_x_continuous(limits = c(0, 35)) +
  labs(title = "Relative Importance of Temp on CSCI",
       x = "Temperature Metric",
       y = "Relative Importance (%)") #+ theme_bw(base_size = 15)




gbm_fin_RI



# ASCI --------------------------------------------------------------------

set.seed(321) # reproducibility

## upload data

load(file="ignore/output/03_algae_temp_joined_by_year.RData")
head(AllDataA)
## fiormat for models
names(data)
data <- AllDataA %>%
  filter(Metric =="ASCI_Hybrid") %>%
  drop_na(CurMetric, CurTemp) %>%
  select(masterid, year, COMID, Metric, MetricValue, CurMetric, CurTemp) %>%
  distinct() %>%
  pivot_wider(names_from = CurMetric, values_from = CurTemp) %>%
  select(-Max_Wkl_Max_StreamT_grt_30_) %>%
  drop_na()

data <- as.data.frame(data)
## gbm functions in brt.functions script

## ASCI

# set up tuning params
hyper_grid <- expand.grid(
  shrinkage = c(0.001, 0.003, 0.005), 
  interaction.depth = c(5), 
  n.minobsinnode = c(3, 5, 10), 
  bag.fraction = c(0.75, 0.8) 
)

# double check and view
hyper_grid

names(data)

# load the GBM.step function (requires dismo and function loaded)
gbm_fit_step <- function(
    shrinkage, interaction.depth, n.minobsinnode, bag.fraction, data) {
  set.seed(123) # make it reproducible
  m_step <- My.gbm.step(
    gbm.y = 5, # response in training data
    gbm.x = 6:10, # temp dat
    family = "gaussian",
    data = data,
    #max.trees = 8000, # can specify but don't for now
    learning.rate = shrinkage,
    tree.complexity = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    bag.fraction = bag.fraction,
    plot.main = FALSE,
    verbose = FALSE
  )
  
  # Compute the Deviance Explained: (total dev - cv dev) / total dev
  if(!is.null(m_step)){ # this helps if there's an error above
    (m_step$self.statistics$mean.null - m_step$cv.statistics$deviance.mean) /
      m_step$self.statistics$mean.null
  } else { 
    return(NA)
  }
}

# use PURRR: this part can take awhile...get some coffee
hyper_grid$dev_explained <-purrr::pmap_dbl(
  hyper_grid,
  ~ gbm_fit_step(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = data) 
)

# look at results:
hyper_grid %>% 
  dplyr::arrange(desc(dev_explained)) %>%
  head(5) # top 5 models

# pick the best solution
(hyper_best <- hyper_grid %>% 
    dplyr::arrange(desc(dev_explained)) %>% #
    head(n=1))


# based on above, run final BRT and save:
gbm_final_step <- function(
    shrinkage, interaction.depth, n.minobsinnode, bag.fraction, data) {
  set.seed(123) # make it reproducible
  m_final <- My.gbm.step(
    gbm.y = 5, # response in training data
    gbm.x = 6:10, # hydro dat
    family = "gaussian",
    data = data,
    learning.rate = shrinkage,
    tree.complexity = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    bag.fraction = bag.fraction,
    plot.main = TRUE,
    verbose = TRUE
  )
}

# set up filename for best model outputs
(gbm_best_file <- paste0("ignore/models/04_gbm_final_asci_model_output.txt"))

# run best option with PURR
capture.output(gbm_fin_out <- purrr::pmap(
  hyper_best,
  ~ gbm_final_step(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = data # CHECK AND CHANGE!!
  )
), file=gbm_best_file, append=T)

#strip off a list layer to view data
(gbm_fin_out <- gbm_fin_out[[1]])

# add hyperbest to capture output file:
cat("\nBest parameters for GBM.STEP:\n\n", 
    file = gbm_best_file, append=TRUE)

# add the parameters used to run the model
write.csv(hyper_best, "ignore/models/04_best_model_asci_output.csv")

# % percent explained
(gbm_fin_out$self.statistics$mean.null - gbm_fin_out$cv.statistics$deviance.mean) / gbm_fin_out$self.statistics$mean.null 
# 0.38


# 10. SAVE FINAL GBM AND DATA ---------------------------------------------------------------

# reassign names for RI outputs and save:
assign(x = tolower(paste0("gbm_final_asci")), value=gbm_fin_out)

# get file name
(fileToSave <- ls(pattern = paste0("gbm_final_asci")))


# save to RDS
write_rds(x = get(fileToSave), path = paste0("ignore/models/04_",fileToSave, "_model.rds"), compress = "gz")

# Save all the datasets used in the model:
save(list = ls(pattern="data_"), file = tolower(paste0("ignore/models/04_",fileToSave,"_model_data.rda")))

gbm_final <- read_rds("ignore/models/04_gbm_final_asci_model.rds")
class(gbm_final)

gbm_fin_RI<-as.data.frame(summary(gbm_final, plotit = F, method=relative.influence)) 
gbm_fin_RI  

# var   rel.inf
# Min_Wkl_Min_StreamT     Min_Wkl_Min_StreamT 35.362129
# Max_Wkl_Max_StreamT     Max_Wkl_Max_StreamT 23.227669
# Max_Wkly_Mean_StreamT Max_Wkly_Mean_StreamT 20.070855
# Mean_Wkl_Rng_StreamT   Mean_Wkl_Rng_StreamT 13.951777
# Max_Wkl_Rng_StreamT     Max_Wkl_Rng_StreamT  7.387571

# Plots and metrics-------------------------------------------------------------------


## combine with rel importance

write.csv(gbm_fin_RI, "ignore/output/04_rel_imp_asci_labels.csv")

## plot
ggplot(data=gbm_fin_RI, aes(x=reorder(var,-rel.inf), y=rel.inf, fill = var)) +
  geom_bar(stat="identity") +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 75, vjust = 1, hjust=1))+
  labs(title = "Relative Importance of Temp on ASCI",
       x = "Temperature Metric",
       y = "Relative Importance (%)") #+ theme_bw(base_size = 15)

gbm_fin_RI

### brt figure

library(tidyverse)

out.dir <- "Figures/"

### upload rel importance
gbm_fin_RI_csci <- read.csv("ignore/output/04_rel_imp_csci_labels.csv")
gbm_fin_RI_asci <- read.csv("ignore/output/04_rel_imp_asci_labels.csv")

gbm_fin_RI_csci <- gbm_fin_RI_csci %>%
  mutate(Index = "CSCI")

gbm_fin_RI_asci <- gbm_fin_RI_asci %>%
  mutate(Index = "ASCI")

gbm_fin_RI <- rbind(gbm_fin_RI_csci, gbm_fin_RI_asci) %>%
  rename(Var = var) #%>%
  # mutate(Index = recode_factor(Index, CSCI="CSCI", ASCI="ASCI"))

gbm_fin_RI <- gbm_fin_RI %>%
  mutate(TempMetric = case_when(Var ==  "Max_Wkl_Max_StreamT_grt_30_"~"Weeks greater than 86F",
                                Var ==  "Max_Wkly_Mean_StreamT" ~ "Weekly Mean",
                                Var == "Max_Wkl_Max_StreamT" ~ "Weekly Maximum",
                                Var == "Min_Wkl_Min_StreamT" ~ "Weekly Minimum",
                                Var == "Max_Wkl_Rng_StreamT" ~ "Max Weekly Range",
                                Var == "Mean_Wkl_Rng_StreamT" ~  "Mean Weekly Range"))
gbm_fin_RI

c1 <- ggplot(data=gbm_fin_RI, aes(x=reorder(TempMetric,-rel.inf), y=rel.inf, fill = TempMetric)) +
  geom_bar(stat="identity") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle = 75, vjust = 1, hjust=1))+
  # scale_x_continuous(limits = c(0, 35)) +
  facet_wrap(~Index) +
  labs(title = "",
       x = "",
       y = "Relative Importance (%)")+
  theme(legend.title=element_blank())
  
c1

out.filename <- paste0(out.dir,"04_rel_imp_csci_asci_bar_plot_n1.jpg")
ggsave(c1, file = out.filename, dpi=300, height=4, width=6)


asci_RI <- gbm_fin_RI_asci %>%
  rename(ASCI = rel.inf) %>%
  select(var, ASCI)

csci_RI <- gbm_fin_RI_csci %>%
  rename(CSCI = rel.inf) %>%
  select(-X)

all_rf <- merge(asci_RI, csci_RI, by = "var")
head(all_rf)

all_rf <- all_rf %>%
  select(var, Flow.Metric.Name, Flow.Component, CSCI, ASCI)

write.csv(all_rf, "ignore/output/06_relative_imp_table.csv")








