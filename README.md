# SGR_regional_temp_curves

Code that made the tech memo - https://sccwrp.sharepoint.com/:w:/s/SanGabrielTempEffects/EasQG1SosVVFhlh_ICy1MGIBY4L-X-QLeFVFvYdGRDtIMQ?e=gKewZI

# Script 01_regional_bio_sites.R

uploads all bio data, joins and formats
looks at SCAPE data for modified threshold

# Script: 02_stream_temp_data.R

joins temp data to NHD lines
formats
calculates alteration
adds channel enginnering 

# Script 03a-e

exploratory models looking at - 
GLM predicting probability - standard and alteration models
Quadratic LM predicting scores
observations visuals
using traits for csci and asci

# Script 04_relative_importance

runs boosted regression trees to compute relative importance

# Script 05_testing_climate_indicator.R

code to test out a climate indicator from Lawrence et al. (2010) 
done as a curiosity after conversion with Nate Butler (Stillwater Sciences)

# Script 06_temp_logger_info

gathers all the temp data we have
it was formatted and mapped externally in the temp modelling teams channel

https://sccwrp.sharepoint.com/:f:/s/TempModelling/EltfbGFh9Y9MpNleP0-tBmoB_d3yANmphAxw343IdspVsg?e=n0IdLV

## Script 07_validation

attempted validation with observed data

## Script 08_temp_bio_categorizing

sorts sites into categories and counts
creates map using google maps - need a key (make sure to delete key before pushing to guthub)


