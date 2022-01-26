# Introduction ----

# Author Information 
# Adam Grottoli
# Email: grottoli4@gmail.com

# Description: Wrangling of data to be used for Powesheik occurence in ArcGIS storymap

# Data from Environment and Climate change Canada and from Belitz M, Hendrick L, 
# Monfils M, Cuthrell D, Marshall C, Kawahara A, Cobb N, Zaspel J, Horton A, 
# Huber S, Warren A, Forthaus G, Monfils A (2018) Aggregated occurrence records
# of the federally endangered Poweshiek skipperling (Oarisma poweshiek). 
# Biodiversity Data Journal 6: e29081. https://doi.org/10.3897/BDJ.6.e29081

# Any records from Belitz paper with the state as the highest resoltion location 
# were removed. Records without a lat/long location were assigned a location based 
# on the centroid of the highest resolution location avalailble. (i.e., county) 

# Libraries ----
library(tidyverse)

# Setup ----
# Change to appropriate workspace
setwd(paste0("P:/Conservation_Research/Restricted/CRD/Research Projects/Pollinators/",
             "Poweshiek_Dakota_Garita_Skippers/R projects/Butterfly-survival/Occurence_map"))

# Load data
canada_occ <- read.csv("Canada_observations.csv")
belitz_occ <- read.csv("occurrence_belitz2018.csv")

# Find totoal number of observation in a year at each location
belitz_simple <- belitz_occ %>% 
  drop_na(year, Latitude) %>%
  mutate(individualCount = replace_na(individualCount, 1)) %>%
  group_by(year, Latitude, Longitude) %>%
  summarise(n = sum(individualCount))
  

# group Canada and Belitz observations
posk_occurrence <- bind_rows(canada_occ, belitz_simple)
posk_occ_simp <-posk_occurrence %>% mutate(across(3:4, round, 2))
posk_occ_simp$date <- paste0("01/", posk_occ_simp$year)

write.csv(posk_occ_simp, paste0("P:/Conservation_Research/Restricted/CRD/Research Projects/",
"Pollinators/Poweshiek_Dakota_Garita_Skippers/R projects/Butterfly-survival/Occurence_map/posk_occurrence.csv"), 
          row.names = FALSE)
