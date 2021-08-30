# Introduction ----

# Author Information 
# Adam Grottoli
# Email: grottoli4@gmail.com

# Description: Analysis and visualization of poweshiek survival and fitness data

# Libraries ----
library(tidyverse)

# Setup ----
# Change to appropriate workspace
setwd(paste0("P:/Conservation_Research/Restricted/CRD/Research Projects/",
             "Pollinators/Poweshiek_Dakota_Garita_Skippers/R projects/Poweshiek Fitness"))

# Load in data
all_survival <- read.csv('all_survival.csv')

# Survival rate summaries ----

# Survival rate from hatching to adult by site; all years combined
site_survival_full <- all_survival %>%
  group_by(Site, survival_to_adult) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# Survival rate from hatching to diapause by site and year
site_survival_to_diapause <- all_survival %>%
  group_by(year, Site, survival_to_diapause) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# Overwinter survival rate by site and year
site_survival_in_diapause <- all_survival %>%
  group_by(year, Site, died_in_diapause) %>%
  drop_na(died_in_diapause) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# Surivival after diapause to adult by site; all years combined
site__spring_survival <- all_survival %>%
  group_by(Site, survival_diapause_adult) %>%
  drop_na(survival_diapause_adult) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

## We could seperate by year as well but sample size is a contraint
site_year_spring_survival <- al_survival %>%
  group_by(Site, year, survival_diapause_adult) %>%
  drop_na(survival_diapause_adult) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# Survival rate from hatching to adult by site and year
site_year_survival <- all_survival %>%
  group_by(year, Site, survival_to_adult) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# Average time as a pupa by site
site_pupa_time <- all_survival %>%
  drop_na(pupa_days) %>%
  group_by(year) %>%
  mutate(meanTime = mean(pupa_days))

# Overwinter analysis ----
mass_dif <- all_survival %>%
  mutate(mass_dif = post-pre) %>%
  filter(year != 2018)  # remove 2018 due to low measurement precision

shapiro.test(pre)  # not normally distributed
shapiro.test(log(pre))  # normally distributed 

mass_dif_glm <- glm(died_in_diapause ~ (log(pre)), data = mass_dif, family = "binomial")
summary(mass_dif_glm)

plot(mass_dif_glm)  # no points are overly influential according to cooks D

mass_survival <- (ggplot(mass_dif, aes(y = died_in_diapause, x = (log(pre)))) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  theme_classic())
