# Introduction ----

# Author Information 
# Adam Grottoli
# Email: grottoli4@gmail.com

# Description: This code merges multiple years of poweshiek and garita survival and 
# measurement data into a dataframes which associates survival of each larva 
# with lineage, site, mass, and pupal time.

# Libraries ----
library(tidyverse)
library(ggplot2)
library(lubridate)
library(car)
library(lme4)
library(MuMIn)

# Setup ----
# Change to appropriate workspace
setwd(paste0("P:/Conservation_Research/Restricted/CRD/Research Projects/Pollinators/",
             "Poweshiek_Dakota_Garita_Skippers/R projects/Butterfly-survival"))

# Load data
all_survival <- read.csv("./Overwinter_analysis/overwinter_full_data.csv")

# GASK analysis ----
# 
diapause_survival_GASK <- all_survival %>%
  filter(species == "GASK", !is.na(diapause_days), !is.na(maternal_ID), !is.na(pre_mass), !is.na(pre_length), 
         !is.na(DiapauseChamber), survival_to_diapause == 1, cohort != 2017, DiapauseChamber != "OUT") %>%
  mutate(maternal_ID = as.factor(maternal_ID),
         cohort = as.factor(cohort), survived_diapause = as.factor(survived_diapause), 
         pre_mass_scale = pre_mass * 1000, post_mass_scale = post_mass * 1000, 
         mass_dif = post_mass_scale - pre_mass_scale,
         DiapauseChamber = as.factor(paste(DiapauseChamber, cohort, sep = "_")))

ggplot() +
  geom_jitter(diapause_survival_GASK, mapping = aes(y = pre_length, x = cohort), width = .1, height = .1)

ggplot() +
  geom_jitter(diapause_survival_GASK, mapping = aes(y = pre_mass, x = cohort), width = .1)

ggplot() +
  geom_jitter(diapause_survival_GASK, mapping = aes(y = diapause_days, x = cohort), width = .1, height = .1)

ggplot() +
  geom_point(diapause_survival_GASK, mapping = aes(y = pre_mass, x = diapause_days))

ggplot() +
  geom_point(diapause_survival_GASK, mapping = aes(y = pre_mass, x = pre_length))

#POSK analysis

adult_survival_POSK <- all_survival %>%
  filter(species == "POSK", !is.na(diapause_days), !is.na(maternal_ID), !is.na(pre_mass), !is.na(pre_length), !is.na(post_mass), !is.na(DiapauseChamber), survived_diapause == 1) %>%
  mutate(maternal_ID = as.factor(maternal_ID),
         cohort = as.factor(cohort), 
         pre_mass_g = pre_mass * 1000, post_mass_g = post_mass * 1000, mass_dif = post_mass_g - pre_mass_g, mass_prop = ifelse(mass_dif == 0, 1, mass_dif / pre_mass_g), DiapauseChamber = as.factor(paste(DiapauseChamber, cohort, sep = "_"))) %>%
  mutate(plant = ifelse(is.na(plant), 0, (plant/100)))

diapause_survival_POSK <- all_survival %>%
  filter(species == "POSK", !is.na(diapause_days), !is.na(maternal_ID), !is.na(pre_mass), !is.na(pre_length), !is.na(DiapauseChamber), survival_to_diapause == 1) %>%
  mutate(maternal_ID = as.factor(maternal_ID),
         cohort = as.factor(cohort), 
         pre_mass_g = pre_mass * 1000, DiapauseChamber = as.factor(paste(DiapauseChamber, cohort, sep = "_"))) %>%
  mutate(plant = ifelse(is.na(plant), 0, (plant/100)))


summary <- diapause_survival_POSK %>%
  group_by(maternal_ID, survival_to_diapause)%>%
summarise(count = n())         
  
ggplot() +
  geom_jitter(diapause_survival_POSK, mapping = aes(y = pre_length, x = cohort), width = .1, height = .1)

ggplot() +
  geom_jitter(diapause_survival_POSK, mapping = aes(y = pre_mass, x = cohort), width = .1)

ggplot() +
  geom_jitter(diapause_survival_POSK, mapping = aes(y = diapause_days, x = cohort), width = .1, height = .1)

ggplot() +
  geom_point(diapause_survival_POSK, mapping = aes(y = pre_mass, x = diapause_days))

ggplot() +
  geom_point(diapause_survival_POSK, mapping = aes(y = pre_mass, x = pre_length))

VIF_test <- glmer(data = diapause_survival_POSK, survived_diapause ~ pre_mass + pre_length + (1|maternal_ID), family = binomial)

vif(VIF_test)

opt_ran_test_1 <- glmer(data = diapause_survival_POSK, survived_diapause ~ pre_mass_g + pre_length + cohort + plant + (1|DiapauseChamber) + (1|maternal_ID), family = binomial)

opt_ran_test_2 <- glm(data = diapause_survival_POSK, survived_diapause ~ pre_mass_g + cohort, family = binomial)

opt_ran_test_3 <- glmer(data = diapause_survival_POSK, survived_diapause ~ pre_mass_g + pre_length + cohort + plant + (1|maternal_ID), family = binomial)

GASK_ran_test_1 <- glmer(data = diapause_survival_GASK, survived_diapause ~ pre_mass_g + pre_length + cohort + plant + (1|DiapauseChamber) + (1|maternal_ID), family = binomial)

GASK_ran_test_2 <- glm(data = diapause_survival_GASK, survived_diapause ~ pre_mass_g + pre_length + cohort + plant, family = binomial)

GASK_ran_test_3 <- glmer(data = diapause_survival_GASK, survived_diapause ~ pre_mass_g + pre_length + cohort + plant +(1|maternal_ID), family = binomial)

GASK_ran_test_4 <- glmer(data = diapause_survival_GASK, survived_diapause ~ pre_mass_g + pre_length + cohort + plant + (1|DiapauseChamber), family = binomial)

aic_POSK <- AICc(opt_ran_test_1, opt_ran_test_2, opt_ran_test_3)

aic_GASK <- AICc(GASK_ran_test_1, GASK_ran_test_2, GASK_ran_test_3, GASK_ran_test_4)

summary(GASK_ran_test_3)

ggplot(data = diapause_survival_GASK, aes(pre_length, survived_diapause)) +
  geom_smooth(color = "steelblue", size = 1, method = "glm", method.args = list(family = "binomial") ) +
  geom_point(color="steelblue") + 
  facet_wrap(~maternal_ID)



