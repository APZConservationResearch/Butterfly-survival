# Introduction ----

# Author Information 
# Adam Grottoli
# Email: grottoli4@gmail.com

# Description: This code merges multiple years of poweshiek, garita and dakota survival and 
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

survival_colors <- c("#E69F00", "#993300")
# Load data
all_survival <- read.csv("./Overwinter_analysis/overwinter_full_data_KM.csv")

# GASK analysis ----

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

ggplot(data = diapause_survival_GASK, aes(pre_length, survived_diapause)) +
  geom_smooth(color = "steelblue", size = 1, method = "glm", method.args = list(family = "binomial") ) +
  geom_point(color="steelblue")

#POSK analysis

adult_survival_POSK <- all_survival %>%
  filter(species == "POSK", !is.na(diapause_days), !is.na(maternal_ID), 
         !is.na(pre_mass), !is.na(pre_length), !is.na(post_mass), 
         !is.na(DiapauseChamber), survived_diapause == 1) %>%
  mutate(maternal_ID = as.factor(maternal_ID),
         cohort = as.factor(cohort), 
         pre_mass_g = pre_mass * 1000, post_mass_g = post_mass * 1000, 
         mass_dif = post_mass_g - pre_mass_g, 
         mass_prop = ifelse(mass_dif == 0, 1, mass_dif / pre_mass_g), 
         DiapauseChamber = as.factor(paste(DiapauseChamber, cohort, sep = "_"))) 

diapause_survival_POSK <- all_survival %>%
  filter(species == "POSK", !is.na(diapause_days), !is.na(maternal_ID), 
         !is.na(pre_mass), !is.na(pre_length), !is.na(DiapauseChamber), survival_to_diapause == 1) %>%
  mutate(maternal_ID = as.factor(maternal_ID),
         cohort = as.factor(cohort), 
         pre_mass_g = pre_mass * 1000, DiapauseChamber = 
           as.factor(paste(DiapauseChamber, cohort, sep = "_"))) 

POSK_adult_summary <- adult_survival_POSK %>%
  group_by(maternal_ID, survival_diapause_adult) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N))
  
ggplot(POSK_adult_summary, aes(x = maternal_ID, 
                               y = freq, fill = survival_diapause_adult)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = survival_colors, labels = c("0", "1")) +
  labs(x = "materal_ID", y = "Percent", fill = "surived to adult") 
  
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

ggplot(data = adult_survival_POSK, aes(pre_length, survived_diapause)) +
  geom_smooth(color = "steelblue", size = 1, method = "glm", method.args = list(family = "binomial") ) +
  geom_point(color="steelblue")

#DASK analysis

adult_survival_DASK <- all_survival %>%
  filter(species == "DASK", !is.na(diapause_days), !is.na(maternal_ID), 
         !is.na(pre_mass), !is.na(pre_length), !is.na(post_mass), 
         !is.na(DiapauseChamber), survived_diapause == 1) %>%
  mutate(maternal_ID = as.factor(maternal_ID),
         cohort = as.factor(cohort), 
         pre_mass_g = pre_mass * 1000, post_mass_g = post_mass * 1000, 
         mass_dif = post_mass_g - pre_mass_g, 
         mass_prop = ifelse(mass_dif == 0, 1, mass_dif / pre_mass_g), 
         DiapauseChamber = as.factor(paste(DiapauseChamber, cohort, sep = "_"))) 

diapause_survival_DASK <- all_survival %>%
  filter(species == "DASK", !is.na(diapause_days), !is.na(maternal_ID), 
         !is.na(pre_mass), !is.na(pre_length), !is.na(DiapauseChamber), survival_to_diapause == 1) %>%
  mutate(maternal_ID = as.factor(maternal_ID),
         cohort = as.factor(cohort), 
         pre_mass_g = pre_mass * 1000, DiapauseChamber = 
           as.factor(paste(DiapauseChamber, cohort, sep = "_"))) 

DASK_adult_summary <- adult_survival_DASK %>%
  group_by(maternal_ID, survival_diapause_adult) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N))

ggplot(DASK_adult_summary, aes(x = maternal_ID, 
                               y = freq, fill = survival_diapause_adult)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = survival_colors, labels = c("0", "1")) +
  labs(x = "materal_ID", y = "Percent", fill = "surived to adult") 

ggplot() +
  geom_jitter(diapause_survival_DASK, mapping = aes(y = pre_length, x = cohort), width = .1, height = .1)

ggplot() +
  geom_jitter(diapause_survival_DASK, mapping = aes(y = pre_mass, x = cohort), width = .1)

ggplot() +
  geom_jitter(diapause_survival_DASK, mapping = aes(y = diapause_days, x = cohort), width = .1, height = .1)

ggplot() +
  geom_point(diapause_survival_DASK, mapping = aes(y = pre_mass, x = diapause_days))

ggplot() +
  geom_point(diapause_survival_DASK, mapping = aes(y = pre_mass, x = pre_length))

ggplot(data = adult_survival_DASK, aes(pre_length, survived_diapause)) +
  geom_smooth(color = "steelblue", size = 1, method = "glm", method.args = list(family = "binomial") ) +
  geom_point(color="steelblue")

##

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




