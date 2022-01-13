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
             "Pollinators/Poweshiek_Dakota_Garita_Skippers/R projects/Butterfly-survival/Poweshiek_fitness"))

# Load in data
all_survival <- read.csv('all_survival.csv')

# Survival rate summaries ----

# Survival rate from neonate to adult by site; all years combined
site_survival_full <- all_survival %>%
  filter(method != "tube") %>%
  group_by(Site, survival_to_adult) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# Survival rate from neonate to diapause by site and year
site_survival_to_diapause <- all_survival %>%
  filter(method != "tube") %>%
  group_by(Site, year, survival_to_diapause) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# Overwinter survival rate by site and year
site_survival_in_diapause <- all_survival %>%
  filter(method != "tube") %>%
  group_by(year, Site, died_in_diapause) %>%
  drop_na(died_in_diapause) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# Surivival after diapause to adult by site; all years combined
site__spring_survival <- all_survival %>%
  filter(method != "tube") %>%
  group_by(Site, survival_diapause_adult) %>%
  drop_na(survival_diapause_adult) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

## We could seperate by year as well but sample size is a constraint
site_year_spring_survival <- all_survival %>%
  filter(method != "tube") %>%
  group_by(Site, year, survival_diapause_adult) %>%
  drop_na(survival_diapause_adult) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# Survival rate from neonate to adult by site and year
site_year_survival <- all_survival %>%
  filter(method != "tube") %>%
  group_by(year, Site, survival_to_adult) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# Average time as a pupa by site
site_pupa_time <- all_survival %>%
  filter(method != "tube") %>%
  drop_na(pupa_days) %>%
  group_by(year) %>%
  mutate(meanTime = mean(pupa_days))

# Maternal ID summary
maternal_survival <- all_survival %>%
  filter(method != "tube", year !=2018) %>%
  group_by(Site, year, survival_to_diapause) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  filter(survival_to_diapause == 1)
ungroup() %>%
  group_by(Site, year) %>%
  summarise(freq = mean(freq), sd = sd(freq))

ggplot(maternal_survival, aes(y = freq, fill = Site, x = year)) +
  geom_col(position = "dodge") +
  theme_classic()


# Overwinter analysis ----
mass_dif <- all_survival %>%
  mutate(mass_dif = post-pre, year = as.factor(year)) %>%
  filter(year != 2018)  # remove 2018 due to low measurement precision


shapiro.test(mass_dif$pre)  # not normally distributed
shapiro.test(log(mass_dif$pre))  # normally distributed 

mass_dif_glm1 <- glm(died_in_diapause ~ (log(pre)), data = mass_dif, family = "binomial")
mass_dif_glm2 <- glm(died_in_diapause ~ (log(pre)) + year, data = mass_dif, family = "binomial")
summary(mass_dif_glm2)

plot(mass_dif_glm)  # no points are overly influential according to cooks D

mass_survival <- (ggplot(mass_dif, aes(y = died_in_diapause, x = (log(pre)))) +
                    geom_point() +
                    geom_smooth(method = "glm", method.args = list(family = "binomial")) +
                    theme_classic())
mass_survival

garita_survival <- read.csv("garita_diapause2018.csv")


# Overwinter analysis for garita ----
mass_dif_gar <- garita_survival %>%
  mutate(mass_dif = mass_out-mass_in, survival_diapause = ifelse 
         (Fate_out == 'A', 1, 0)) %>%
  filter(DC_in != 'OUT')


shapiro.test(mass_dif_gar$mass_in)  # not normally distributed
shapiro.test(log(mass_dif_gar$headcap_in))  # normally distributed 

mass_dif_anova <- anova(mass_dif_glm1)
mass_dif_glm2 <- glm(died_in_diapause ~ (pre) + year, data = mass_dif, family = "binomial")
mass_dif_anova

plot(mass_dif_glm)  # no points are overly influential according to cooks D

mass_survival <- ggplot(mass_dif_gar, aes(group = survival_diapause, y = mass_in)) +
  geom_boxplot() 
geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  theme_classic()
mass_survival
