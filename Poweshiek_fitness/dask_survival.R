# Introduction ----

# Author Information 
# Adam Grottoli
# Email: grottoli4@gmail.com

# Description: This code merges multiple years of poweshiek survival and 
# measurement data into a dataframes which associates survival of each larva 
# with lineage, site, mass, and pupal time.

# Libraries ----
library(tidyverse)

# Setup ----
# Change to appropriate workspace
setwd(paste0("P:/Conservation_Research/Restricted/CRD/Research Projects/Pollinators/",
             "Poweshiek_Dakota_Garita_Skippers/R projects/Butterfly-survival"))

# Load data
weekly_check2021 <- read.csv('./Raw_data/dask_weekly_check2021.csv')
larva_measurement2021 <- read.csv('./Raw_data/dask_larva_measurements2021.csv')


# Functions ----

# A function to make survival binary at each of the stages and bind mass data


survival <- function (weekly_data, measurement_data, years_var, first_year, 
                      week_of_diapause, week_after_diapause, final_week) {
  
  mass_wide <- measurement_data %>% drop_na(Mass) %>%
    mutate(pre_post = ifelse(Year == first_year, 'pre', 'post')) %>%
    pivot_wider(id_cols = ID, names_from = pre_post, values_from = Mass)
  weekly_data %>% 
    mutate(survival_to_diapause = ifelse 
           (week_of_diapause == 'A', 1, 0),  # did it enter diapause after hatching
           died_in_diapause = ifelse(week_of_diapause != 'A', NA, ifelse
                                     (week_after_diapause == 'A', 1, 0)),  # did it die in diapause
           survival_diapause_adult = ifelse(week_after_diapause != 'A', NA, 
                                            ifelse(final_week == 'A', 1, 0)),  # did it die between diapasue and adulthood
           survival_after_diapause = ifelse(week_of_diapause == 'A', 1, 0),  # did it exit diapause
           survival_to_adult = ifelse(final_week == 'A', 1, 0)) %>% # did it survive to adult
    select(ID, year, maternal_ID, egg_batch_ID, survival_to_diapause:survival_to_adult) %>%
    left_join(mass_wide, by = 'ID', suffix = c(".x", ".y"))
}


weekly_survival2021 <- weekly_check2021 %>% 
  select(ID, maternal_ID, egg_batch_ID, X11.Oct:X25.Oct) %>%
  mutate(year = 2021)


survival2021 <- survival(weekly_data = weekly_survival2021, measurement_data = 
                           larva_measurement2021, first_year = 2021, 
                         week_of_diapause = weekly_survival2021$X25.Oct, 
                         week_after_diapause = weekly_survival2021$X25.Oct, 
                         final_week = weekly_survival2021$X25.Oct)


maternal_survival <- survival2021 %>%
  group_by(maternal_ID, survival_to_diapause) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  filter(survival_to_diapause == 1)

old = larva_measurement2021 %>%
  mutate(date = make_date(Year, Month, Day)) %>%
  group_by(ID) %>%
  mutate(days_old = max(date) - min(date))
