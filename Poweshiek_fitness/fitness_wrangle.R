# Introduction ----

# Author Information 
# Adam Grottoli
# Email: grottoli4@gmail.com

# Description: This code merges multiple years of poweshiek survival and 
# measurement data into a dataframes which associates survival of each larva 
# with lineage, site, mass, and pupal time.

# Libraries ----
library(tidyverse)
library(lubridate)

# Setup ----
# Change to appropriate workspace
setwd(paste0("P:/Conservation_Research/Restricted/CRD/Research Projects/Pollinators/",
             "Poweshiek_Dakota_Garita_Skippers/R projects/Butterfly-survival"))

# Load data
egg_batches2018 <- read.csv('./Raw_data/POSK_egg_batches2018.csv', header = T)

weekly_check2018 <- read.csv('./Raw_data/POSK_weekly_check2018.csv')

weekly_check2019 <- read.csv('./Raw_data/POSK_weekly_check2019.csv')

weekly_check2020 <- read.csv('./Raw_data/POSK_weekly_check2020.csv')

weekly_check2021 <- read.csv('./Raw_data/POSK_weekly_check2021.csv')

egg_collection2018 <- read.csv('./Raw_data/POSK_egg_collection2018.csv')

egg_collection2019 <- read.csv('./Raw_data/POSK_egg_collection2019.csv')

egg_collection2020 <- read.csv('./Raw_data/POSK_egg_collection2020.csv')

egg_collection2021 <- read.csv('./Raw_data/POSK_egg_collection2021.csv')

pupa_check2019 <- read.csv('./Raw_data/POSK_pupa_check2019.csv')

pupa_check2018 <- read.csv('./Raw_data/POSK_pupa_check2018.csv')

pupa_check2020 <- read.csv('./Raw_data/POSK_pupa_check2020.csv')

larva_measurement2020 <- read.csv('./Raw_data/POSK_larva_measurement2020.csv')

larva_measurement2019 <- read.csv('./Raw_data/POSK_larva_measurement2019.csv')

larva_measurement2018 <- read.csv('./Raw_data/POSK_larva_measurement2018.csv') %>%
  mutate(DiapauseChamber = parse_number(CurrentLocation))

larva_measurement2021 <- read.csv('./Raw_data/POSK_larva_measurement2021.csv')

GASK_larva_measurement2018 <- read.csv('./Raw_data/GASK_larva_measurement2018.csv') %>%
  mutate(DiapauseChamber = parse_number(CurrentLocation))

GASK_weekly_check2019 <- read.csv('./Raw_data/GASK_weekly_check2019.csv')


# Functions ----

# A function to make survival binary at each of the stages and bind mass data

survival <- function (weekly_data, measurement_data, cohort, 
                      week_of_diapause, week_after_diapause, final_week) {
  
  measures_pre <- measurement_data %>% 
    mutate(date_in = make_date(Year, Month, Day)) %>%
    filter(Year == cohort) %>% 
    group_by(ID) %>% 
    filter(date_in == max(date_in)) %>% 
    ungroup() %>% 
    select(ID, Length, CurrentLocation, date_in, Mass, DiapauseChamber) %>% 
    drop_na(Mass) %>% 
    mutate(pre_length = Length, pre_mass = Mass) %>% 
    select(-Length, -Mass)
  measures_post <- measurement_data %>% 
    mutate(date_out = make_date(Year, Month, Day)) %>%
    filter(Year != cohort) %>% 
    group_by(ID) %>% 
    filter(date_out == min(date_out)) %>% 
    ungroup() %>% 
    select(ID, Length, date_out, Mass) %>% 
    drop_na(Mass) %>% 
    mutate(post_length = Length, post_mass = Mass) %>% 
    select(-Length, -Mass)
    
  weekly_data %>% 
    mutate(method = ifelse(method == "tube", "tube", "plant"), 
           survival_to_diapause = ifelse 
           (week_of_diapause == 'A', 1, 0),  # did it enter diapause after hatching
           died_in_diapause = ifelse(week_of_diapause != 'A', NA, ifelse
                                     (week_after_diapause == 'A', 1, 0)),  # did it die in diapause
           survival_diapause_adult = ifelse(week_after_diapause != 'A', NA, 
                                            ifelse(final_week == 'A', 1, 0)),  # did it die between diapasue and adulthood
           survival_after_diapause = ifelse(week_of_diapause == 'A', 1, 0),  # did it exit diapause
           survival_to_adult = ifelse(final_week == 'A', 1, 0)) %>% # did it survive to adult
    select(ID, year, maternal_ID, Site, egg_batch_ID, method, survival_to_diapause:survival_to_adult) %>%
  left_join(measures_pre, by = 'ID', suffix = c(".x", ".y")) %>%
  left_join(measures_post, by = 'ID', suffix = c(".x", ".y")) %>%
    mutate(diapause_days = (date_out - date_in)) %>%
    mutate(species = "POSK")
}

  
# Function to count the number of days each individual spent as a pupa
# Input the different characters for different stages of pupation 

pupa.time <- function (df, pupa_character1, pupa_character2, pupa_character3, pupa_character4= NULL) {
  df <- data.frame(lapply(df, function (x)
    replace(x, x %in% c(pupa_character1, pupa_character2, pupa_character3), "P"))) 
  pupa_days <- rowSums(df == "P", na.rm = T)  # Count the number of days as pupa, or "P", per row
  p_days <- cbind(pupa_days, df)
  p_days <- p_days[p_days$pupa_days != 0, ]  # get rid of any that didn't pupate
  p_days %>% select(ID, pupa_days)
}


# Merging data ---- 
# Create an egg batch column from the larva id coloumn if not already there
weekly_check2018$egg_batch_ID <- substr(weekly_check2018$"ID",0 ,5)

GASK_weekly_check2019$egg_batch_ID <- substr(GASK_weekly_check2019$"ID",0, 6)

weekly_check2019$egg_batch_ID <- substr(weekly_check2019$"ID",0, 6)

# Join maternal ID and site from the egg batch column if not already there and
# select important variables and create a year column
weekly_survival2018 <- inner_join(weekly_check2018, egg_batches2018, 
                                  by = 'egg_batch_ID', suffix = c(".x", ".y")) %>%
  inner_join(egg_collection2018, by = 'maternal_ID', suffix = c(".x", ".y")) %>%
  select(ID, maternal_ID, Site, egg_batch_ID, X09.Jul:X08.Jul) %>%
  mutate(year = 2018, method = "")

weekly_survival2019 <- inner_join(weekly_check2019, egg_collection2019, 
                                  by = 'egg_batch_ID', suffix = c(".x", ".y")) %>% 
  select(ID, maternal_ID, Site, egg_batch_ID, X22.Jul:Adult) %>%
  mutate(year = 2019, method = "")

GASK_weekly_survival2019 <- inner_join(GASK_weekly_check2019, GASK_egg_collection2019, 
                                      by = 'egg_batch_ID', suffix = c(".x", ".y")) %>% 
  select(ID, maternal_ID, Site, egg_batch_ID, X22.Jul:Adult) %>%
  mutate(year = 2019, method = "")

weekly_survival2020 <- inner_join(weekly_check2020, egg_collection2020, 
                                  by = 'maternal_ID', suffix = c(".x", ".y")) %>% 
  rename(c('egg_batch_ID' = 'Eggbatch')) %>%
  select(ID, maternal_ID, Site, egg_batch_ID, method, X13.Jul:Adult) %>%
  mutate(year = 2020) 

weekly_survival2021 <- inner_join(weekly_check2021, egg_collection2021, 
                                  by = 'egg_batch_ID', suffix = c("", ".y")) %>% 
  select(ID, maternal_ID, Site, egg_batch_ID, X11.Oct:X25.Oct) %>%
  mutate(year = 2021, method = "")

# Run the survial function for each year 
survival2018 <- survival(weekly_data = weekly_survival2018, measurement_data = 
                           larva_measurement2018, cohort = 2018, week_of_diapause = 
                           weekly_survival2018$X12.Nov, week_after_diapause = 
                           weekly_survival2018$X06.May, final_week = 
                           weekly_survival2018$X08.Jul)

survival2019 <- survival(weekly_data = weekly_survival2019, measurement_data =
                         larva_measurement2019, cohort = 2019,
                         week_of_diapause = weekly_survival2019$X07.Oct, 
                         week_after_diapause = weekly_survival2019$X27.Apr, 
                         final_week = weekly_survival2019$Adult)

survival2020 <- survival(weekly_data = weekly_survival2020, measurement_data = 
                           larva_measurement2020, cohort = 2020, 
                         week_of_diapause = weekly_survival2020$X05.Oct, 
                         week_after_diapause = weekly_survival2020$X26.Apr, 
                         final_week = weekly_survival2020$Adult)

survival2021 <- survival(weekly_data = weekly_survival2021, measurement_data = 
                           larva_measurement2021, cohort = 2021, 
                         week_of_diapause = weekly_survival2021$X25.Oct, 
                         week_after_diapause = weekly_survival2021$X25.Oct, 
                         final_week = weekly_survival2021$X25.Oct)

# Run the pupa time function for each year
pupa_time2019 <- pupa.time(pupa_check2019, "O", "R", "B")  
pupa_time2018 <- pupa.time(pupa_check2018, "O", "R", "B")
pupa_time2020 <- pupa.time(pupa_check2020, "O", "R", "B", "Field")

# Bind all of the data together
all_survival <- left_join((bind_rows(survival2018, survival2019, survival2020, survival2021)),
                          (bind_rows(pupa_time2018, pupa_time2019, pupa_time2020)), 
                          by = c('ID'), suffix = c(".x", ".y"))

# Export it as a csv. to aviod rerunning this code to do analysis
write.csv(all_survival, paste0("P:/Conservation_Research/Restricted/CRD/Research Projects/",
                               "Pollinators/Poweshiek_Dakota_Garita_Skippers/R projects/Butterfly-survival/Poweshiek_fitness/all_survival.csv"), 
          row.names = FALSE)