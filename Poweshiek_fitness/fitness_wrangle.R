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
egg_batches2018 <- read.csv('./Raw_data/egg_batches2018.csv', header = T)
weekly_check2018 <- read.csv('./Raw_data/weekly_check2018.csv')
weekly_check2019 <- read.csv('./Raw_data/weekly_check2019.csv')
weekly_check2020 <- read.csv('./Raw_data/weekly_check2020.csv')
weekly_check2021 <- read.csv('./Raw_data/weekly_check2021.csv')
egg_collection2018 <- read.csv('./Raw_data/egg_collection2018.csv')
egg_collection2019 <- read.csv('./Raw_data/egg_collection2019.csv')
egg_collection2020 <- read.csv('./Raw_data/egg_collection2020.csv')
egg_collection2021 <- read.csv('./Raw_data/egg_collection2021.csv')
pupa_check2019 <- read.csv('./Raw_data/pupa_check2019.csv')
pupa_check2018 <- read.csv('./Raw_data/pupa_check2018.csv')
pupa_check2020 <- read.csv('./Raw_data/pupa_check2020.csv')
larva_measurement2020 <- read.csv('./Raw_data/larva_measurement2020.csv')
larva_measurement2019 <- read.csv('./Raw_data/larva_measurement2019.csv')
larva_measurement2018 <- read.csv('./Raw_data/larva_measurement2018.csv')


# Functions ----

# A function to make survival binary at each of the stages and bind mass data

survival <- function (weekly_data, measurement_data, years_var, first_year, 
                      week_of_diapause, week_after_diapause, final_week) {
  
  mass_wide <- measurement_data %>% drop_na(Mass) %>%
    mutate(pre_post = ifelse(Year == first_year, 'pre', 'post')) %>%
    pivot_wider(id_cols = ID, names_from = pre_post, values_from = Mass)
  weekly_data %>% 
    mutate(survival_to_diapause = ifelse 
           (week_of_diapause == 'A', 1, 0)) %>%  # did it enter diapause after hatching
    mutate(died_in_diapause = ifelse(week_of_diapause != 'A', NA, ifelse
          (week_after_diapause == 'A', 1, 0))) %>% # did it die in diapause
    mutate(survival_diapause_adult = ifelse(week_after_diapause != 'A', NA, 
          (ifelse(final_week == 'A', 1, 0)))) %>% # did it die between diapasue and adulthood
    mutate(survival_after_diapause = ifelse(week_of_diapause == 'A', 1, 0)) %>% # did it exit diapause
    mutate(survival_to_adult = ifelse(final_week == 'A', 1, 0)) %>% # did it survive to adult
    select(ID, year, maternal_ID, Site, egg_batch_ID, survival_to_diapause:survival_to_adult) %>%
  left_join(mass_wide, by = 'ID', suffix = c(".x", ".y"))
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

weekly_check2019$egg_batch_ID <- substr(weekly_check2019$"ID",0, 6)

# Join maternal ID and site from the egg batch column if not already there and
# select important variables and create a year column
weekly_survival2018 <- inner_join(weekly_check2018, egg_batches2018, 
                                 by = 'egg_batch_ID', suffix = c(".x", ".y")) %>%
  inner_join(egg_collection2018, by = 'maternal_ID', suffix = c(".x", ".y")) %>%
  select(ID, maternal_ID, Site, X09.Jul:X08.Jul) %>%
  mutate(year = 2018)

weekly_survival2019 <- inner_join(weekly_check2019, egg_collection2019, 
                                 by = 'egg_batch_ID', suffix = c(".x", ".y")) %>% 
  select(ID, maternal_ID, Site, egg_batch_ID, X22.Jul:Adult) %>%
  mutate(year = 2019)
  
weekly_survival2020 <- inner_join(weekly_check2020, egg_collection2020, 
                                 by = 'maternal_ID', suffix = c(".x", ".y")) %>% 
  rename(c('Eggbatch' = 'egg_batch_ID')) %>%
  select(ID, maternal_ID, Site, egg_batch_ID, X13.Jul:Adult) %>%
  mutate(year = 2020) 

weekly_survival2021 <- inner_join(weekly_check2021, egg_collection2021, 
                                  by = 'egg_batch_ID', suffix = c("", ".y")) %>% 
  select(ID, maternal_ID, Site, egg_batch_ID, X11.Oct:X18.Oct) %>%
  mutate(year = 2021)

# Run the survial function for each year 
survival2018 <- survival(weekly_data = weekly_survival2018, measurement_data = 
                        larva_measurement2018, first_year = 2018, week_of_diapause = 
                        weekly_survival2018$X12.Nov, week_after_diapause = 
                        weekly_survival2018$X06.May, final_week = 
                        weekly_survival2018$X08.Jul)

survival2019 <- survival(weekly_data = weekly_survival2019, measurement_data = 
                        larva_measurement2019, first_year = 2019,
                        week_of_diapause = weekly_survival2019$X07.Oct, 
                        week_after_diapause = weekly_survival2019$X27.Apr, 
                        final_week = weekly_survival2019$Adult)

survival2020 <- survival(weekly_data = weekly_survival2020, measurement_data = 
                        larva_measurement2020, first_year = 2020, 
                        week_of_diapause = weekly_survival2020$X05.Oct, 
                        week_after_diapause = weekly_survival2020$X26.Apr, 
                        final_week = weekly_survival2020$Adult)

survival2021 <- survival(weekly_data = weekly_survival2021, measurement_data = 
                           larva_measurement2020, first_year = 2021, 
                         week_of_diapause = weekly_survival2020$X05.Oct, 
                         week_after_diapause = weekly_survival2020$X26.Apr, 
                         final_week = weekly_survival2020$Adult)

# Run the pupa time function for each year
pupa_time2019 <- pupa.time(pupa_check2019, "O", "R", "B")  
pupa_time2018 <- pupa.time(pupa_check2018, "O", "R", "B")
pupa_time2020 <- pupa.time(pupa_check2020, "O", "R", "B", "Field")

# Bind all of the data together
all_survival <- left_join((bind_rows(survival2018, survival2019, survival2020)),
                          (bind_rows(pupa_time2018, pupa_time2019, pupa_time2020)), 
                          by = c('ID'), suffix = c(".x", ".y"))

# Export it as a csv. to aviod rerunning this code to do analysis
write.csv(all_survival, paste0("P:/Conservation_Research/Restricted/CRD/Research Projects/",
"Pollinators/Poweshiek_Dakota_Garita_Skippers/R projects/Butterfly-survival/Poweshiek_fitness/all_survival"), 
row.names = FALSE)

