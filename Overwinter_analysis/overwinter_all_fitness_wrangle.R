# Introduction ----

# Author Information 
# Adam Grottoli
# Email: grottoli4@gmail.com

# Description: This code merges multiple years of poweshiek and garita survival and 
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

larva_measurement2018 <- read.csv('./Raw_data/POSK_larva_measurement2018.csv') 

larva_measurement2021 <- read.csv('./Raw_data/POSK_larva_measurement2021.csv')

GASK_larva_measurement2017 <- read.csv('./Raw_data/GASK_larva_measurement2017.csv') 

GASK_larva_measurement2018 <- read.csv('./Raw_data/GASK_larva_measurement2018.csv') 

GASK_larva_measurement2019 <- read.csv('./Raw_data/GASK_larva_measurement2019.csv') 

GASK_adult2017 <- read.csv('./Raw_data/GASK_adult2017.csv')

GASK_weekly_check2018 <- read.csv('./Raw_data/GASK_weekly_check2018.csv')

GASK_weekly_check2019 <- read.csv('./Raw_data/GASK_weekly_check2019.csv')

GASK_egg_batches2018 <- read.csv('./Raw_data/GASK_egg_batches2018.csv')

GASK_egg_batches2019 <- read.csv('./Raw_data/GASK_egg_batches2019.csv')

GASK_pupa_check2018 <- read.csv('./Raw_data/GASK_pupa_check2018.csv')

GASK_pupa_check2019 <- read.csv('./Raw_data/GASK_pupa_check2019.csv')

DASK_egg_batches2021 <- read.csv('./Raw_data/DASK_egg_batches2021')

DASK_weekly_check2021 <- read.csv('./Raw_data/DASK_weekly_check2021')

DASK_egg_collection2021 <- read.csv('./Raw_data/DASK_egg_collection2021')

DASK_pupa_check2021 <- read.csv('./Raw_data/DASK_egg_batches2021.csv')

DASK_larva_measurement2021 <- read.csv('./Raw_data/DASK_larva_measurement2021')

# Functions ----

# A function to make survival binary at each of the stages and bind mass data

survival <- function (adult_data, measurement_data, date_diapause_start, cohort, is_adult, species)  { 
  
  measures_pre <- measurement_data %>% 
    mutate(date_in = make_date(Year, Month, Day)) %>%
    filter(Year == cohort) %>% 
    group_by(ID) %>% 
    filter(date_in == max(date_in)) %>% 
    ungroup() %>%
    mutate(survival_to_diapause = ifelse(Status %in% "A", 1, 0)) %>%
    filter(date_in >= as.Date(date_diapause_start)) %>% 
    mutate(pre_length = Length, pre_mass = Mass)
             # did it survive through diapause
  
  measures_post <- measurement_data %>%
    mutate(date_out = make_date(Year, Month, Day)) %>%
    filter(Year != cohort) %>% 
    group_by(ID) %>% 
    filter(date_out == min(date_out)) %>% 
    ungroup() %>% 
    mutate(survived_diapause = ifelse(Status %in% "A", 1, 0)) %>% 
    mutate(post_length = Length, post_mass = Mass) %>% 
    select(-DiapauseChamber) 
    
  adult_data %>% 
           left_join(measures_pre, by = 'ID', suffix = c("", ".y"))  %>%
           left_join(measures_post, by = 'ID', suffix = c("", ".y"))  %>%
            distinct(ID, .keep_all = TRUE) %>%
           mutate(method = ifelse(Treatment == "tube", "tube", "plant"),
                  survival_to_diapause = ifelse(is_adult %in% "A", 1, 
                                                ifelse(is.na(survival_to_diapause), 
                                                       0, survival_to_diapause)),
                  survived_diapause = ifelse(is_adult %in% "A", 1, 
                                             ifelse(survival_to_diapause %in% 0, 
                                                    NA, survived_diapause)),
                  survival_diapause_adult = ifelse(survived_diapause %in% 1, 
                                   ifelse(is_adult %in% "A", 1, 0), NA)) %>%  
    select(ID, maternal_ID, egg_batch_ID, method, survival_to_diapause, survived_diapause, 
           Adult, survival_diapause_adult,
           pre_mass, post_mass, pre_length, post_length, date_in, date_out,
           DiapauseChamber) %>%
    mutate(diapause_days = (date_out - date_in), species = species, cohort = cohort)
}

is.adult <- function (pupa_data, measurement_data, string_1, string_2 = NULL, string_3 = NULL) {
  
  no_duplicate <- measurement_data %>%
    distinct(ID, .keep_all = TRUE)
  pupa_data %>%
    mutate(Adult = ifelse(apply(pupa_data == c(string_1, string_2, string_3), 1, any), "A", NA)) %>%
    right_join(no_duplicate, by = "ID", suffix = c(".x", ".y")) %>%
    select(ID, Adult, egg_batch_ID, maternal_ID, contains('Treatment'))
}
  
# Function to count the number of days each individual spent as a pupa
# Input the different characters as strings used for different stages of pupation 

pupa.time <- function (df, pupa_character1, pupa_character2 = NULL, 
                       pupa_character3 = NULL, pupa_character4 = NULL) {
  df <- data.frame(lapply(df, function (x)
    replace(x, x %in% c(pupa_character1, pupa_character2, pupa_character3), "P"))) 
  pupa_days <- rowSums(df == "P", na.rm = T)  # Count the number of days as pupa, or "P", per row
  p_days <- cbind(pupa_days, df)
  p_days <- p_days[p_days$pupa_days != 0, ]  # get rid of any that didn't pupate
  p_days %>% select(ID, pupa_days)
}


# Merging data ---- 
# Create an egg batch column from the larva id coloumn if not already there


larva_measurement2018$egg_batch_ID <- substr(larva_measurement2018$"ID",0, 5)


# Join maternal ID from the egg batch column if not already there and
# select important variables and create a year column
larva_measurement2018 <- inner_join(larva_measurement2018, egg_batches2018, 
                                 by = 'egg_batch_ID', suffix = c(".x", ".y")) %>%
  inner_join(egg_collection2018, by = 'maternal_ID', suffix = c("", ".y")) %>%
  mutate(Treatment = "", DiapauseChamber = as.character(parse_number(CurrentLocation)))
POSK_all2018 <- is.adult(pupa_check2018, larva_measurement2018, string_1 = "A", string_2 = "REL")

larva_measurement2019 <- larva_measurement2019 %>%
  rename(c('egg_batch_ID' = 'EggBatch')) %>%
  mutate(Treatment = "", DiapauseChamber =  as.character(DiapauseChamber)) %>%
  inner_join(egg_collection2019, by = 'egg_batch_ID', suffix = c(".x", ".y"))
POSK_all2019 <- is.adult(pupa_check2019, larva_measurement2019, string_1 = "A", string_2 = "F")
 
larva_measurement2020 <- larva_measurement2020 %>%
  rename(c('egg_batch_ID' = 'EggBatch', 'maternal_ID' = 'MaternalID')) %>%
  mutate(DiapauseChamber =  as.character(DiapauseChamber))
POSK_all2020 <- is.adult(pupa_check2020, larva_measurement2020, 
                         string_1 = "Adult", string_2 = "Released") %>%
  mutate(method = ifelse(Treatment == "Tube", "tube", "plant"))

GASK_larva_measurement2017 <- GASK_larva_measurement2017 %>%
  rename(c('maternal_ID' = 'Lineage', "Length" = "LarvaLength")) %>%
  mutate(egg_batch_ID = NA, Treatment = "", 
         DiapauseChamber = as.character(parse_number(CurrentLocation)),
         Date = dmy(Date), Length = as.numeric(Length)) %>% 
  mutate_at(vars(Date), funs(Year = year, Month = month, Day = day))
GASK_all2017 <- is.adult(GASK_adult2017, GASK_larva_measurement2017, string_1 = "ADULT")

GASK_larva_measurement2018 <- GASK_larva_measurement2018 %>% 
  mutate(Treatment = "", DiapauseChamber = as.character(parse_number(CurrentLocation))) %>%
  rename(c('egg_batch_ID' = 'Egg_batch')) %>%
  left_join(GASK_egg_batches2018, 
            by = 'egg_batch_ID', suffix = c(".x", ""))
GASK_all2018 <- is.adult(GASK_pupa_check2018, GASK_larva_measurement2018, string_1 = "A") 

GASK_larva_measurement2019 <- GASK_larva_measurement2019 %>%
  rename(c('egg_batch_ID' = 'egg_batch')) %>%
  inner_join(GASK_egg_batches2019, by = 'egg_batch_ID', suffix = c(".x", "")) %>% 
  mutate(DiapauseChamber =  as.character(DiapauseChamber), Treatment = "")
GASK_all2019 <- is.adult(GASK_pupa_check2019, GASK_larva_measurement2019, string_1 = "A")

#weekly_survival2021 <- inner_join(weekly_check2021, egg_collection2021, 
#                                  by = 'egg_batch_ID', suffix = c("", ".y")) %>% 
#  select(ID, maternal_ID, egg_batch_ID, X11.Oct:X25.Oct) %>%
#  mutate(year = 2021, method = "")

# Run the survival function for each year 
POSK_survival2018 <- survival(adult_data = POSK_all2018, measurement_data = 
                                larva_measurement2018, cohort = 2018, 
                              is_adult = POSK_all2018$Adult, 
                              date_diapause_start = "2018-09-25", species = "POSK")

POSK_survival2019 <- survival(adult_data = POSK_all2019, measurement_data =
                              larva_measurement2019, cohort = 2019,
                              is_adult = POSK_all2019$Adult, 
                              date_diapause_start = "2019-10-09", species = "POSK")
  
POSK_survival2020 <- survival(adult_data = POSK_all2020, measurement_data =
                              larva_measurement2020, cohort = 2020, 
                              date_diapause_start = "2020-10-14",
                              is_adult = POSK_all2020$Adult, species = "POSK")

#POSK_survival2021 <- survival(weekly_data = weekly_survival2021, measurement_data = 
#                           larva_measurement2021, cohort = 2021, 
#                           date_diapause_start = "2021-10-18", 
#                         week_after_diapause = weekly_survival2021$X25.Oct, 
#                         is_adult = weekly_survival2021$X25.Oct, species = "POSK")

GASK_survival2017 <- survival(adult_data = GASK_all2017, measurement_data = 
                              GASK_larva_measurement2017, cohort = 2017, 
                              is_adult = GASK_all2017$Adult, date_diapause_start = "2017-10-06", species = "GASK")

GASK_survival2018 <- survival(adult_data = GASK_all2018, measurement_data = 
                                GASK_larva_measurement2018, cohort = 2018, 
                              is_adult = GASK_all2018$Adult, date_diapause_start = "2018-09-20", species = "GASK")

GASK_survival2019 <- survival(adult_data = GASK_all2019, measurement_data = 
                              GASK_larva_measurement2019, cohort = 2019, 
                              is_adult = GASK_all2019$Adult, date_diapause_start = "2019-10-16", species = "GASK")

# Run the pupa time function for each year
POSK_pupa_time2019 <- pupa.time(pupa_check2019, "O", "R", "B")  
POSK_pupa_time2018 <- pupa.time(pupa_check2018, "O", "R", "B")
POSK_pupa_time2020 <- pupa.time(pupa_check2020, "O", "R", "B", "Field")
GASK_pupa_time2018 <- pupa.time(GASK_pupa_check2018, "O", "R", "B")
GASK_pupa_time2019 <- pupa.time(GASK_pupa_check2019, "O", "R", "B")

# Bind all of the data together
all_survival <- left_join((bind_rows(POSK_survival2018, POSK_survival2019, POSK_survival2020, 
                                     GASK_survival2017, GASK_survival2018,
                                     GASK_survival2019)),
                          (bind_rows(POSK_pupa_time2018, POSK_pupa_time2019, POSK_pupa_time2020, 
                                    GASK_pupa_time2018, GASK_pupa_time2019)), 
                          by = c('ID'), suffix = c(".x", ".y"))

# Export it as a csv. to avoid rerunning this code to do analysis
write.csv(all_survival, "./Overwinter_analysis/overwinter_full_data.csv")
