# Introduction ----

# Author Information 
# Original author: Hannah Carey
# Adapted by: Adam Grottoli
# Email: grottoli4@gmail.com

# Description: This code wranges multiple years of garita skipperling data to a
# and exports a csv to be used in the overwinter analysis.

# Libraries ----
library(tidyverse)
library(dplyr)
library(lubridate)
library(purrr)

# Setup ----
# Change to appropriate workspace
setwd(paste0("P:/Conservation_Research/Restricted/CRD/Research Projects/Pollinators/",
             "Poweshiek_Dakota_Garita_Skippers/R projects/Butterfly-survival/Overwinter_analysis"))

# 2018 data ----

skip_master <- read.csv("data/Garita Data Master - 2018 cohort - larva_measurements.csv") %>% 
  select(Year, Month, Day, ID, Egg_batch, Species, Found, Length, Height, Mass, Status, CurrentLocation, PlantHeight)

skip_entire <- read.csv("data/Garita Data Master - 2018 cohort - larva_measurements.csv") 

skip_master$Month = sprintf("%02d", skip_master$Month)
skip_master$Day = sprintf("%02d", skip_master$Day)

gar_master = skip_master %>% 
  unite(ymd.date, 1:3, sep = "-", remove = TRUE) %>% 
  mutate(date = as.Date(ymd.date),
         ID=as.character(ID))
gar_master$ymd.date = as.Date(gar_master$ymd.date, format = "%Y-%m-%d")

gar_master = gar_master %>% 
  select(ID, Egg_batch, Species, Found, Height, Length, Mass, Status, PlantHeight, CurrentLocation, date)


last_check = gar_master %>%
  group_by(ID) %>% 
  filter(date == max(date)) %>% 
  ungroup() %>% 
  mutate(outcome=ifelse(Status=="A", 1,0))

# n.eggs = gar_master %>% 
# select(ID) %>% 
# count(ID)

# counted number of checks for each individual and simplified ID column to distinct ID's so that rows = number of individuals
# n.eggs have 405 IDs 
# doesn't make last_check number of individuals... 

gar_master = gar_master %>% 
  group_by(ID) %>% 
  arrange(date) %>% 
  mutate(check=row_number()) %>% 
  ungroup()

# pre diapause #
pre_diapause = gar_master %>% 
  filter(date <= as.Date("2018-12-01")) %>% 
  group_by(ID) %>% 
  filter(date == max(date)) %>% 
  ungroup() %>% 
  select(ID, Length, Status, CurrentLocation, date, check, Mass) %>% 
  filter(!is.na(Length)) %>% 
  mutate(pre.length = Length) %>% 
  select(-Length)

pre_diapause = pre_diapause %>% 
  filter(date >= as.Date("2018-09-20")) 

pre.ID = c(pre_diapause$ID)

# post diapause # 
post_diapause = gar_master %>% 
  filter(date >= as.Date("2018-12-01"),
         ID %in% pre.ID)

post_diapause = post_diapause %>%
  group_by(ID) %>% 
  filter(date == min(date)) %>% 
  ungroup() %>% 
  mutate(outcome=ifelse(Status=="A", 1,0))

# join pre and post # 
GASK_post_diap = post_diapause %>% 
  select(ID, date, outcome)

GASK_diapause = pre_diapause %>% 
  left_join(GASK_post_diap, by = "ID")

colnames(GASK_diapause)[4] = "date_in"
colnames(GASK_diapause)[8] = "date_out"


# Adding in treatment location --------------------------------------------

CurrentLocation = c("Incubator plant","Incubator Plant", "Overwinter outside")
treatment = c("plant_in", "plant_in", "plant_out")

treatment_type = data.frame(CurrentLocation, treatment)

GASK_diapause_0523 = GASK_diapause %>% 
  left_join(treatment_type, by = "CurrentLocation") 

GASK_diapause_0523$treatment = as.character(GASK_diapause_0523$treatment)
GASK_diapause_0523$treatment = ifelse(is.na(GASK_diapause_0523$treatment), "cup_in", 
                                      GASK_diapause_0523$treatment)  

GASK_complete = GASK_diapause_0523 %>%
  filter(!is.na(date_out))

GASK_complete = GASK_complete %>% 
  mutate(diapause_days = (date_out - date_in))

write.csv(GASK_complete, file = "data/garita 2018 survival.csv")



# Finding data errors -----------------------------------------------------

GASK_unk = GASK_diapause %>% 
  filter(is.na(date_out))

test = gar_master %>% 
  filter(date >= as.Date("2018-12-01")) %>%
  group_by(ID) %>% 
  filter(date == min(date)) %>% 
  ungroup() 

find_unk = test %>% 
  left_join(pre_diapause) %>% 
  filter(check==1)

find_id = find_unk %>% 
  select(ID)


# 2017 data ---------------------------------------------------------------

gask_2017 = read.csv("data/Garita - 2017 Generation - Larval Measurements - Main Data.csv") %>%
  select(Date, ID, Lineage, Found, LarvaLength, Mass, Status, CurrentLocation)

gask_2017$Date = as.Date(gask_2017$Date, format = "%d/%m/%Y")

gask_2017 = gask_2017 %>%
  mutate(ID=as.character(ID))

last_17 = gask_2017 %>% 
  group_by(ID) %>% 
  filter(Date == max(Date)) %>% 
  ungroup() %>% 
  mutate(outcome=ifelse(Status=="A", 1,0))

gask_2017 = gask_2017 %>% 
  group_by(ID) %>% 
  arrange(Date) %>% 
  mutate(check=row_number()) %>% 
  ungroup()

pre.diap_17 = gask_2017 %>% 
  filter(Date <= as.Date("2017-12-01")) %>% 
  group_by(ID) %>% 
  filter(Date == max(Date)) %>% 
  ungroup() %>% 
  select(ID, LarvaLength, Status, CurrentLocation, Date, check, Mass) %>% 
  filter(!is.na(LarvaLength)) %>% 
  mutate(pre.length = LarvaLength) %>% 
  select(-LarvaLength)

pre.diap_17 = pre.diap_17 %>% 
  filter(Date >= as.Date("2017-10-01")) 

pre.ID.17 = c(pre.diap_17$ID)

post.diap_17 = gask_2017 %>% 
  filter(Date >= as.Date("2017-12-01"),
         ID %in% pre.ID.17)

post.diap_17 = post.diap_17 %>%
  group_by(ID) %>% 
  filter(Date == min(Date)) %>% 
  ungroup() %>% 
  mutate(outcome=ifelse(Status=="A", 1,0))

GASK_post_diap.17 = post.diap_17 %>% 
  select(ID, Date, outcome)

GASK.2017 = pre.diap_17 %>% 
  left_join(GASK_post_diap.17, by = "ID") %>% 
  rename(date_in = Date.x ,
         date_out = Date.y) %>% 
  mutate(CurrentLocation = as.character(CurrentLocation)) %>% 
  filter(Status == "A")

GASK.2017 = GASK.2017 %>% 
  mutate(treatment = ifelse(CurrentLocation=="Outside Overwinter", "plant_out", "cup_in")) %>% 
  filter(pre.length != "") %>%
  mutate(pre.length = as.character(pre.length))

GASK.2017$pre.length = as.numeric(GASK.2017$pre.length)

GASK.2017 = GASK.2017 %>% 
  mutate(diapause_days = (date_out - date_in))

# plant_out = GASK.2017 %>% 
# filter(treatment == "plant_out") %>% 
# mutate(indiv_loc = row_number(CurrentLocation))


write.csv(GASK.2017, file = "data/garita 2017 survival.csv")

#Create location identification --------------------------------------------------
# creating location identification to be used as random variable in model
# for each plant or cup an individual(s) was on for diapause
plant_in = all.GASK %>% 
  filter(treatment == "plant_in") %>% 
  mutate(indiv_loc = (row_number(CurrentLocation)))

plant_in$indiv_loc = sub("^", "plant_in_", plant_in$indiv_loc)

plant_out = all.GASK %>% 
  filter(treatment == "plant_out") %>% 
  mutate(indiv_loc = (row_number(CurrentLocation)))

plant_out$indiv_loc = sub("^", "plant_out_", plant_out$indiv_loc)

cup_in = all.GASK %>% 
  filter(treatment == "cup_in") %>% 
  mutate(indiv_loc = CurrentLocation)

all.GASK = cup_in %>% 
  rbind(plant_out)

all.GASK = all.GASK %>% 
  rbind(plant_in)

all.mass.GASK = all.GASK %>%
  filter(!is.na(Mass),
         Mass < 0.07)

all.GASK.cup = all.GASK %>% 
  filter(treatment == "cup_in",
         Mass < 0.07)


# Complete data set 2017 + 2018 -------------------------------------------

GASK_complete = GASK_complete %>% 
  mutate(year = "2018")

GASK.2017 = GASK.2017 %>% 
  mutate(year = "2017")

all.GASK = GASK_complete %>% 
  rbind(GASK.2017)

write.csv(all.GASK, file = "data/all garita survival.csv")


# Data addition: 2018 egg batch --------------------------------------------------------
femID_18 <- read.csv(file = "data/GASK_2018_EggBatches.csv", header = TRUE,
                     stringsAsFactors = FALSE) %>% 
  select(egg_batch_ID, maternal_ID, percent_hatch)

gask18_fem <- gask18 %>% 
  mutate(egg_batch_ID = str_sub(ID, 1, 6))

gask18_fem <- gask18_fem %>% 
  left_join(femID_18, by = "egg_batch_ID")

write.csv(gask18_fem, file = "data/GASK_2018_complete.csv")