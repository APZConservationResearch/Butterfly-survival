# Introduction ----

# Author Information 
# Laura Burns
# Email: lburns@assiniboinepark.ca

# Description: This code summarises and visualises egg batch data by site and by individual.

# Libraries ---- 
library(tidyverse)
library(lubridate)

# Setup ----
data<- read.csv("POSK_eggbatches_2021.csv")
sites <- read.csv("sites.csv")

data<- data %>%
  mutate("perc_hatch" = num_larva/number_eggs, "year" = dmy(date_collected)) %>%
  mutate_at(vars(year), funs(year))

method <- data %>%
  select(maternal_ID, method) %>% 
  unique()


# Summarizing Females -----------------------------------------------------

female_hatch_summary<- data %>%
  group_by(maternal_ID, year) %>%
  summarise (total_eggs = sum(number_eggs), percent_hatched = round((mean(perc_hatch))*100,2)) %>%
  left_join(sites) %>% 
  left_join(method)

ggplot(female_hatch_summary, aes(maternal_ID, percent_hatched, fill = site)) +
  geom_col()


female_summary <- female_hatch_summary %>%
  filter(percent_hatched != 0 & total_eggs >=10)

ggplot(female_summary, aes(x = reorder(maternal_ID, percent_hatched), y = percent_hatched, fill = method)) +
  geom_col()+
  xlab("Maternal ID") +
  ylab("Percent hatched") +
  theme_bw() +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 13)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(0.2, 0.8)) +
  theme(legend.text = element_text(size = 12))

ggplot(female_summary, aes(x = percent_hatched, y=method, color = method)) +
  geom_point(size = 5) +
  xlab("Percent hatched") +
  ylab("Cross type") +
  theme_bw() +
  theme(axis.title = element_text(size = 18)) +
  theme(axis.text = element_text(size = 13)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=12)) +
  theme(legend.position = "none")


# Summarizing Sites -------------------------------------------------------

site_summary <- data %>%
  group_by(maternal_ID, year) %>%
  left_join(sites, by = c("maternal_ID")) %>%
  group_by(site, year) %>%
  summarise(total_eggs = sum(number_eggs), total_hatched = sum(num_larva)) %>%
  mutate("hatch_rate" = total_hatched/total_eggs, "siteID" = paste(year, site))

ggplot(site_summary, aes(siteID, hatch_rate, fill = site))+
  geom_col()


# Time series -------------------------------------------------------------

egg_output <- data %>%
  mutate("date_collected" = dmy(date_collected)) %>%
  filter(year == 2020) %>%
  group_by(date_collected, maternal_ID) %>%
  summarize(eggs_per_day = sum(number_eggs))

ggplot(egg_output, aes(date_collected, eggs_per_day, color = maternal_ID))+
  geom_line(size=1.5)



# 2020 egg output ---------------------------------------------------------

data2020 <- read_csv("2020POSKeggs.csv") %>% 
  filter(!is.na(ID))


ggplot(data2020, aes(Day, Eggs, color = ID))+
  geom_point(size = 2)+
  geom_smooth(method="glm", method.args = list(family = "poisson"))+
  theme_bw(base_size = 14)+
  theme(panel.grid.minor.x = element_blank())+
  theme(legend.position = c(0.9, 0.8))+
  theme(legend.background = element_rect(size=0.5, linetype="solid", colour="black"))+
  xlab("Days after mating")+
  ylab("Eggs collected")+
  scale_x_continuous(n.breaks = 17)
