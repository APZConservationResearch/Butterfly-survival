# Introduction ----

# Author Information 
# Laura Burns
# Email: lburns@assiniboinepark.ca

# Description: This code summarises and visualises egg batch data by site and by individual for egg batches up to 2021.

# Libraries ---- 
library(tidyverse)
library(lubridate)
library(lme4)
library(ggeffects)
library(MuMIn)

# Setup ----
setwd("P:/Conservation_Research/Restricted/CRD/Research Projects/Pollinators/Poweshiek_Dakota_Garita_Skippers/R projects/Butterfly-survival")

egg_batches <- read.csv("Raw_data/POSK_eggbatches_2021.csv")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

egg_batches <- egg_batches %>%
  mutate("perc_hatch" = num_larva/number_eggs, "year" = dmy(date_collected), date_collected = dmy(date_collected), mate_day = dmy(mate_day)) %>%
  mutate_at(vars(year), funs(year))

# Summarizing Females -----------------------------------------------------

female_hatch_summary <- egg_batches %>%
  drop_na(maternal_lineage) %>%
  group_by(maternal_ID, year) %>%
  mutate(total_eggs = sum(number_eggs),
         total_larvae = sum(num_larva), 
         percent_hatched = round((mean(perc_hatch)) * 100,2), 
        'site_method' = paste(maternal_lineage, method)) %>%
  select(maternal_ID, maternal_lineage, method, total_eggs, percent_hatched, year, total_larvae) %>%
  distinct()

method_summary <- female_hatch_summary %>% 
  drop_na(maternal_lineage) %>%
  filter(percent_hatched != 0.00) %>%
  group_by(method) %>%
  summarise(percent_hatched = mean(percent_hatched))

ggplot(female_hatch_summary, aes(method, percent_hatched)) +
  geom_boxplot()

site_summary <- female_hatch_summary %>% 
  drop_na(maternal_lineage) %>%
  filter(percent_hatched != 0.00) %>%
  group_by(maternal_lineage) %>%
  summarise(percent_hatched = mean(percent_hatched))

ggplot(female_hatch_summary, aes(maternal_lineage, percent_hatched)) +
  geom_boxplot()

female_summary <- female_hatch_summary %>%
  filter(percent_hatched != 0)

ggplot(female_summary, aes(x = reorder(maternal_ID, percent_hatched), y = percent_hatched)) +
  geom_col()+
  xlab("Maternal ID") +
  ylab("Percent hatched") +
  theme_bw() +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 13)) +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(0.2, 0.8)) +
  theme(legend.text = element_text(size = 12))

ggplot(female_summary, aes(x = percent_hatched, y = method, color = method)) +
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

site_summary <- female_hatch_summary %>%
  group_by(maternal_lineage, year) %>%
  summarise(percent_hatched = mean(percent_hatched)) %>%
  mutate("siteID" = paste(year, maternal_lineage))

ggplot(site_summary, aes(siteID, percent_hatched, fill = maternal_lineage)) +
  geom_col()


# Time series -------------------------------------------------------------

egg_output <- egg_batches %>%
  drop_na(mate_day) %>%
  mutate(BETWEEN0 = as.numeric(difftime(date_collected, (mate_day), 1)), days_after_mate = ifelse(is.na(BETWEEN0), 0, BETWEEN0)) %>%
  group_by(days_after_mate, maternal_ID) %>%
  summarize(eggs_per_day = sum(number_eggs)) %>%
  group_by(maternal_ID) %>%
  arrange(days_after_mate) %>%
  mutate(total_eggs = sum(eggs_per_day), cumulative_proportion = cumsum(eggs_per_day)/total_eggs,
         proportion_laid = eggs_per_day/total_eggs)

p2 <- ggplot(egg_output, aes(x = maternal_ID, fill = maternal_ID)) +
  stat_summary(geom = "bar", aes(y = total_eggs)) +
  scale_color_manual(values = cbPalette) + 
  theme_classic()       
                     
  
png("captive_time_series.png", units = 'in', width = 7, height = 4, res = 600)
ggplot(egg_output, aes(days_after_mate, cumulative_proportion, color = maternal_ID, shape = maternal_ID)) +
  geom_point(size = 2) +
  geom_line(alpha = 0.3, size = 1.3) +
  theme_bw(base_size = 14) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(legend.position = c(0.9, 0.8)) +
  theme(legend.background = element_rect(size = 0.5, linetype = "solid", colour = "black")) +
  guides(colour = guide_legend(title = "Maternal ID")) +
  guides(shape = guide_legend(title = "Maternal ID")) +
  xlab("Days after mating") +
  ylab("Proprotion of total eggs layed") +
  scale_x_continuous(n.breaks = 17) +
  ylim(0,1) +
  scale_color_manual(values = cbPalette) +
  annotation_custom(ggplotGrob(p2), xmin = 10, xmax = 15, 
                    ymin = 0.2, ymax = 0.6, legend = FALSE)
  
  

dev.off()

# Hatch rate and survival as a function of date layed ----

lay_date_hatch <- egg_batches %>%
  drop_na(mate_day) %>%
  filter(year == 2021 | 2020) %>%
  mutate(BETWEEN0 = as.numeric(difftime(date_collected, (mate_day),1)), days_after_mate = ifelse(is.na(BETWEEN0),0,BETWEEN0)) %>%
  group_by(days_after_mate, maternal_ID) %>%
  mutate(eggs_per_day = sum(number_eggs), hatched_per_day = sum(num_larva), day_hatch_rate = hatched_per_day/eggs_per_day, unfert_per_day = sum(no_development), unfert_rate = no_development/eggs_per_day, hatch_rate_fert = hatched_per_day/(eggs_per_day-unfert_per_day)) %>%
  distinct(maternal_ID, day_hatch_rate, days_after_mate, eggs_per_day, year, unfert_rate, hatch_rate_fert)


ggplot(lay_date_hatch, aes(x = days_after_mate, y = hatch_rate_fert, group = maternal_ID)) +
  geom_point(size = 2) +
  geom_smooth(method ="glm", alpha = 0.1) +
  theme_bw(base_size = 14) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(legend.position = c(0.9, 0.8)) +
  theme(legend.background = element_rect(size = 0.5, linetype = "solid", colour = "black")) +
  xlab("Days after mating") +
  ylab("Hatch rate") +
  scale_x_continuous(n.breaks = 17) 

#  Test the random effects structure using a beyond optimal mode (i.e purposeful;ly overfit the data) 
lay_date_hatch$maternal_ID <- factor(lay_date_hatch$maternal_ID) 
lmer1 <- glmer(day_hatch_rate ~ days_after_mate + eggs_per_day + (as.factor(year)) + (1 + days_after_mate|maternal_ID), data = lay_date_hatch)
lmer2 <- glmer(day_hatch_rate ~ days_after_mate + eggs_per_day + (as.factor(year)) + (1|maternal_ID), data = lay_date_hatch)
lmer3 <- glm(day_hatch_rate ~ days_after_mate + eggs_per_day + (as.factor(year)), data = lay_date_hatch)

AICc_out <- model.sel(lmer1, lmer2, lmer3)  # check which model has the lowest AIC

pred.mm2 <- ggpredict(lmer3, terms = c("days_after_mate"))  # get the predicted values from the model
pred.mm2 <- pred.mm2 %>% rename(c('x' = 'days_after_mate', 'predicted' = 'day_hatch_rate', 'group' = 'maternal_ID'))  # rename columns
# lay_date_hatch$pred.lme <- predict(lmer3, newdata = lay_date_hatch, level = 0)  use pred.lme to plot random effects if using mixed effects model 

ggplot(lay_date_hatch, aes(x = days_after_mate, y = day_hatch_rate, group = maternal_ID)) +
  geom_rug(sides = 'b', alpha = 0) +
  geom_smooth(data = lay_date_hatch, aes(y = day_hatch_rate, colour = (as.factor(year))), size = .8, alpha = 0.5, method = 'glm', se = FALSE, method.args = list(family = 'quasibinomial')) +
  geom_line(data = pred.mm2, aes(x = days_after_mate, y = day_hatch_rate), size = 1.25) +
  geom_ribbon(data = pred.mm2, aes(ymin = conf.low, ymax = conf.high,), alpha = 0.3) +
  scale_x_continuous(breaks = c(0:20)) +
  labs(x = "Days Between Mating and Egg Laying", y = "Hatch Rate", colour = "Year") + 
  theme_classic()

png("Hatch Rate.png", units = 'in', width = 7, height = 4, res = 600)
plot1
dev.off()


ggplot(lay_date_hatch, aes(x = days_after_mate, y = day_hatch_rate, group = maternal_ID)) +
  geom_smooth(data = lay_date_hatch, aes(y = day_hatch_rate, colour = (as.factor(year))), size = .8, alpha = 0.5, method = 'glm', se = FALSE)
