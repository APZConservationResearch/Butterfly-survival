# Introduction ----

# Author Information 
# Adam Grottoli
# Email: grottoli4@gmail.com

# Description: Analysis and visualization of poweshiek head capsule and mass data


library(tidyverse)
library(lubridate)
library(ellipse)
library(RColorBrewer)


# Setup ----
# Change to appropriate workspace
setwd(paste0("P:/Conservation_Research/Restricted/CRD/Research Projects/Pollinators/",
             "Poweshiek_Dakota_Garita_Skippers/R projects/Butterfly-survival"))
# Load data
larva_measurement2021 <- read.csv('./Raw_data/larva_measurement2021.csv')

# Load colour palette
colorpal <- brewer.pal(12, "Paired")

# Wrangle ----
pre_diapause_measure <- larva_measurement2021 %>%
  drop_na(HeadCapAvg) %>%
  mutate(maternal_ID = as.factor(MaternalID)) 

# Model ----
head_mass_mod <- lm(HeadCapAvg ~ Mass, data = pre_diapause_measure)  
summary(head_mass_mod)

## Create ellipsoid for each mother to see if the mother impacts size
df_ell <- data.frame() 
for(g in levels(pre_diapause_measure$maternal_ID)) {
   df_ell <- rbind(df_ell, 
   cbind(as.data.frame(with(pre_diapause_measure[pre_diapause_measure$maternal_ID == g, ], 
   ellipse(cor(Mass, HeadCapAvg), scale = c(sd(Mass), sd(HeadCapAvg)),
           centre=c(mean(Mass), mean(HeadCapAvg))))), maternal_ID = g))
   } 

# Visualise ----
png("Mass v. head capsule.png", units = 'in', width = 6, height = 6, res = 600)
ggplot(pre_diapause_measure, aes(x = Mass, y = HeadCapAvg)) +
  geom_point() +
  geom_smooth(method = "glm") +
  theme_bw(base_size = 14) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(legend.position = c(0.9, 0.65)) +
  theme(legend.background = element_rect(size = 0.5, linetype = "solid", colour = "black")) +
  xlab("Mass (g)") +
  ylab("Head Capsule Width (mm)")
  #geom_path(data = df_ell, aes(x = x, y = y, colour = maternal_ID)) + scale_colour_manual(values=colorpal) ADD FOR ELLIPSES
  dev.off()

# Visualise differences in size among mothers
ggplot(pre_diapause_measure, aes(y = Mass, group = maternal_ID)) +
  geom_boxplot()
