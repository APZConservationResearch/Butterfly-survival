# Introduction ----

# Author Information 
# Adam Grottoli
# Email: grottoli4@gmail.ca

# Description: This code visualises a pedigree for the poweshiek managed breeding program at Assiniboine Park Conservancy.

# Libraries ---- 
library(pedigree)

# Setup ----
setwd("P:/Conservation_Research/Restricted/CRD/Research Projects/Pollinators/Poweshiek_Dakota_Garita_Skippers/R projects/Butterfly-survival")

# Pedigree ----
data <- read.csv("Raw_data/pedigree.csv")
pedAll <- pedigree(id = data$id, dadid = data$father, momid = data$mother, sex = data$sex) 
plot <- plot(pedAll)