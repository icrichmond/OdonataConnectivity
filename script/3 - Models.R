# Regressions 
# Author: Isabella Richmond
# This code is for testing the relationships between surrounding resistance and 
# Odonata communities 

#### Load Packages ####
easypackages::packages("tidyverse", "data.table")

#### Load Data & Functions ####
ani <- fread("input/cleaned/AnisopteraCleaned.csv")
zyg <- fread("input/cleaned/ZygopteraCleaned.csv")
source("script/function - diagnosticplots.R")

#### Linear Models ####
# Use values from the 2 km buffer for Anisoptera, biologically relevant 
# Use values from the 300 m buffer for Zygoptera, biologically relevant

## Anisoptera ##
# Abundance
ani_abun <- lm(abundance ~  mean.two + sd.two,  data = ani)
summary(ani_abun)
# significant

# Shannon Diversity 
ani_shann <- lm(shannon ~ mean.two + sd.two, data = ani)
summary(ani_shann)
# almost significant
# test number of habitats?

# Species Richness
ani_sr <- lm(speciescount ~ mean.two + sd.two, data = ani)
summary(ani_sr)
# mean significant

# list models with names
normalmodels_ani <- list("Ani Abundance ~ Current" = ani_abun, "Ani Shannon ~ Current" = ani_shann, "Ani Sp. Rich. ~ Current" = ani_sr)

# make diagnostic plots for each model 
normalmodels_residplots_ani <- imap(normalmodels_ani, resid_plots)
pdf("graphics/modeldiagnostics/normalmodels_ani.pdf")
normalmodels_residplots_ani
dev.off()

# model diagnostic plots look ok

# write summary tables
summ1 <- broom::tidy(ani_abun)
write.csv(summ1, "output/models/AnisopteraAbundanceCurrent_Summary.csv")

summ2 <- broom::tidy(ani_shann)
write.csv(summ2, "output/models/AnisopteraShannonCurrent_Summary.csv")

summ3 <- broom::tidy(ani_sr)
write.csv(summ3, "output/models/AnisopteraRichnessCurrent_summary.csv")

## Zygoptera ##
# Abundance
zyg_abun <- lm(abundance ~ mean.threeh + sd.threeh, data = zyg)
summary(zyg_abun)
# not significant

# Shannon Diversity 
zyg_shann <- lm(shannon ~ mean.threeh + sd.threeh, data = zyg)
summary(zyg_shann)
# almost significant (0.08)

# Species Richness
zyg_sr <- lm(speciescount ~ mean.threeh + sd.threeh, data = zyg)
summary(zyg_sr)
# mean significant

# list models with names
normalmodels_zyg <- list("Zyg Abundance ~ Current" = zyg_abun, "Zyg Shannon ~ Current" = zyg_shann, "Zyg Sp. Rich. ~ Current" = zyg_sr)

# make diagnostic plots for each model 
normalmodels_residplots_zyg <- imap(normalmodels_zyg, resid_plots)
pdf("graphics/modeldiagnostics/normalmodels_zyg.pdf")
normalmodels_residplots_zyg
dev.off()

# model diagnostic plots look ok

# no significant results