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

ani_abun_hab <- lm(abundance ~ n.two, data = ani)
summary(ani_abun_hab)
# significant

# Shannon Diversity 
ani_shann <- lm(shannon ~ mean.two + sd.two, data = ani)
summary(ani_shann)
# almost significant
ani_shann_hab <- lm(shannon ~ n.two, data = ani)
summary(ani_shann_hab)
# not significant 

# Species Richness
ani_sr <- lm(speciescount ~ mean.two + sd.two, data = ani)
summary(ani_sr)
# mean significant
ani_sr_hab <- lm(speciescount ~ n.two, data = ani)
summary(ani_sr_hab)
# significant

# list models with names
normalmodels_ani <- list("Ani Abundance ~ Current" = ani_abun, "Ani Abundance ~ Habitat" = ani_abun_hab,
                         "Ani Shannon ~ Current" = ani_shann, "Ani Shannon ~ Habitat" = ani_shann_hab,
                         "Ani Sp. Rich. ~ Current" = ani_sr, "Ani Sp. Rich ~ Habitat" = ani_sr_hab)

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

summ4 <- broom::tidy(ani_abun_hab)
write.csv(summ4, "output/models/AnisopteraAbundanceHabitat_Summary.csv")

summ5 <- broom::tidy(ani_sr_hab)
write.csv(summ5, "output/models/AnisopteraShannonHabitat_Summary.csv")

## Zygoptera ##
# Abundance
zyg_abun <- lm(abundance ~ mean.threeh + sd.threeh, data = zyg)
summary(zyg_abun)
# not significant

zyg_abun_hab <- lm(abundance ~ n.threeh, data = zyg)
summary(zyg_abun_hab)
# not significant

# Shannon Diversity 
zyg_shann <- lm(shannon ~ mean.threeh + sd.threeh, data = zyg)
summary(zyg_shann)
# almost significant (0.08)

zyg_shann_hab <- lm(shannon ~ n.threeh, data = zyg)
summary(zyg_shann_hab)
# not significant

# Species Richness
zyg_sr <- lm(speciescount ~ mean.threeh + sd.threeh, data = zyg)
summary(zyg_sr)
# mean significant

zyg_sr_hab <- lm(speciescount ~ n.threeh, data = zyg)
summary(zyg_sr_hab)
# not significant

# list models with names
normalmodels_zyg <- list("Zyg Abundance ~ Current" = zyg_abun, "Zyg Abundance ~ Habitat" = zyg_abun_hab, 
                         "Zyg Shannon ~ Current" = zyg_shann, "Zyg Shannon ~ Habitat" = zyg_shann_hab,
                         "Zyg Sp. Rich. ~ Current" = zyg_sr, "Zyg Sp. Rich. ~ Habitat" = zyg_sr_hab)

# make diagnostic plots for each model 
normalmodels_residplots_zyg <- imap(normalmodels_zyg, resid_plots)
pdf("graphics/modeldiagnostics/normalmodels_zyg.pdf")
normalmodels_residplots_zyg
dev.off()

# model diagnostic plots look ok

summ6 <- broom::tidy(zyg_sr)
write.csv(summ6, "output/models/ZygopteraRichnessCurrent_Summary.csv")

summ7 <- broom::tidy(ani_shann)
write.csv(summ7, "output/models/ZygopteraShannonCurrent_Summary.csv")
