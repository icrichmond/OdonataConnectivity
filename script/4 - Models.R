# Regressions 
# Author: Isabella Richmond
# This code is for testing the relationships between surrounding resistance and 
# Odonata communities 

#### Load Packages ####
easypackages::packages("tidyverse", "data.table", "ggpubr")

#### Load Data & Functions ####
ani <- fread("input/cleaned/AnisopteraCleaned.csv")
zyg <- fread("input/cleaned/ZygopteraCleaned.csv")
source("script/function - diagnosticplots.R")

#### Correlations ####
# check correlations between mean/sd and mean/hab 
ggplot(ani, aes(x = n.nine, y = mean.nine)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  stat_cor()
# highly correlated

ggplot(ani, aes(x = n.three, y = mean.three)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  stat_cor()
# correlation of 0.54, fairly correlated 

ggplot(ani, aes(x = mean.nine, y = sd.nine)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  stat_cor()
# correlation of -0.21, OK

ggplot(zyg, aes(x = mean.three, y = sd.three)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  stat_cor()
# correlation of 0.23, OK

# number of surrounding habitats and mean conductance are 
# highly correlated

#### Linear Models ####
# Use values from the 2 km buffer for Anisoptera, biologically relevant 
# Use values from the 300 m buffer for Zygoptera, biologically relevant

## Anisoptera ##
# Abundance
ani_abun <- lm(abundance ~  mean.nine,  data = ani)
summary(ani_abun)
# significant

ani_abun_hab <- lm(abundance ~ n.nine, data = ani)
summary(ani_abun_hab)
# significant

# Shannon Diversity 
ani_shann <- lm(shannon ~ mean.nine, data = ani)
summary(ani_shann)
# not significant

ani_shann_hab <- lm(shannon ~ n.nine, data = ani)
summary(ani_shann_hab)
# not significant 

# Species Richness
ani_sr <- lm(speciescount ~ mean.nine, data = ani)
summary(ani_sr)
# mean significant

ani_sr_hab <- lm(speciescount ~ n.nine, data = ani)
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

summ2 <- broom::tidy(ani_abun_hab)
write.csv(summ2, "output/models/AnisopteraAbundanceHabitat_summary.csv")

summ3 <- broom::tidy(ani_sr)
write.csv(summ3, "output/models/AnisopteraSpeciesRichnessCurrent_Summary.csv")

summ4 <- broom::tidy(ani_sr_hab)
write.csv(summ4, "output/models/AnisopteraSpeciesRichnessHabitat_Summary.csv")

## Zygoptera ##
# Abundance
zyg_abun <- lm(abundance ~ mean.three, data = zyg)
summary(zyg_abun)
# not significant

zyg_abun_hab <- lm(abundance ~ n.three, data = zyg)
summary(zyg_abun_hab)
# not significant

# Shannon Diversity 
zyg_shann <- lm(shannon ~ mean.three, data = zyg)
summary(zyg_shann)
# almost significant (0.07)

zyg_shann_hab <- lm(shannon ~ n.three, data = zyg)
summary(zyg_shann_hab)
# not significant

# Species Richness
zyg_sr <- lm(speciescount ~ mean.three, data = zyg)
summary(zyg_sr)
# mean significant

zyg_sr_hab <- lm(speciescount ~ n.three, data = zyg)
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

summ6 <- broom::tidy(zyg_shann)
write.csv(summ6, "output/models/ZygopteraShannonCurrent_Summary.csv")

summ7 <- broom::tidy(zyg_sr)
write.csv(summ7, "output/models/ZygopteraSpeciesRichnessCurrent_Summary.csv")

# IMPORTANT: sd seems to be a dummy variable - switches direction and magnitude 
# of effect depending on what else is in the model, do not include in models. Can't 
# be confident of effects
