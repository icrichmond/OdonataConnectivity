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
# Use values from the 2 km buffer for Anisoptera, biologically relevant for them 
# Use values from the 300 m buffer for Zygoptera, biologically relevant for them
## Abundance ##
# Anisoptera 
ani_abun <- lm(abundance ~ mean.two,  data = ani)
summary(ani_abun)
# significant

ani_abun_hab <- lm(abundance ~ n , data = ani)
summary(ani_abun_hab)
# significant

# Zygoptera 
zyg_abun_res <- lm(abundance ~ meanres + sdres, data = zyg)
summary(zyg_abun_res)
# sd significant

zyg_abun_hab <- lm(abundance ~ n, data = zyg)
summary(zyg_abun_hab)

## Shannon Diversity ## 
# Anisoptera 
ani_shann_res <- lm(shannon ~ meanres + sdres, data = ani)
summary(ani_shann_res)
# almost significant

ani_shann_hab <- lm(shannon ~ n, data = ani)
summary(ani_shann_hab)

# Zygoptera 
zyg_shann_res <- lm(shannon ~ meanres + sdres, data = zyg)
summary(zyg_shann_res)
# significant

zyg_shann_hab <- lm(shannon ~ n, data = zyg)
summary(zyg_shann_hab)

## Species Richness ##
# Anisoptera 
ani_sr_res <- lm(speciescount ~ meanres + sdres, data = ani)
summary(ani_sr_res)
# significant

ani_sr_hab <- lm(speciescount ~ n, data = ani)
summary(ani_sr_hab)

# Zygoptera 
zyg_sr_res <- lm(speciescount ~ meanres + sdres, data = zyg)
summary(zyg_sr_res)
# significant

zyg_sr_hab <- lm(speciescount ~ n, data = zyg)
summary(zyg_sr_hab)

# list models with names
normalmodels <- list("Ani Abundance ~ Resistance" = ani_abun_res, "Ani Abundance ~ Neighbours" = ani_abun_hab,  
                     "Zyg Abundance ~ Resistance" = zyg_abun_res, "Zyg Abundance ~ Neighbours" = zyg_abun_hab, 
                     "Ani Shannon ~ Resistance" = ani_shann_res, "Ani Shannon ~ Neighbours" = ani_shann_hab, 
                     "Zyg Shannon ~ Resistance" = zyg_shann_res, "Zyg Shannon ~ Neighbours" = zyg_shann_hab, 
                     "Ani Sp. Rich. ~ Resistance" = ani_sr_res, "Ani Sp. Rich. ~ Neighbours" = ani_sr_hab, 
                     "Zyg Sp. Rich. ~ Resistance" = zyg_sr_res, "Zyg Sp. Rich. ~ Neighbours" = zyg_sr_hab)

# make diagnostic plots for each model 
normalmodels_residplots <- imap(normalmodels, resid_plots)
pdf("graphics/modeldiagnostics/normalmodels.pdf")
normalmodels_residplots
dev.off()

# model diagnostic plots look good

# significant results include Anisoptera abundance, Zygoptera abundance, 
# Zygoptera species richness

# write summary tables
summ1 <- broom::tidy(ani_abun_res)
write.csv(summ1, "output/models/AnisopteraAbundanceResistance_Summary.csv")

summ2 <- broom::tidy(zyg_abun_res)
write.csv(summ2, "output/models/ZygopteraAbundanceResistance_Summary.csv")

summ3 <- broom::tidy(zyg_sr_res)
write.csv(summ3, "output/models/ZygopteraSpeciesRichnessResistance_summary.csv")