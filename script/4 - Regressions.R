# Regressions 
# Author: Isabella Richmond
# This code is for testing the relationships between resistance and distance
# to nearest neighbours and Odonata communities 

#### Load Packages ####
easypackages::packages("tidyverse", "data.table", "ggpubr")

#### Load Data & Functions ####
ani <- fread("output/AnisopteraResistance.csv")
zyg <- fread("output/ZygopteraResistance.csv")
source("script/function - diagnosticplots.R")

#### Linear Models ####
## Abundance ##
# Anisoptera 
ani_abun_res <- lm(abundance ~ meanres + sdres, data = ani)
summary(ani_abun_res)

ani_abun_dist <- lm(abundance ~ meandist + sddist, data=ani)
summary(ani_abun_dist)

# Zygoptera 
zyg_abun_res <- lm(abundance ~ meanres + sdres, data = zyg)
summary(zyg_abun_res)

zyg_abun_dist <- lm(abundance ~ meandist + sddist, data=zyg)
summary(zyg_abun_dist)

## Shannon Diversity ## 
# Anisoptera 
ani_shann_res <- lm(shannon ~ meanres + sdres,data = ani)
summary(ani_shann_res)

ani_shann_dist <- lm(shannon ~ meandist + sddist, data=ani)
summary(ani_shann_dist)

# Zygoptera 
zyg_shann_res <- lm(shannon ~ meanres + sdres, data = zyg)
summary(zyg_shann_res)

zyg_shann_dist <- lm(shannon ~ meandist + sddist, data=zyg)
summary(zyg_shann_dist)

## Simpson Diversity ##
# Anisoptera 
ani_simp_res <- lm(simpson ~ meanres + sdres, data = ani)
summary(ani_simp_res)

ani_simp_dist <- lm(simpson ~ meandist + sddist, data=ani)
summary(ani_simp_dist)

# Zygoptera 
zyg_simp_res <- lm(simpson ~ meanres + sdres, data = zyg)
summary(zyg_simp_res)

zyg_simp_dist <- lm(simpson ~ meandist + sddist, data=zyg)
summary(zyg_simp_dist)

# list models with names
normalmodels <- list("Ani Abundance ~ Resistance" = ani_abun_res, "Ani Abundance ~ Distance" = ani_abun_dist,  
                     "Zyg Abundance ~ Resistance" = zyg_abun_res, "Zyg Abundance ~ Distance" = zyg_abun_dist, 
                     "Ani Shannon ~ Resistance" = ani_shann_res, "Ani Shannon ~ Distance" = ani_shann_dist, 
                     "Zyg Shannon ~ Resistance" = zyg_shann_res, "Zyg Shannon ~ Distance" = zyg_shann_dist, 
                     "Ani Simp ~ Resistance" = ani_simp_res, "Ani Simp ~ Distance" = ani_simp_dist, 
                     "Zyg Simp ~ Resistance" = zyg_simp_res, "Zyg Simp ~ Distance" = zyg_simp_dist)

# make diagnostic plots for each model 
normalmodels_residplots <- imap(normalmodels, resid_plots)
pdf("graphics/modeldiagnostics/normalmodels.pdf")
normalmodels_residplots
dev.off()

# ani_abun_dist, zyg_abun_res, zyg_abun_dist, and Simpson diversity models don't meet assumptions
# try these as Generalized Linear Models

#### Generalized Linear Model ####
# going to use a Gamma distribution
ani_abun_dist_g <- glm(abundance ~ meandist + sddist, family = Gamma, data=ani)
summary(ani_abun_dist_g)

zyg_abun_res_g <- glm(abundance ~ meanres + sdres, family = Gamma, data = zyg)
summary(zyg_abun_res_g)

zyg_abun_dist_g <- glm(abundance ~ meandist + sddist, family = Gamma, data=zyg)
summary(zyg_abun_dist_g)

# row 3 of anisoptera has Simpson diversity value of zero, Gamma doesn't accept 
# zero values, will switch it to 0.000001 so it is functionally acting as zero 
# but still works with error structure
ani$simpson[3] <- 0.000001

ani_simp_res_g <- glm(simpson ~ meanres + sdres, family = Gamma, data = ani)
summary(ani_simp_res_g)

ani_simp_dist_g <- glm(simpson ~ meandist + sddist, family = Gamma, data=ani)
summary(ani_simp_dist_g)

zyg_simp_res_g <- glm(simpson ~ meanres + sdres, family = Gamma, data = zyg)
summary(zyg_simp_res_g)

zyg_simp_dist_g <- glm(simpson ~ meandist + sddist, family = Gamma, data=zyg)
summary(zyg_simp_dist_g)

# list models with names
gammamodels <- list("Ani Abundance ~ Distance" = ani_abun_dist_g,  
                     "Zyg Abundance ~ Resistance" = zyg_abun_res_g, "Zyg Abundance ~ Distance" = zyg_abun_dist_g, 
                     "Ani Simp ~ Resistance" = ani_simp_res_g, "Ani Simp ~ Distance" = ani_simp_dist_g, 
                     "Zyg Simp ~ Resistance" = zyg_simp_res_g, "Zyg Simp ~ Distance" = zyg_simp_dist_g)

# make diagnostic plots for each model 
gammamodels_residplots <- imap(gammamodels, resid_plots)
pdf("graphics/modeldiagnostics/gammamodels.pdf")
gammamodels_residplots
dev.off()
