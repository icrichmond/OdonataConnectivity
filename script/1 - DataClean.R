# Data Cleaning
# Author: Isabella Richmond
# This script is for cleaning datasets to be used for analysis 

#### Load Packages ####
easypackages::packages("tidyverse", "data.table", "sf", "sp", "vegan")

#### Load Data ####
ode <- fread("input/AdultOdonata.csv")
conn <- fread("input/AvgInterpondResistances.csv")
ponds <- fread("input/CoorMetre.csv", quote="")

#### Clean Spatial Data ####
# remove NAT-4 and NAT-5 as they use a different land cover layer 
# and are thus removed from analysis 
ponds <- rename(ponds, "PondName" = "Pond Name")
ponds <- filter(ponds, !PondName %in% c("NAT-4", "NAT-5"))
# pond coordinates were obtained from the City of Ottawa and have 
# a Transverse Mercator projection in meters and a GCS of 
# GCS North American 1983 (EPSG 4269)
coordinates(ponds) <- c("X_Meters", "Y_Meters")
proj4string(ponds) <- CRS("+proj=tmerc +lat_0=0 +lon_0=-76.5 +k=0.9999 +x_0=304800 +y_0=0 +datum=NAD83 +units=m")
# reproject to be in WGS 84
ponds <- spTransform(ponds, CRS("+init=epsg:4326"))
# save projected spatial object 
saveRDS(ponds, file="output/CoorWGS.rds")


#### Clean Odonate Data ####
# remove NAT-4 and NAT-5 to match spatial data
ode <- ode[!(ode$Pond=="NAT-4" | ode$Pond =="NAT-5"),]
# split into two datasets - Anisoptera and Zygoptera - due to 
# differing life histories 
zyg <- ode %>%
  dplyr::select(-Group) %>% # remove the grouping column
  dplyr::select(-seq(22,53)) # remove Anisoptera species

ani <- ode %>%
  dplyr::select(-Group) %>%
  dplyr::select(c(1, seq(22,53)))

# we want abundance and diversity for each pond for ani and zyg
# calculate raw abundance  
ani <- ani %>%
 mutate(abundance = rowSums(.[,2:33]))
zyg <- zyg %>%
  mutate(abundance = rowSums(.[,2:21]))
# calculate diversity using vegan package
ani <- ani %>% 
  mutate(speciescount = specnumber(.[,2:33])) %>%
  mutate(shannon = diversity(.[,2:33], index="shannon")) %>%
  mutate(simpson = diversity(.[,2:33], index="simpson"))
zyg <- zyg %>% 
  mutate(speciescount = specnumber(.[,2:21])) %>%
  mutate(shannon = diversity(.[,2:21], index="shannon")) %>%
  mutate(simpson = diversity(.[,2:21], index="simpson"))
# save cleaned datasets
fwrite(ani, "output/AnisopteraCleaned.csv")
fwrite(zyg, "output/ZygopteraCleaned.csv")