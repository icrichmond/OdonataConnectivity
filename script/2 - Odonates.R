# Regressions 
# Author: Isabella Richmond
# This code is for cleaning the Odonate data to be usable in models

#### Load Packages ####
easypackages::packages("tidyverse", "data.table", "vegan")

#### Load Data ####
ode <- fread("input/AdultOdonata.csv")

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

# add groups to differentiate between natural and stormwater ponds 
ani <- add_column(ani, Group = "SWF")
ani$Group[42:49] = "NAT"

zyg <- add_column(zyg, Group = "SWF")
zyg$Group[42:49] = "NAT"
# save cleaned datasets
fwrite(ani, "input/cleaned/AnisopteraCleaned.csv")
fwrite(zyg, "input/cleaned/ZygopteraCleaned.csv")
