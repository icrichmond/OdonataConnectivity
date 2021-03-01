# Data Cleaning
# Author: Isabella Richmond
# This script is for cleaning datasets to be used for analysis 

#### Load Packages ####
easypackages::packages("tidyverse", "data.table", "sf", "sp", "vegan")

#### Load Data ####
ode <- fread("input/AdultOdonata.csv")
conn <- fread("input/InterpondResistances2km.txt")
ponds <- fread("input/cleaned/PondCoords2km.csv", quote="")

#### Clean Connectivity Data ####
# need to associated V1 and V2 columns with pond names in ponds.csv
ponds <- tibble::rowid_to_column(ponds, "ID")
# we want the names for V1 and V2 in our dataset 
# rename V1 to ID for first join 
conn <- rename(conn, ID = V1)
# join to ponds 
conn <- left_join(conn, ponds, by = "ID")
# prep dataset for next join
conn <- conn %>%
  rename(Pond1 = OBJECTID) %>%
  rename(X1 = X) %>%
  rename(Y1 = Y) %>%
  rename(V1 = ID) %>%
  rename(Resistance = V3) %>%
  select(-geometry) %>%
  rename(ID = V2) #name V2 as ID for next join
# join to get second pond IDs
conn <- left_join(conn, ponds, by = "ID")
conn <- conn %>%
  rename(V2 = ID) %>%
  rename(X2 = X) %>%
  rename(Y2 = Y) %>%
  rename(Pond2 = OBJECTID) %>% 
  select(-geometry)
# remove connectivity values with NAT-4 and NAT-5 included 
# NAT-4 and NAT-5 are in the randomized buffer of our connectivity
# map, not appropriate measurements 
conn <- conn[!(conn$Pond1=="NAT-4" | conn$Pond1 =="NAT-5"
               | conn$Pond2 =="NAT-4" | conn$Pond2 == "NAT-5"),]
fwrite(conn, "input/cleaned/ConnectivityCleaned.csv")

#### Clean Spatial Data ####
# remove NAT-4 and NAT-5 as they use a different land cover layer 
# and are thus removed from analysis 
ponds <- rename(ponds, "PondName" = "OBJECTID")
ponds <- filter(ponds, !PondName %in% c("NAT-4", "NAT-5"))
# pond coordinates were obtained from the City of Ottawa and have 
# a Transverse Mercator projection in meters and a GCS of 
# GCS North American 1983 (EPSG 4269)
coordinates(ponds) <- c("X", "Y")
proj4string(ponds) <- CRS("+proj=tmerc +lat_0=0 +lon_0=-76.5 +k=0.9999 +x_0=304800 +y_0=0 +datum=NAD83 +units=m")
# save metric projection before transforming 
saveRDS(ponds, file="input/cleaned/CoorOttawa.rds")

# reproject to be in km
ponds_km <- spTransform(ponds, CRS("+proj=utm +zone=18N +datum=WGS84 +units=km"))
# save SpatialPointsDataFrame
saveRDS(ponds_km, file = "input/cleaned/CoorMetric.rds")

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
fwrite(ani, "input/cleaned/AnisopteraCleaned.csv")
fwrite(zyg, "input/cleaned/ZygopteraCleaned.csv")
