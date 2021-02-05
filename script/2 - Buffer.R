# Buffer & Extraction
# Author: Isabella Richmond 
# This script is for producing buffers around each study pond and extracting 
# all other study ponds within the buffer for future analysis

#### Load Packages ####
easypackages::packages("tidyverse", "sf", "sp")

#### Load Data ####
ponds <- readRDS("output/CoorWGS.rds")

#### Create Buffer ####
# convert sp to sf 
ponds <- st_as_sf(ponds)
plot(ponds)
# transform to a metric coordinate system 
ponds_km = st_transform(ponds, "+proj=utm +zone=18N +datum=WGS84 +units=km")
plot(ponds_km)
# produce 2 km buffer for Anisoptera - consistent with Perron et al 2021
ponds_2buffer <- st_buffer(ponds_km, 2)
# produce 500 m buffer for Zygoptera - consistent with Purse et al 2003
ponds_500buffer <- st_buffer(ponds_km, 0.5)

#### Calculate Intersections #### 
# rename pond name column in points and buffers to reduce confusion 
ponds_km <- rename(ponds_km, IntersectingPonds = PondName)
ponds_2buffer <- rename(ponds_2buffer, Buffer = PondName)
ponds_500buffer <- rename(ponds_500buffer, Buffer=PondName)
# calculate intersection for each buffer 
int2 <- st_intersection(ponds_2buffer, ponds_km)
int500 <- st_intersection(ponds_500buffer, ponds_km)