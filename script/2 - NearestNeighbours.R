# Buffer & Nearest Neighbours
# Author: Isabella Richmond 
# This script is for producing biologically relevant buffers around each study pond to 
# identify how many study ponds are within a biologically relevant distance. 
# We then calculate the 5 nearest neighbours of each study pond and the distance between them

#### Load Packages ####
easypackages::packages("tidyverse", "sf", "sp", "spatstat", "maptools", "data.table")

#### Load Data ####
ponds <- readRDS("input/cleaned/CoorWGS.rds")

#### Create Buffer ####
# convert sp to sf 
ponds <- st_as_sf(ponds)
plot(ponds)
# transform to a metric coordinate system 
ponds_km = st_transform(ponds, "+proj=utm +zone=18N +datum=WGS84 +units=km")
plot(ponds_km)
# produce 2 km buffer for Anisoptera - consistent with Perron et al 2021
ponds_2buffer <- st_buffer(ponds_km, 2)
saveRDS(ponds_2buffer, "output/buffers/twokmbuffer.rds")
# produce 500 m buffer for Zygoptera - consistent with Purse et al 2003
ponds_500buffer <- st_buffer(ponds_km, 0.5)
saveRDS(ponds_500buffer, "output/buffers/fivehundredmbuffer.rds")

#### Calculate Intersections #### 
# rename pond name column in points and buffers to reduce confusion 
ponds_km <- rename(ponds_km, IntersectingPonds = PondName)
ponds_2buffer <- rename(ponds_2buffer, Buffer = PondName)
ponds_500buffer <- rename(ponds_500buffer, Buffer=PondName)
# calculate intersection for each buffer 
int2 <- st_intersection(ponds_2buffer, ponds_km)
int500 <- st_intersection(ponds_500buffer, ponds_km)

#### Calculate Nearest Neighbours #### 
# we are going to find the  nearest neighbours outside the buffer so we can 
# average the connectivity with those if the buffer intersects with less than 5 ponds

# want to calculate the nearest neighbours for the 5 closest study ponds

# convert ponds_km back to sp 
ponds_km_sp <- as_Spatial(ponds_km)
# convert sp to ppp for spatsstat package 
ponds_km_ppp <- as.ppp(ponds_km_sp)
# calculate distances for 5 closest neighbours
nd <- nndist(ponds_km_ppp, k = 1:5)
nd <- as.data.table(nd)
nd <- nd %>%
  mutate(Pond = ponds$PondName)
# find which ponds are the nearest neighbours 
nn <- nnwhich(ponds_km_ppp, k=1:5)
nn <- as.data.table(nn)
nn <- nn %>%
  mutate(Pond = ponds$PondName)
# replace numbers with appropriate pond names 
nn2 <-
  reshape2::dcast(mutate(
    reshape2::melt(nn, id.var="Pond"), 
    value = plyr::mapvalues(
      value, c(seq(1:49)), c(ponds_km_ppp$marks))
    ), Pond~variable)
# add distances to dataframe 
nearestneighbours <- inner_join(nn2, nd, by = "Pond")


# identify which ponds have more than 5 ponds in their buffer for the 2 km buffer
# get a count for each pond identifier in the buffer column 
# select ponds with a count higher than 5 
int2tally <- int2 %>%
  group_by(Buffer)%>%
  tally %>%
  filter(n > 6) # specify 6 not 5 because every buffer contains its own pond

int500tally <- int500 %>%
  group_by(Buffer)%>%
  tally %>%
  filter(n > 6)
# important to note that there are no ponds with 5 other study ponds within a 500 m 
# radius, that is relevant to damselfly dispersal

# only 1 pond with more than 5 nearest neighbours in the 2 km buffer, SWF-1215 (6 ponds)
# SWF-1204 is the 6th pond in a 2 km radius of SWF-1215
# calculate the distance between 
ponds_km_1215 <- dplyr::filter(ponds_km, IntersectingPonds == "SWF-1215" | IntersectingPonds == "SWF-1204")
# convert ponds_km back to sp 
ponds_km_1215 <- as_Spatial(ponds_km_1215)
# convert sp to ppp for spatsstat package 
ponds_km_1215 <- as.ppp(ponds_km_1215)
# calculate distances for 5 closest neighbours
nd1215 <- nndist(ponds_km_1215)
nd1215 <- as.data.table(nd1215)
# add columns for 6th closest neighbour and distance to dataframe 
nearestneighbours <- nearestneighbours %>% 
  add_column(which.6 = 0) %>%
  add_column(dist.6 = 0) %>%
  na_if(0)
# add values for 6th nearest neighbour to SWF-1215: Pond 1204, 1.824412
# SWF-1215 is row 18
nearestneighbours$dist.6[18] <- 1.824412
nearestneighbours$which.6[18] <- "SWF-1204"

# save the nearest neighbours dataset
fwrite(nearestneighbours, "output/nearestneighbours/nearestneighbours.csv")