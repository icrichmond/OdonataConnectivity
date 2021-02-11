# Buffer & Extraction
# Author: Isabella Richmond 
# This script is for producing buffers around each study pond and extracting 
# all other study ponds within the buffer for future analysis

#### Load Packages ####
easypackages::packages("tidyverse", "sf", "sp", "spatstat", "maptools")

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
saveRDS(ponds_2buffer, "output/twokmbuffer.rds")
# produce 500 m buffer for Zygoptera - consistent with Purse et al 2003
ponds_500buffer <- st_buffer(ponds_km, 0.5)
saveRDS(ponds_500buffer, "output/fivehundredmbuffer.rds")

#### Calculate Intersections #### 
# rename pond name column in points and buffers to reduce confusion 
ponds_km <- rename(ponds_km, IntersectingPonds = PondName)
ponds_2buffer <- rename(ponds_2buffer, Buffer = PondName)
ponds_500buffer <- rename(ponds_500buffer, Buffer=PondName)
# calculate intersection for each buffer 
int2 <- st_intersection(ponds_2buffer, ponds_km)
int500 <- st_intersection(ponds_500buffer, ponds_km)

#### Calculate Nearest Neighbours #### 
# if the buffer intersects with less than 5 ponds, we are going to find the 
# nearest neighbours outside the buffer so we can average the connectivity 
# with those

# identify which ponds have less than 5 ponds in their buffer for the 2 km buffer
# get a count for each pond identifier in the buffer column 
# select ponds with a count lower than 5 
int2tally <- int2 %>%
  group_by(Buffer)%>%
  tally %>%
  filter(n < 6) # specify 6 not 5 because every buffer contains its own pond

int500tally <- int500 %>%
  group_by(Buffer)%>%
  tally %>%
  filter(n <6)
int500tally
# important to note that there are no ponds with 5 other study ponds within a 500 m 
# radius, that is relevant to damselfly dispersal

# want to calculate the nearest neighbours for the 5 closest point outside the 
# 2 km buffer zone for each of the ponds in our list - we will then select the appropriate
# number of neighbours so each pond has minimum 5 neighbours

# we want our ponds as a dataframe so we can present the nearest neighbour later
ponds_km_df <- as.data.frame(ponds_km)
# use st_distance to calculate distances between study sites and create a matrix
dist.mat <- st_distance(ponds_km)
# sort and calculate the nearest distances 
nndist2 <- apply(dist.mat, 1, function(x){
  return(sort(x,partial=2)[2:7]) # select closest 5 neighbours, max. one pond could need is 5
})
# get the indices for nearest distance
nnindex2 <- apply(dist.mat, 1, function(x){ order(x, decreasing=F)[2:7]})
# make a new dataset showing the nearest neighbour data
nndata <- ponds_km_df
colnames(nndata)[1] <- "neighbour"
colnames(nndata)[2:ncol(nndata)] <- 
  paste0("n.", colnames(nndata)[2:ncol(nndata)])

ponds_km_2 <- data.frame(ponds_km,
                      nndata[nnindex2, ],
                      n.distance = nndist2
                      )
rownames(ponds_km_2) <- seq(nrow(ponds_km_2))
ponds_km_2

# convert ponds_km back to sp 
ponds_km_sp <- as_Spatial(ponds_km)
# convert sp to ppp for spatsstat package 
ponds_km_ppp <- as.ppp(ponds_km_sp)
nndist(ponds_km_ppp, k = 1:5)

