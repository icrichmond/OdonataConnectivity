# Buffer & Nearest Neighbours
# Author: Isabella Richmond 
# This script is for producing biologically relevant buffers around each study pond to 
# identify how many study ponds are within a biologically relevant distance. 
# If ponds have less than 5 neighbours within 2 km then we calculate the 5 nearest neighbours 

#### Load Packages ####
easypackages::packages("tidyverse", "sf", "sp", "spatstat", "maptools", "data.table")

#### Load Data ####
ponds <- readRDS("input/cleaned/CoorMetric.rds")
studyponds <- fread("input/CoorMetre.csv")

#### Create Buffer ####
# convert sp to sf 
ponds <- st_as_sf(ponds)
ponds <- select(ponds, -ID)
plot(ponds)
# produce 2 km buffer for Anisoptera - consistent with Perron et al 2021
ponds_2buffer <- st_buffer(ponds, 2)
# plot looks misaligned but when you use Viewer buffer and ponds are lined up
saveRDS(ponds_2buffer, "output/buffers/twokmbuffer.rds")
# produce 500 m buffer for Zygoptera - consistent with Purse et al 2003
ponds_500buffer <- st_buffer(ponds, 0.5)
saveRDS(ponds_500buffer, "output/buffers/fivehundredmbuffer.rds")

#### Calculate Intersections #### 
# rename pond name column in points and buffers to reduce confusion 
ponds <- rename(ponds, IntersectingPonds = PondName)
ponds_2buffer <- rename(ponds_2buffer, Buffer = PondName)
ponds_500buffer <- rename(ponds_500buffer, Buffer=PondName)
# calculate intersection for each buffer 
int2 <- st_intersection(ponds_2buffer, ponds)
int500 <- st_intersection(ponds_500buffer, ponds)

# identify which study ponds have less than 5 ponds in their buffer for the 2 km buffer
# we expect 3 ponds - 1133, 1501, 1622
# get a count for each pond identifier in the buffer column 
# select ponds with a count less than 5 
int2tally <- int2 %>%
  group_by(Buffer)%>%
  tally %>%
  filter(n < 6) # specify 6 not 5 because every buffer contains its own pond
# join with study ponds 
studyponds <- rename(studyponds, "Buffer" = "Pond Name")
int2tally <- inner_join(int2tally, studyponds, by = "Buffer")
# 3 study ponds without 5 ponds in 2 km, as expected (SWF-1133, SWF-1501, and SWF-1622)

int500tally <- int500 %>%
  group_by(Buffer)%>%
  tally %>%
  filter(n < 6)
# join with study ponds
studyponds <- rename(studyponds, "Buffer" = "Pond Name")
int500tally <- inner_join(int500tally, studyponds, by = 'Buffer')
# 33 out of 51 study ponds have less than 5 ponds within 500 m, relevant for Zygoptera

#### Calculate Nearest Neighbours #### 
# we are going to find the  nearest neighbours outside the 2 km buffer so we can 
# average the connectivity with those if the buffer intersects with less than 5 ponds
# want to calculate the nearest neighbours for the 5 closest study ponds

# 3 ponds do not have 5 intersections within 2 km, SWF-1133, SWF-1501, SWF-1622
# calculate the 5 nearest neighbours for these three ponds
# convert studyponds_km back to sp 
ponds_sp <- as_Spatial(ponds)
# convert sp to ppp for spatsstat package 
ponds_ppp <- as.ppp(ponds_sp)
# find which ponds are the nearest neighbours 
nn <- nnwhich(ponds_ppp, k=1:5)
nn <- as.data.table(nn)
nn <- nn %>%
  mutate(Pond = ponds$IntersectingPonds)
nn2 <-
  reshape2::dcast(mutate(
    reshape2::melt(nn, id.var="Pond"), 
    value = plyr::mapvalues(
      value, c(seq(1:2383)), c(ponds_ppp$marks))
  ), Pond~variable)
# NOTE: removed calculation of distances because there aren't the same number of 
# neighbours for every pond and distance is not a good metric for connectivity

# select SWF-1133, SWF-1501, SWF-1622
nn_sub <- filter(nn2, Pond == "SWF-1133" | Pond == "SWF-1501" | Pond == "SWF-1622")
# melt to match format of int2 
nn_sub_m <- melt(nn_sub, id.var="Pond")
nn_sub_m <- nn_sub_m %>%
  rename(Buffer = Pond) %>%
  rename(IntersectingPonds = value) %>%
  select(-variable)
# remove geometry from int 
int2_fin <- st_set_geometry(int2, NULL)
# remove rows of SWF-1133, SWF-1501, and SWF-1622 from int to avoid duplicates
int2_fin <- filter(int2_fin, ! Buffer %in% c("SWF-1133", "SWF-1501", "SWF-1622") )
# add SWF-1133 and SWF-1501 to int dataset 
int2_fin <- rbind(nn_sub_m, int2_fin)
# now every study pond has at least 5 nearest neighbours 


#### Select Study Ponds ####
# subset dataset to include study pond buffers only
final <- inner_join(int2_fin, studyponds, by = "Buffer")
fwrite(final, "output/large/IntersectionsAllPonds.csv")
