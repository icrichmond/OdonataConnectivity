# Circuitscape Prep
# Author: Isabella Richmond 
# This script is for preparing a dataset of all stormwater ponds and natural 
# ponds present in the City of Ottawa to be run through Circuitscape 
# by Sean Boyle 

#### Load Packages ####
easypackages::packages("tidyverse", "data.table", "sf")

#### Load Data ####
# landcover 
landcov <- st_read("input/landcover/LandCover2011.shp")
# stormwater facilities 
swf <- st_read("input/landcover/StormwaterFacilities.shp")
# all ponds not included in wetlands with rivers excluded 
# i.e. golf course ponds
miscwater <- st_read("input/landcover/LandCover2011_WaterNoRivers.shp") 

#### Select Relevant Data ####
# we only want the class wetlands from the land cover dataset 
wetlands <- dplyr::filter(landcov, LABEL == "Wetland")

# select the operating stormwater facilities, those are the ones 
# that have water and are biologically relevant 
# also want to select the ones with names otherwise they do not 
# have valid coordinates 
operating <- swf %>% 
  dplyr::filter(OP_STATUS == "Operational") %>%
  dplyr::filter(PRI_CLASS == "SW-WPOND" | PRI_CLASS == "SW-DPOND") %>%
  drop_na(SWM_NO)


#### Calculate Centroids ####
# wetlands and water are polygons and need centroid points so we can use 
# them in Circuitscape 
wetlands_cent <- st_centroid(wetlands)
saveRDS(wetlands_cent, file = "output/large/Wetlands_Points.rds")

misc_cent <- st_centroid(miscwater)
saveRDS(misc_cent, file = "output/large/WaterNoRivers_Points.rds")

#### Combine Datasets #### 
# rename columns to match OBJECTID
operating <- rename(operating, OBJECTID = SWM_NO)
# select geometry and ID
operating <- select(operating, OBJECTID | geometry)
misc_cent <- select(misc_cent, OBJECTID | geometry)
wetlands_cent <- select(wetlands_cent, OBJECTID | geometry)
# combine datasets 
allpoints <- rbind(operating, misc_cent, wetlands_cent)
coords <- as.data.table(st_coordinates(allpoints))
allpoints <- cbind(allpoints, coords)
# save 
saveRDS(allpoints, "output/large/AllPointsCoordinates.rds")
fwrite(allpoints, "input/cleaned/AllPointsCoordinates.csv")
