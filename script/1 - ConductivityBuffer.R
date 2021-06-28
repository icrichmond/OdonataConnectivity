# Conductivity Buffers
# Author: Isabella Richmond 
# This script is for producing buffers around each study site and extracting all conductance 
# values within the buffers 

#### Load Packages ####
easypackages::packages("raster", "data.table", "sf", "tidyverse")

#### Load Data ####
con <- raster("input/CurrentMapRivers.asc")
ponds <- fread("input/CoorMetre.csv")

#### Create Buffers ####
# remove NAT-4 and NAT-5 as they use a different land cover layer 
# and are thus removed from analysis 
ponds <- rename(ponds, "PondName" = "Pond Name")
ponds <- filter(ponds, !PondName %in% c("NAT-4", "NAT-5"))
# pond coordinates were obtained from the City of Ottawa and have 
# a Transverse Mercator projection in meters and a GCS of 
# GCS North American 1983 (EPSG 4269)
coordinates(ponds) <- c("X_Meters", "Y_Meters")
proj4string(ponds) <- CRS("+proj=tmerc +lat_0=0 +lon_0=-76.5 +k=0.9999 +x_0=304800 +y_0=0 +datum=NAD83 +units=m")
ponds <- st_as_sf(ponds)
# save metric projection before transforming 
saveRDS(ponds, file="input/cleaned/CoorOttawa.rds")

#### Extract Conductance ####
# extract conductance values for each buffer for dragonflies
ex900 <- raster::extract(con, ponds, buffer = 900,df = T)
# create dataframes 
ex900t <- as_tibble(ex900)
# get mean, median, and summary for each stormwater pond
summary900 <- ex900t %>% 
  group_by(ID) %>%
  summarise(mean = mean(CurrentMapRivers), median = median(CurrentMapRivers), 
            sum = sum(CurrentMapRivers), sd = sd(CurrentMapRivers))
# get pond names and coordinates 
final900 <- cbind(ponds, summary900)
# save
saveRDS(final900, "input/cleaned/BufferStats900m.rds")

# damselflies
ex300 <- raster::extract(con, ponds, buffer = 300, df = T)
ex300t <- as_tibble(ex300)
summary300 <- ex300t %>% 
  group_by(ID) %>%
  summarise(mean = mean(CurrentMapRivers), median = median(CurrentMapRivers), 
            sum = sum(CurrentMapRivers), sd = sd(CurrentMapRivers))
# get pond names and coordinates 
final300 <- cbind(ponds, summary300)
# save
saveRDS(final300, "input/cleaned/BufferStats300m.rds")
