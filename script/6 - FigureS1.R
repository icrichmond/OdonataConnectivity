# Figure S1 - Study Area with Satellite Imagery
# Author: Isabella Richmond 

#### Load Packages ####
libs <- c('sf', 'ggmap', 'ggplot2', 'dplyr', 'tibble', 'purrr')
lapply(libs, require, character.only = TRUE)

#### Study Sites ####
ponds <- readRDS("input/cleaned/CoorOttawa.rds")
ponds <- add_column(ponds, Group = "SWF")
ponds$Group[42:49] = "NAT"
# reproject data into WGS 84 for ease of interpretation
ponds <- st_transform(ponds, 4326)
# extract latitude and longitude 
ponds <- ponds %>%
  mutate(lat = unlist(map(ponds$geometry,1)),
         long = unlist(map(ponds$geometry,2)))
#### Study Site Figure ####
# Note: for this code to work you need a Google API with Geocoding and Maps Static enabled
qmap(location = "Ottawa, Ontario", maptype = "satellite", source = "google") +
  geom_point(data = ponds, aes(x = lat, y = long, colour = Group), size = 4, shape = 20) + 
  scale_colour_manual(labels = c("Natural", "Stormwater"), 
                      values = c("burlywood4", "burlywood"))
  
ggsave("graphics/SupplementaryMaterialS1.pdf", dpi = 450)
