# Habitat
# Author: Isabella Richmond
# This script is for determining the number of habitats present surrounding 
# each study pond within a 2 km and 300 m buffer

#### Load Packages ####
easypackages::packages("data.table", "tidyverse", "sf", "sp")

#### Load Data ####
allponds <- fread("input/cleaned/PondCoords2km.csv", quote="")
studyponds <- fread("input/CoorMetre.csv")
# remove NAT-4 and NAT-5
# NAT-4 and NAT-5 are in the randomized buffer of our connectivity
# map, not appropriate measurements 
ode <- ode[!(ode$Pond=="NAT-4" | ode$Pond =="NAT-5"),]
allponds <- allponds[!(allponds$OBJECTID=="NAT-4" | allponds$OBJECTID =="NAT-5"),]
studyponds <- studyponds[!(studyponds$`Pond Name`=="NAT-4" | studyponds$`Pond Name` =="NAT-5"),]


#### Create Buffer ####
# convert sp to sf 
coordinates(allponds) <- c("X", "Y")
proj4string(allponds) <- CRS("+proj=tmerc +lat_0=0 +lon_0=-76.5 +k=0.9999 +x_0=304800 +y_0=0 +datum=NAD83 +units=m")
allponds <- st_as_sf(allponds)
plot(allponds)
# produce 2 km buffer for Anisoptera - consistent with Perron et al 2021
ponds_2buffer <- st_buffer(allponds, 2000)
# produce 300 m buffer for Zygoptera - consistent with Purse et al 2003
ponds_300buffer <- st_buffer(allponds, 300)

#### Calculate Intersections #### 
# rename pond name column in points and buffers to reduce confusion 
allponds <- rename(allponds, IntersectingPonds = OBJECTID)
ponds_2buffer <- rename(ponds_2buffer, Buffer = OBJECTID)
ponds_300buffer <- rename(ponds_300buffer, Buffer=OBJECTID)
# calculate intersection for each buffer 
int2 <- st_intersection(ponds_2buffer, allponds)
int300 <- st_intersection(ponds_300buffer, allponds)

# count how many intersections there are for each study pond
# subset dataset to include study pond buffers only
studyponds <- rename(studyponds, Buffer = "Pond Name")
int2_study <- inner_join(int2, studyponds, by = "Buffer")
int300_study <- inner_join(int300, studyponds, by = "Buffer")
# also tally the number of neighbours they have
habitats2 <- int2_study %>% 
  group_by(Buffer) %>%
  dplyr::summarise(n = n())

habitats300 <- int300_study %>% 
  group_by(Buffer) %>%
  dplyr::summarise(n = n())

# join datasets
habitats2 <- as_tibble(habitats2)
habitats300 <- as_tibble(habitats300)
habitats <- inner_join(habitats2, habitats300, by = "Buffer", suffix = c(".two", ".threeh"))
habitats <- habitats %>%
  rename(Pond = Buffer) %>%
  select(-c(geometry.two, geometry.threeh))

# save
write.csv(habitats, "input/cleaned/HabitatNumbers.csv")
