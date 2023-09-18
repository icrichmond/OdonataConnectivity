library(dplyr)
library(gt)

# data
ani <- read.csv('input/cleaned/AnisopteraCleaned.csv') %>%
  mutate(DragonflyAbundance = abundance,
         DragonflySpeciesRich = speciescount, 
         DragonflyShannon = shannon) %>%
  select(Pond, Group, DragonflyAbundance, DragonflySpeciesRich, DragonflyShannon, mean.nine, sd.nine)
zyg <- read.csv('input/cleaned/ZygopteraCleaned.csv')%>%
  mutate(DamselflyAbundance = abundance,
         DamselflySpeciesRich = speciescount, 
         DamselflyShannon = shannon) %>%
  select(Pond, DamselflyAbundance, DamselflySpeciesRich, DamselflyShannon, mean.three, sd.three)
hab <- read.csv('input/cleaned/HabitatNumbers.csv') %>%
  select(-X)
ponds <- read.csv('input/pond_descriptive.csv')


# join
anizyg <- full_join(ani, zyg, by = "Pond")
pondshab <- left_join(hab, ponds, by = "Pond")
all <- full_join(anizyg, pondshab, by = "Pond")

# clean up
df<- all %>%
  select(Pond, Group, X, Y, age..years., area..m2., catchment..km2., imperviousness.of.catchment...., 
         DragonflyAbundance, DragonflySpeciesRich, DragonflyShannon, DamselflyAbundance, DamselflySpeciesRich, DamselflyShannon,
         mean.nine, sd.nine, n.nine, mean.three, sd.three, n.three) %>%
  rename('Dragonfly Abundance' = DragonflyAbundance,
         'Dragonfly Species Richness' = DragonflySpeciesRich, 
         'Dragonfly Shannon Diversity' = DragonflyShannon,
         'Mean Current Density (900 m)' = mean.nine,
         'Std. Dev. Current Density (900 m)' = sd.nine,
         'Damselfly Abundance' = DamselflyAbundance,
         'Damselfly Species Richness' = DamselflySpeciesRich,
         'Damselfly Shannon Diversity' = DamselflyShannon,
         'Mean Current Density (300 m)' = mean.three,
         'Std. Dev. Current Density (300 m)' = sd.three,
         'Number of Nearest Neighbours (900 m)' = n.nine,
         'Number of Nearest Neighbours (300 m)' = n.three,
         'X coordinate' = X,
         'Y coordinate' = Y,
         'Age (years)' = age..years.,
         'Area (m2)' = area..m2., 
         'Catchment Area (km2)' = catchment..km2.,
         'Imperviousness of Catchment' = imperviousness.of.catchment....) %>%
  gt()
