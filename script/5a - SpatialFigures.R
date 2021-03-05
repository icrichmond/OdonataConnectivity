# Spatial Figures 
# Author: Isabella Richmond 
# This script is for producing spatial figures for this manuscript

#### Load Packages ####
easypackages::packages("raster", "sf", "tmap")

#### Load Data ####
current <- raster("input/CurrentMap.txt")
ponds <- readRDS("input/cleaned/CoorOttawa.rds")
# assign CRS to current map
crs(current) <- "+proj=tmerc +lat_0=0 +lon_0=-76.5 +k=0.9999 +x_0=304800 +y_0=0 +datum=NAD83 +units=m +no_defs"
# assign SWF and NAT identifiers
ponds <- add_column(ponds, Group = "SWF")
ponds$Group[42:49] = "NAT"

#### Study Site Figure ####