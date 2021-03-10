# Spatial Figures 
# Author: Isabella Richmond 
# This script is for producing spatial figures for this manuscript

#### Load Packages ####
easypackages::packages("raster", "sf", "tmap", "tidyverse")

#### Load Data ####
current <- raster("input/CurrentMap.txt")
ponds <- readRDS("input/cleaned/CoorOttawa.rds")
# assign CRS to current map
crs(current) <- "+proj=tmerc +lat_0=0 +lon_0=-76.5 +k=0.9999 +x_0=304800 +y_0=0 +datum=NAD83 +units=m +no_defs"
# assign SWF and NAT identifiers
ponds <- add_column(ponds, Group = "SWF")
ponds$Group[42:49] = "NAT"

#### Reproject ####
# reproject data into WGS 84 for ease of interpretation
current <- projectRaster(current, crs="+init=epsg:4326")
wgs84 <- st_crs('EPSG:4326')
ponds <- st_transform(ponds, wgs84)

#### Study Site Figure ####
# greyscale
ss <- tm_shape(current)+
  tm_raster(palette = "Greys", midpoint = NA, style = "cont", title = "Current", legend.reverse = T) +
tm_grid()+
tm_shape(ponds)+
  tm_dots(size = 0.12, shape = "Group", shapes = c(21, 24), col = "black",
          title.shape = "Pond Type", shapes.labels = c("Natural", "Stormwater"))+
tm_layout(legend.position = c("left", "bottom"), legend.bg.color = "white")

tmap_save(ss, "graphics/currentmap.png", dpi = 450)

sscol <- tm_shape(current)+
  tm_raster(palette = "YlOrRd", midpoint = NA, style = "cont", title = "Current", legend.reverse = T) +
  tm_grid()+
  tm_shape(ponds)+
  tm_dots(size = 0.12, shape = "Group", shapes = c(21, 24), col = "black",
          title.shape = "Pond Type", shapes.labels = c("Natural", "Stormwater"))+
  tm_layout(legend.position = c("left", "bottom"), legend.bg.color = "white")

tmap_save(sscol, "graphics/currentmapcolour.png", dpi = 450)
