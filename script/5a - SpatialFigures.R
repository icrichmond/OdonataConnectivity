# Spatial Figures 
# Author: Isabella Richmond 
# This script is for producing spatial figures for this manuscript

#### Load Packages ####
p <- c('stars', 'sf', 'tibble', 'dplyr', 'purrr', 'ggmap', 'ggplot2', 'patchwork')
lapply(p, library, character.only=T)

#### Load Data ####
current <- read_stars("input/CurrentMapRivers.asc")
ponds <- readRDS("input/cleaned/CoorOttawa.rds")
# assign CRS to current map
st_crs(current) <- "+proj=tmerc +lat_0=0 +lon_0=-76.5 +k=0.9999 +x_0=304800 +y_0=0 +datum=NAD83 +units=m +no_defs"
ponds <- add_column(ponds, Group = "SWF")
ponds$Group[42:49] = "NAT"
current_df <- as.data.frame(current, xy = TRUE) %>%
  na.omit()


#### Study Site Figure ####
# Circuitscape
sscol <- ggplot() + 
  geom_raster(data = current_df, aes(x = x, y = y, fill = CurrentMapRivers.asc)) + 
  coord_equal() + 
  geom_sf(aes(shape = Group), data = ponds) +
  scale_shape_manual(labels = c('Natural', 'Stormwater'), values = c(21, 24)) + 
  scale_fill_viridis_c(direction = -1, option = 'B') + 
  theme(legend.position = 'top',
        legend.title = element_blank(), 
        panel.border = element_rect(size = 1, fill = NA),
        panel.background = element_rect(fill = NA),
        panel.grid = element_line(color = '323232', size = 0.2),
        axis.text = element_text(size = 13, color = 'black'),
        axis.title = element_blank(), 
        legend.text = element_text(size = 20),
        plot.background = element_rect(fill = NA, colour = NA))

# satellite imagery
# reproject data into WGS 84 for ease of interpretation
ponds_t <- st_transform(ponds, 4326)
# extract latitude and longitude 
ponds_e <- ponds_t %>%
  mutate(lat = unlist(map(ponds_t$geometry,1)),
         long = unlist(map(ponds_t$geometry,2)))
# API: AIzaSyDlRF5BYeskCH7qWtq13WUV5ifG9Q1kT1c
register_google(key = "AIzaSyDlRF5BYeskCH7qWtq13WUV5ifG9Q1kT1c")
satcol <- qmap(location = "Ottawa, Ontario", zoom = 9, maptype = "satellite", source = "google") +
  geom_point(data = ponds_e, aes(x = lat, y = long, shape = Group), size = 2) + 
  scale_shape_manual(values = c(21, 24)) + 
  theme(legend.position = 'none',
        panel.border = element_rect(size = 1, fill = NA),
        panel.background = element_rect(fill = NA),
        panel.grid = element_line(color = 'black', size = 0.2),
        axis.text = element_text(size = 13, color = 'black'),
        axis.title = element_blank(), 
        panel.ontop = TRUE)

sscol + satcol + plot_layout(widths = c(2,1))


ggsave('graphics/studymapcolour.jpg', dpi = 450)
