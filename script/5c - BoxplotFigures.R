# Manuscript Figures 
# Author: Isabella Richmond 
# This script is for producing figures for the manuscript 

#### Load Packages ####
easypackages::packages("tidyverse", "ggpubr", "sf", "data.table")

#### Load Data ####
twokm <- readRDS("input/cleaned/BufferStats2km.rds")
threehm <- readRDS("input/cleaned/BufferStats300m.rds")
habitats <- read_csv("input/cleaned/HabitatNumbers.csv")
# join all buffer/habitat data 
twokm <- st_set_geometry(twokm, NULL)
threehm <- st_set_geometry(threehm, NULL)
buffers <- inner_join(twokm, threehm, by = "PondName", suffix = c(".two", ".threeh"))
habitats <- rename(habitats, PondName = Pond)
buffers <- inner_join(buffers, habitats, by = "PondName")
# assign SWF and NAT identifiers
buffers <- add_column(buffers, Group = "SWF")
buffers$Group[42:49] = "NAT"
# select relevant columns 
buffers_s <- select(buffers, c("PondName", "mean.two", "n.two",
                               "mean.threeh", "n.threeh",
                               "Group"))
# melt
meltbuffers <- melt(buffers_s, id.vars = c("PondName", "Group"))

#### Figure 1 - Boxplot ####
levels(meltbuffers$variable) <- c("Mean Current (2 km)", "Number of Surrounding Habitats (2 km)",
                                  "Mean Current (300 m)", "Number of Surrounding Habitats (300 m)")

ggboxplot(meltbuffers, x = "Group", y = "value", add = "jitter")+
  theme_classic()+
  theme(strip.background = element_rect(colour = "black", fill = "grey"),
        text = element_text(size = 20))+
  scale_x_discrete(labels = c("NAT" = "Natural", "SWF" = "Stormwater"))+
  xlab("Pond Type")+
  ylab("")+
  stat_compare_means(method = "t.test", size = 5)+
  facet_wrap(~ variable, scales = "free")

ggsave("graphics/ComparisonBoxplots.jpg", width = 12, height = 10, dpi = 400)
