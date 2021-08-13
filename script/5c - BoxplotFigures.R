# Manuscript Figures 
# Author: Isabella Richmond 
# This script is for producing figures for the manuscript 

#### Load Packages ####
p <- c("dplyr", "ggplot2", "readr", "tibble", "data.table", "ggpubr", "sf")
lapply(p, library, character.only=T)

#### Load Data ####
ninem <- readRDS("input/cleaned/BufferStats900m.rds")
threem <- readRDS("input/cleaned/BufferStats300m.rds")
habitats <- read_csv("input/cleaned/HabitatNumbers.csv")
# join all buffer/habitat data 
ninem <- st_set_geometry(ninem, NULL)
threem <- st_set_geometry(threem, NULL)
buffers <- inner_join(ninem, threem, by = "PondName", suffix = c(".nine", ".three"))
habitats <- rename(habitats, PondName = Pond)
buffers <- inner_join(buffers, habitats, by = "PondName")
# assign SWF and NAT identifiers
buffers <- add_column(buffers, Group = "SWF")
buffers$Group[42:49] = "NAT"
# select relevant columns 
buffers_s <- select(buffers, c("PondName", "mean.nine", "n.nine",
                               "mean.three", "n.three",
                               "Group"))
# melt
meltbuffers <- melt(buffers_s, id.vars = c("PondName", "Group"))

#### Figure 1 - Boxplot ####
levels(meltbuffers$variable) <- c("Mean Current (900 m)", "Number of Surrounding Habitats (900 m)",
                                  "Mean Current (300 m)", "Number of Surrounding Habitats (300 m)")

ggboxplot(meltbuffers, x = "Group", y = "value", add = "jitter")+
  theme_classic()+
  theme(strip.background = element_rect(colour = "black", fill = "grey"),
        text = element_text(size = 20))+
  scale_x_discrete(labels = c("NAT" = "Natural", "SWF" = "Stormwater"))+
  xlab("Pond Type")+
  ylab("")+
  stat_compare_means(method = "wilcox.test", paired = FALSE, size = 5)+
  facet_wrap(~ variable, scales = "free")

ggsave("graphics/ComparisonBoxplots.jpg", width = 12, height = 10, dpi = 400)
