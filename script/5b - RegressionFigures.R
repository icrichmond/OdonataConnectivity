# Manuscript Figures 
# Author: Isabella Richmond 
# This script is for producing figures for the manuscript 

#### Load Packages ####
easypackages::packages("tidyverse", "data.table", "ggpubr", "patchwork", "tmap")


#### Load Data ####
ani <- fread("input/cleaned/AnisopteraCleaned.csv")
zyg <- fread("input/cleaned/ZygopteraCleaned.csv")
twokm <- readRDS("input/cleaned/BufferStats2km.rds")
threehm <- readRDS("input/cleaned/BufferStats300m.rds")
habitats <- fread("input/cleaned/HabitatNumbers.csv")
# join all buffer/habitat data 
twokm <- as_tibble(twokm)
threehm <- as_tibble(threehm)
buffers <- inner_join(twokm, threehm, by = "PondName", suffix = c(".two", ".threeh"))
habitats <- rename(habitats, PondName = Pond)
buffers <- inner_join(buffers, habitats, by = "PondName")
# assign SWF and NAT identifiers
buffers <- add_column(buffers, Group = "SWF")
buffers$Group[42:49] = "NAT"

#### Figure 1 - Anisoptera Abundance Regressions ####
mean <- ggplot(ani, aes(mean.two, abundance, shape=Group))+
  geom_point(aes(group = Group), size = 2)+
  geom_smooth(aes(group=1),method="lm",se=FALSE,col="black")+
  stat_cor(aes(mean.two,abundance,label = paste(..p.label..), group=1), label.y = 290, label.x = -0.23)+
  stat_regline_equation(aes(mean.two,abundance, group=1), label.y = 310, label.x = -0.23)+
  theme_classic() +
  xlab("Mean Current") +
  ylab("Anisoptera Abundance")+
  scale_shape_discrete(name = "Pond Type", labels = c("Natural", "Stormwater"))


(mean / sd) + plot_layout(guides = "collect")
ggsave("graphics/AnisopteraAbundanceRegression.jpg", dpi = 400)

#### Figure 2 - Anisoptera Species Richness Regressions ####
ggplot(ani, aes(meanres, speciescount, shape=Group))+
  geom_point(aes(group = Group), size = 2)+
  geom_smooth(aes(group=1),method="lm",se=FALSE,col="black")+
  stat_cor(aes(meanres,speciescount,label = paste(..p.label..), group=1), label.y = 11, label.x = 48)+
  stat_regline_equation(aes(meanres,speciescount, group=1), label.y = 12, label.x = 48)+
  theme_classic() +
  xlab("Mean Resistance") +
  ylab("Anisoptera Species Richness")+
  scale_shape_discrete(name = "Pond Type", labels = c("Natural", "Stormwater"))
ggsave("graphics/AnisopteraSpeciesRichnessRegression.jpg", dpi=400)

#### Figure 3 - Boxplot ####
meltponds <- select(ponds, -Pond1)
meltponds <- melt(meltponds, id.vars = "Group")

levels(meltponds$variable) <- c("Mean Resistance", "Standard Deviation Resistance","Number of Neighbours")

ggboxplot(twokm, x = "Group", y = "mean", add = "jitter")+
  theme_classic()+
  theme(strip.background = element_rect(colour = "black", fill = "grey"))+
  scale_x_discrete(labels = c("NAT" = "Natural", "SWF" = "Stormwater"))+
  xlab("Pond Type")+
  ylab("")+
  stat_compare_means(method = "t.test")+
  #facet_wrap(~ variable)
ggsave("graphics/PondComparisons.jpg", dpi = 400)
