# Manuscript Figures 
# Author: Isabella Richmond 
# This script is for producing figures for the manuscript 

#### Load Packages ####
easypackages::packages("tidyverse", "data.table", "ggpubr", "patchwork")


#### Load Data ####
ani <- fread("output/AnisopteraFull.csv")
zyg <- fread("output/ZygopteraFull.csv")
twokm <- fread("input/cleaned/BufferStats2km.csv")
threehm <- fread("input/cleaned/BufferStats300m.csv")
# add groups to differentiate between natural and stormwater ponds 
twokm <- add_column(twokm, Group = "SWF")
twokm$Group[42:49] = "NAT"

threehm <- add_column(threehm, Group = "SWF")
threehm$Group[42:49] = "NAT"

#### Figure 1 - Anisoptera Abundance Regressions ####
mean <- ggplot(ani, aes(median.x, abundance, shape=Group))+
  geom_point(aes(group = Group), size = 2)+
  geom_smooth(aes(group=1),method="lm",se=FALSE,col="black")+
  stat_cor(aes(median.x,abundance,label = paste(..p.label..), group=1), label.y = 290, label.x = -0.25)+
  stat_regline_equation(aes(median.x,abundance, group=1), label.y = 310, label.x = -0.25)+
  theme_classic() +
  xlab("Mean Resistance") +
  ylab("Anisoptera Abundance")+
  scale_shape_discrete(name = "Pond Type", labels = c("Natural", "Stormwater"))

sd <- ggplot(ani, aes(sdres, abundance, shape=Group))+
  geom_point(aes(group = Group), size = 2)+
  geom_smooth(aes(group=1),method="lm",se=FALSE,col="black")+
  stat_cor(aes(sdres,abundance,label = paste(..p.label..), group=1), label.y = 280, label.x = -0.24)+
  stat_regline_equation(aes(sdres,abundance, group=1), label.y = 300, label.x = 0.0)+
  theme_classic() +
  xlab("Standard Deviation Resistance") +
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
