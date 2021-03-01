# Manuscript Figures 
# Author: Isabella Richmond 
# This script is for producing figures for the manuscript 

#### Load Packages ####
easypackages::packages("tidyverse", "data.table", "ggpubr", "patchwork")


#### Load Data ####
ani <- fread("output/AnisopteraResistance.csv")
zyg <- fread("output/ZygopteraResistance.csv")
ponds <- fread("output/AverageResistance.csv")
# add groups to differentiate between natural and stormwater ponds 
ani <- add_column(ani, Group = "SWF")
ani$Group[42:49] = "NAT"

zyg <- add_column(zyg, Group = "SWF")
zyg$Group[42:49] = "NAT"

ponds <- add_column(ponds, Group = "SWF")
ponds$Group[1:8] = "NAT"

#### Figure 1 - Anisoptera Abundance Regressions ####
mean <- ggplot(ani, aes(meanres, abundance, shape=Group))+
  geom_point(aes(group = Group), size = 2)+
  geom_smooth(aes(group=1),method="lm",se=FALSE,col="black")+
  stat_cor(aes(meanres,abundance,label = paste(..p.label..), group=1), label.y = 290, label.x = 0.09)+
  stat_regline_equation(aes(meanres,abundance, group=1), label.y = 310, label.x = 0.09)+
  theme_classic() +
  xlab("Mean Resistance") +
  ylab("Anisoptera Abundance")+
  scale_shape_discrete(name = "Pond Type", labels = c("Natural", "Stormwater"))

sd <- ggplot(ani, aes(sdres, abundance, shape=Group))+
  geom_point(aes(group = Group), size = 2)+
  geom_smooth(aes(group=1),method="lm",se=FALSE,col="black")+
  stat_cor(aes(sdres,abundance,label = paste(..p.label..), group=1), label.y = 280, label.x = 0.07)+
  stat_regline_equation(aes(sdres,abundance, group=1), label.y = 300, label.x = 0.07)+
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

ggboxplot(meltponds, x = "Group", y = "value", add = "jitter")+
  theme_classic()+
  theme(strip.background = element_rect(colour = "black", fill = "grey"))+
  scale_x_discrete(labels = c("NAT" = "Natural", "SWF" = "Stormwater"))+
  xlab("Pond Type")+
  ylab("")+
  stat_compare_means(method = "t.test")+
  facet_wrap(~ variable)
ggsave("graphics/PondComparisons.jpg", dpi = 400)
