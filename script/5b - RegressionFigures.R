# Regression Figures
# Author: Isabella Richmond 
# This script is for producing figures demonstrating the 
# relationships between current and Odonates

#### Load Packages ####
p <- c("dplyr", "ggplot2", "data.table", "ggpubr")
lapply(p, library, character.only=T)

#### Load Data ####
ani <- fread("input/cleaned/AnisopteraCleaned.csv")
zyg <- fread("input/cleaned/ZygopteraCleaned.csv")

# select the relevant columns 
anis <- select(ani, c(Pond, abundance, speciescount, shannon, Group, mean.nine, sd.nine, n.nine))
zygs <- select(zyg, c(Pond, abundance, speciescount, shannon, Group, mean.three, sd.three, n.three))

#### Figure 1 - Anisoptera Regressions ####
am <- ggplot(anis, aes(x = mean.nine, y = abundance))+
  geom_point(aes(size = 3))+
  geom_smooth(method="lm",se=FALSE,col="black")+
  stat_cor(aes(x = mean.nine,y = abundance,label = paste(..rr.label.., ..p.label.., sep = "~`,`~"), group=1), label.y = 295, label.x = -1.2, size = 5)+
  stat_regline_equation(aes(x = mean.nine,y = abundance), label.y = 310, label.x = -1.2, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20), 
        legend.position = "none")+
  xlab("Mean Current") +
  ylab("Anisoptera Estimated Abundance")

asd <- ggplot(anis, aes(x= sd.nine, y = abundance))+
  geom_point(aes(size = 3))+
  geom_smooth(method="lm",se=FALSE,col="black")+
  stat_cor(aes(x = sd.nine,y = abundance,label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.y = 295, label.x = 0, size = 5)+
  stat_regline_equation(aes(x = sd.nine, y = abundance), label.y = 310, label.x = 0, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20), 
        legend.position = "none")+
  xlab("Standard Deviation Current") +
  ylab("Anisoptera Estimated Abundance")

ah <- ggplot(anis, aes(n.nine, abundance))+
  geom_point(aes(size = 3))+
  geom_smooth(method="lm",se=FALSE,col="black")+
  stat_cor(aes(n.nine,abundance,label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.y = 290, label.x = 0.45, size = 5)+
  stat_regline_equation(aes(n.nine,abundance), label.y = 310, label.x = 0.45, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20), 
        legend.position = "none")+
  xlab("Number of Surrounding Habitats") +
  ylab("Anisoptera Estimated Abundance")

as <- ggplot(anis, aes(mean.nine, shannon))+
  geom_point(aes(size = 3))+
  geom_smooth(method="lm",se=FALSE,col="black")+
  stat_cor(aes(mean.nine,shannon,label = paste(..rr.label.., ..p.label.., sep = "~`,`~"), group=1), label.y = 2, label.x = -0.35, size = 5)+
  stat_regline_equation(aes(mean.nine,shannon), label.y = 2.1, label.x = -0.35, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20), 
        legend.position = "none")+
  xlab("Mean Current") +
  ylab("Anisoptera Shannon Diversity")

srm <- ggplot(anis, aes(mean.nine, speciescount))+
  geom_point(aes(size = 3))+
  geom_smooth(method="lm",se=FALSE,col="black")+
  stat_cor(aes(mean.nine,speciescount,label = paste(..rr.label.., ..p.label.., sep = "~`,`~"), group=1), label.y = 12.5, label.x = -1.2,  size = 5)+
  stat_regline_equation(aes(mean.nine,speciescount), label.y = 13, label.x = -1.2, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20), 
        legend.position = "none")+
  xlab("Mean Current") +
  ylab("Anisoptera Species Richness")

srh <- ggplot(anis, aes(n.nine, speciescount))+
  geom_point(aes(size = 3))+
  geom_smooth(method="lm",se=FALSE,col="black")+
  stat_cor(aes(n.nine,speciescount,label = paste(..rr.label.., ..p.label.., sep = "~`,`~"), group=1), label.y = 12.5, label.x = 0.4, size = 5)+
  stat_regline_equation(aes(n.nine,speciescount), label.y = 13, label.x = 0.4, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20), 
        legend.position = "none")+
  xlab("Number of Surrounding Habitats") +
  ylab("Anisoptera Species Richness")


ggarrange(am, ah, srm, srh, ncol = 2, nrow = 2, labels = "auto", font.label = list(size = 20))

ggsave("graphics/AnisopteraRegression.jpg", width = 15, height = 12, dpi = 400)

#### Figure 2 - Anisoptera Species Richness Regressions ####
zsdc <- ggplot(zygs, aes(x = mean.three, y = shannon))+
  geom_point(aes(size=3))+
  stat_cor(aes(label = paste(..p.label..), group=1), label.y = 1.9, label.x = -0.3, size = 5)+
  stat_cor(aes(label = paste(..rr.label..), group=1), label.y = 1.8, label.x = -0.3, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20), 
        legend.position = "none")+
  xlab("Mean Current") +
  ylab("Zygoptera Shannon Diversity")

zsdh <- ggplot(zygs, aes(x= n.three, y = shannon))+
  geom_point(aes(size=3))+
  stat_cor(aes(label = paste(..rr.label..), group =1), label.y = 1.9, label.x = 16, size = 5)+
  stat_cor(aes(label = paste(..p.label..), group = 1), label.y = 1.8, label.x = 16, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20), 
        legend.position = "none")+
  xlab("Number of Surrounding Habitats") +
  ylab("Zygoptera Shannon Diversity")

zsrc <- ggplot(zygs, aes(mean.three, speciescount))+
  geom_point(aes(size=3))+
  geom_smooth(method="lm",se=FALSE,col="black")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.y = 10.5, label.x = -0.5, size = 5)+
  stat_regline_equation(label.y = 11, label.x = -0.5, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20), 
        legend.position = "none")+
  xlab("Mean Current") +
  ylab("Zygoptera Species Richness")

zsrh <- ggplot(zygs, aes(n.three, speciescount))+
  geom_point(aes(size=3))+
  stat_cor(aes(label = paste(..rr.label..), group=1), label.y = 11, label.x = 15.5, size = 5)+
  stat_cor(aes(label = paste(..p.label..), group=1), label.y = 10.5, label.x = 15.5, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20), 
        legend.position = "none")+
  xlab("Number of Surrounding Habitats") +
  ylab("Zygoptera Species Richness")


ggarrange(zsdc, zsdh, zsrc, zsrh, ncol = 2, nrow = 2, labels = "auto", font.label = list(size = 20))

ggsave("graphics/ZygopteraRegression.jpg", width = 15, height = 12, dpi = 400)
