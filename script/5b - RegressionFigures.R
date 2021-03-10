# Regression Figures
# Author: Isabella Richmond 
# This script is for producing figures demonstrating the 
# relationships between current and Odonates

#### Load Packages ####
easypackages::packages("tidyverse", "data.table", "ggpubr")

#### Load Data ####
ani <- fread("input/cleaned/AnisopteraCleaned.csv")
zyg <- fread("input/cleaned/ZygopteraCleaned.csv")

# select the relevant columns 
anis <- select(ani, c(Pond, abundance, speciescount, shannon, Group, mean.two, sd.two, n.two))
zygs <- select(zyg, c(Pond, abundance, speciescount, shannon, Group, mean.threeh, sd.threeh, n.threeh))

#### Figure 1 - Anisoptera Regressions ####
am <- ggplot(anis, aes(x = mean.two, y = abundance))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE,col="black")+
  stat_cor(aes(x = mean.two,y = abundance,label = paste(..rr.label..), group=1), label.y = 295, label.x = -1, size = 5)+
  stat_regline_equation(aes(x = mean.two,y = abundance), label.y = 310, label.x = -1, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20))+
  xlab("Mean Current") +
  ylab("Anisoptera Abundance")

asd <- ggplot(anis, aes(x= sd.two, y = abundance))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE,col="black")+
  stat_cor(aes(x = sd.two,y = abundance,label = paste(..rr.label..)), label.y = 290, label.x = 0, size = 5)+
  stat_regline_equation(aes(x = sd.two, y = abundance), label.y = 310, label.x = 0, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20))+
  xlab("Standard Deviation Current") +
  ylab("Anisoptera Abundance")

ah <- ggplot(anis, aes(n.two, abundance))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE,col="black")+
  stat_cor(aes(n.two,abundance,label = paste(..rr.label..)), label.y = 290, label.x = 0.45, size = 5)+
  stat_regline_equation(aes(n.two,abundance), label.y = 310, label.x = 0.45, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20))+
  xlab("Number of Surrounding Habitats") +
  ylab("Anisoptera Abundance")

as <- ggplot(anis, aes(mean.two, shannon))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE,col="black")+
  stat_cor(aes(mean.two,shannon,label = paste(..rr.label..), group=1), label.y = 2, label.x = -0.35, size = 5)+
  stat_regline_equation(aes(mean.two,shannon), label.y = 2.1, label.x = -0.35, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20))+
  xlab("Mean Current") +
  ylab("Anisoptera Shannon Diversity")

srm <- ggplot(anis, aes(mean.two, speciescount))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE,col="black")+
  stat_cor(aes(mean.two,speciescount,label = paste(..rr.label..), group=1), label.y = 12.5, label.x = -0.9, size = 5)+
  stat_regline_equation(aes(mean.two,speciescount), label.y = 13, label.x = -0.9, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20))+
  xlab("Mean Current") +
  ylab("Anisoptera Species Richness")

srh <- ggplot(anis, aes(n.two, speciescount))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE,col="black")+
  stat_cor(aes(n.two,speciescount,label = paste(..rr.label..), group=1), label.y = 12.5, label.x = 0.4, size = 5)+
  stat_regline_equation(aes(n.two,speciescount), label.y = 13, label.x = 0.4, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20))+
  xlab("Number of Surrounding Habitats") +
  ylab("Anisoptera Species Richness")


ggarrange(am, ah, srm, srh, ncol = 2, nrow = 2, labels = "auto", font.label = list(size = 20))

ggsave("graphics/AnisopteraRegression.jpg", width = 15, height = 12, dpi = 400)

#### Figure 2 - Anisoptera Species Richness Regressions ####
zsdc <- ggplot(zygs, aes(x = mean.threeh, y = shannon))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE,col="black")+
  stat_cor(aes(label = paste(..rr.label..), group=1), label.y = 1.8, label.x = -0.3, size = 5)+
  stat_regline_equation(label.y = 1.9, label.x = -0.3, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20))+
  xlab("Mean Current") +
  ylab("Zygoptera Shannon Diversity")

zsdh <- ggplot(zygs, aes(x= n.threeh, y = shannon))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE,col="black")+
  stat_cor(aes(label = paste(..rr.label..)), label.y = 1.8, label.x = 16, size = 5)+
  stat_regline_equation(label.y = 1.9, label.x = 16, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20))+
  xlab("Number of Surrounding Habitats") +
  ylab("Zygoptera Shannon Diversity")

zsrc <- ggplot(zygs, aes(mean.threeh, speciescount))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE,col="black")+
  stat_cor(aes(label = paste(..rr.label..)), label.y = 10.5, label.x = -0.3, size = 5)+
  stat_regline_equation(label.y = 11, label.x = -0.3, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20))+
  xlab("Mean Current") +
  ylab("Zygoptera Species Richness")

zsrh <- ggplot(zygs, aes(n.threeh, speciescount))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE,col="black")+
  stat_cor(aes(label = paste(..rr.label..), group=1), label.y = 10.5, label.x = 16, size = 5)+
  stat_regline_equation(label.y = 11, label.x = 16, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20))+
  xlab("Number of Surrounding Habitats") +
  ylab("Zygoptera Species Richness")


ggarrange(zsdc, zsdh, zsrc, zsrh, ncol = 2, nrow = 2, labels = "auto", font.label = list(size = 20))

ggsave("graphics/ZygopteraRegression.jpg", width = 15, height = 12, dpi = 400)
