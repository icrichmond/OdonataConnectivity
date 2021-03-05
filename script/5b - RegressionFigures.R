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
  stat_cor(aes(x = mean.two,y = abundance,label = paste(..p.label..), group=1), label.y = 295, label.x = -1, size = 5)+
  stat_regline_equation(aes(x = mean.two,y = abundance), label.y = 310, label.x = -1, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20))+
  xlab("Mean Current") +
  ylab("Anisoptera Abundance")

asd <- ggplot(anis, aes(x= sd.two, y = abundance))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE,col="black")+
  stat_cor(aes(x = sd.two,y = abundance,label = paste(..p.label..)), label.y = 290, label.x = 0, size = 5)+
  stat_regline_equation(aes(x = sd.two, y = abundance), label.y = 310, label.x = 0, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20))+
  xlab("Standard Deviation Current") +
  ylab("Anisoptera Abundance")

ah <- ggplot(anis, aes(n.two, abundance))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE,col="black")+
  stat_cor(aes(n.two,abundance,label = paste(..p.label..)), label.y = 290, label.x = 0.45, size = 5)+
  stat_regline_equation(aes(n.two,abundance), label.y = 310, label.x = 0.45, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20))+
  xlab("Number of Surrounding Habitats") +
  ylab("Anisoptera Abundance")

as <- ggplot(anis, aes(mean.two, shannon))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE,col="black")+
  stat_cor(aes(mean.two,shannon,label = paste(..p.label..), group=1), label.y = 2, label.x = -0.35, size = 5)+
  stat_regline_equation(aes(mean.two,shannon), label.y = 2.1, label.x = -0.35, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20))+
  xlab("Mean Current") +
  ylab("Anisoptera Shannon Diversity")

srm <- ggplot(anis, aes(mean.two, speciescount))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE,col="black")+
  stat_cor(aes(mean.two,speciescount,label = paste(..p.label..), group=1), label.y = 12.5, label.x = -0.9, size = 5)+
  stat_regline_equation(aes(mean.two,speciescount), label.y = 13, label.x = -0.9, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20))+
  xlab("Mean Current") +
  ylab("Anisoptera Species Richness")

srh <- ggplot(anis, aes(n.two, speciescount))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE,col="black")+
  stat_cor(aes(n.two,speciescount,label = paste(..p.label..), group=1), label.y = 12.5, label.x = 0.4, size = 5)+
  stat_regline_equation(aes(n.two,speciescount), label.y = 13, label.x = 0.4, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20))+
  xlab("Number of Surrounding Habitats") +
  ylab("Anisoptera Species Richness")


ggarrange(am, ah, srm, srh, ncol = 2, nrow = 2, labels = "auto", font.label = list(size = 20))

ggsave("graphics/AnisopteraRegression.jpg", width = 15, height = 12, dpi = 400)

#### Figure 2 - Anisoptera Species Richness Regressions ####
zsm <- ggplot(zygs, aes(mean.threeh, shannon))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE,col="black")+
  stat_cor(aes(mean.threeh,shannon,label = paste(..p.label..), group=1), label.y = 1.85, label.x = -0.35, size = 5)+
  stat_regline_equation(aes(mean.threeh,shannon), label.y = 2, label.x = -0.35, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20))+
  xlab("Mean Current") +
  ylab("Zygoptera Shannon Diversity")

zsrm <- ggplot(zygs, aes(mean.threeh, speciescount))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE,col="black")+
  stat_cor(aes(mean.threeh,speciescount,label = paste(..p.label..), group=1), label.y = 11.5, label.x = -0.35, size = 5)+
  stat_regline_equation(aes(mean.threeh,speciescount), label.y = 12, label.x = -0.35, size = 5)+
  theme_classic() +
  theme(text = element_text(size = 20))+
  xlab("Mean Current") +
  ylab("Zygoptera Species Richness")

ggarrange(zsm, zsrm, ncol = 1, nrow = 2, labels = "auto", font.label = list(size = 20))

ggsave("graphics/ZygopteraRegression.jpg", width = 12, height = 10, dpi = 400)
