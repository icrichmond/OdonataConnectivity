# Redundancy Analyses
# Author: Mary Ann C. Perron
# This script is for producing and analyzing redundancy analyses performed on community composition data

#### Load Packages ####
# ggvegan not available on CRAN yet 
#install.packages("devtools")
#devtools::install_github("gavinsimpson/ggvegan")
p <- c("dplyr", "vegan", "ggvegan", "patchwork")
lapply(p, library, character.only=T)

#### Load Data ####
# odonates 
AdultAnisoptera <- read.csv("input/cleaned/AnisopteraCleaned.csv")
# mean current density 
Current900 <- dplyr::select(AdultAnisoptera, c(mean.nine))
# number of surrounding habitats 
Habitats900 <- dplyr::select(AdultAnisoptera, c(n.nine))
# remove extra columns from odonate datasets 
AdultAnisoptera <- AdultAnisoptera[,2:33]


## Dragonflies ---------
# Mean Current Density @ 900 m
# transform species matrix with Hellinger transformation to downweight zeroes
spe.hel.ani <- decostand(AdultAnisoptera, method="hellinger")
# run RDA on mean current density at 900 m
spe.rda.ani.current <- rda(spe.hel.ani~., data=Current900)
summary(spe.rda.ani.current, display=NULL) 
# check significance of mean current density at 900 m
ordiR2step(rda(spe.hel.ani~1, data=Current900), scope= formula(spe.rda.ani.current), direction= "forward", R2scope=TRUE, pstep=1000)
# get the R2 adjusted
(R2adj <- RsquareAdj(spe.rda.ani.current)$adj.r.squared)
# test the significance of the axis and model
anova.cca(spe.rda.ani.current, step=1000)
anova.cca(spe.rda.ani.current, step=1000, by="axis")

# Number of Surrounding Habitats @ 900 m
spe.rda.ani.habs <- rda(spe.hel.ani~., data=Habitats900)
summary(spe.rda.ani.habs, display=NULL) 
# check significance of surrounding habitats at 900 m
ordiR2step(rda(spe.hel.ani~1, data=Habitats900), scope= formula(spe.rda.ani.habs), direction= "forward", R2scope=TRUE, pstep=1000)
# get the R2 adjusted
(R2adj <- RsquareAdj(spe.rda.ani.habs)$adj.r.squared)
# test the significance of the axis and model
anova.cca(spe.rda.ani.habs, step=1000)
anova.cca(spe.rda.ani.habs, step=1000, by="axis")


#### Plotting ####
current_fort <- fortify(spe.rda.ani.current)
take <- c('RDA1', 'PC1')  # which columns contain the scores we want
arrows <- subset(current_fort, Score == 'biplot' | Score == 'species')  # take biplot and species arrow scores
## multiplier for arrows to scale them to the plot range
mul <- ggvegan:::arrowMul(arrows[, take],
                          subset(current_fort, select = take, Score == 'sites'))
arrows[, take] <- arrows[, take] * mul  # scale arrows

ggplot() +
  geom_point(data = subset(current_fort, Score == 'sites'),
             mapping = aes(x = RDA1, y = PC1)) + 
  geom_segment(data = arrows,
               mapping = aes(x = 0, y = 0, xend = RDA1, yend = PC1),
               arrow = arrow(length = unit(0.01, "npc"))) +
  geom_text(data = arrows,
            mapping = aes(label = Label, x = RDA1, y = PC1, hjust = 0.5*(1 - sign(RDA1)), vjust = 0.5*(1-sign(PC1)))) +
  coord_fixed() +
  scale_x_continuous(expand = c(.1, .1)) +
  scale_y_continuous(expand = c(.1, .1)) + 
  theme_classic()




currentplot <- autoplot(spe.rda.ani.current, colour = ("grey"), const = 2)+
  theme_classic()+
  theme(legend.position = "none")


habplot <- autoplot(spe.rda.ani.habs, const = 2)+
  theme_classic()+
  theme(legend.position = "none")


currentplot + habplot
