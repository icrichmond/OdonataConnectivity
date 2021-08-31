# Redundancy Analyses
# Author: Mary Ann C. Perron
# This script is for producing and analyzing redundancy analyses performed on community composition data

#### Load Packages ####
p <- c("dplyr", "vegan")
lapply(p, library, character.only=T)

#### Load Data ####
# odonates 
AdultZygoptera <- read.csv("input/cleaned/ZygopteraCleaned.csv")
AdultAnisoptera <- read.csv("input/cleaned/AnisopteraCleaned.csv")
# mean current density 
Current300 <- dplyr::select(AdultZygoptera, c(mean.three))
Current900 <- dplyr::select(AdultAnisoptera, c(mean.nine))
# number of surrounding habitats 
Habitats300 <- dplyr::select(AdultZygoptera, c(n.three))
Habitats900 <- dplyr::select(AdultAnisoptera, c(n.nine))

# remove extra columns from odonate datasets 
AdultZygoptera <- AdultZygoptera[,2:21]
AdultAnisoptera <- AdultAnisoptera[,2:33]


#### Redundancy Analyses (RDA) ####
## Damselflies ---------
# Mean Current Density @ 300 m
# transform species matrix with Hellinger transformation to downweight zeroes
spe.hel.zyg <- decostand(AdultZygoptera, method="hellinger")
# run RDA on mean current density at 300 m
spe.rda.zyg.current <- rda(spe.hel.zyg~., data=Current300)
summary(spe.rda.zyg.current, display=NULL) 
# check significance of mean current density at 300 m
ordiR2step(rda(spe.hel.zyg~1, data=Current300), scope= formula(spe.rda.zyg.current), direction= "forward", R2scope=TRUE, pstep=1000)
# get the R2 adjusted
(R2adj <- RsquareAdj(spe.rda.zyg.current)$adj.r.squared)
# test the significance of the axis and model
anova.cca(spe.rda.zyg.current, step=1000)
anova.cca(spe.rda.zyg.current, step=1000, by="axis")
 
# Number of Surrounding Habitats @ 300 m
# RDA
spe.rda.zyg.habs <- rda(spe.hel.zyg~., data=Habitats300)
summary(spe.rda.zyg.habs, display=NULL) 
# check significance of Habitat49 and Leucorrhinia proxima and L. frigida both had positive linear 300
ordiR2step(rda(spe.hel.zyg~1, data=Habitats300), scope= formula(spe.rda.zyg.habs), direction= "forward", R2scope=TRUE, pstep=1000)
# get the R2 adjusted
(R2adj <- RsquareAdj(spe.rda.zyg.habs)$adj.r.squared)
#test the significance of the axis and model
anova.cca(spe.rda.zyg.habs, step=1000)
anova.cca(spe.rda.zyg.habs, step=1000, by="axis")
                

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
# Damselflies / Mean Current Density @ 300 m
jpeg("graphics/RDA/ZygopteraMeanCurrent.jpg")
plot(spe.rda.zyg.current, scaling=2, main="Triplot RDA - scaling 2", type="none", xlab=c("RDA1"), ylab=c("RDA2"), xlim=c(-1,1), ylim=c(-1.5,1.75))
points(scores(spe.rda.zyg.current, display="sites", choices=c(1,2), scaling=2),
       pch=21, col="black", bg="steelblue", cex=1.2)
arrows(0,0,
       scores(spe.rda.zyg.current, display="species", choices=c(1), scaling=2)*2,
       scores(spe.rda.zyg.current, display="species", choices=c(2), scaling=2)*2,
       col="black",length=0)
text(scores(spe.rda.zyg.current, display="species", choices=c(1), scaling=2)*2.1,
     scores(spe.rda.zyg.current, display="species", choices=c(2), scaling=2)*2.1,
     labels=rownames(scores(spe.rda.zyg.current, display="species", scaling=2)),
     col="black", cex=0.8)    
arrows(0,0,
       scores(spe.rda.zyg.current, display="bp", choices=c(1), scaling=2),
       scores(spe.rda.zyg.current, display="bp", choices=c(2), scaling=2),
       col="red")
text(scores(spe.rda.zyg.current, display="bp", choices=c(1), scaling=2)+0.05,
     scores(spe.rda.zyg.current, display="bp", choices=c(2), scaling=2)+0.05,
     labels=rownames(scores(spe.rda.zyg.current, display="bp", choices=c(2), scaling=2)),
     col="red", cex=1)  
dev.off()
# Damselflies / Number of Surrounding Habitats @ 300 m
jpeg("graphics/RDA/ZygopteraSurroundingHabitats.jpg")
plot(spe.rda.zyg.habs, scaling=2, main="Triplot RDA - scaling 2", type="none", xlab=c("RDA1"), ylab=c("RDA2"), xlim=c(-1,1), ylim=c(-1.5,1.75))
points(scores(spe.rda.zyg.habs, display="sites", choices=c(1,2), scaling=2),
       pch=21, col="black", bg="steelblue", cex=1.2)
arrows(0,0,
       scores(spe.rda.zyg.habs, display="species", choices=c(1), scaling=2)*2,
       scores(spe.rda.zyg.habs, display="species", choices=c(2), scaling=2)*2,
       col="black",length=0)
text(scores(spe.rda.zyg.habs, display="species", choices=c(1), scaling=2)*2.1,
     scores(spe.rda.zyg.habs, display="species", choices=c(2), scaling=2)*2.1,
     labels=rownames(scores(spe.rda.zyg.habs, display="species", scaling=2)),
     col="black", cex=0.8)    
arrows(0,0,
       scores(spe.rda.zyg.habs, display="bp", choices=c(1), scaling=2),
       scores(spe.rda.zyg.habs, display="bp", choices=c(2), scaling=2),
       col="red")
text(scores(spe.rda.zyg.habs, display="bp", choices=c(1), scaling=2)+0.05,
     scores(spe.rda.zyg.habs, display="bp", choices=c(2), scaling=2)+0.05,
     labels=rownames(scores(spe.rda.zyg.habs, display="bp", choices=c(2), scaling=2)),
     col="red", cex=1)  
dev.off()
# Dragonflies / Mean Current Density @ 900 m
jpeg("graphics/RDA/AnisopteraMeanCurrent.jpg")
plot(spe.rda.ani.current, scaling=2, main="Triplot RDA - scaling 2", type="none", xlab=c("RDA1"), ylab=c("RDA2"), xlim=c(-1,1), ylim=c(-1.5,1.75))
points(scores(spe.rda.ani.current, display="sites", choices=c(1,2), scaling=2),
       pch=21, col="black", bg="steelblue", cex=1.2)
arrows(0,0,
       scores(spe.rda.ani.current, display="species", choices=c(1), scaling=2)*2,
       scores(spe.rda.ani.current, display="species", choices=c(2), scaling=2)*2,
       col="black",length=0)
text(scores(spe.rda.ani.current, display="species", choices=c(1), scaling=2)*2.1,
     scores(spe.rda.ani.current, display="species", choices=c(2), scaling=2)*2.1,
     labels=rownames(scores(spe.rda.ani.current, display="species", scaling=2)),
     col="black", cex=0.8)    
arrows(0,0,
       scores(spe.rda.ani.current, display="bp", choices=c(1), scaling=2),
       scores(spe.rda.ani.current, display="bp", choices=c(2), scaling=2),
       col="red")
text(scores(spe.rda.ani.current, display="bp", choices=c(1), scaling=2)+0.05,
     scores(spe.rda.ani.current, display="bp", choices=c(2), scaling=2)+0.05,
     labels=rownames(scores(spe.rda.ani.current, display="bp", choices=c(2), scaling=2)),
     col="red", cex=1)  
dev.off()
# Dragonflies / Number of Surrounding Habitats @ 900 m
jpeg("graphics/RDA/AnisopteraSurroundingHabitats.jpg")
plot(spe.rda.ani.habs, scaling=2, main="Triplot RDA - scaling 2", type="none", xlab=c("RDA1"), ylab=c("RDA2"), xlim=c(-1,1), ylim=c(-1.5,1.75))
points(scores(spe.rda.ani.habs, display="sites", choices=c(1,2), scaling=2),
       pch=21, col="black", bg="steelblue", cex=1.2)
arrows(0,0,
       scores(spe.rda.ani.habs, display="species", choices=c(1), scaling=2)*2,
       scores(spe.rda.ani.habs, display="species", choices=c(2), scaling=2)*2,
       col="black",length=0)
text(scores(spe.rda.ani.habs, display="species", choices=c(1), scaling=2)*2.1,
     scores(spe.rda.ani.habs, display="species", choices=c(2), scaling=2)*2.1,
     labels=rownames(scores(spe.rda.ani.habs, display="species", scaling=2)),
     col="black", cex=0.8)    
arrows(0,0,
       scores(spe.rda.ani.habs, display="bp", choices=c(1), scaling=2),
       scores(spe.rda.ani.habs, display="bp", choices=c(2), scaling=2),
       col="red")
text(scores(spe.rda.ani.habs, display="bp", choices=c(1), scaling=2)+0.05,
     scores(spe.rda.ani.habs, display="bp", choices=c(2), scaling=2)+0.05,
     labels=rownames(scores(spe.rda.ani.habs, display="bp", choices=c(2), scaling=2)),
     col="red", cex=1)  
dev.off()
