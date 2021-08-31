# Redundancy Analyses
# Author: Mary Ann C. Perron
# This script is for producing and analyzing redundancy analyses performed on community composition data

#### Load Packages ####
p <- c("vegan")
lapply(p, library, character.only=T)

#### Load Data ####
# odonates 

# mean current density 

# number of surrounding habitats 



# Damselflies
AdultZygoptera <- read.csv("D:/Users/maperron/Downloads/AdultZygoptera.csv", row.names=1)

Current300 <- read.csv("D:/Users/maperron/Downloads/Current300.csv", row.names=1)

Habitats300 <- read.csv("D:/Users/maperron/Downloads/Habitats300.csv", row.names=1)


#Dragonflies

AdultAnisoptera <- read.csv("D:/Users/maperron/Downloads/AdultAnisoptera.csv", row.names=1)

Current900 <- read.csv("D:/Users/maperron/Downloads/Current900.csv", row.names=1)

Habitats900 <- read.csv("D:/Users/maperron/Downloads/Habitats900.csv", row.names=1)

#### Redundancy Analyses (RDA) ####

## Damselflies ---------

#Tranform species matrix with Hellinger transformation

spe.hel <- decostand(AdultZygoptera, method="hellinger")

#Run RDA on Mean Current at 300m

spe.rda <- rda(spe.hel~., data=Current300)
summary(spe.rda, display=NULL) 

#Check significance of Current300

ordiR2step(rda(spe.hel~1, data=Current300), scope= formula(spe.rda), direction= "forward", R2scope=TRUE, pstep=1000)

#get the R2 adjusted
(R2adj <- RsquareAdj(spe.rda)$adj.r.squared)

#test the significance of the axis and model
anova.cca(spe.rda, step=1000)

anova.cca(spe.rda, step=1000, by="axis")
 





#Zygoptera with number of surrounding habitats

spe.rda1 <- rda(spe.hel~., data=Habitats300)
summary(spe.rda1, display=NULL) 


#Check significance of Habitat49)and Leucorrhinia proxima and L. frigida both had positiv e linear 300

ordiR2step(rda(spe.hel~1, data=Habitats300), scope= formula(spe.rda1), direction= "forward", R2scope=TRUE, pstep=1000)


#get the R2 adjusted
(R2adj <- RsquareAdj(spe.rda1)$adj.r.squared)


#test the significance of the axis and model
anova.cca(spe.rda1, step=1000)

anova.cca(spe.rda1, step=1000, by="axis")
                
  
  


## Dragonflies ------

#Tranform species matrix with Hellinger transformation

spe.hel <- decostand(AdultAnisoptera, method="hellinger")

#Run RDA on Mean Current at 900m

spe.rda <- rda(spe.hel~., data=Current900)
summary(spe.rda, display=NULL) 


#Check significance of Current 900

ordiR2step(rda(spe.hel~1, data=Current900), scope= formula(spe.rda), direction= "forward", R2scope=TRUE, pstep=1000)


#get the R2 adjusted
(R2adj <- RsquareAdj(spe.rda)$adj.r.squared)


#test the significance of the axis and model
anova.cca(spe.rda, step=1000)

anova.cca(spe.rda, step=1000, by="axis")
  





#Dragonflies with number of surrounding habitats

spe.rda1 <- rda(spe.hel~., data=Habitats900)
summary(spe.rda1, display=NULL) 


#Check significance of Habitats at 900

ordiR2step(rda(spe.hel~1, data=Habitats900), scope= formula(spe.rda1), direction= "forward", R2scope=TRUE, pstep=1000)


#get the R2 adjusted
(R2adj <- RsquareAdj(spe.rda1)$adj.r.squared)


#test the significance of the axis and model
anova.cca(spe.rda1, step=1000)
 
anova.cca(spe.rda1, step=1000, by="axis")
            





#### Plotting #### 
#windows()
plot(spe.rda, scaling=2, main="Triplot RDA - scaling 2", type="none", xlab=c("RDA1"), ylab=c("RDA2"), xlim=c(-1,1), ylim=c(-1.5,1.75))
points(scores(spe.rda, display="sites", choices=c(1,2), scaling=2),
       pch=21, col="black", bg="steelblue", cex=1.2)
arrows(0,0,
       scores(spe.rda, display="species", choices=c(1), scaling=2)*2,
       scores(spe.rda, display="species", choices=c(2), scaling=2)*2,
       col="black",length=0)
text(scores(spe.rda, display="species", choices=c(1), scaling=2)*2.1,
     scores(spe.rda, display="species", choices=c(2), scaling=2)*2.1,
     labels=rownames(scores(spe.rda, display="species", scaling=2)),
     col="black", cex=0.8)    
arrows(0,0,
       scores(spe.rda, display="bp", choices=c(1), scaling=2),
       scores(spe.rda, display="bp", choices=c(2), scaling=2),
       col="red")
text(scores(spe.rda, display="bp", choices=c(1), scaling=2)+0.05,
     scores(spe.rda, display="bp", choices=c(2), scaling=2)+0.05,
     labels=rownames(scores(spe.rda, display="bp", choices=c(2), scaling=2)),
     col="red", cex=1)  

#PLOT
#windows()
plot(spe.rda1, scaling=2, main="Triplot RDA - scaling 2", type="none", xlab=c("RDA1"), ylab=c("RDA2"), xlim=c(-1,1), ylim=c(-1.5,1.75))
points(scores(spe.rda1, display="sites", choices=c(1,2), scaling=2),
       pch=21, col="black", bg="steelblue", cex=1.2)
arrows(0,0,
       scores(spe.rda1, display="species", choices=c(1), scaling=2)*2,
       scores(spe.rda1, display="species", choices=c(2), scaling=2)*2,
       col="black",length=0)
text(scores(spe.rda1, display="species", choices=c(1), scaling=2)*2.1,
     scores(spe.rda1, display="species", choices=c(2), scaling=2)*2.1,
     labels=rownames(scores(spe.rda1, display="species", scaling=2)),
     col="black", cex=0.8)    
arrows(0,0,
       scores(spe.rda1, display="bp", choices=c(1), scaling=2),
       scores(spe.rda1, display="bp", choices=c(2), scaling=2),
       col="red")
text(scores(spe.rda1, display="bp", choices=c(1), scaling=2)+0.05,
     scores(spe.rda1, display="bp", choices=c(2), scaling=2)+0.05,
     labels=rownames(scores(spe.rda1, display="bp", choices=c(2), scaling=2)),
     col="red", cex=1)  



#PLOT
#windows()
plot(spe.rda, scaling=2, main="Triplot RDA - scaling 2", type="none", xlab=c("RDA1"), ylab=c("RDA2"), xlim=c(-1,1), ylim=c(-1.5,1.75))
points(scores(spe.rda, display="sites", choices=c(1,2), scaling=2),
       pch=21, col="black", bg="steelblue", cex=1.2)
arrows(0,0,
       scores(spe.rda, display="species", choices=c(1), scaling=2)*2,
       scores(spe.rda, display="species", choices=c(2), scaling=2)*2,
       col="black",length=0)
text(scores(spe.rda, display="species", choices=c(1), scaling=2)*2.1,
     scores(spe.rda, display="species", choices=c(2), scaling=2)*2.1,
     labels=rownames(scores(spe.rda, display="species", scaling=2)),
     col="black", cex=0.8)    
arrows(0,0,
       scores(spe.rda, display="bp", choices=c(1), scaling=2),
       scores(spe.rda, display="bp", choices=c(2), scaling=2),
       col="red")
text(scores(spe.rda, display="bp", choices=c(1), scaling=2)+0.05,
     scores(spe.rda, display="bp", choices=c(2), scaling=2)+0.05,
     labels=rownames(scores(spe.rda, display="bp", choices=c(2), scaling=2)),
     col="red", cex=1)  





#PLOT
#windows()
plot(spe.rda1, scaling=2, main="Triplot RDA - scaling 2", type="none", xlab=c("RDA1"), ylab=c("RDA2"), xlim=c(-1,1), ylim=c(-1.5,1.75))
points(scores(spe.rda1, display="sites", choices=c(1,2), scaling=2),
       pch=21, col="black", bg="steelblue", cex=1.2)
arrows(0,0,
       scores(spe.rda1, display="species", choices=c(1), scaling=2)*2,
       scores(spe.rda1, display="species", choices=c(2), scaling=2)*2,
       col="black",length=0)
text(scores(spe.rda1, display="species", choices=c(1), scaling=2)*2.1,
     scores(spe.rda1, display="species", choices=c(2), scaling=2)*2.1,
     labels=rownames(scores(spe.rda1, display="species", scaling=2)),
     col="black", cex=0.8)    
arrows(0,0,
       scores(spe.rda1, display="bp", choices=c(1), scaling=2),
       scores(spe.rda1, display="bp", choices=c(2), scaling=2),
       col="red")
text(scores(spe.rda1, display="bp", choices=c(1), scaling=2)+0.05,
     scores(spe.rda1, display="bp", choices=c(2), scaling=2)+0.05,
     labels=rownames(scores(spe.rda1, display="bp", choices=c(2), scaling=2)),
     col="red", cex=1)  