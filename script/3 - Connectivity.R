# Connectivity 
# Author: Isabella Richmond 
# This script is for getting the average resistance and connectivity 
# for each of the study ponds based on their nearest neighbours

#### Load Packages ####
easypackages::packages("tidyverse", "data.table", "ggpubr")

#### Load Data ####
conn <- fread("input/cleaned/ConnectivityCleaned.csv")
nn <- fread("input/cleaned/NearestNeighbours.csv")
ani <- fread("input/cleaned/AnisopteraCleaned.csv")
zyg <- fread("input/cleaned/ZygopteraCleaned.csv")

#### Connectivity & Nearest Neighbours ####
# we need to extract the connectivity values for each of the nearest neighbours 
# melt the dataset so its in the same format as conn 
colA = paste("which", c(1:6), sep = ".")
colB = paste("dist", c(1:6), sep=".")
nn_melt <- data.table::melt(nn, measure = list(colA, colB), value.name = c("which", "dist"))
# rename columns so they match conn 
nn_melt <- nn_melt %>%
  rename(Pond1 = Pond) %>%
  rename(Pond2 = which) %>%
  rename(Neighbour = variable)
# connectivity dataset only lists pond relationships once 
# nearest neighbour dataset goes through each individually 
# so need to do join between Pond 1 and Pond 2 and then the reverse 
# relationship to find resistance values for each nearest neighbour 
conn_nn1 <- inner_join(nn_melt, conn, by = c("Pond1", "Pond2"))
conn_nn2 <- inner_join(nn_melt, conn, by = c("Pond1" = "Pond2", "Pond2" = "Pond1"))
# combine datasets 
conn_nn <- rbind(conn_nn1, conn_nn2)
# save dataset 
fwrite(conn_nn, "output/nearestneighbours/ConnectivityNearestNeigbour.csv")

#### Average Distance & Resistance ####
# calculate the average distance for the five closest neighbours of each pond 
# and calculate the average resistance
average <- conn_nn %>% 
  group_by(Pond1) %>%
  dplyr::summarise(meandist = mean(dist), sddist = sd(dist), meanres = mean(Resistance), sdres = sd(Resistance))
fwrite(average, "output/AverageResistance.csv")

average <- rename(average, Pond = Pond1)
# join odonate datasets with resistance and distance data
ani_full <- inner_join(ani, average, by = "Pond")
fwrite(ani_full, "output/AnisopteraResistance.csv")
zyg_full <- inner_join(zyg, average, by = "Pond")
fwrite(zyg_full, "output/ZygopteraResistance.csv")
