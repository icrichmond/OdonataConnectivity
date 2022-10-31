# Lack of connectivity around stormwater ponds impacts Odonate abundance and species richness  

Manuscript in preparation for submission to Landscape Ecology  


- Authors 
  - [Isabella C. Richmond](https://github.com/icrichmond)
  - [Mary Ann C. Perron](https://www.researchgate.net/profile/Mary-Ann-Perron)
  - [Sean P. Boyle](https://www.sites.google.com/view/seanboylephd)
  - [Frances R. Pick](https://www.researchgate.net/profile/Frances-Pick)
  
This repository contains the code and data accompanying the paper "Lack of connectivity around stormwater ponds impacts Odonate abundance and species richness" (In Prep.). R scripts are organized in ```script/```, raw data can be found in ```input/```, figures used in the manuscript can be found in ```graphics/```, and model output tables can be found in ```output```.

Odonate dataset was originally archived in [Dataverse](https://dataverse.scholarsportal.info/dataset.xhtml?persistentId=doi:10.5683/SP2/JKRF0M)  
All code & data is archived on Zenodo [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5337110.svg)](https://zenodo.org/record/5337110)  
**NOTE:** we do not have permission to publicly share the original landcover dataset provided by the City of Ottawa. To access this dataset, you must request the data from the city's data officers (https://engage.ottawa.ca/openottawa/)

Connectivity analysis was done in [Circuitscape](https://circuitscape.org/) GUI.


Package dependencies include: ```dplyr```, ```tibble```, ```tidyr```, ```broom```, ```ggplot2```, ```data.table```, ```sf```, ```sp```, ```raster```, ```vegan```, ```ggpubr```.  

# Abstract  
*Context:* The successful dispersal of an animal depends on landscape connectivity. Urbanization poses risks to dispersal activities by creating hostile land cover types.  
*Objectives:* We investigated how connectivity of urban ponds, measured using circuit theory, and the quantity of adjacent breeding habitats impacted community metrics of Odonata (dragonflies and damselflies), an order of semi-aquatic insects that rely on active dispersal.  
*Methods:* We sampled 41 constructed stormwater ponds and 8 natural ponds of similar size in a large metropolitan area. The effect of connectivity and the quantity of available adjacent habitats was tested at different scales for dragonflies (900m) and damselflies (300m) as determined from a literature analysis, to account for differences in suborder dispersal capabilities.  
*Results:* Lower levels of connectivity and fewer surrounding habitats negatively impacted abundance, species richness, and composition of dragonflies (p-values < 0.01, R2 = 0.18-0.70). In particular, the dragonfly Leucorrhinia frigida had a significant positive relationship with connectivity. In contrast, connectivity and the number of surrounding habitats had no significant impact on damselflies apart from a negative relationship between connectivity and species richness (p-value = 0.02, R2 = 0.11). Overall, natural ponds had significantly higher levels of connectivity and surrounding habitats when compared to constructed stormwater ponds.  
*Conclusions:* Our results suggest that dragonflies are positively affected by increased connectivity in an urban landscape, with little benefit of connectivity to damselflies at the scale measured. We recommend intentional planning of urban stormwater pond networks, where individual ponds can act as stepping stones, incorporated with strategic inclusion of low resistance land cover types. 

