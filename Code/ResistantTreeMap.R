## ---------------------------
##
## Script name: Resistant sugar pine script
##
## Author: Dr. Joan Dudney
##
## Date Created: 2024-02-23
##
## Copyright (c) Joan Dudney, 2024
## Email: dudney@ucsb.edu
##
## ---------------------------



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Packages
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Install the librarian package if not yet installed using the Require package 
#install.packages("Require")
if (!require("librarian")) install.packages("librarian")
librarian::shelf(tidyverse, curl, terra, tmap, sf, sp, tmaptools, rsconnect, data.table)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## pila polygon
yose_pila <- "Data/YOSE_PILA_Dist/YOSE_SugarPine_Dist_20210324.shp"
pila.shp <- vect(yose_pila)
plot(pila.shp)

## creating a project CRS
prjcrs <- toString(crs(pila.shp))


## Reading in Yosemite plots
resistutms <- read_csv("Data/PossibleResistantSugarPine.csv") %>% 
  na.omit()

points <- resistutms[c(9,10)] %>% 
  rename(y = UTM_N , x = UTM_E) %>% 
  na.omit() %>% 
  as.matrix

projpoints <- vect(points, crs="+proj=utm +zone=11 +datum=NAD83  +units=m")

rightpts <- project(projpoints, prjcrs)



## converting to sf to be able to use tmap
newprojpts <- sf::st_as_sf(rightpts)
newprojpts$treeid = as.character(resistutms$TreeNum)

tmap_mode("view")

tm_shape(newprojpts)+
  tm_dots(col = "darkgreen", size = .05) 



plot(pila.shp)
plot(newprojpts, add=TRUE, col="blue", legend=T)


