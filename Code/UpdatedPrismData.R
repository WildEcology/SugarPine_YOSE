## ---------------------------
##
## Script name: 
##
## Author: Dr. Joan Dudney
##
## Date Created: 2024-02-20
##
## Copyright (c) Joan Dudney, 2024
## Email: dudney@ucsb.edu
##
## ---------------------------
##
## Notes: code extracts prism vpdmax, tmean, and ppt 
## values for sugar pine plots
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

## prism data files
vpd <- "Data/Prism/PRISM_vpdmax/PRISM_vpdmax_30yr_normal_800mM4_annual_bil.bil"
temp <-"Data/Prism/prism_tmean/PRISM_tmean_30yr_normal_800mM3_annual_bil.bil"
precip <- "Data/Prism/prism_ppt/PRISM_ppt_30yr_normal_800mM3_annual_bil.bil"

## reading in prism data and reprojecting to proj crs
vpd_prism <- rast(vpd)
hist(vpd_prism)
vpd_prism_proj <- project(vpd_prism, prjcrs)

temp_prism <- rast(temp)
hist(temp_prism)
temp_prism_proj <- project(temp_prism, prjcrs)

precip_prism <- rast(precip)
hist(precip_prism)
precip_prism_proj <- project(precip_prism, prjcrs)

## Reading in Yosemite plots
plotutms <- read_csv("Data/PlotUTMs.csv")

points <- plotutms[c(2,3)] %>% 
  rename(y = plot_beg_UTM_N , x = plot_beg_UTM_E) %>% 
  as.matrix

projpoints <- vect(points, crs="+proj=utm +zone=11 +datum=NAD83  +units=m")

rightpts <- project(projpoints, prjcrs)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Plotting data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Visualize to make sure they look right
## plot 43,50 and 32 are wrong
## why is there a zero plot ID?

## converting to sf to be able to use tmap
newprojpts <- sf::st_as_sf(rightpts)
newprojpts$plotid = as.character(plotutms$plotID)

tmap_mode("view")

tm_shape(newprojpts)+
  tm_dots("plotid")+
  tm_layout(legend.outside = T)

plot(pila.shp)
plot(newprojpts, add=TRUE, col="blue")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Extracting prism data and writing to csv
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## extracting prism data for each plot
plotprism <- terra::extract(c(vpd_prism_proj,temp_prism_proj,precip_prism_proj), rightpts)
colnames(plotprism) <- c("PlotID", 'vpdmax','tmean','ppt') 

write_csv(plotprism, "Data/updatedPRISMdata.csv")

