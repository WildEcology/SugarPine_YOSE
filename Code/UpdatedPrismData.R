## ---------------------------
##
## Script name: Extracting prism data
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
plotutms <- read_csv("Data/Plot_UTMsno0.csv") %>% 
  na.omit()
plotutms$newPlotID = 1:length(plotutms$plotID)


points10 <- plotutms %>% 
  select(UTM_zone, plot_beg_UTM_N, plot_beg_UTM_E) %>%
  filter(UTM_zone==10) %>% 
  rename(y = plot_beg_UTM_N , x = plot_beg_UTM_E) %>% 
  select(x,y) %>% 
  as.matrix

points11 <- plotutms %>% 
  select(UTM_zone, plot_beg_UTM_N, plot_beg_UTM_E) %>%
  filter(UTM_zone==11) %>% 
  rename(y = plot_beg_UTM_N , x = plot_beg_UTM_E) %>% 
  select(x,y) %>% 
  as.matrix

projpoints10 <- vect(points10, crs="+proj=utm +zone=10 +datum=NAD83  +units=m")
rightpts10 <- project(projpoints10, prjcrs)

projpoints11 <- vect(points11, crs="+proj=utm +zone=11 +datum=NAD83  +units=m")
rightpts11 <- project(projpoints11, prjcrs)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Plotting data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Visualize to make sure they look right
## plot 43,50 and 32 are wrong
## why is there a zero plot ID?

## converting to sf to be able to use tmap
newprojpts <- sf::st_as_sf(rightpts11)
newprojpts10 <- sf::st_as_sf(rightpts10)
#newprojpts$plotid = as.character(plotutms$plotID)

tmap_mode("view")

tm_shape(newprojpts)+
  tm_dots()+
  tm_layout(legend.outside = T)+
  tm_shape(newprojpts10, col="blue")+
  tm_dots(col="blue")


plot(newprojpts, col="blue")
plot(newprojpts10, add=T, col="green")
plot(pila.shp, add=T)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Extracting prism data and writing to csv
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
exts.poly <- do.call(rbind, list(rightpts10, rightpts11))
#writeVector(exts.poly, "Data/projectedPILAplots.shp")

## extracting prism data for each plot
plotprism <- terra::extract(c(vpd_prism_proj,temp_prism_proj,precip_prism_proj), exts.poly)
colnames(plotprism) <- c("seqPlotID", 'vpdmax','tmean','ppt') 
plotprism$PlotID = plotutms$plotID


write_csv(plotprism, "Data/updatedPRISMdata.csv")
hist(plotprism$tmean)
