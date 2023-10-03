
## ---------------------------
##
## Script name: Downloading DEM (1/3 Arc Second) for 
## Yosemite and surrounding regions
##
## Author: Dr. Joan Dudney
##
## Date Created: 2023-05-24
##
## Copyright (c) Joan Dudney, 2023
## Email: dudney@ucsb.edu
##
## ---------------------------
##
## Notes: This code downloads DEM data from https://apps.nationalmap.gov for the 
## Yosemite and surrounding regions, then merges the tifs and 
## extracts slope and aspect
##   
##
## ---------------------------


## Install the librarian package if not yet installed using the Require package 

#install.packages("Require")
if (!require("librarian")) install.packages("librarian")

## this code will load package or download packages that are not yet loaded 
librarian::shelf(tidyverse, curl, terra, tmap, sf)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Downloading DEM data from USGS
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## create a folder where the DEM data will be downloaded to and set it as the working directory:
## e.g., setwd("Data/YOSE DEM")

## from the website https://apps.nationalmap.gov, these are the USGS 1/3 Arc Second DEM files that cover Yosemite and surrounding areas  

names_urls <- c("https://data.lpdaac.earthdatacloud.nasa.gov/lp-prod-protected/ASTGTM.003/ASTGTMV003_N36W120_dem.tif",
  "https://data.lpdaac.earthdatacloud.nasa.gov/lp-prod-protected/ASTGTM.003/ASTGTMV003_N39W121_dem.tif",
  "https://data.lpdaac.earthdatacloud.nasa.gov/lp-prod-protected/ASTGTM.003/ASTGTMV003_N38W121_dem.tif",
  "https://data.lpdaac.earthdatacloud.nasa.gov/lp-prod-protected/ASTGTM.003/ASTGTMV003_N38W120_dem.tif",
  "https://data.lpdaac.earthdatacloud.nasa.gov/lp-prod-protected/ASTGTM.003/ASTGTMV003_N37W120_dem.tif",
  "https://data.lpdaac.earthdatacloud.nasa.gov/lp-prod-protected/ASTGTM.003/ASTGTMV003_N39W120_dem.tif",
  "https://data.lpdaac.earthdatacloud.nasa.gov/lp-prod-protected/ASTGTM.003/ASTGTMV003_N36W118_dem.tif",
  "https://data.lpdaac.earthdatacloud.nasa.gov/lp-prod-protected/ASTGTM.003/ASTGTMV003_N37W119_dem.tif",
  "https://data.lpdaac.earthdatacloud.nasa.gov/lp-prod-protected/ASTGTM.003/ASTGTMV003_N36W119_dem.tif",
  "https://data.lpdaac.earthdatacloud.nasa.gov/lp-prod-protected/ASTGTM.003/ASTGTMV003_N38W119_dem.tif")


## this is a for loop that downloads each DEM tif into the target folder
for(i in seq(length(names_urls))){
  curl::curl_download(names_urls[i], paste0(i, "YoseDem.tif"))
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Reading in DEM files and merging them for slope and aspect calculations
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## creating a list of the files
file_list <- list.files()

## reading in and merging the files
filenames <- lapply(file_list, rast)
mergdat <- do.call(merge, filenames)

## visualizing the data to make sure everything worked
plot(mergdat)


## clipping the data by a rough perimeter of YOSE
## create your own or download park boundaries from IRMA 
## e.g., https://irma.nps.gov/DataStore/Reference/Profile/2224545?lnv=True

## change working directory to read in park boundaries
#setwd("~/Documents/YOSE sugar pine project/Code and data/Analyses")

yose_poly <- "Data/YOSE poly/Yosemite_perim_PILA.shp"
yose.shp <- st_read(yose_poly)
plot(yose.shp)

## making sure both files are in the same CRS
prj2<-toString(crs(mergdat))
yose_proj <- st_transform(yose.shp, prj2)


## clipping DEM data by the extent of the YOSE polygon
cropdim <- yose.shp

cropped_dem <- mergdat %>% 
  terra::crop(yose_proj)

plot(cropped_dem)

## exporting the merged DEM raster
#writeRaster(cropped_dem, "Data/YOSE DEM/YosePerimDEM.tif")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Using the merged DEM file to extract slope and aspect
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

slope <- terrain(cropped_dem, v="slope", unit="degrees") 
plot(slope)

#writeRaster(slope, "Data/YOSE DEM/YoseSlope.tif")

aspect <- terrain(cropped_dem, v="aspect", unit="degrees") 
plot(aspect)

#writeRaster(aspect, "Data/YOSE DEM/YoseAspect.tif")
