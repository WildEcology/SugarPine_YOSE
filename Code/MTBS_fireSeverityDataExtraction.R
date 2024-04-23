
## ---------------------------
##
## Script name: 
##
## Author: Dr. Joan Dudney
##
## Date Created: 2024-04-23
##
## Copyright (c) Joan Dudney, 2024
## Email: dudney@ucsb.edu
##
## ---------------------------
##
## Notes:Notes: This unzips the fire severity data from 
##   https://www.mtbs.gov/ and creates a stack
##   
##
## ---------------------------



#install.packages("Require")
if (!require("librarian")) install.packages("librarian")

## this code will load package or download packages that are not yet loaded 
librarian::shelf(tidyverse, curl, terra, tmap, sf, sp, tmaptools, rsconnect,spatstat.utils)

## ggplot theme
theme_set(
  theme_bw(base_size = 17)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

## first unzipping all the downloaded files

## create a file path for the folder containing all downloaded files
# outdir <- paste0("/Users/treelife/Documents/YOSE sugar pine project/Code and data/Analyses_sugarpine/Data/US fire history/Fire severity/")
# 
# ## list all the folders
# filelist <- list.files()
# 
# ## for loop that loops through each folder; sets that folder the working directory 
# ## so the unzip function knows where to place the unzipped files
# 
# for (file in filelist) {
#   setwd(paste0(outdir,"/", file))
#   newlist <- list.files()
#   unzip(newlist)
# }

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                     reading in fire severity tif data and creating a stack
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## creating a for loop that reads in each unzipped tif file as a raster stack

## this for loop lists all the files to read in
setwd("/Users/treelife/Documents/Sierra Projects/Yosemite Projects/YOSE sugar pine project/Code and data/Analyses_sugarpine/SugarPineProj/Data/US fire history/Fire severity")
wdir <- getwd()
folderlist <- list.files()[-c(39:44)]

newdataframe <-  list()

for (folder in folderlist){
  newpath = paste0(wdir, "/", folder)
  rastlist = data.frame(name = list.files(path = newpath, pattern='.tif$', all.files= T, full.names= T))
  newdataframe = rbind(newdataframe, rastlist)
}

files <- as.vector(newdataframe)


newdata <- newdataframe %>% 
  #filter(row_number()%in%c(27:30)) %>% 
  pull(name)

raster_list <- list()

extent <- ext(-2072370, -2000723, 1854591, 1933098)
plot(extent)


plot(rightpts11)
plot(pila.shp, add=T)
plot(extent)

for (i in seq(1:length(newdata))){
  file <- newdata[i]
  raster1 <- rast(file)
  croprast <- crop(raster1, extent)
  maskpila <- mask(croprast, projpila)
  resampila <- resample(maskpila, croprast)
  raster_list[i] <- resampila
}

## for some reason the 10th element has a different extent and not much data so removing it
raster_list_filter <- raster_list[-10]
rasterbrick <- terra::rast(raster_list_filter)
rasterbrick[[5]]

plot(raster_list_filter[[29]])
rasterbrick[1]



## Reading in Yosemite plots
setwd("/Users/treelife/Documents/Sierra Projects/Yosemite Projects/YOSE sugar pine project/Code and data/Analyses_sugarpine/SugarPineProj/")
plotutms <- read_csv("Data/PlotUTMsYOSE_SEKI.csv")
plotutms$newPlotID = 1:length(plotutms$plotID)

points10 <- plotutms %>% 
  select(zone, plot_beg_UTM_N, plot_beg_UTM_E) %>%
  filter(zone==10) %>% 
  rename(y = plot_beg_UTM_N , x = plot_beg_UTM_E) %>% 
  select(x,y) %>% 
  as.matrix

points11 <- plotutms %>% 
  select(zone, plot_beg_UTM_N, plot_beg_UTM_E) %>%
  filter(zone==11) %>% 
  rename(y = plot_beg_UTM_N , x = plot_beg_UTM_E) %>% 
  select(x,y) %>% 
  as.matrix

projpoints10 <- vect(points10, crs="+proj=utm +zone=10 +datum=NAD83  +units=m")
rightpts10 <- project(projpoints10, prjcrs)

projpoints11 <- vect(points11, crs="+proj=utm +zone=11 +datum=NAD83  +units=m")
rightpts11 <- project(projpoints11, prjcrs)


exts.poly <- do.call(rbind, list(rightpts10, rightpts11))

plotfire<- terra::extract(c(), exts.poly)


