## ---------------------------
##
## Script name: RIM fire plot selection
##
## Author: Dr. Joan Dudney
##
## Date Created: 2023-06-08
##
## Copyright (c) Joan Dudney, 2023
## Email: dudney@ucsb.edu
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------
yose_pila <- "/Users/treelife/Documents/YOSE sugar pine project/Code and data/Analyses_sugarpine/Data/YOSE_PILA_Dist/YOSE_SugarPine_Dist_20210324.shp"
pila.shp <- vect(yose_pila)
plot(pila.shp)

## creating a project CRS
prjcrs <- toString(crs(pila.shp))

## read in using sf package for the sgsR package
roads_sf <- st_read("Data/Trails and Roads/Union_roads_trails_YOSE.shp")
proj_roads_sf <- st_transform(roads_sf, prjcrs)

## reading in stream buffers
buffer_streams <- vect("Data/Lakes and Streams/buffer_lakesstreams_25m10m.shp")
buff_streamsrivers <- project(buffer_streams,prjcrs)

## slope data
slope <- rast("Data/YOSE DEM/YoseSlope.tif")
plot(slope)

## clipping by 35°
slope_rast <- app(slope, fun=function(x) { 
  x[x > 35] <- NA; return(x)} )
plot(slope_rast)

## reproject the file
proj_slope <- project(slope_rast, prjcrs)

## checking results
m.slope <- as.matrix(slope_rast)
max(m.slope, na.rm=T)
hist(m.slope)


## RIM fire raster
piladat <- rast("RimFireVPDmax.tif")
plot(piladat)
hist(piladat)

resampslope <- resample(proj_slope, piladat)


## first masking pila dist by slope
mask_slope <- piladat %>% 
  mask(resampslope) %>% 
  mask(buff_streamsrivers, inverse=T)

plot(mask_slope)
hist(mask_slope)

prismvals_rim <- as.data.frame(mask_slope)
names(prismvals_rim)[1]="vpd"

strataRIM <- prismvals_rim %>% 
  summarize(value = sort(quantile(vpd, probs = c(.1, .3, .6, .9)))) %>% 
  mutate(value=as.numeric(value)) %>% 
  pull(value)

## Raster from the previous analysis
alldat <- rast("Data/sugarpineVPDslopePrepped.tif")
hist(alldat)


prismvals <- as.data.frame(alldat)
names(prismvals)[1]="vpd"

## vpd quantiles across sugar pine
strata <- prismvals %>% 
  summarize(value = sort(quantile(vpd, probs = c(.1, .3, .6, .98)))) %>% 
  mutate(value=as.numeric(value)) %>% 
  pull(value)

## RIM FIRE
## 13.76829 14.13113 15.01707 16.09991

## ALL PILA
## 13.31643 13.90855 14.70765 16.31113


## create the stratified raster
raster_strat <- strat_breaks(
  mraster = mask_slope,
  breaks =  strata)

plot(raster_strat)

plots_rim <- sample_strat(sraster = raster_strat,# input mraster
                           allocation = "equal",
                           nSamp = 3, # number of desired samples
                           access = proj_roads_sf, # define access road network
                           mindist = 400, # minimum distance samples must be apart from one another
                           buff_inner = 10, # inner buffer - no samples within this distance from road
                           buff_outer = 1000, # outer buffer - no samples further than this distance from road
                           plot = TRUE) # plot


## creating a vector layer and projecting it
plotspila_rim <- vect(plots_rim)
crs(plotspila_rim) <- prjcrs

ext_rim <- extract_strata(sraster = raster_strat, existing = plots_rim)
ext_pila_rim <- st_sf(ext_rim)

## projecting xy
st_crs(ext_pila_rim) <- prjcrs

## writing a kml file
st_write(ext_pila_rim, "Data/plots_pilaRIM.kml", driver = "kml", append=F)

## writing a shapefile
st_write(ext_pila_rim, "Data/pilaplotsRIMJune9.shp")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#
#                                    Investigation
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## looking at the map

highsev <- vect("Data/pilaplotsRIMJune9.shp")
plot(highsev, col = c("blue", "purple", "green", "yellow", "orange"),
     lwd=3 , pch=19 , type="b")

legend(x = "top",          # Position
       legend = c("strata 1", "strata 2", "strata 3", "strata 4", "strata 5"),  # Legend texts
       lty = c(1, 2),           # Line types
       col = c("blue", "purple", "green", "yellow", "orange"),           # Line colors
       lwd = 2)    



## extracting VPD
vpd_rim <- extract(cropvpd, highsev)
highsevplots <- as.data.frame(highsev)

highsevplots$ID <- vpd_rim$ID

cords_highsev <- data.frame(crds(highsev))
cords_highsev$ID <- vpd_rim$ID
cords_highsev$vpd <- vpd_rim$PRISM_vpdmax_30yr_normal_800mM4_annual_bil
cords_highsev$strata <- highsevplots$strata
cords_highsev$crs <- prjcrs
