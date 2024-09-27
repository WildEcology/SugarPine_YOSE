## ---------------------------
##
## Script name: Plot selection for sugar pine
##
## Author: Dr. Joan Dudney
##
## Date Created: 2023-06-05
##
## Copyright (c) Joan Dudney, 2023
## Email: dudney@ucsb.edu
##
## ---------------------------
##
## Notes: uses processed and raw spatial data 
##  to develop a stratified random sampling design
##  for yosemite sugar pine in the rim fire
## ---------------------------



## Install the librarian package if not yet installed using the Require package 

#install.packages("Require")
if (!require("librarian")) install.packages("librarian")

## this code will load package or download packages that are not yet loaded 
librarian::shelf(tidyverse, curl, terra, tmap, sf, sgsR, tmaptools,tmap, rsconnect, here)

## ggplot theme
theme_set(
  theme_bw(base_size = 17)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#
#                                   STEP 1                
#           Reading in spatial data, cleaning them, and reprojecting 
#           them so they all have the same CRS
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## note for reading in files; may need to change file path depending on organization
## on your computer; using the package here, which calls your main directory path

## yosemite area polygon
#yose_poly <- here("Data", "YOSE poly" ,"Yosemite_perim_PILA_1.shp")
yose_poly <- here("Data",  "Yosemite_perim_PILA_1.shp")
yose.shp <- vect(yose_poly)


## pila polygon
#yose_pila <- here("Data", "YOSE_PILA_Dist", "YOSE_SugarPine_Dist_20210324.shp")
yose_pila <- here("Data", "YOSE_SugarPine_Dist_20210324.shp")
pila.shp <- vect(yose_pila)
plot(pila.shp)

## creating a project CRS
prjcrs <- toString(crs(pila.shp))

## reading in roads and trails data
#roads_trails <- vect("Data/Trails and Roads/bufferToLines_YOSEtrailsRoads.shp")
#plot(roads_trails)
#proj_roadstrails <- project(roads_trails, prjcrs)
#plot(proj_roadstrails)

## read in using sf package for the sgsR package
#roads_sf <- st_read(here("Data" , "Trails and Roads", "Union_roads_trails_YOSE.shp"))
roads_sf <- st_read(here("Data" , "Union_roads_trails_YOSE.shp"))
proj_roads_sf <- st_transform(roads_sf, prjcrs)
crs(proj_roads_sf)

## reading in prism data that's loosely masked to pila distribution
#vpd <- here("Data" , "Prism", "PRISM_vpdmax/PRISM_vpdmax_30yr_normal_800mM4_annual_bil.bil")
vpd <- here("Data" , "PRISM_vpdmax" , "PRISM_vpdmax_30yr_normal_800mM4_annual_bil.bil")
file.exists(vpd)

vpd_prism <- rast(vpd)
#hist(vpd_prism)
#dataprism <- data.frame(vpd_prism)
#quantile(dataprism$PRISM_vpdmax_30yr_normal_800mM4_annual_bil,probs = c(.1, .3, .6, .9))

vpd_proj <- project(vpd_prism, prjcrs)
cropvpd <- crop(vpd_proj, yose.shp)

plot(cropvpd)
plot(pila.shp, add=TRUE, col="blue")

## calculating the prism strata
maskvpdpila <- mask(cropvpd, pila.shp)
data_maskvpd <- data.frame(maskvpdpila)
percentile <- ecdf(data_maskvpd$PRISM_vpdmax_30yr_normal_800mM4_annual_bil)
round(percentile(c(12, 14, 16, 18)),digits =2)
round(percentile(c(12.5, 14, 16, 18)),digits =2)

max(data_maskvpd$PRISM_vpdmax_30yr_normal_800mM4_annual_bil)
min(data_maskvpd$PRISM_vpdmax_30yr_normal_800mM4_annual_bil)
#(21-11.5)/4; every 2.3


## reading in buffer data that includes rivers, streams and lakes

#buffer_streams <- vect(here("Data", "Lakes and Streams", "buffer_lakesstreams_25m10m.shp"))
buffer_streams <- vect(here("Data",  "buffer_lakesstreams_25m10m.shp"))
buff_streamsrivers <- project(buffer_streams,prjcrs)
plot(buff_streamsrivers)


## reading in slope data and creating a new raster file with slope < 35°

## slope data
slope <- rast(here("Data", "YOSE DEM", "YoseSlope.tif"))
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



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#
#                                    STEP 2
#      Extracts climate data by species ranges
#      and buffered by streams and lakes
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## first masking pila dist by slope
mask_slope <- proj_slope %>% 
  mask(pila.shp)

plot(mask_slope)

## resampling vpd to match with proj_slope
resampvpd <- resample(cropvpd, proj_slope)
#plot(resampvpd)

## masking vpd with slope raster
mask_vpd <- mask(resampvpd,mask_slope)
plot(mask_vpd)

hist(mask_vpd$PRISM_vpdmax_30yr_normal_800mM4_annual_bil)

mask_all <- mask(mask_vpd, buff_streamsrivers, inverse=T)
plot(mask_all)
max(mask_all$PRISM_vpdmax_30yr_normal_800mM4_annual_bil)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                                         STEP 3
#                            
#                     Calculating VPD quantiles and creating a raster       
#                     Extracting the vpd values across the sugar pine distribution
#                     and calculating the quantiles of interest
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## extracting the prism vpd from species distribution
hist(mask_all)

prismvals <- as.data.frame(mask_all)
names(prismvals)[1]="vpd"


## reading in for high fire severity
firerast <- rast(here("Data","RimFireHighSeverity.tif"))
plot(firerast)

projectfire <- project(firerast, prjcrs)
resamplefire <- resample(projectfire, mask_all)

mask_fire <- mask(mask_all, resamplefire)
plot(mask_fire)
max(mask_fire$PRISM_vpdmax_30yr_normal_800mM4_annual_bil)

## vpd quantiles across sugar pine
# strata <- prismvals %>% 
#   summarize(value = sort(quantile(vpd, probs = c(.05, .3, .6, .95)))) %>% 
#   mutate(value=as.numeric(value)) %>% 
#   pull(value)


strata_all <- c(12, 14, 16, 18)
strata_fire <- c(12.7, 14, 16, 17.5)



## create the stratified raster
raster_strat <- strat_breaks(
  mraster = mask_all,
  breaks =  strata_all)

## raster for rim fire high severity
raster_strat_fire <- strat_breaks(
  mraster = mask_fire,
  breaks =  strata_fire)

#proj_rasterstrat <- project(raster_strat, prjcrs)
proj_rasterstrat_fire <- project(raster_strat_fire, prjcrs)
#plot(proj_rasterstrat_fire)
#plot(proj_roads_sf, add=T)

#writeRaster(raster_strat, "Data/StratifiedRasterPILA.tif")
#writeRaster(proj_rasterstrat, "Data/ProjectedStratifiedRasterPILA.tif")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#
#                                    STEP 4: PLOT SELECTION
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



## test it first
plots_pila <- sample_strat(sraster = proj_rasterstrat_fire, 
                           nSamp = 1,
                           plot = TRUE)

ext <- extract_strata(sraster = proj_rasterstrat, existing = plots_pila)

## game time -- it can take a while to run this ~ 4-6 hours
plots_pila_fire <- sample_strat(sraster = proj_rasterstrat_fire,# input mraster
                                allocation = "equal",
                                nSamp = 8, # number of desired samples
                                access = proj_roads_sf, # define access road network
                                mindist = 400, # minimum distance samples must be apart from one another
                                buff_inner = 10, # inner buffer - no samples within this distance from road
                                buff_outer = 1000, # outer buffer - no samples further than this distance from road
                                plot = TRUE) # plot



## creating a vector layer and projecting it
plots_pila_fire
plotspila_vect_fire <- vect(plots_pila_fire)
crs(plotspila_vect_fire) <- prjcrs
plot(plotspila_vect_fire)

ext_fire <- extract_strata(sraster = proj_rasterstrat_fire, existing = plots_pila_fire)
ext_pila_fire <- st_sf(ext_fire)

## projecting xy
st_crs(ext_pila_fire) <- prjcrs

## writing a kml file
#st_write(ext_pila_fire, "Data/plots_pila60plots.kml", driver = "kml", append=F)

## writing a shapefile
#st_write(ext_pila_fire, "highseverityplotsJune13_LAST.shp")

