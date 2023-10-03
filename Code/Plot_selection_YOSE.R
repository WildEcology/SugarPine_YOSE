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
##  for yosemite sugar pine
## ---------------------------



## Install the librarian package if not yet installed using the Require package 

#install.packages("Require")
if (!require("librarian")) install.packages("librarian")

## this code will load package or download packages that are not yet loaded 
librarian::shelf(tidyverse, curl, terra, tmap, sf, sgsR, tmaptools,tmap, rsconnect)

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

## yosemite area polygon
yose_poly <- "Data/YOSE poly/Yosemite_perim_PILA_1.shp"
yose.shp <- vect(yose_poly)


## pila polygon
yose_pila <- "/Users/treelife/Documents/YOSE sugar pine project/Code and data/Analyses_sugarpine/Data/YOSE_PILA_Dist/YOSE_SugarPine_Dist_20210324.shp"
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
roads_sf <- st_read("Data/Trails and Roads/Union_roads_trails_YOSE.shp")
proj_roads_sf <- st_transform(roads_sf, prjcrs)
crs(proj_roads_sf)

## reading in prism data that's loosely masked to pila distribution
vpd <- "Data/Prism/PRISM_vpdmax/PRISM_vpdmax_30yr_normal_800mM4_annual_bil.bil"
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

buffer_streams <- vect("Data/Lakes and Streams/buffer_lakesstreams_25m10m.shp")
#buffer_streams <- vect("Data/Lakes and Streams/buffer_lakesstreams_2m1m.shp")
buff_streamsrivers <- project(buffer_streams,prjcrs)
plot(buff_streamsrivers)


## reading in slope data and creating a new raster file with slope < 35°

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
firerast <- rast("RimFireHighSeverity.tif")
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
#proj_rasterstrat_fire <- project(raster_strat_fire, prjcrs)
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

## game time -- it takes a while to run this ~ 4-6 hours
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



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#
#                                   Buffering by roads hopefully makes it faster
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                         FIRST STRATIFIED RANDOM SAMPLE ACROSS SUGAR PINE RANGE
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##road shapefile
roads_trails_new <- vect("Data/Trails and Roads/bufferToLines_YOSEtrailsRoads.shp")
plot(roads_trails_new)
proj_roadstrails_new <- project(roads_trails_new, prjcrs)
plot(proj_roadstrails_new)

##buffer roads
buffer_10m <- terra::buffer(proj_roadstrails_new, 10)
plot(buffer_10m)

buffer_1000 <- terra::buffer(proj_roadstrails_new, 1500)

## first masking by 10; then by 800
mask_10 <-mask(mask_all, buffer_10m, inverse=T)
plot(mask_10)
mask_1000 <- mask(mask_10, buffer_1000)
plot(mask_1000)
plot(buffer_1000, add=T)

maskalldat <- data.frame(mask_all) %>% 
  rename(vpd = PRISM_vpdmax_30yr_normal_800mM4_annual_bil)

percentile <- ecdf(maskalldat$vpd)
round(percentile(c(12.5, 14, 16, 18)),digits =2)

strata_all <- c(12.5, 14, 16, 18)
#strata_fire <- c(12.9, 14, 16, 17.5)

## create the stratified raster
raster_strat_buff <- strat_breaks(
  mraster = mask_1000,
  breaks =  strata_all)

plot(raster_strat_buff)

# ## raster for rim fire high severity
# raster_strat_fire <- strat_breaks(
#   mraster = mask_fire,
#   breaks =  strata_fire)

##calculating strata
plots_finalsample <- sample_strat(sraster = raster_strat_buff,# input mraster
                                allocation = "equal",
                                nSamp = 15, # number of desired samples
                                mindist = 900, # minimum distance samples must be apart from one another
                                plot = TRUE) 




plots_finalsample_vect <- vect(plots_finalsample)
crs(plots_finalsample_vect) <- prjcrs
plot(plots_finalsample_vect)

sample_vect_ext <- extract_strata(sraster = raster_strat_buff, existing = plots_finalsample)
pila_samp <- st_sf(sample_vect_ext)
pila_samp$plot = "random"
data_random <- vect(pila_samp)

data_random_coords = terra::crds(data_random) %>% 
  as.data.frame() %>% 
  mutate(ID = 1:length(x))

allrandom <- data.frame(data_random) %>% 
  mutate(ID = 1:length(strata)) %>%
  left_join(data_random_coords)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#       STRATIFIED RANDOM SAMPLE OF RIM FIRE
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## first masking by 10; then by 800
plot(mask_fire)
mask_10_fire <-mask(mask_fire, buffer_10m, inverse=T)
plot(mask_10_fire)
mask_1000_fire <- mask(mask_10_fire, buffer_1000)
plot(mask_1000_fire)
plot(buffer_1000, add=T)

#strata_fire <- c(12.5, 14, 16, 17.3)


## create the stratified raster
strat_buff_fire <- strat_breaks(
  mraster = mask_fire,
  breaks =  strata_fire)


plots_firesample <- sample_strat(sraster = strat_buff_fire,# input mraster
                                  allocation = "equal",
                                  nSamp = 10, # number of desired samples
                                  mindist = 1000, # minimum distance samples must be apart from one another
                                  plot = TRUE) 


firesample_vect <- vect(plots_firesample)
crs(firesample_vect) <- prjcrs
plot(cropvpd)
plot(firesample_vect)
plot(resamplefire, add=T,col="blue")

fire_vect_ext <- extract_strata(sraster = strat_buff_fire, existing = plots_firesample)
fire_samp <- st_sf(fire_vect_ext)


## high severity plots creating a dataframe
fire_samp$plot = "highseverity"
data_fire <- vect(fire_samp)

data_firecoords <- terra::crds(data_fire) %>% 
  as.data.frame() %>% 
  mutate(ID = 1:length(x))

alldatafire <- data.frame(data_fire) %>% 
  mutate(ID = 1:length(strata)) %>% 
  left_join(data_firecoords)


## running multiple samples to get more high and low severity plots
highsevplots_1 <- alldatafire %>% 
  filter(!strata%in%c(2:4))

highsevplots_2 <- alldatafire %>% 
  filter(!strata%in%c(2:4)) %>% 
  filter(ID != 1)

highsevplots_3 <- alldatafire %>% 
  filter(!strata%in%c(2:4))

highsevplots_4 <- alldatafire %>% 
  filter(!strata%in%c(1:4))

highsevplots_5 <- alldatafire %>% 
  filter(!strata%in%c(1:4))

fullhigh <- highsevplots_1 %>% 
  full_join(highsevplots_2) %>% 
  full_join(highsevplots_3) %>%
  full_join(highsevplots_4) %>% 
  full_join(highsevplots_5) %>% 
  filter(!duplicated(x)) %>% 
  select(-ID) 
  #mutate(crs = "NAD83 UTM11")

## combining random and high severity plots

randomAndhighsev <- alldatafire %>% 
  full_join(allrandom) %>% 
  select(-ID) 
  #mutate(crs = "NAD83 UTM11")

additionalplots <- randomAndhighsev %>% 
  full_join(fullhigh) %>% 
  filter(!duplicated(x))

#write_csv(randomAndhighsev, "Data/HIGHSEV_RANDOMFINALJune13.csv")
#write_csv(additionalplots, "Data/HIGHSEV_RANDOMFINALJune13MORE.csv")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# EXTRACTING VPD AND FIRE DATA
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## converting the dataframe to a vector
allplots_fire <- vect(alldatafire, geom=c("x", "y"), crs = prjcrs)
#allplots <- vect(additionalplots, geom=c("x", "y"), crs = prjcrs)
## extracting vpd data
plotext_fire <- terra::extract(mask_all, allplots_fire)

## getting correct coords
allcoords_fire <- terra::crds(allplots_fire) %>% 
  as.data.frame() %>% 
  mutate(ID = 1:length(x))

plotframe_fire <- data.frame(allplots_fire) %>% 
  mutate(ID = 1:length(strata)) %>% 
  left_join(plotext_fire) %>% 
  rename(vpd = PRISM_vpdmax_30yr_normal_800mM4_annual_bil) %>% 
  left_join(allcoords_fire)
  
## extracting from fire severity
## no longer in NAD83 but changed to fire severity CRS
project_allplots <- project(allplots_fire, proj1)
plotfire <- terra::extract(rasterbrick, project_allplots)

## cleaning the fire data
plotfire[is.na(plotfire)] <- 0

newplotfire <- plotfire %>%
  pivot_longer(-ID) %>% 
  mutate(value = replace(value, value == 6, 0)) %>% 
  mutate(value= as.numeric(value)) %>% 
  group_by(ID) %>% 
  slice(which.max(value))

firerimfire <- plotframe_fire %>% 
  left_join(newplotfire) %>% 
  mutate(strata_2 = ifelse(vpd<12.5, 1,
                 ifelse(vpd>=12.5 & vpd < 14,2,
                       ifelse(vpd >=14& vpd<16, 3,
                             ifelse(vpd>=16&vpd<18, 4, 5))))) %>% 
  group_by(strata_2) %>% 
  slice_sample(n = 8)

#write_csv(firerimfire, "SlicedRimFireHighSeverity2-4strata.csv")
#write_csv(fireANDallplots, "FinalPlotSelection_June13_highsevRandom.csv")
#write_csv(fireANDallplots, "FinalPlotSelection_June13_highsevRandom_ADDITIONAL.csv")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# VISUALIZING DATA
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  

fireANDallplots %>% 
  ggplot(aes(x=vpd, y=strata, color=strata))+
  geom_point()+
  facet_grid(strata~plot)


## now making sure we sample the hottest and coolest plots for high severity and random

# used original file "FinalPlotSelection_June13_highsevRandom.csv"

# randomplots_keep <- filter(fireANDallplots, plot=="random") %>% 
#   mutate(keep = ifelse(vpd > 18.5, "keep",
#                        ifelse(vpd < 12.4, "keep", "sample"))) %>% 
#   filter(keep == "keep")
# 
# randomplots_sample_med <- filter(fireANDallplots, plot=="random") %>% 
#   mutate(keep = ifelse(vpd > 18.5, "keep",
#                        ifelse(vpd < 12.4, "keep", "sample"))) %>% 
#   filter(keep != "keep") %>% 
#   filter(strata%in%c(2:4)) %>% 
#   group_by(strata) %>% slice_sample(n=8)
#   
# randomplots_highlow <- filter(fireANDallplots, plot=="random") %>% 
#   mutate(keep = ifelse(vpd > 18.5, "keep",
#                        ifelse(vpd < 12.4, "keep", "sample"))) %>% 
#   filter(keep != "keep") %>% 
#   filter(!strata%in%c(2:4)) %>% 
#   group_by(strata) %>% slice_sample(n=3)
# 
# 
# fulljoinsdata <- randomplots_highlow %>% 
#   full_join(randomplots_keep) %>% 
#   full_join(randomplots_sample_med) %>% 
#   select(-keep)

## random plot data
randomplots_final <- fulljoinsdata %>% 
  select(-crs)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# BURNED PLOTS
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## now doing the same thing but for high severity plots
## putting fire plots in true strata
fireplots_keep <- filter(fireANDallplots, plot=="highseverity") %>% 
  mutate(strata_2 = ifelse(vpd<12.5, 1,
                           ifelse(vpd>=12.5 & vpd < 14,2,
                                  ifelse(vpd >=14& vpd<16, 3,
                                         ifelse(vpd>=16&vpd<18, 4, 5))))) %>% 
  filter(strata_2!=5) %>% 
  group_by(strata_2) %>% 
  slice_sample(n = 6)

medsevfire <- fireANDallplots %>%
  filter(plot=="highseverity") %>% 
  mutate(strata_2 = ifelse(vpd<12.5, 1,
                           ifelse(vpd>=12.5 & vpd < 14,2,
                                  ifelse(vpd >=14& vpd<16, 3,
                                         ifelse(vpd>=16&vpd<18, 4, 5))))) %>% 
  filter(strata_2==5) %>% 
  full_join(fireplots_keep) %>% 
  select(-strata, -strata.1) %>% 
  rename(strata = strata_2)
  


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#
#                                    COMBINING MOST TOGETHER
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fulljoinplots <- randomplots_final %>% 
  select(-strata.1) %>% 
  full_join(medsevfire) %>% 
  select(-crs)

#write_csv(fulljoinplots, "RandomlyselectedFireANDrandomplotsJune13.csv")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#
#                                    ADDITIONAL FIRE DATA
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## reading in fire severity data between 2009-2015
highsevpila <- rast("Data/highseverity3.4.PILA2009,2010,2014.tif")
plot(highsevpila)


projecthighsevpila <- project(highsevpila, prjcrs)
resmplevpd <- resample(cropvpd, projecthighsevpila)
maskhighsevpila <- mask(resmplevpd, projecthighsevpila)
plot(maskhighsevpila)
hist(maskhighsevpila$PRISM_vpdmax_30yr_normal_800mM4_annual_bil)

strata_all <- c(12.6, 14, 16, 17)
#strata_fire <- c(12.9, 14, 16, 17.5)

## create the stratified raster
raster_strat_buff <- strat_breaks(
  mraster = maskhighsevpila,
  breaks =  strata_all)

plot(raster_strat_buff)


##calculating strata
plots_smallsamp <- sample_strat(sraster = raster_strat_buff,# input mraster
                                  allocation = "equal",
                                  nSamp = 10, # number of desired samples
                                  mindist = 400, # minimum distance samples must be apart from one another
                                  plot = TRUE) 

firesample_small <- vect(plots_smallsamp)
crs(firesample_small) <- prjcrs
plot(firesample_small)

fire_small <- extract_strata(sraster = raster_strat_buff, existing = plots_smallsamp)
fire_samp_small <- st_sf(fire_small)

fire_samp_small$plot = "highseverity"
data_fire_small <- vect(fire_samp_small)

data_firecoords_small <- terra::crds(data_fire_small) %>% 
  as.data.frame() %>% 
  mutate(ID = 1:length(x))

alldatafire_small <- data.frame(data_fire_small) %>% 
  mutate(ID = 1:length(strata)) %>% 
  left_join(data_firecoords_small) %>% 
  filter(strata%in%c(1,5)) %>% 
  select(-ID) %>% 
  mutate(ID = 1:length(x))

allplots_small <- vect(alldatafire_small, geom=c("x", "y"), crs = prjcrs)

## extracting vpd data
plotext_small <- terra::extract(resmplevpd, allplots_small)

## getting correct coords
allcoords_small <- terra::crds(allplots_small) %>% 
  as.data.frame() %>% 
  mutate(ID = 1:length(x))

plotframe_small <- data.frame(plotext_small) %>% 
  rename(vpd = PRISM_vpdmax_30yr_normal_800mM4_annual_bil) %>% 
  #left_join(allcoords_small) %>% 
  left_join(alldatafire_small) %>% 
  mutate(plot="highseverity", crs = "NAD83 UTM11") %>% 
  select(-strata.1)


allplots_small_proj <- vect(plotframe_small, geom=c("x", "y"), crs = prjcrs)
plot(allplots_small_proj)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#
#                                    actually combining
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

allplotsfinally <- fulljoinplots %>% 
  full_join(plotframe_small)

allplotsfinally_proj <- vect(allplotsfinally, geom=c("x", "y"), crs = prjcrs)
plot(allplotsfinally_proj)

plot(cropvpd)
plot(allplotsfinally_proj, add=T)

tmap_mode("view")
st_allplots <- st_as_sf(allplotsfinally_proj)

tm_shape(st_allplots)+
  tm_dots(c("plot"))+
  tm_shape(mask_all)+
  tm_raster()+
  tm_shape(mask_1000)+
  tm_raster()+
  tm_layout(legend.outside = T)

##tomorrow: check that the xy is not extracted from the fire data; which would not be nad83



## filter out burns above 2017 that are high severity; we had to filter 1 plot

# tmap_mode("view")
# st_allplots <- st_as_sf(allplots)

# tm_shape(st_allplots)+
#   tm_dots("plot")+
#   tm_shape(resamplefire)+
#   tm_raster()+
#   tm_layout(legend.outside = T)

highsevcoords <- terra::crds(projhighsev) %>% 
  as.data.frame()

highsevcoords$ID <- highsevext$ID

mergedat <- highsevext %>% 
  left_join(highsevcoords) %>% 
  filter(mtbs_CA_2013>3)

strata <- as.data.frame(projhighsev) %>% 
  mutate(ID = 1:length(strata))



## writing a shapefile
#st_write(pila_samp, "RandomPLOTSJune13_LAST.shp")


## combining plots and writing a shapefile


