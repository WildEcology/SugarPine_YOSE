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


maskalldat <- data.frame(mask_all) %>% 
  rename(vpd = PRISM_vpdmax_30yr_normal_800mM4_annual_bil)

percentile <- ecdf(maskalldat$vpd)
round(percentile(c(12.5, 14, 16, 18)),digits =2)

strata_all <- c(12.5, 14, 16, 18)
#strata_fire <- c(12.9, 14, 16, 17.5)

## create the stratified raster
raster_strat_buff <- strat_breaks(
  mraster = mask_all,
  breaks =  strata_all)

plot(raster_strat_buff)

# ## raster for rim fire high severity
# raster_strat_fire <- strat_breaks(
#   mraster = mask_fire,
#   breaks =  strata_fire)

##calculating strata
plots_finalsample_higher <- sample_strat(sraster = raster_strat_buff,# input mraster
                                  allocation = "equal",
                                  nSamp = 11, # number of desired samples
                                  mindist = 2500, # minimum distance samples must be apart from one another
                                  plot = TRUE) 




plots_jun14 <- vect(plots_finalsample_higher)
crs(plots_jun14) <- prjcrs
plot(plots_jun14)


sample_vect_jun14 <- extract_strata(sraster = raster_strat_buff, existing = plots_finalsample_higher)

pila_samp_jun14 <- st_sf(sample_vect_jun14)
pila_samp_jun14$plot = "random"
data_random_june14 <- vect(pila_samp_jun14)
plot(data_random_june14)

data_random_coords_jun14 = terra::crds(data_random_june14) %>% 
  as.data.frame() %>% 
  mutate(ID = 1:length(x))

allrandom_jun14 <- data.frame(data_random_june14) %>% 
  mutate(ID = 1:length(strata)) %>%
  left_join(data_random_coords_jun14)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# EXTRACTING VPD AND FIRE DATA
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## converting the dataframe to a vector
allplots_jun14 <- vect(allrandom_jun14, geom=c("x", "y"), crs = prjcrs)

## extracting vpd data
plotext_jun14 <- terra::extract(mask_all, allplots_jun14)

## getting correct coords
allcoords_jun14 <- terra::crds(allplots_jun14) %>% 
  as.data.frame() %>% 
  mutate(ID = 1:length(x))

plotframe_jun14 <- data.frame(allplots_jun14) %>% 
  mutate(ID = 1:length(strata)) %>% 
  left_join(plotext_jun14) %>% 
  rename(vpd = PRISM_vpdmax_30yr_normal_800mM4_annual_bil) %>% 
  left_join(allcoords_jun14)

## extracting from fire severity
## no longer in NAD83 but changed to fire severity CRS
project_allplots_jun14 <- project(allplots_jun14, proj1)
plotfire_jun14 <- terra::extract(rasterbrick, project_allplots_jun14)
colnames(plotfire_jun14)

## cleaning the fire data
plotfire_jun14[is.na(plotfire_jun14)] <- 0

newplotfire_jun14 <- plotfire_jun14 %>%
  pivot_longer(-ID) %>% 
  mutate(value = replace(value, value == 6, 0)) %>% 
  mutate(value= as.numeric(value)) %>% 
  group_by(ID) %>% 
  slice(which.max(value))

fireANDallplots_jun14_thirdbit <- plotframe_jun14 %>% 
  left_join(newplotfire_jun14)

secondbit <- fireANDallplots_jun14_secondbit

#write_csv(fireANDallplots_jun14_thirdbit, "allRandomplots.csv")
## removing high severity 3 recent burns > 2017
## keeping rim fire 3s as potential high severity for that strata

thirdbitall <- fireANDallplots_jun14_thirdbit %>% 
  separate(name, c("none", "ca", "year")) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(!(value==3 & year>2017)) %>% ## filtering out recent burned plots
  filter(!(value==4 & year>2017)) #filtering out recent burned plots

deletedplots <- fireANDallplots_jun14_thirdbit %>% 
  separate(name, c("none", "ca", "year")) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(value > 2 & year>2017) %>% 
  slice_sample(n=3)

deletevect <- vect(deletedplots, geom=c("x", "y"), crs = prjcrs)
deletesf <- st_as_sf(deletevect)
st_crs(deletesf) <- prjcrs

## writing a kml file
#st_write(deletesf, "deletedplotsRandomHighSev.kml", driver = "kml")


## selecting from the other run unburned plots
unburnedplots <- filter(secondbit, value==0) %>% 
  separate(name, c("none", "ca", "year")) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(plot_type = "Random")

## selectin burned plots
burnedhigh <- filter(allplotsfinally, plot !="random") %>% 
  separate(name, c("none", "ca", "year")) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(plot_type = "HighSeverityRim") %>% 
  select(-type, -crs,  -rule, -none, -ca) %>% 
  filter(strata%in%c(1,5)) %>% 
  filter(!ID==6)


## rim fire high severity plots 2-4 strata
#"SlicedRimFireHighSeverity2-4strata.csv"

rimfireadd <- firerimfire %>% 
  separate(name, c("none", "ca", "year")) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(plot_type = "HighSeverityRim") %>% 
  select(-type, -strata.1, -rule, -none, -ca) %>% 
  select(-strata) %>% 
  rename(strata = strata_2)

## combining all plots
moreplots_all <- thirdbitall %>% 
  mutate(plot_type = ifelse(value>2&year == 2013, "HighSeverityRim_Random",
                            ifelse(value>2&year < 2013, "HighSeverityRandomEarly", "Random"))) %>% 
  full_join(unburnedplots) %>% 
  full_join(burnedhigh) %>% 
  full_join(rimfireadd) %>% 
  select(-type, -strata.1, -rule, -none, -ca)

randomplotslice <- moreplots_all %>% 
  filter(plot=="random") %>% 
  group_by(strata) %>% 
  slice_sample(n=9)

totalslice <- randomplotslice %>% 
  full_join(burnedhigh) %>% 
  full_join(rimfireadd)

slicedvect <- vect(totalslice, geom=c("x", "y"), crs = prjcrs)

## extracting kml
vectkml <- st_as_sf(slicedvect)

## projecting xy
st_crs(vectkml) <- prjcrs

## writing a kml file
#st_write(vectkml, "SlicedPlotsJune14.kml", driver = "kml")


#allplots_new <- vect(moreplots_all, geom=c("x", "y"), crs = prjcrs)
#plot(allplots_new)

tmap_mode("view")
st_allplots <- st_as_sf(slicedvect)

tm_shape(st_allplots)+
  tm_dots(c("plot_type"))+
  tm_shape(mask_all)+
  tm_raster()+
  tm_layout(legend.outside = T)
  
ggplot(moreplots_all, aes(x=x, y=y, color=strata))+
  geom_point()

#writeVector(slicedvect, "SLICEDplots_June14_NOROADBUFFERS.shp", overwrite=T)
#write_csv(totalslice, "RandomHighSeverity_Sliced.csv")
#write_csv(fireANDallplots, "PlotSelection_June14_DONTUSEHIGHSEVERITY.csv")
#write_csv(moreplots_all, "PlotSelection_June14_NOROADBUFFERS.csv")
#writeVector(allplots_new, "ALLPlotSelection_June14_NOROADBUFFERS.shp")
#write_csv(fireANDallplots, "FinalPlotSelection_June13_highsevRandom.csv")
#write_csv(fireANDallplots, "FinalPlotSelection_June13_highsevRandom_ADDITIONAL.csv")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# READING IN DATA
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


subset_plots <- allplotsfinally %>% 
  filter(plot == "highseverity" | ID %in%c(60, 58, 113, 121)) %>% 
  select(-crs, -rule)


fulljoin_jun14 <- fireANDallplots_jun14 %>% 
  full_join(subset_plots) %>% 
  select(-strata.1, -rule) %>% 
  filter(!vpd%in%c(12.57172, 12.59086,17.06120,17.07333)) ## taking out plots that were replaced by the previous ones

allplotsJun14 <- vect(fulljoin_jun14, geom=c("x", "y"), crs = prjcrs)
plot(allplotsJun14)

tmap_mode("view")
st_allplots <- st_as_sf(allplotsJun14)

tm_shape(st_allplots)+
  tm_dots(c("plot"))+
  tm_shape(mask_all)+
  tm_raster()+
  tm_layout(legend.outside = T)

#writeVector(allplotsJun14, "allplotsJune14.shp")



# ## now making sure we sample the hottest and coolest plots for high severity and random
# 
# # used original file "FinalPlotSelection_June13_highsevRandom.csv"
# 
# # randomplots_keep <- filter(fireANDallplots, plot=="random") %>% 
# #   mutate(keep = ifelse(vpd > 18.5, "keep",
# #                        ifelse(vpd < 12.4, "keep", "sample"))) %>% 
# #   filter(keep == "keep")
# # 
# # randomplots_sample_med <- filter(fireANDallplots, plot=="random") %>% 
# #   mutate(keep = ifelse(vpd > 18.5, "keep",
# #                        ifelse(vpd < 12.4, "keep", "sample"))) %>% 
# #   filter(keep != "keep") %>% 
# #   filter(strata%in%c(2:4)) %>% 
# #   group_by(strata) %>% slice_sample(n=8)
# #   
# # randomplots_highlow <- filter(fireANDallplots, plot=="random") %>% 
# #   mutate(keep = ifelse(vpd > 18.5, "keep",
# #                        ifelse(vpd < 12.4, "keep", "sample"))) %>% 
# #   filter(keep != "keep") %>% 
# #   filter(!strata%in%c(2:4)) %>% 
# #   group_by(strata) %>% slice_sample(n=3)
# # 
# # 
# # fulljoinsdata <- randomplots_highlow %>% 
# #   full_join(randomplots_keep) %>% 
# #   full_join(randomplots_sample_med) %>% 
# #   select(-keep)
# 
# ## random plot data
# randomplots_final <- fulljoinsdata %>% 
#   select(-crs)
# 
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# # BURNED PLOTS
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 
# ## now doing the same thing but for high severity plots
# ## putting fire plots in true strata
# fireplots_keep <- filter(fireANDallplots, plot=="highseverity") %>% 
#   mutate(strata_2 = ifelse(vpd<12.5, 1,
#                            ifelse(vpd>=12.5 & vpd < 14,2,
#                                   ifelse(vpd >=14& vpd<16, 3,
#                                          ifelse(vpd>=16&vpd<18, 4, 5))))) %>% 
#   filter(strata_2!=5) %>% 
#   group_by(strata_2) %>% 
#   slice_sample(n = 6)
# 
# medsevfire <- fireANDallplots %>%
#   filter(plot=="highseverity") %>% 
#   mutate(strata_2 = ifelse(vpd<12.5, 1,
#                            ifelse(vpd>=12.5 & vpd < 14,2,
#                                   ifelse(vpd >=14& vpd<16, 3,
#                                          ifelse(vpd>=16&vpd<18, 4, 5))))) %>% 
#   filter(strata_2==5) %>% 
#   full_join(fireplots_keep) %>% 
#   select(-strata, -strata.1) %>% 
#   rename(strata = strata_2)
# 
# 
# 
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# #
# #
# #                                    COMBINING MOST TOGETHER
# #
# #
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 
# fulljoinplots <- randomplots_final %>% 
#   select(-strata.1) %>% 
#   full_join(medsevfire) %>% 
#   select(-crs)
# 
# #write_csv(fulljoinplots, "RandomlyselectedFireANDrandomplotsJune13.csv")
# 
# 
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# #
# #
# #                                    ADDITIONAL FIRE DATA
# #
# #
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 
# ## reading in fire severity data between 2009-2015
# highsevpila <- rast("Data/highseverity3.4.PILA2009,2010,2014.tif")
# plot(highsevpila)
# 
# 
# projecthighsevpila <- project(highsevpila, prjcrs)
# resmplevpd <- resample(cropvpd, projecthighsevpila)
# maskhighsevpila <- mask(resmplevpd, projecthighsevpila)
# plot(maskhighsevpila)
# hist(maskhighsevpila$PRISM_vpdmax_30yr_normal_800mM4_annual_bil)
# 
# strata_all <- c(12.6, 14, 16, 17)
# #strata_fire <- c(12.9, 14, 16, 17.5)
# 
# ## create the stratified raster
# raster_strat_buff <- strat_breaks(
#   mraster = maskhighsevpila,
#   breaks =  strata_all)
# 
# plot(raster_strat_buff)
# 
# 
# ##calculating strata
# plots_smallsamp <- sample_strat(sraster = raster_strat_buff,# input mraster
#                                 allocation = "equal",
#                                 nSamp = 10, # number of desired samples
#                                 mindist = 400, # minimum distance samples must be apart from one another
#                                 plot = TRUE) 
# 
# firesample_small <- vect(plots_smallsamp)
# crs(firesample_small) <- prjcrs
# plot(firesample_small)
# 
# fire_small <- extract_strata(sraster = raster_strat_buff, existing = plots_smallsamp)
# fire_samp_small <- st_sf(fire_small)
# 
# fire_samp_small$plot = "highseverity"
# data_fire_small <- vect(fire_samp_small)
# 
# data_firecoords_small <- terra::crds(data_fire_small) %>% 
#   as.data.frame() %>% 
#   mutate(ID = 1:length(x))
# 
# alldatafire_small <- data.frame(data_fire_small) %>% 
#   mutate(ID = 1:length(strata)) %>% 
#   left_join(data_firecoords_small) %>% 
#   filter(strata%in%c(1,5)) %>% 
#   select(-ID) %>% 
#   mutate(ID = 1:length(x))
# 
# allplots_small <- vect(alldatafire_small, geom=c("x", "y"), crs = prjcrs)
# 
# ## extracting vpd data
# plotext_small <- terra::extract(resmplevpd, allplots_small)
# 
# ## getting correct coords
# allcoords_small <- terra::crds(allplots_small) %>% 
#   as.data.frame() %>% 
#   mutate(ID = 1:length(x))
# 
# plotframe_small <- data.frame(plotext_small) %>% 
#   rename(vpd = PRISM_vpdmax_30yr_normal_800mM4_annual_bil) %>% 
#   #left_join(allcoords_small) %>% 
#   left_join(alldatafire_small) %>% 
#   mutate(plot="highseverity", crs = "NAD83 UTM11") %>% 
#   select(-strata.1)
# 
# 
# allplots_small_proj <- vect(plotframe_small, geom=c("x", "y"), crs = prjcrs)
# plot(allplots_small_proj)
# 
# 
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# #
# #
# #                                    actually combining
# #
# #
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 
# allplotsfinally <- fulljoinplots %>% 
#   full_join(plotframe_small) %>% 
#   select(-crs, -ID)
# 
# allplotsfinally_proj <- vect(allplotsfinally, geom=c("x", "y"), crs = prjcrs)
# plot(allplotsfinally_proj)
# 
# plot(cropvpd)
# plot(allplotsfinally_proj, add=T)
# 
# tmap_mode("view")
# st_allplots <- st_as_sf(allplotsfinally_proj)
# 
# tm_shape(st_allplots)+
#   tm_dots(c("plot"))+
#   tm_shape(mask_all)+
#   tm_raster()+
#   tm_shape(mask_1000)+
#   tm_raster()+
#   tm_layout(legend.outside = T)
# 
# ##tomorrow: check that the xy is not extracted from the fire data; which would not be nad83
# 
# 
# 
# ## filter out burns above 2017 that are high severity; we had to filter 1 plot
# 
# # tmap_mode("view")
# # st_allplots <- st_as_sf(allplots)
# 
# # tm_shape(st_allplots)+
# #   tm_dots("plot")+
# #   tm_shape(resamplefire)+
# #   tm_raster()+
# #   tm_layout(legend.outside = T)
# 
# highsevcoords <- terra::crds(projhighsev) %>% 
#   as.data.frame()
# 
# highsevcoords$ID <- highsevext$ID
# 
# mergedat <- highsevext %>% 
#   left_join(highsevcoords) %>% 
#   filter(mtbs_CA_2013>3)
# 
# strata <- as.data.frame(projhighsev) %>% 
#   mutate(ID = 1:length(strata))
# 
# 
# 
# ## writing a shapefile
# #st_write(pila_samp, "RandomPLOTSJune13_LAST.shp")
# 
# 
# ## combining plots and writing a shapefile


