## ---------------------------
##
## Script name: Fire history
##
## Author: Dr. Joan Dudney
##
## Date Created: 2023-05-26
##
## Copyright (c) Joan Dudney, 2023
## Email: dudney@ucsb.edu
##
## ---------------------------
##
## Notes: Analyses and extraction of fire history layers
## from https://data-nifc.opendata.arcgis.com/datasets/nifc::wfigs-interagency-fire-perimeters/about
##   
##
## ---------------------------

## Install the librarian package if not yet installed using the Require package 

#install.packages("Require")
if (!require("librarian")) install.packages("librarian")

## this code will load package or download packages that are not yet loaded 
librarian::shelf(tidyverse, curl, terra, tmap, sf, sp, tmaptools, rsconnect)

## ggplot theme
theme_set(
  theme_bw(base_size = 17)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# PILA distribution data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


## pila polygon
yose_pila <- "/Users/treelife/Documents/YOSE sugar pine project/Code and data/Analyses_sugarpine/Data/YOSE_PILA_Dist/YOSE_SugarPine_Dist_20210324.shp"
pila.shp <- vect(yose_pila)
proj = crs(pila.shp)

prj2<-toString(crs(pila.shp))


## yosemite area polygon
yose_poly <- "Data/YOSE poly/Yosemite_perim_PILA_1.shp"
yose.shp <- vect(yose_poly)

plot(yose.shp)
plot(pila.shp, add=T)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Fire polygon data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## fire history layers in the yosemite area
# yosefire <- "Data/US fire history/Fireperims_yose.shp"
# fire.shp <- st_read(yosefire)
# fire_proj <- st_transform(fire.shp, prj2)

# ## clipping fire data by the extent of the YOSE polygon
# cropdim <- yose.shp
# 
# cropped_fire <- fire_proj %>% 
#  st_crop(cropdim)
# 
# 
# ## visualizing layers
# plot(cropped_fire)
# 
# tmap_mode("view")
# 
# figfire <- tm_shape(fire.shp)+
#   tm_polygons("Ig_Date")+
#   tm_fill(col="red", alpha=.3)+
#   tm_polygons(style= "pretty",
#             title="Fire", legend.show = T)
# 
# figfire
# 
# ## reading in sugar pine layer
# yose_pila <- "Data/YOSE_PILA_Dist/YOSE_SugarPine_Dist_20210324.shp"
# pila.shp <- st_read(yose_pila)
# plot(pila.shp)
# 
# 
# pilafig <- tm_shape(pila.shp)+
#   tm_fill(col="darkgreen")+
#   tm_polygons(style= "pretty",
#               title="PILA")+
#   tm_polygons(style= "cont",title="Slope")+
#   tm_layout(legend.outside = F)+
#   tmap_options(check.and.fix = TRUE)
# 
# figfire + pilafig
# 
# ## combining fire layers to identify unburned sugar pine
# comb_fire <- st_union(fire.shp)
# plot(comb_fire)
# 
# comfirefig <- tm_shape(comb_fire)+
#   tm_fill(col="red")+
#   tm_polygons(style= "pretty",
#               title="Fire")+
#   tm_polygons(style= "cont",title="Slope")+
#   tm_layout(legend.outside = F)+
#   tmap_options(check.and.fix = TRUE)
# 
# comfirefig




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#
#                                     PRISM DATA
#               Using vpdmax; this was the most parsimonious variable from previous work
#               
#                                       Results: 
#                           10%      30%      60%      90% 
#                        13.02437 13.80830 14.96025 16.77551         
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(prism)

wdir <- 'Data/prism'
#prism_set_dl_dir("Data/spatialdat/prism")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Precip Data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#get_prism_normals(type = "vpdmax", resolution = "800m", annual = TRUE, keepZip = TRUE)
#get_prism_normals(type = "tmean", resolution = "800m", annual = TRUE, keepZip = FALSE)

# ppt <- "Data/Prism/prism_ppt/PRISM_ppt_30yr_normal_800mM3_annual_bil.bil"
# file.exists(ppt)
# 
# ppt_prism <- terra::rast(ppt)
# 
# ppt_proj <- terra::project(ppt_prism, prj2)
# #plot(ppt_proj)
# #plot(Pinus, add=TRUE, col="blue")
# 
# ppt_crop <- ppt_proj %>% 
#   crop(pila.shp)
# 
# ## view the precip files
# pptfig <- tm_shape(ppt_crop)+
#   tm_raster(style= "pretty",
#             title="Precip")
#   
# pptfig


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# VPDMAX data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
vpd <- "Data/Prism/PRISM_vpdmax/PRISM_vpdmax_30yr_normal_800mM4_annual_bil.bil"
file.exists(vpd)

vpd_prism <- terra::rast(vpd)

vpd_proj <- terra::project(vpd_prism, prj2)
#plot(vpd_proj)
#plot(pila.shp, add=TRUE, col="blue")

vpd_crop <- vpd_proj %>% 
  crop(pila.shp)

## view the precip files
vpdfig <- tm_shape(vpd_crop)+
  tm_raster(style= "pretty",
            title="VPD")

vpdfig

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Extracting prism data using PILA dist in SEKI
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## masking to the pila distribution over prism data
prism_pila <- mask(vpd_crop, pila.shp)

## checking to see if it worked
plot(prism_pila)
plot(pila.shp, add=TRUE, col="blue")

#writeRaster(prism_pila, "Data/Prism/YOSE_SugarPine_VPDmax.tif")

## extracting the values

hist(prism_pila)
prismvals <- as.data.frame(prism_pila)
names(prismvals)[1]="vpd"

quantile(prismvals$vpd, probs = c(.1, .3, .6, .9))

## results
##     10%      30%      60%      90% 
##   13.02437 13.80830 14.96025 16.77551 




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#
#                               ROADS and TRAILS
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## reading in roads and trails for YOSE

roadtrail <- st_read("Data/Trails and Roads/Union_roads_trails_YOSE.shp")
plot(roadtrail)


# ## reading in FS roads
# roads <- "Data/Trails and Roads/Merged_FS_roads_trails.shp"
# file.exists(roads)
# roads.shp <- vect(roads)
# roads.shp <- st_read(roads)
# 
# ## reprojecting the files and cropping by sugar pine distribution extent
# roads_proj <- terra::project(roads.shp, prj2)
# roads_proj <- st_transform(roads.shp, prj2)
# 
# #st_write(roads_proj, "Data/roadsYOSE_export.shp")
# 
# ## reading in NPS trails
# yose_trails <- "Data/Trails and Roads/NationalParksTrails.shp"
# file.exists(yose_trails)
# trails.shp <- vect(yose_trails)
# trails.shp <- st_read(yose_trails)
# plot(trails.shp)
# 
# ## reprojecting trails data
# trails_proj <- terra::project(trails.shp, prj2)
# trails_proj <- st_transform(trails.shp, prj2)
# 
# #st_write(trails_proj, "Data/trailsYOSE_export.shp")
# 
# 
# #merge_trailsroads <- st_union(roads_proj, trails_proj)
# 
# croptrails <- crop(trails_proj, yose.shp)
# croptrails <- st_crop(trails_proj, yose.shp)
# plot(croptrails)
# 
# merge1 <- combineGeoms(croptrails, crop_roads)
# plot(merge1)
# 
# trails_sf <- sf::st_as_sf(croptrails)
# plot(trails_sf["OBJECTID"])
# 
# ## buffer or 100
# # buff_trails <- buffer(croptrails, 100)
# # plot(buff_trails)
# # 
# # buff_roads <- buffer(crop_roads, 100)
# # plot(buff_roads)
# 
# ## to easily merge files, creating a buffer of 1
# buff_trails <- buffer(croptrails, 1)
# plot(buff_trails)
# 
# buff_roads <- buffer(crop_roads, 1)
# plot(buff_roads)
# 
# ## now easy to use the function union to combine buffers
# buff_union <- union(buff_roads, buff_trails)
# plot(buff_union)
# 
# #writeVector(buff_union, "Data/Trails and Roads/1mBufferRoadsTrails.shp", overwrite=T)
# 


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#
#                                    LAKES AND RIVERS
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## reading in lakes data and cropping by pila distribution extent
lakes <- "Data/Lakes and Streams/Lakes SN/NHD_Waterbodies/Waterbody_SNV.shp"
file.exists(lakes)

lakes_yose <- vect(lakes)
proj_lakes <- project(lakes_yose, prj2)

lakes_crop <- crop(proj_lakes, yose.shp)
plot(lakes_crop) ## there are no lakes overlappingw with the sugar pine distribution

## creating buffer
buff_lakes <- buffer(lakes_crop, 2)
plot(buff_lakes)

## reading in streams and rivers data and cropping by pila extent
streams <- "Data/Lakes and Streams/Streams and Rivers/NHD_Stream and Rivers/Rivers_Stream_SNV.shp"
file.exists(streams)

streams_yose <- vect(streams)
proj_streams <- project(streams_yose, prj2)

streams_crop <- crop(proj_streams, yose.shp)
plot(streams_crop)

## buffer the streams data by pila dist
buff_streams <- buffer(streams_crop, 1)
plot(buff_streams)

## combining buffers
buff_lakesrivers25m10m <- union(buff_lakes, buff_streams)
plot(buff_lakesrivers25m10m)


## writing buffers
#writeVector(buff_lakesrivers100m50m, "Data/Lakes and Streams/buffer_lakesstreams_100m50m.shp", overwrite=T)
#writeVector(buff_lakesrivers25m10m, "Data/Lakes and Streams/buffer_lakesstreams_2m1m.shp", overwrite=T)




# ## rasterizing species and fire data
# yose_pila <- "Data/YOSE_PILA_Dist/YOSE_SugarPine_Dist_20210324.shp"
# pila.shp2 <- vect(yose_pila)
# plot(pila.shp2)
# 
# fig <- tm_shape(pila.shp2)+
#   tm_fill(col="darkgreen")+
#   tm_polygons(style= "pretty",
#               title="PILA")
# 
# 
# comb_pila <- terra::union(pila.shp2)
# plot(comb_pila)
# 
# 
# plot(raster)
# 
# pila_rast <- rasterize(pila.shp2,raster, touches=TRUE)
# 
# ## check it worked
# plot(pila_rast)
# help(rasterize)
# 
# yosefire <- "Data/US fire history/Fireperims_yose.shp"
# fire.shp <- vect(yosefire)
# 
# cellsize = 100
# r.new <- rast(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, resolution = c(cellsize, cellsize))
# 
# fire_rast <- rasterize(comb_pila, ppt_prism, touches=TRUE)
# plot(fire_rast)
# dim(ppt_prism)
# 
# 
# v1 <- vect("Data/YOSE_PILA_Dist/YOSE_SugarPine_Dist_20210324.shp")
# v2 <- vect("Data/US fire history/Fireperims_yose.shp")
# prj2<-toString(crs(v1))
# v <- project(v2, prj2)
# #writeVector(v, "Data/US fire history/Fireperims_yose_projected.shp")
# 
# cellsize = 100
# e <- floor(ext(v)) %% cellsize
# r.new <- terra::rast(e, crs = crs(v), resolution=cellsize)
# 
# r <- terra::rasterize(x=v, y=r.new, filename="Data/fireyose.tif", overwrite=TRUE) 
# plot(r)
# 
# help("extract,SpatRaster,SpatExtent-method")
# 
# 
# 
# # Extract polygons touching another layer
# touching_polygons <- extract(v2, v2, what="touches")
# help("extract")
# 
# # Identify polygons in layer1 touching polygons in layer2
# crop_polygons <- terra::relate(v, v1)
# plot(crop_polygons)
# 
# 
# # Print the summary of the touching polygons
# print(touching_polygons)
# 
# ## next
# 
# ## only extract polygons 4years post fire
# ## all new fire = don't sample/buffer
# ## compile all data
# 
# sug <- st_read("Data/YOSE_PILA_Dist/YOSE_SugarPine_Dist_20210324.shp")
# fire <- st_read("Data/US fire history/Fireperims_yose.shp")
# prj2<-toString(crs(sug))
# firepolys <- st_transform(fire, prj2)
# 
# ## extracting all fires intersecting with PILA polys
# overlap <- st_intersection(sug, firepolys)
# 
# ## view
# plot(overlap['Ig_Date'])
# 
# ## creating a dataframe from the attribute table
# firedata <- overlap[,c(1:34)]
# 
# ## cleaning and selecting data that's useful
# sugar_fires <- firedata %>% 
#   rename_with(tolower) %>% 
#   select(fid, ig_date, incid_name, incid_type, burnbndac) %>% 
#   filter(!duplicated(incid_name)) %>% 
#   separate(ig_date, c("year", "month", "day")) %>% 
#   mutate(year=as.numeric(year), timesince = 2023-year)
# 
# mean(sugar_fires$burnbndac)
# 
# sugar_fires %>% 
#   filter(burnbndac>15000) %>% 
#   ggplot(aes(y=burnbndac, x=timesince, fill=incid_name))+
#   geom_bar(stat="identity", width=.3)+
#   scale_fill_brewer(palette = "Dark2")+
#   ylab("Burn area")+
#   guides(color=F)+
#   scale_x_continuous(breaks = c(0:40, 2))
# 
# sugar_fires %>% 
#   filter(burnbndac>15000) %>% 
#   ggplot(aes(y=burnbndac, x=year, fill=incid_name))+
#   geom_bar(stat="identity", width=.3)+
#   scale_fill_brewer(palette = "Dark2")+
#   ylab("Burn area")+
#   guides(color=F)+
#   scale_x_continuous(breaks = c(1986:2021, 2))
# 
# sugar_fires %>% 
#   ggplot(aes(y=burnbndac, x=timesince, fill=incid_name, color=factor(timesince)))+
#   geom_bar(stat="identity", width=.3)+
#   guides(fill=F, color=F)+
#   ylab("Burn area")+
#   scale_x_continuous(breaks = c(0:40, 2))
# 
# 
# ## creating new columns for specific fires of interest
# fire_add <- overlap %>% 
#   mutate(bigfire = ifelse(Incid_Name%in%c("FERGUSON","LARSON", "HAMM", "RIM"), "bigfire", "nontarget")) %>% 
#   mutate(fireclass = ifelse(Incid_Name%in%c("FERGUSON","LARSON", "HAMM", "RIM"), paste(Incid_Name), 
#                             ifelse(Incid_Name=="CREEK", "CREEK",  "nontarget")))
# 
# plot(fire_add['bigfire'])
# plot(fire_add['fireclass'])
# 
# tmap_mode("view")
# firefig <- tm_shape(fire_add)+
#   tm_polygons("fireclass", style= "pretty",alpha=.8,
#               title="Fire",legend.show = TRUE)+
#   tm_fill(palette = "Dark2")
# firefig
# 
# 
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# # prism
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 
# 
# 
# path='Data/white pine polygons/Prism Ranges/'
# #files <- sort(list.files('Data/white pine polygons/Prism Ranges/', pattern = "(buffered)+(.tif)"))
# #files <- sort(list.files('Data/white pine polygons/Prism Ranges/', pattern = "(buff)+(.tif)"))
# files <- sort(list.files('Data/white pine polygons/Prism Ranges/', pattern = "(NoBuffers)+(.tif)"))
# 
# filenames <- paste0(path, files)
# 
# ## reading in files
# myfiles = lapply(filenames, terra::rast)
# 
# species = sort(c("PIBA", "PIMO", "PICO", "PILA", "PIAL"))
# newfiles=list()
# 
# for (file in myfiles) {
#   newlist = list(file)
#   newfiles = rbind(newfiles, newlist)
# }
# 
# newfiles
# 
# sp_data <- tibble(rasters=newfiles, species=species)
# 
# xy_clim <- function(sp_map) {
#   new_map <- as.data.frame(sp_map, xy = TRUE) %>% 
#     drop_na()
#   return(new_map)
# }
# 
# sp_data_all <- sp_data %>%
#   mutate(clim_vals = map(rasters, xy_clim)) %>%
#   unnest(cols = clim_vals) %>% 
#   dplyr::select(-rasters)
# 
# colnames(sp_data_all)[c(4)]=c("ppt")
# 
# quantsuneven = sp_data_all %>% 
#   group_by(species) %>% 
#   summarize(quant = quantile(ppt, probs = c(0.01,.10, .20, .40, .60, .80, .90, .99)), 
#             probs = c("0.01",".10", ".20", ".40", ".60", ".80",".90", ".99")) %>% 
#   ungroup()
# #write_csv(quantsuneven, paste0(path, "strata_quants_NoBuffers_quantsuneven.csv"))
