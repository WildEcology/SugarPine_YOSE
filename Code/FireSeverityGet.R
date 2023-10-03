## ---------------------------
##
## Script name: Extracting fire severity data
##
## Author: Dr. Joan Dudney
##
## Date Created: 2023-05-28
##
## Copyright (c) Joan Dudney, 2023
## Email: dudney@ucsb.edu
##
## ---------------------------
##
## Notes: This unzips the fire severity data from 
##   https://www.mtbs.gov/ and creates a stack
##
## ---------------------------

## Install the librarian package if not yet installed using the Require package 

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
setwd("/Users/treelife/Documents/YOSE sugar pine project/Code and data/Analyses_sugarpine/Data/US fire history/Fire severity")
wdir <- getwd()
folderlist <- list.files()

newdataframe <-  list()

for (folder in folderlist){
  newpath = paste0(wdir, "/", folder)
  rastlist = data.frame(name = list.files(path = newpath, pattern='.tif$', all.files= T, full.names= T))
  newdataframe = rbind(newdataframe, rastlist)
}

files <- as.vector(newdataframe)


## now reading in and stacking raster files
yose_pila <- "/Users/treelife/Documents/YOSE sugar pine project/Code and data/Analyses_sugarpine/Data/YOSE_PILA_Dist/YOSE_SugarPine_Dist_20210324.shp"
pila.shp <- vect(yose_pila)
proj = crs(pila.shp)


rasterlist <- list()

rast2010 = newdataframe %>% 
  filter(row_number()%in%c(27)) %>% 
  pull(name) %>% 
  rast()

newdata <- newdataframe %>% 
  #filter(row_number()%in%c(27:30)) %>% 
  pull(name)

proj1 = crs(rast2010)
plot(rast2010)


rast2013 <- rast("/Users/treelife/Documents/YOSE sugar pine project/Code and data/Analyses_sugarpine/Data/US fire history/Fire severity/2013/mtbs_CA_2013.tif")
plot(rast2013)
proj1 = crs(rast2013)


projpila <- project(pila.shp, proj1)
plot(projpila)
extent <- ext(-2072370, -2000723, 1854591, 1933098)

newdata <- newdataframe %>% 
  #filter(row_number()%in%c(27:30)) %>% 
  pull(name)

raster_list <- list()

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




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#
#                                   SUBSET of High severity plots
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newdata_subset <- newdataframe %>% 
  filter(row_number()%in%c(26:33)) %>% 
  pull(name)

rast2011 <- rast("/Users/treelife/Documents/YOSE sugar pine project/Code and data/Analyses_sugarpine/Data/US fire history/Fire severity/2011/mtbs_CA_2011.tif")
plot(rast2011)

raster_list_sub <- list()

for (i in seq(1:length(newdata_subset))){
  file <- newdata_subset[i]
  raster1 <- rast(file)
  croprast <- crop(raster1, extent)
  maskpila <- mask(croprast, projpila)
  resampila <- resample(maskpila, croprast)
  raster_list_sub[i] <- resampila
}

## for some reason the 10th element has a different extent and not much data so removing it
rasterbrick_subset <- terra::rast(raster_list_sub)
rasterbrick_subset[[5]]
plot(rasterbrick_subset[[1]])

onlyhighsev <- mask(rasterbrick_subset, rasterbrick_subset %in% c(3,4), maskvalue=FALSE)
plot(onlyhighsev)




onlyhighsev_subset <- onlyhighsev[[c(1)]]



##masking by projected pila distribution
projectedhigh <- mask(onlyhighsev, projpila)
plot(projectedhigh)

firstrast <- projectedhigh[["mtbs_CA_2009"]]
secondrast <- projectedhigh[["mtbs_CA_2010"]]
thirdrast <- projectedhigh[["mtbs_CA_2014"]]

mergerast <- terra::merge(firstrast, secondrast, thirdrast)
plot(mergerast)

#writeRaster(mergerast,"Data/highseverity3.4.PILA2009,2010,2014.tif")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#
#                                 Extracting fire data by plots
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setwd("~/Documents/YOSE sugar pine project/Code and data/Analyses_sugarpine")

pilaplots <- st_read("Data/pilaplots60plotsJune8.shp")
plot(pilaplots)

projectplots <- st_transform(pilaplots, proj1)


# separated_coord <- projectplots %>%
#   mutate(x = unlist(map(projectplots$geometry,1)),
#          y = unlist(map(projectplots$geometry,2)))
# 
# separated_coord


projectplots_vect <- vect(projectplots)
firesev <- terra::extract(rasterbrick, projectplots_vect)

## now combining the extracted fire data for each plot with the strata
projectplots_sf <- 
  #--- back to sf ---#
  st_as_sf(projectplots_vect) %>% 
  #--- define ID ---#
  mutate(ID := seq_len(nrow(.))) %>% 
  #--- merge by ID ---#
  left_join(., firesev, by = "ID")

spat_fire <- as.data.frame(projectplots1_sf)
class(spat_fire)

#write_csv(spat_fire, "Data/Fireplotdat60June8.csv")

spatvectfire <- vect(projectplots1_sf)
plot(spatvect_fire)

#writeVector(spatvectfire, "Data/Fireplotdat60June8.shp")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#
#                                    Visualizing data
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## new analysis
firedat <- read_csv("Data/Fireplotdat60June8.csv")
firedatvect <- vect("Data/Fireplotdat60June8.shp")

## all plot data CRS: USA_Contiguous_Albers_Equal_Area_Conic_USGS_version (ESRI:102039) 

geom_fire <- firedat %>% 
  select(ID, geometry)

yeardat <- firedat %>%
  mutate(geometry = str_replace(geometry, "c", "")) %>% 
  mutate(geometry = unparen(geometry)) %>% 
  separate(geometry, c("x", "y"), ",") %>% 
  select(-fire) %>% 
  pivot_longer(-c(strata, ID, x, y)) %>% 
  na.omit() %>% 
  separate(name, c("none", "ca", "year")) %>% 
  select(ID, year, value) 
  
firedat_clean <- firedat %>%
  mutate(geometry = str_replace(geometry, "c", "")) %>% 
  mutate(geometry = unparen(geometry)) %>% 
  separate(geometry, c("x", "y"), ",") %>% 
  select(-fire) %>% 
  pivot_longer(-c(strata, ID, x, y)) %>% 
  separate(name, c("none", "ca", "year")) %>% 
  select(-none, -ca, -year) %>%
  distinct() %>% 
  left_join(yeardat) %>% ## exclude recent burned plots
  filter(is.na(year)|year < 2020 & !value > 2.5 ) %>% ## code keeps NAs
  mutate(fire = as.numeric(value), year= as.numeric(year)) %>% 
  select(-value) %>% 
  mutate_at(c('fire','year'), ~replace_na(.,0))

coordatandmore <- firedat_clean %>% 
  mutate(dup = duplicated(ID, fromLast=T)) %>% 
  filter(dup!=TRUE) %>% 
  select(-dup)

plotnum <- coordatandmore %>% 
  group_by(strata) %>% 
  summarize(num = length(strata))

medstrat = coordatandmore %>% 
  filter(strata %in%c(2:4)) %>% 
  group_by(strata) %>% 
  summarize(ID = sample(ID, 10))

alldat <- coordatandmore %>% 
  filter(strata %in% c(1,5)) %>% 
  select(strata, ID) %>% 
  full_join(medstrat) %>% 
  left_join(coordatandmore) %>% 
  mutate(plot_type = "Random") %>% 
  select(-ID)%>% 
  mutate(x=as.numeric(x), y = as.numeric(y))




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#
#                              READING IN DATA FROM HIGH SEVERITY PLOTS
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
highsev <- vect("Data/US fire history/Fire severity/highseverityplots.shp")
crs(highsev)

projhighsev <- project(highsev, proj1)

highsevext <- terra::extract(rasterbrick, projhighsev) 

highsevcoords <- terra::crds(projhighsev) %>% 
  as.data.frame()

highsevcoords$ID <- highsevext$ID

mergedat <- highsevext %>% 
  left_join(highsevcoords) %>% 
  filter(mtbs_CA_2013>3)

strata <- as.data.frame(projhighsev) %>% 
  mutate(ID = 1:length(strata))

##combining data
comstrat <- mergedat %>% 
  left_join(strata) %>% 
  select(ID, x, y, mtbs_CA_2013, strata) %>% 
  rename("rim fire" = mtbs_CA_2013) %>% 
  mutate(plot_type = "high severity") %>% 
  select(-ID) %>% 
  mutate(x=as.numeric(x), y = as.numeric(y))


## combining with random plots
randomhighsev <- comstrat %>% 
  full_join(alldat)

vpd_prism <- rast(vpd)
projvpd <- project(vpd_prism, proj1)

## projecting

newhighsev <- vect(randomhighsev, geom=c("x", "y"), crs = proj1, keepgeom=T)
combsf <- st_as_sf(newhighsev)
st_crs(combsf) <- proj1

plot(combsf["plot_type"])

maskvpd_all <- terra::extract(projvpd, newhighsev)

idfiles <- as.data.frame(newhighsev) %>% 
  mutate(ID = 1:length(x)) %>% 
  left_join(maskvpd_all) %>% 
  rename("VPD" ="PRISM_vpdmax_30yr_normal_800mM4_annual_bil") %>% 
  mutate(strata = as.character(strata))


ggplot(idfiles, aes(x=VPD, y=strata, fill=plot_type))+
  geom_point()+
  facet_grid(strata~plot_type)

highsev_all <- vect(idfiles, geom=c("x", "y"), crs = proj1, keepgeom=T)
fire_all_sf <- st_as_sf(highsev_all)
st_crs(fire_all_sf) <- proj1



## visualizing plots

tmap_mode("view")

tm_shape(fire_all_sf)+
  tm_dots("VPD")







# summaryfire <- firedat_clean %>%
#   mutate(value = replace_na(value, 0)) %>% 
#   mutate(year = as.numeric(year)) %>% 
#   filter(!value == 3) %>%  ## no moderate fire severity
#   filter(!value == 4) %>% 
#   filter(!(value == 2 & year > 2016)) %>%
#   group_by(ID) %>% 
#   dplyr::summarize(lastyear = ifelse(value > 0 & year > 0, 1, 0))
# 
# 
# firstburned = ifelse(value > 0 & year > 0, min(year, na.rm=T), 0),
# numfire = ifelse(value > 0, mean(value), "nofire"),
# burn = ifelse(numfire == "nofire", "None", "Lowseverity")) %>% 
#   select(-numfire) 
  
  
# firedat_clean <- firedat %>%
#   mutate(geometry = str_replace(geometry, "c", "")) %>% 
#   mutate(geometry = unparen(geometry)) %>% 
#   separate(geometry, c("x", "y"), ",") %>% 
#   select(-fire) %>% 
#   pivot_longer(-c(strata, ID, x, y)) %>% 
#   separate(name, c("none", "ca", "year")) %>% 
#   select(-none, -ca) %>% 
#   na.omit() %>% 
#   mutate(crs = proj1) %>% 
#   mutate(year = as.numeric(year)) %>% 
#   mutate(firesev = ifelse(value == 0, "No data",
#                           ifelse(value == 1, "Unburned",
#                                  ifelse(value == 2, "Low severity",
#                                         ifelse(value == 3, "Moderate",
#                                                ifelse(value == 4, "High severity", 
#                                                       ifelse(value == 5, "More green",
#                                                              ifelse(value == 6, "Not processed", "NA")))))))) %>% 
#   mutate(climstrata = ifelse(strata == 1, "1_Coldest",
#                               ifelse(strata == 2, "2_Mild",
#                                      ifelse(strata == 3, "3_Warm",
#                                             ifelse(strata == 4, "4_Hotter",
#                                                    ifelse(strata == 5, "5_Hottest", "NA"))))))
# 


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Filtering further
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# filtered <- firedat_clean %>% 
#   filter(firesev!="High severity") %>% 
#   filter(firesev!="Moderate")

newplots <- summaryfire %>% 
  mutate(x=as.numeric(x), y=as.numeric(y)) %>% 
  mutate(strata = as.character(strata))


newplotsvector <- vect(newplots, geom=c("x", "y"), crs = proj1, keepgeom=T)
plot(newplotsvector)

## converting to sf

sf_plots <- st_as_sf(newplotsvector)
st_crs(sf_plots) <- proj1

sf_pila <- st_as_sf(projpila)
st_crs(sf_pila) <- proj1

#tmap_mode("view")

tm_shape(sf_pila)+
  tm_polygons(col = 'darkgreen', alpha=.3)+
  tmap_options(check.and.fix = TRUE)+
  tm_shape(sf_plots, size = 1) +
  tm_dots("strata")+
  tm_layout(legend.outside = T)
  # tm_shape(trails_seki)+
  #   tm_lines()+

help(tm_shape)

## writing kml files
st_write(sf_plots, "plots_LowSeverity.kml", driver = "kml", append = F)


## looking at data

newplots %>% 
  ggplot(aes(x=climstrata))+
  geom_bar()



# strata
## 1 = below 10th; coldest
## 2 = 10-30; warmer
## 3 = 30-60; warmer still
## 4 = 60-90; hot
## 5 > 90; hottest

firedat_clean %>% 
  #filter(firesev == "highsev") %>% 
  ggplot(aes(x=climstrata))+
  geom_bar()+
  facet_grid(.~firesev)


summaryplots <- firedat_clean %>% 
  group_by(firesev, climstrata) %>% 
  summarize(num = length(unique(ID)))


comb_dat <- firedat_clean %>% 
  left_join(summaryplots)

comb_dat %>% 
  #filter(firesev == "highsev") %>% 
  ggplot(aes(x=climstrata, fill = factor(num)))+
  geom_bar()+
  facet_grid(.~firesev)

comb_dat %>% 
  #filter(firesev == "highsev") %>% 
  ggplot(aes(x=strata))+
  geom_bar()+
  facet_grid(firesev~year)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#
#                                    Filtering plots more
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

geom <- vect("Data/Fireplotdat60June8.shp")
coords <- crds(geom)






#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#
#                                   RIM FIRE
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newdata <- rast("/Users/treelife/Documents/YOSE sugar pine project/Code and data/Analyses_sugarpine/Data/US fire history/Fire severity/2013/mtbs_CA_2013.tif")

resmask = resample(newdata, rast2010)
maskrast <- mask(resmask, projpila)
plot(maskrast)


highsev_rast <- app(maskrast, fun=function(x) { 
  x[x = 4] <- 4; return(x)} )

hist(highsev_rast$lyr.1)

highsev_rast[highsev_rast < 4|highsev_rast>4] <- NA
crophighsev <- crop(highsev_rast, projpila)
plot(crophighsev)

projectfire <- project(crophighsev, prjcrs)

#writeRaster(crophighsev, "RimFireHighSeverity.tif")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# VPD DATA
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setwd("~/Documents/YOSE sugar pine project/Code and data/Analyses_sugarpine/")

## pila polygon
yose_pila <- "/Users/treelife/Documents/YOSE sugar pine project/Code and data/Analyses_sugarpine/Data/YOSE_PILA_Dist/YOSE_SugarPine_Dist_20210324.shp"
pila.shp <- vect(yose_pila)
plot(pila.shp)

## creating a project CRS
prjcrs <- toString(crs(pila.shp))


## reading in vpd data
vpd <- "Data/Prism/PRISM_vpdmax/PRISM_vpdmax_30yr_normal_800mM4_annual_bil.bil"
file.exists(vpd)

vpd_prism <- rast(vpd)
#hist(vpd_prism)
#dataprism <- data.frame(vpd_prism)
#quantile(dataprism$PRISM_vpdmax_30yr_normal_800mM4_annual_bil,probs = c(.1, .3, .6, .9))

vpd_proj <- project(vpd_prism, prjcrs)
cropvpd <- crop(vpd_proj, pila.shp)

plot(cropvpd)
#plot(pila.shp, add=TRUE, col="blue")

## resampling vpd to match with proj_slope
resampvpd <- resample(cropvpd, projectfire)
#plot(resampvpd)

## masking vpd with slope raster
mask_vpd <- mask(resampvpd,projectfire)
plot(mask_vpd)
hist(mask_vpd$PRISM_vpdmax_30yr_normal_800mM4_annual_bil)

hist(mask_vpd)
prismvals <- as.data.frame(mask_vpd)
names(prismvals)[1]="vpd"

## vpd quantiles across sugar pine
strata <- prismvals %>% 
  summarize(value = sort(quantile(vpd, probs = c(.1, .3, .6, .9)))) %>% 
  mutate(value=as.numeric(value)) %>% 
  pull(value)

## extracting prism values


vpd <- "Data/Prism/PRISM_vpdmax/PRISM_vpdmax_30yr_normal_800mM4_annual_bil.bil"
file.exists(vpd)

vpd_prism <- rast(vpd)
vpd_proj <- project(vpd_prism, proj1)




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#
#                                    DEPRECATED
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# 
# 
# croprast2013 <- crop(rast2013, extent)
# maskpila <- mask(croprast2013, projpila)
# plot(maskpila)
# dataframepila <- as.data.frame(maskpila)
# 
# plot(rast2013)
# plot(croprast2013)
# plot(projpila, add=T)
# 
# tmap_mode("view")
# firefig <- tm_shape(croprast2010)+
#   tm_raster()
# 
# firefig
# 
# projpila2 <- sf::st_as_sf(projpila)
# pilafig <- tm_shape(projpila2)+
#   tmap_options(check.and.fix = TRUE)+
#   tm_polygons( style= "pretty",alpha=.8,
#                title="Pila",legend.show = TRUE)+
#   tm_fill(palette = "Dark2")
# pilafig + firefig
# 
# raster_list = list()
# 
# newdata <- rast("/Users/treelife/Documents/YOSE sugar pine project/Code and data/Analyses_sugarpine/Data/US fire history/Fire severity/2013/mtbs_CA_2013.tif")
# 
# resmask = resample(newdata, rast2010)
# maskrast <- mask(resmask, projpila)
# plot(maskrast)
# 
# raster_list[i] <- maskrast
# 
# 
# 
# plot(maskrast)
# 
# tmap_mode("view")
# 
# projpila2 <- sf::st_as_sf(projpila)
# 
# pilafig <- tm_shape(projpila2)+
#   tmap_options(check.and.fix = TRUE)+
#   tm_polygons( style= "pretty",alpha=.8,
#               title="Pila",legend.show = TRUE)+
#   tm_fill(palette = "Dark2")
# pilafig
# 
# # firefig <- tm_shape(maskrast)+
# #   tm_raster()
# 
# firefig
# 
# pilafig+firefig
# 
# # for (i in seq(1:length(newdata))){
# #   file <- newdata[i]
# #   raster1 <- rast(file)
# #   projpila <- project(pila.shp, proj1)
# #   resmask = resample(raster1, rast2010)
# #   maskrast <- mask(resmask, projpila)
# #   raster_list[i] <- maskrast
# # }
# 
# rasterbrick <- rast(raster_list)
# 
# plot(rasterbrick)
# plot(raster_list[[21]])
# 
# rast_results <- do.call(merge, raster_list)
# plot(rast_results)
# 
# 
# #writeRaster(rasterbrick, "/Users/treelife/Documents/YOSE sugar pine project/Code and data/Analyses_sugarpine/Data/rasterbrickTRY.tif", overwrite=T)
# 
# 
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# #
# #
# #                                 Extracting fire data by plots
# #
# #
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 
# setwd("~/Documents/YOSE sugar pine project/Code and data/Analyses_sugarpine")
# 
# pilaplots <- vect("Data/pilaplots.shp")
# plot(pilaplots)
# 
# firesev <- extract(rasterbrick, pilaplots)
# 
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# # RIM and FERGUSON FIRES
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# yose_pila <- "Data/YOSE_PILA_Dist/YOSE_SugarPine_Dist_20210324.shp"
# pila.shp <- vect(yose_pila)
# proj = crs(pila.shp)
# 
# projpila <- project(pila.shp, readraster)
# 
# 
# ## great a for loop that crops the raster to the pila extent and then stacks the rasters
# files_sub = newdataframe[c(1:38),1]
# 
# listrast <- list()
# # 
# # for (file in files_sub){
# #   
# #     readraster <- rast(file) 
# #     cropraster <- crop(readraster, projpila)
# #     listrast <- c(listrast, cropraster)
# #     
# # }
# 
# plot(listrast[[30]])
# 
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# #                   Ferguson and Rim fires
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## 2018 = Ferguson
# ## 2013 = Rim
# fergfire <- rast("/Users/treelife/Documents/YOSE sugar pine project/Code and data/Analyses_sugarpine/Data/US fire history/Fire severity/2018/mtbs_CA_2018.tif")
# projferg = project(fergfire, proj)
# plot(projferg)
# 
# rimfire <- rast("/Users/treelife/Documents/YOSE sugar pine project/Code and data/Analyses_sugarpine/Data/US fire history/Fire severity/2013/mtbs_CA_2013.tif")
# projrim = project(rimfire, proj)
# plot(projrim)
# 
# 
# ferg <- mask(projferg, pila.shp)
# plot(ferg)
#  
# rim <- mask(projrim, pila.shp)
# plot(rim)
# 
# 
# #writeRaster(rim, "Data/RimFirePILA.tif")
# #writeRaster(ferg, "Data/FergusonFirePILA.tif")
# 
# 
# ## plotting
# 
# tmap_mode("view")
# 
# fergsf <- sf::st_as_sf(ferg)
# 
# tm_shape(rim, raster.downsample = TRUE)+
#   tm_raster(style= "pretty",
#             title="Rim Fire")
#   