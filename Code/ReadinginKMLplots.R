

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


getwd()
plots <- read_sf("Final Products/For Jenny/SlicedPlotsJune14_nocliffs.kml")

# Watch data
plots %>%
  glimpse()

plot(plots)

librarian::shelf(mapview)

# See map
mapview(plots)

