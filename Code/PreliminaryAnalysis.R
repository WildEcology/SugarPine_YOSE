
## ---------------------------
##
## Script name: 
##
## Author: Dr. Joan Dudney
##
## Date Created: 2023-07-27
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

theme_set(
  theme_bw(base_size = 17)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)


librarian::shelf(tidyverse, readxl, viridis, patchwork)

setwd("/Users/treelife/Documents/YOSE sugar pine project/Code and data/Analyses_sugarpine/Data/YPE_Data")
files <- list.files()
outdir <- "/Users/treelife/Documents/YOSE sugar pine project/Code and data/Analyses_sugarpine/Data/YPE_Data"

# ## for loop to unzip files
# for (file in files) {
#   unzip(paste0(outdir,"/", file))
# }

help(list.files)
## reading in all data
folders <- list.dirs(outdir)[-c(1,4)]

tree_list <- data.frame()

for (folder in folders) {
  newfiles = list.files(folder, pattern = "PILAdata")
    for(file in newfiles) {
      xlsfile <- read_excel(paste0(folder,"/",file)) %>% 
        dplyr::select(plotID, treeNum,DBH_cm, pitchTubes, exitHoles, activeBranchCanker, inactiveBranchCanker, 
                      activeBoleCanker, inactiveBoleCanker, notes, plot_elevation_m)
      tree_list <- rbind(tree_list, xlsfile)
  }
}


## summarizing data
piladat <- tree_list %>% 
  mutate(bole_canker = activeBoleCanker+ inactiveBoleCanker,
         branch_canker = activeBranchCanker + inactiveBranchCanker) %>% 
  mutate(infected = ifelse(bole_canker|branch_canker>0,1,0))

piladat %>% 
  ggplot(aes(y=branch_canker, fill=branch_canker))+
  geom_bar()

summarydat <- piladat %>% 
  rename(elevation = plot_elevation_m) %>% 
  group_by(plotID, elevation) %>% 
  summarize(numtrees = length(unique(treeNum, na.rm=T)), suminf = sum(infected),
            per_infected = (suminf/numtrees)*100) %>% 
  na.omit() %>% 
  ungroup() %>% 
  mutate(plot = row_number(), plot2 = as.character(plot))

perinf <- summarydat %>% 
  summarize(perin = mean(per_infected))

perfig <- summarydat %>% 
  ggplot(aes(x=elevation, y=per_infected, color = "infected", fill="infected"))+
  geom_point()+ geom_smooth(method="lm")+
  ylab("% infected") + xlab("Elevation (m)")+
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  guides(fill=F, color=F)

perfig

numfig <- summarydat %>%
  ggplot(aes(x=plot, y=per_infected, color = plot, fill=plot))+
  geom_bar(stat = "identity")+
  ylab("% infected") + xlab("Plot")+
  scale_color_viridis()+
  scale_fill_viridis()+
  #scale_color_brewer(palette = "Dark2") +
  #scale_fill_brewer(palette = "Dark2")+
  guides(fill=F, color=F)+
  ylim(0,10)

numfig + perfig

3/5


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#
#                                    Text
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
librarian::shelf(BiodiversityR, ggsci, readxl, tidyverse)
data(warcom)
str(warcom)
data(warenv)
summary(warenv)

Accum.1 <- accumcomp(warcom, y=warenv, factor='population', 
                     method='exact', conditioned=FALSE, plotit=FALSE)
Accum.1


accum.long1 <- accumcomp.long(Accum.1, ci=NA, label.freq=5)
head(accum.long1)

plotgg1 <- ggplot(data=accum.long1, aes(x = Sites, y = Richness, ymax = UPR, ymin = LWR)) + 
  scale_x_continuous(expand=c(0, 1), sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  geom_line(aes(colour=Grouping), size=2) +
  geom_point(data=subset(accum.long1, labelit==TRUE), 
             aes(colour=Grouping, shape=Grouping), size=5) +
  geom_ribbon(aes(colour=Grouping), alpha=0.2, show.legend=FALSE) + 
  scale_colour_npg() +
  labs(x = "Trees", y = "Loci", colour = "Population", shape = "Population")



# % decline
average
