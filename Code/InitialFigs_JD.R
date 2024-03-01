## ---------------------------
##
## Script name: preliminary plots
##
## Author: Dr. Joan Dudney
##
## Date Created: 2024-02-29
##
## Copyright (c) Joan Dudney, 2024
## Email: dudney@ucsb.edu
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

library(tidyverse)

dat <- read_csv("Data/tree_list_climatedata.csv")

# data viz
hist(dat$DBH_cm)
max(dat$DBH_cm)

## classify size class
size_class <- dat %>% 
  mutate(sizeclass = ifelse(DBH_cm <= 5, "0-5",
                            ifelse(DBH_cm > 5 & DBH_cm <=  10, "5.1-10",
                                   ifelse(DBH_cm > 10.1 & DBH_cm <= 30, "10.1-30",
                                          ifelse(DBH_cm > 30 & DBH_cm <= 60, "30.1-60",
                                                 ifelse(DBH_cm > 60 & DBH_cm <= 90, "60.1-90", "90.1-226"))))))
plotcheck <- size_class %>% 
  mutate(uniquetreeid = 1:length(treeNum)) %>% 
  group_by(sizeclass, plotID) %>% 
  summarize(inf = sum(infected), totaltrees = length(uniquetreeid))

## first two figs
totalinf <- size_class %>% 
  ggplot(aes(x = sizeclass, y = infected, fill = "sizeclass"))+
  geom_col(width = .8)+
  scale_fill_manual(values="#084f63")+
  scale_x_discrete(limits = c("0-5", "5.1-10", "10.1-30", "30.1-60", "60.1-90", "90.1-226"))+
  xlab("Size class (cm)")+
  ylab("# WPBR infections")+
  guides(fill=F)+
  ylim(0,11)

totalinf

perinf <- size_class %>% 
  mutate(uniquetreeid = 1:length(treeNum)) %>% 
  group_by(sizeclass) %>% 
  summarize(perinf = (sum(infected)/length(uniquetreeid))*100) %>% 
  ggplot(aes(x = sizeclass, y = perinf, fill = "sizeclass"))+
  geom_col(width = .8)+
  scale_fill_manual(values="#084f63")+
  scale_x_discrete(limits = c("0-5", "5.1-10", "10.1-30", "30.1-60", "60.1-90", "90.1-226"))+
  xlab("Size class (cm)")+
  ylab("WPBR incidence (% infected)")+
  guides(fill=F)+
  ylim(0,8)

perinf



