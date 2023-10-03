

#install.packages("Require")
if (!require("librarian")) install.packages("librarian")

## this code will load package or download packages that are not yet loaded 
librarian::shelf(tidyverse, curl, terra, tmap, sf, sp, tmaptools, rsconnect, data.table)

## ggplot theme
theme_set(
  theme_bw(base_size = 17)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)


firedat <- read_csv("Data/fireseveritydata.csv") 


fire_more <- firedat %>% 
  group_by(plot, year) %>% 
  summarise(firesev = ifelse(value>0, value, 0)) %>% 
  na.omit()

length(fire_more$plot)

## 53/64 burned

highsev <- filter(fire_more, firesev>3)
length(unique(highsev$plot)) ## 11 high severity (4 or more); mostly rim and ferguson fires

lowersev <- filter(fire_more, firesev>1&firesev<4)
length(unique(lowersev$plot)) ##23 plots burned with med sev


lowestsev <- filter(fire_more, firesev==1)
length(unique(lowestsev$plot)) #16 plots burned with 1


ggplot(firedat, aes(x = plot, y = value, fill=factor(year)))+
  geom_bar(stat="identity", position = "stack")




countfire <- firedat_0 %>% 
  group_by(plot) %>% 
  na.omit()
  summarize(firecounts = ifelse(value==0, 0, sum(value))) %>% 
  distinct(plot, firecounts)
