
## ---------------------------
##
## Script name: WPBR severity
##
## Author: Dr. Joan Dudney
##
## Date Created: 2024-04-26
##
## Copyright (c) Joan Dudney, 2024
## Email: dudney@ucsb.edu
##
## ---------------------------
##
## Notes: example code that estimates severity for 
##   sugar pine trees using a modified Duriscoe severity rating
##        cs + (100 - dbh)/5
#             if dbh > 100, then dbh == 0
## ---------------------------

## packages
if (!require("librarian")) install.packages("librarian")
librarian::shelf(tidyverse, tidylog, purrr)


## variables
bole = sample(0:3, 20, replace = T)
branch = sample(0:7, 20, replace = T)
dbh <- sample(1:150, 20, replace=T)
treeID <- seq(1:20)

## create dataframe and estimate cs
blist_dat <- data.frame(bole = bole,
                       branch = branch,
                       dbh = dbh,
                       treeID = treeID) %>% 
  mutate(cs = ifelse (bole == 0 & branch < 3, 1,
                      ifelse (bole == 0 & branch > 2, 2,
                              ifelse (bole == 1, 3,
                                      ifelse (bole > 1, 4, 0)))))


## function that calculate severity 
## could easily do this with an ifelse statement too

severity_fun = function(cs, dbh) {
  if (dbh <= 100) {
    severity = (cs + (100-dbh))/5
    
  }
  else 
    severity = (cs/5)
}


## severity calculation

blister_severity <- blist_dat %>%
  mutate(severity = map2_dbl(cs, dbh, severity_fun))

