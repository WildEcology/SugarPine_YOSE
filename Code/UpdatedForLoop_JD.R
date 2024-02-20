

## reading in all data
folders <- list.dirs(outdir)[-c(1,4)]

tree_list <- data.frame()

for (folder in folders) {
  newfiles = list.files(folder, pattern = "PILAdata")
  for(file in newfiles) {
    xlsfile <- read_excel(paste0(folder,"/",file, na = "NA")) %>%
      mutate_if(is.numeric, as.numeric) %>% 
      dplyr::select(plotID, treeNum,DBH_cm, pitchTubes, exitHoles, activeBranchCanker, inactiveBranchCanker, 
                    activeBoleCanker, inactiveBoleCanker, notes, plot_elevation_m)
    tree_list <- rbind(tree_list, xlsfile)
  }
}