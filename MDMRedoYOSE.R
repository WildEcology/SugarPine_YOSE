

library(tidyverse)
library(here)
library(librarian)



#Joan's code chunk part 1

theme_set(
  theme_bw(base_size = 17)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

if (!require("librarian")) install.packages("librarian")
librarian::shelf(tidyverse, readxl, viridis, patchwork)

#Michelle set working directory
setwd("/Users/michellemohr/Desktop/SugarPine_YOSE")
files <- list.files()
outdir <- "/Users/michellemohr/Desktop/SugarPine_YOSE"

# ## for loop to unzip files on your computer
# for (file in files) {
#   unzip(paste0(outdir,"/", file))
# }

## reading in all data
folders <- list.dirs(outdir)[-c(1,4)]

tree_list <- data.frame()

for (folder in folders) {
  newfiles = list.files(folder, pattern = "PILAdata")
  for(file in newfiles) {
    xlsfile <- read_excel(paste0(folder,"/",file), na = "NA") %>%
      mutate_if(is.numeric, as.numeric) %>% 
      dplyr::select(plotID, plot_type, treeNum,DBH_cm, pitchTubes, exitHoles, activeBranchCanker, inactiveBranchCanker, 
                    activeBoleCanker, inactiveBoleCanker, notes, plot_elevation_ft, trans_length, width)
    tree_list <- rbind(tree_list, xlsfile)
  }
}

#to rename the plot #s 

tree_list <- tree_list %>%
  mutate(plot_num = dense_rank(plotID))

tree_list <- tree_list[!is.na(tree_list$plotID), ]

# to create an infected column

tree_list$infected <- ifelse(tree_list$activeBoleCanker > 0 | tree_list$activeBranchCanker > 0 | tree_list$inactiveBoleCanker > 0 | tree_list$inactiveBranchCanker > 0, 1, 0)

tree_list$total_trees <- nrow(tree_list)

#to bring in associated trees data
folders <- list.dirs(outdir)[-c(1,4)]

assoc_list <- data.frame()

for (folder in folders) {
  newfiles = list.files(folder, pattern = "YPE_Treedata")
  for(file in newfiles) {
    xlsfile <- read_excel(paste0(folder,"/",file), na = "NA") %>%
      mutate_if(is.numeric, as.numeric) %>% 
      dplyr::select(plot, treeNum, DBH_cm, height_m, species, notes)
    assoc_list <- rbind(assoc_list, xlsfile)
  }
}


wpbr_plots <- tree_list %>%
  group_by(plot_num) %>%
  summarise(totalinfected = sum(infected))



plotlevel_incidence <- ggplot(data = wpbr_plots, aes(x = plot_num, y = totalinfected)) +
  geom_col() +
  labs(x = "Plot Number", y = "Total Infections", title = "Plot-Level Incidence of WPBR") +
  theme_bw()

plotlevel_incidence

plotlevel_incidence +
  scale_x_continuous(breaks = unique(wpbr_plots$plot_num[seq(1, length(unique(wpbr_plots$plot_num)), by = 1)]))


##############################
# How many plots had WPBR

plots_with_wpbr <- wpbr_plots %>%
  filter(totalinfected > 0) %>%
  distinct(plot_num)

# Count the number of unique plot numbers with WPBR
num_plots_with_wpbr <- nrow(plots_with_wpbr)

# Print the number of plots with WPBR
print(num_plots_with_wpbr)


#######
plots_with_wpbr <- sum(wpbr_plots$totalinfected > 0)
plots_without_wpbr <- nrow(wpbr_plots) - plots_with_wpbr
total_plots <- nrow(wpbr_plots)

# Create a dataframe for plotting
plot_data <- data.frame(
  wpbr_status = c("With WPBR", "Without WPBR"),
  num_plots = c(plots_with_wpbr, plots_without_wpbr),
  total_plots = total_plots)




wbpr_trees <- tree_list %>%
  group_by(infected) %>%
  summarise(total_infected_trees = n())

print(total_infected_trees)


ggplot(wbpr_trees, aes(x = factor(infected), y = total_infected_trees, fill = factor(infected))) +
  geom_bar(stat = "identity") +
  labs(x = "Infected", y = "Total Trees", fill = "Infected Status") +
  ggtitle("Total Infected and Non-Infected Trees")

wbpr_trees$infected <- factor(wbpr_trees$infected, levels = c(0, 1), labels = c("Not Infected", "Infected"))

# Create a simple bar graph with reordered x-axis
trees_infected_plot <- ggplot(wbpr_trees, aes(x = infected, y = total_infected_trees, fill = infected)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(drop = FALSE) +  # Prevents dropping unused levels
  labs(x = "Infection Status", y = "Total Trees", fill = "Infection Status") +
  ggtitle("Total Infected and Non-Infected Trees")

#####
# Filter out rows where 'infected' is NA
wbpr_trees_filtered <- wbpr_trees[!is.na(wbpr_trees$infected), ]

# Create the ggplot with filtered data
tree_WPBR <- ggplot(wbpr_trees_filtered, aes(x = factor(infected), y = total_infected_trees, fill = factor(infected))) +
  geom_bar(stat = "identity") +
  labs(x = "Infection Status", y = "Total Trees", fill = "Infected Status") +
  ggtitle("Number of Trees with WBPR vs. Without WPBR") +
  theme_minimal() +
  theme(legend.position = "none")
tree_WPBR

####HERHEE@####

trees_infected_plot

combined_plot_trees_infected <- plotlevel_incidence + trees_infected_plot

combined_plot_trees_infected



# infected individuals 
# DBH size classes (0-10, 10-20, etc.)

# columns are infected and DBH_cm, create a new size class column 

prop_infected <- tree_list %>%
  select(DBH_cm, infected)


# Define breaks and labels for size classes
breaks <- c(seq(0, 230, by = 10), Inf)
labels <- c(paste0(seq(0, 220, by = 10), "-", seq(10, 230, by = 10)), ">230")


##### END OF WORKING

# Create a new column size_class based on DBH_cm values
prop_infected$size_class <- cut(prop_infected$DBH_cm, breaks = breaks, labels = labels)
prop_infected <- na.omit(prop_infected)

# Plot the sizeclass population data
sizeclass_pop <- ggplot(prop_infected, aes(x = size_class)) +
  geom_bar(fill= "skyblue")+
  labs(x = "Size Class", y = "Count", title = "Size Class Distribution") +
  theme_minimal()

sizeclass_pop

# Plot the infected sizeclass data
sizeclass_infected <- ggplot(prop_infected, aes(x = size_class, y = infected)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Size Class", y = "Infected", title = "Infected Trees by Size Class") +
  theme_minimal()

sizeclass_infected

sizeclass_infected <- ggplot(prop_infected, aes(x = size_class, y = infected)) +
  geom_boxplot() +  # Change to boxplot
  labs(x = "Size Class", y = "Infected", title = "Infected Trees by Size Class") +
  theme_minimal()

sizeclass_infected

#create a new column called area which takes the trans_length column and multiplies it by width column 
tree_list$width <- ifelse(is.na(tree_list$width), 40, tree_list$width)
tree_list$width2 <- tree_list$width * 2
tree_list$area <- tree_list$trans_length * tree_list$width2

tree_list <- tree_list %>%
  group_by(plot_num) %>%
  mutate(trees_plot = n()) %>%
  ungroup()

tree_list$PILAdensity <- tree_list$trees_plot / tree_list$area

infected_by_plot <- tree_list %>%
  group_by(plot_num) %>%
  summarise(infection_plot = sum(infected))

# Merge the infected_by_plot dataframe with tree_list
tree_list <- merge(tree_list, infected_by_plot, by = "plot_num", all.x = TRUE)

#create incidence percentage

tree_list$incidence <- (tree_list$infection_plot / tree_list$trees_plot) * 100

PILAincidencepercent <- ggplot(tree_list, aes(x = PILAdensity, y = incidence)) +
  geom_point(color = "blue") +
  labs(x = "PILA Density", y = "Incidence %", title = "PILA Density vs. Incidence") +
  theme_minimal() +
  geom_smooth(method = "lm")

PILAincidencepercent


assoc_list <- assoc_list %>%
  mutate(plot_num = dense_rank(plot))

assoc_list <- assoc_list %>%
  group_by(plot_num) %>%
  mutate(total_PandA_trees_plot = n()) %>%
  ungroup()

mergedtrees_df <- merge(tree_list, assoc_list, by = "plot_num")

mergedtrees_df$treedensity <- mergedtrees_df$total_PandA_trees_plot / mergedtrees_df$area

ggplot(mergedtrees_df, aes(x = treedensity, y = infection_plot)) +
  geom_point(color = "blue") +
  labs(x = "Tree Density", y = "Incidence", title = "Tree Density vs. Incidence") +
  theme_minimal() +
  geom_smooth(method = "lm")

Associateincidencepercent <- ggplot(mergedtrees_df, aes(x = treedensity, y = incidence)) +
  geom_point(color = "blue") +
  labs(x = "Tree Density", y = "Incidence %", title = "Tree Density vs. Incidence") +
  theme_minimal() +
  geom_smooth(method = "lm")

library(cowplot)

# Assuming plot1 and plot2 are your individual ggplot objects

# Combine the two plots into a single plot grid
combined_tree_incidence_plot <- plot_grid(PILAincidencepercent, Associateincidencepercent, labels = c("A", "B"), ncol = 2)

# Print the combined plot
print(combined_tree_incidence_plot)



# Count the number of unique plots for each plot type
plot_type_counts <- tree_list %>%
  distinct(plot_num, plot_type) %>%
  count(plot_type)

# Display the counts
print(plot_type_counts)

plot_type_counts <- tree_list %>%
  filter(!is.na(plot_type)) %>%  # Remove rows with NA values in plot_type
  distinct(plot_num, plot_type) %>%
  count(plot_type)

# Plot the counts
ggplot(plot_type_counts, aes(x = plot_type, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Adjust fill color as needed
  labs(title = "Number of Unique Plots for Each Plot Type",
       x = "Plot Type",
       y = "Number of Unique Plots") +
  theme_minimal()

# Combine observations with different spellings or capitalizations
tree_list <- tree_list %>%
  mutate(plot_type = recode(plot_type, "highseverity" = "high severity"))

# Count the number of unique plots for each plot type
plot_type_counts <- tree_list %>%
  filter(!is.na(plot_type)) %>%  # Remove rows with NA values in plot_type
  distinct(plot_num, plot_type) %>%
  count(plot_type)

# Print the counts
print(plot_type_counts)

ggplot(plot_type_counts, aes(x = plot_type, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Adjust fill color as needed
  labs(title = "Number of Unique Plots for Each Plot Type",
       x = "Plot Type",
       y = "Number of Unique Plots") +
  theme_minimal()

ggplot(plot_type_counts, aes(x = plot_type, y = n, fill = plot_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Random vs. High Severity Plots",
       x = "Plot Type",
       y = "Number of Plots",
       fill = "Plot Type") +
  scale_fill_manual(values = c("lightcoral", "skyblue")) +  # Set fill colors
  theme_minimal()




#Jenny's beetle code
tree_list$pt <- ifelse(tree_list$pitchTubes == "N", 0, 1)
tree_list$eh <- ifelse(tree_list$exitHoles == "N", 0, 1)

tree_list <- tree_list %>% 
  mutate(beetles = ifelse(pt|eh>0,1,0))

####HERE@@@@###
ggplot(tree_list, aes(x = factor(plot_num), y = beetles)) +
  geom_col() + 
  labs(title = "Number of Plots with Beetle Attacks",
       x = "Plot Number",
       y = "Number of Trees with Beetles") +
  theme_bw() +
  scale_x_discrete(breaks = seq(min(tree_list$plot_num), max(tree_list$plot_num), by = 4)) +
  theme_minimal()


#####

# Calculate the number of trees with beetles and without beetles
beetle_counts <- tree_list %>%
  count(beetles)

ggplot(beetle_counts, aes(x = factor(beetles), y = n, fill = factor(beetles))) +
  geom_col(stat = "identity") +  # No need to specify fill in geom_bar
  labs(title = "Number of Trees with Beetles vs. Without Beetles",
       x = "Beetles",
       y = "Number of Trees with Beetles",
       fill = "MPB Attack") +
  scale_x_discrete(labels = c("No Beetles", "With Beetles")) +  # Adjust x-axis labels
  scale_fill_manual(values = c("lightcoral", "skyblue")) +  # Define fill colors
  theme_minimal()

####
# Filter out rows with NA values in the 'beetles' column
beetle_counts_filtered <- beetle_counts[!is.na(beetle_counts$beetles), ]

# Create the ggplot with filtered data
ggplot(beetle_counts_filtered, aes(x = factor(beetles), y = n, fill = factor(beetles))) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Trees with Beetles vs. Without Beetles",
       x = "Beetles",
       y = "Number of Trees with Beetles",
       fill = "MPB Attack") +
  scale_x_discrete(labels = c("No Beetles", "With Beetles")) +
  scale_fill_manual(values = c("lightcoral", "skyblue")) +
  theme_minimal()+
  theme(legend.position = "none")

#####

ggplot(beetle_counts, aes(x = factor(beetles), y = n, fill = factor(beetles))) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Trees with Beetles vs. Without Beetles",
       x = "Beetles",
       y = "Number of Trees",
       fill = "MPB Attack") +  # Set legend title
  scale_x_discrete(labels = c("Without Beetles", "With Beetles")) +  # Adjust x-axis labels
  scale_fill_manual(values = c("Without Beetles" = "lightcoral", "With Beetles" = "skyblue")) +  # Define fill colors
  theme_minimal()

summarydatbeetles <- tree_list %>% 
  group_by(plotID) %>% 
  summarize(numtrees = length(unique(treeNum, na.rm=T)), sumbeetles = sum(beetles),
            per_beetles = (sumbeetles/numtrees)*100) %>% 
  na.omit() %>% 
  ungroup() 

perbeetles <- summarydatbeetles %>% 
  summarize(perbeetles = mean(per_beetles))
perbeetles <- summarydatbeetles %>% 
  summarize(perbeetles = (per_beetles))


###
# Calculate the number of unique plots where beetles were observed
plots_with_beetles <- tree_list %>%
  filter(beetles == 1) %>%  # Filter rows where beetles were observed
  distinct(plot_num) %>%    # Select distinct plot numbers
  n()                       # Count the number of distinct plot numbers

# Print the number of plots with beetles
print(plots_with_beetles)


plots_with_beetles <- tree_list %>%
  filter(beetles == 1) %>%  # Filter rows where beetles were observed
  summarise(plots_with_beetles = n_distinct(plot_num))  # Count the number of distinct plot numbers

# Print the number of plots with beetles
print(plots_with_beetles)
#need to add strata data for this code

# Calculate the total number of trees that had beetles
total_trees_with_beetles <- sum(tree_list$beetles)

# Print the total number of trees with beetles
print(total_trees_with_beetles)


missing_values <- sum(is.na(tree_list$beetles))

if (missing_values > 0) {
  # If there are missing values, handle them
  tree_list <- na.omit(tree_list)  # Remove rows with missing values
}

# Calculate the total number of trees that had beetles
total_trees_with_beetles <- sum(tree_list$beetles)




# perfigb <- summarydatbeetles %>% 
#   ggplot(aes(x=strata, y=per_beetles, color = "beetles", fill="beetles"))+
#   geom_jitter(height = 0.02, width = 0.05)+ geom_smooth(method = "lm")+
#   ylab("Beetle Extent (%)") + xlab("Climate Strata") + 

#   #scale_x_discrete(guide = guide_axis(angle = 90)) +
#   scale_color_viridis(discrete = TRUE, option = "D")+
#   scale_fill_viridis(discrete = TRUE) +
#   guides(fill=F, color=F)
# 
# # display summary results
# perfigb

##### Climate
climatedata <- read_csv(here("updatedPRISMdata.csv"))
climatedata$plot_num <- climatedata$PlotID

tree_list_climatedata <- merge(tree_list, climatedata, by = "plot_num")

ggplot(tree_list_climatedata, aes(x = vpdmax, y = incidence)) +
  geom_point()+
  geom_smooth(method = "lm") +
  labs(x = "VPD Max", y = "Incidence %") +  # Labels for axes
  ggtitle("Relationship between VPD Max and Incidence") +
theme_minimal()


#SIZE CLASS
# Plot the sizeclass population data
sizeclass_beetles <- ggplot(tree_list, aes(x = size_class, y = beetles)) +
  geom_bar(fill= "skyblue")+
  labs(x = "Size Class", y = "Count", title = "Size Class Distribution") +
  theme_minimal()

sizeclass_beetles

# Plot the infected sizeclass data
sizeclass_infected <- ggplot(beetle_counts, aes(x = size_class, y = infected)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Size Class", y = "Infected", title = "Infected Trees by Size Class") +
  theme_minimal()

sizeclass_infected

ggplot(tree_list, aes(x = size_class, fill = factor(beetles))) +
  geom_col(position = "stack") +
  labs(x = "Size Class", y = "Count of Beetles", fill = "Beetles") +
  ggtitle("Count of Beetles by Size Class")

# Filter the data to include only rows where beetles = 1
filtered_data <- subset(tree_list, beetles == 1)

# Plot the count of beetles (where beetles = 1) by size class
ggplot(filtered_data, aes(x = size_class)) +
  geom_bar() +
  labs(x = "Size Class", y = "Count of Beetles", title = "Count of Beetles by Size Class (Beetles = 1)")

# Filter the data to include only rows where beetles = 1
filtered_data <- subset(tree_list, beetles == 1)

# Specify the order of size classes
size_class_order <- c("0-5", "5-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100", "100-110", "110-120", "120-130", "130-140", "140-150", "150-160", "160-170", "170-180", "180-190", "190-200", "200-210", "210-220", "220-230")  # Add more if needed

# Convert size_class to a factor with specified levels and order
filtered_data$size_class <- factor(filtered_data$size_class, levels = size_class_order)

# Plot the count of beetles (where beetles = 1) by size class
ggplot(filtered_data, aes(x = size_class)) +
  geom_bar() +
  labs(x = "Size Class", y = "Count of Beetles", title = "Beetles by Size Class")  +
  theme_minimal()


