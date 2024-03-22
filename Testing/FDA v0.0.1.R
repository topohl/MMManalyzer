# FDA Analysis Script

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(fda)

# Load your dataset
data_filtered_agg <- read.csv("your_data_file.csv")

# Define the folder to store the result files
results_folder <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/Activity/"

# Prepare functional data objects for each group
fd_objects <- lapply(unique(data_filtered_agg$Group), function(group) {
  group_data <- data_filtered_agg %>% filter(Group == group)
  group_fd <- smooth.basis(group_data$HalfHourElapsed, group_data$ActivityIndex, 
                           fdParobj = create.bspline.basis(c(1, max(group_data$HalfHourElapsed)), nbasis = 10))
  return(group_fd)
})

# Plot functional data objects
plot_list_fda <- lapply(fd_objects, function(fd_obj) {
  plotfit.fd(fd_obj, ylim = range(data_filtered_agg$ActivityIndex))
})

# Arrange and display all plots
grid_arrange_fda <- grid.arrange(grobs = plot_list_fda, ncol = 2, top = "Functional Data Analysis")

# Save the plots to a PDF file
pdf(file.path(results_folder, "fda_plots.pdf"))
print(grid_arrange_fda)
dev.off()
