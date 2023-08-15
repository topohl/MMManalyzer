# DTW Analysis Script
# this script uses data_filtered_agg provided by the MMM_process.R script

# Load required libraries
library(dplyr)
library(dtw)

# Load your dataset
data_filtered_agg <- read.csv("your_data_file.csv")

# Define the folder to store the result files
results_folder <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/Activity/"

# List of groups to compare
groups_to_compare <- c("CON", "RES", "SUS")

# Prepare a list to store DTW distances between groups
dtw_distances <- list()

# Loop through pairs of groups and calculate DTW distances
for (i in 1:(length(groups_to_compare) - 1)) {
  group1_data <- data_filtered_agg %>% filter(Group == groups_to_compare[i])
  for (j in (i + 1):length(groups_to_compare)) {
    group2_data <- data_filtered_agg %>% filter(Group == groups_to_compare[j])
    
    # Loop through pairs of time series and calculate DTW distances
    dtw_distances[[paste(groups_to_compare[i], "-", groups_to_compare[j])]] <- matrix(NA, nrow(group1_data), nrow(group2_data))
    
    for (k in 1:nrow(group1_data)) {
      ts1 <- group1_data[k, ]
      for (l in 1:nrow(group2_data)) {
        ts2 <- group2_data[l, ]
        dtw_result <- dtw(ts1$ActivityIndex, ts2$ActivityIndex)
        dtw_distances[[paste(groups_to_compare[i], "-", groups_to_compare[j])]][k, l] <- dtw_result$distance
      }
    }
  }
}

# Visualize DTW distances
for (groups_pair in names(dtw_distances)) {
  dtw_matrix <- dtw_distances[[groups_pair]]
  heatmap(dtw_matrix, 
          Rowv = NA, Colv = NA,
          col = colorRampPalette(c("white", "blue"))(100),
          main = paste("DTW Distances between", groups_pair))
}

# Save the plots to a PDF file
pdf(file.path(results_folder, "dtw_plots.pdf"))
print(plot)  # Replace 'plot' with the actual plot name if you're using individual plotting commands
dev.off()
