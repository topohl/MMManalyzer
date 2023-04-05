## Plot graphs for group changes in separate graphs and group the plots together
#  Plots data in line plot and 2x2 grid with a separate plot for each change
#  Groups CON and SIS are grouped and the mean and SD is plotted
#  Time (y axis) is plotted as the number of half hour intervals starting from the first active phase

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(forcats)
library(cowplot)

group_cols <- c("#1e3791", "#00ac8c")

# Split data into separate data frames for each CC
data_filtered_agg_Change <- data_filtered_agg %>% 
  split(data_filtered_agg$Change)

# Create plots for each CC
plot_list <- lapply(sort(unique(data_filtered_agg$Change)), function(Change) {
  
  # Subset data for the current CC
  data_filtered_agg_Change_subset <- data_filtered_agg_Change[[Change]]
  
  # Create the plot
  ggplot(data_filtered_agg_Change_subset, aes(x = HalfHourElapsed, y = ActivityIndex, group = Group, color = Group)) +
    geom_path(stat = "summary") +
    stat_summary(aes(fill = Group), fun = mean,
                 fun.min = function(x) mean(x) - sd(x), 
                 fun.max = function(x) mean(x) + sd(x), 
                 geom = "ribbon", 
                 alpha = 0.2, colour = NA) +
    # Set color and fill scales
    scale_color_manual(name = NULL, values = group_cols) + 
    scale_fill_manual(name = NULL, values = group_cols) +
    theme_minimal_hgrid(12, rel_small = 1) +
    labs(title = bquote(~bold(.(paste("Activity Index")))),
         y = "Activity [a.u.]", x = "Day/Night", caption = "", subtitle = Change) +
    theme(plot.title = element_text(hjust = 0.5, face = "plain"),
          plot.subtitle = element_text(hjust = 0.5, face = "plain"),
          legend.position = "top",
          legend.justification = "right",
          legend.text = element_text(size = 9),
          legend.box.spacing = unit(1, "pt"),
          axis.text.x = element_text(angle = 45, vjust = 1, size = 11, hjust = 1),
          axis.title.x = element_blank())
  })

# Arrange plots in a grid
grid.arrange(grobs = plot_list, ncol = 2, top = "Activity Index by Cage Change")
