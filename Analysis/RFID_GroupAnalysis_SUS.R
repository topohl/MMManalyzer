# This R code runs ANOVA for group comparisons and creates visual plots.
# It loads libraries, splits data by cage change, and generates plots.
# Mixed-effects models are fitted, results saved in text files, and summaries printed.
# Finally, it arranges plots in a grid, offering insights into group activities.

# Load essential libraries for data analysis and visualization.
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(lme4)        # For mixed-effects models
library(lmerTest)   # For p-values

# Split data into separate data frames for each cage change and sex.
data_filtered_agg_Change_Sex <- data_filtered_agg %>% 
  split(list(data_filtered_agg$Change, data_filtered_agg$Sex))

# Create plots for each cage change and sex.
plot_list <- lapply(sort(unique(data_filtered_agg$Change)), function(Change) {
  lapply(sort(unique(data_filtered_agg$Sex)), function(Sex) {
    # Subset data for the current cage change and sex.
    data_filtered_agg_Change_Sex_subset <- data_filtered_agg_Change_Sex[[c(Change, Sex)]]
    
    # Categorize "SIS" group into "RES" and "SUS" based on the "SUS" column.
    data_filtered_agg_Change_Sex_subset <- data_filtered_agg_Change_Sex_subset %>%
      mutate(Group = ifelse(Group == "SIS" & SUS == FALSE, "RES", 
                            ifelse(Group == "SIS" & SUS == TRUE, "SUS", Group)))
    
    # Create and customize the plot using ggplot2.
    ggplot(data_filtered_agg_Change_Sex_subset, aes(x = HalfHourElapsed, y = ActivityIndex, group = Group, color = Group)) +
      geom_path(stat = "summary") +
      stat_summary(aes(fill = Group), fun = mean,
                   fun.min = function(x) mean(x) - sd(x), 
                   fun.max = function(x) mean(x) + sd(x), 
                   geom = "ribbon", 
                   alpha = 0.2, colour = NA) +
      
      # Set color and fill scales.
      scale_color_manual(name = NULL, values = c("#1e3791", "#8aacdb", "#f49620")) + 
      scale_fill_manual(name = NULL, values = c("#1e3791", "#8aacdb", "#f49620")) +
      
      # Customize the x-axis, labels, and other plot aesthetics.
      scale_x_continuous(breaks = seq(0, max(data_filtered_agg_Change_Sex_subset$HalfHourElapsed), by = 24), 
                         labels = seq(0, max(data_filtered_agg_Change_Sex_subset$HalfHourElapsed), by = 24)/2) +
      theme_minimal_hgrid(12, rel_small = 1) +
      labs(title = bquote(~bold(.(paste("Activity Index")))),
           y = "Activity [a.u.]", x = "Time elapsed [h]", caption = paste(Change, "-", Sex)) +
      theme(plot.title = element_text(hjust = 0.5, face = "plain"),
            plot.subtitle = element_text(hjust = 0.5, face = "plain"),
            legend.position = "top",
            legend.justification = "right",
            legend.text = element_text(size = 9),
            legend.box.spacing = unit(1, "pt"),
            axis.text.x = element_text(vjust = 1, size = 11, hjust = 0.5))
  })
})

# Perform mixed-effects models.
# Create a list to store model results.
model_results <- list()

# Loop through each cage change and sex.
for (Change in sort(unique(data_filtered_agg$Change))) {
  for (Sex in sort(unique(data_filtered_agg$Sex))) {
    # Subset data for the current cage change and sex.
    data_filtered_agg_Change_Sex_subset <- data_filtered_agg_Change_Sex[[c(Change, Sex)]]
    
    # Categorize "SIS" group into "RES" and "SUS" based on the "SUS" column.
    data_filtered_agg_Change_Sex_subset <- data_filtered_agg_Change_Sex_subset %>%
      mutate(Group = ifelse(Group == "SIS" & SUS == FALSE, "RES", 
                            ifelse(Group == "SIS" & SUS == TRUE, "SUS", Group)))
    
    # Fit a mixed-effects model (lmer) to the data.
    model <- lmerTest::lmer(ActivityIndex ~ Group + HalfHourElapsed + (1 | AnimalNum), data = data_filtered_agg_Change_Sex_subset)
    model_results[[paste(Change, "-", Sex)]] <- model
    
    # Save the mixed-effects model summary as a text file for each cage change and sex.
    mixed_model_results_file <- paste0("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/Activity/mixed_model_results_", Change, "_", Sex, ".txt")
    model_summary <- capture.output(summary(model))
    writeLines(model_summary, con = mixed_model_results_file)
    
    # Print the model summary.
    cat("Mixed-effects model results for", Change, "-", Sex, ":\n")
    print(summary(model))
  }
}

# Arrange the generated plots in a grid.
grid.arrange(grobs = plot_list, ncol = 2, top = "Activity Index by Cage Change and Sex")
