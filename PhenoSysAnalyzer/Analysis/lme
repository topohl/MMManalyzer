## Mixed models analysis to analyse both the active and inactive phase
#  compares susceptible and resilient groups to controls
#  will create results text files in defined folder

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(lme4)
library(lmerTest)

# Define the folder to store the result files
results_folder <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/Activity/"

# Split data into separate data frames for each CC
data_filtered_agg_Change <- data_filtered_agg %>% 
  split(data_filtered_agg$Change)

# Create plots for each CC
plot_list <- lapply(sort(unique(data_filtered_agg$Change)), function(Change) {
  # Subset data for the current CC
  data_filtered_agg_Change_subset <- data_filtered_agg_Change[[Change]]
  
  # Split "SIS" group into "RES" and "SUS" based on "SUS" column
  data_filtered_agg_Change_subset <- data_filtered_agg_Change_subset %>%
    mutate(Group = ifelse(Group == "SIS" & SUS == FALSE, "RES", 
                          ifelse(Group == "SIS" & SUS == TRUE, "SUS", Group)))
  
  # Create the plot
  ggplot(data_filtered_agg_Change_subset, aes(x = HalfHourElapsed, y = ActivityIndex, group = Group, color = Group)) +
    geom_path(stat = "summary") +
    stat_summary(aes(fill = Group), fun = mean,
                 fun.min = function(x) mean(x) - sd(x), 
                 fun.max = function(x) mean(x) + sd(x), 
                 geom = "ribbon", 
                 alpha = 0.2, colour = NA) +
    scale_color_manual(name = NULL, values = c("#1e3791", "#8aacdb", "#f49620")) + 
    scale_fill_manual(name = NULL, values = c("#1e3791", "#8aacdb", "#f49620")) +
    scale_x_continuous(breaks = seq(0, max(data_filtered_agg_Change_subset$HalfHourElapsed), by = 24), 
                       labels = seq(0, max(data_filtered_agg_Change_subset$HalfHourElapsed), by = 24)/2) +
    theme_minimal_hgrid(12, rel_small = 1) +
    labs(title = bquote(~bold(.(paste("Activity Index")))),
         y = "Activity [a.u.]", x = "Time elapsed [h]", caption = Change) +
    theme(plot.title = element_text(hjust = 0.5, face = "plain"),
          plot.subtitle = element_text(hjust = 0.5, face = "plain"),
          legend.position = "top",
          legend.justification = "right",
          legend.text = element_text(size = 9),
          legend.box.spacing = unit(1, "pt"),
          axis.text.x = element_text(vjust = 1, size = 11, hjust = 0.5))
})

# Perform mixed-effects models
model_results <- list()

for (Change in sort(unique(data_filtered_agg$Change))) {
  for (PhaseValue in c("Active", "Inactive")) {
    data_filtered_agg_Change_Phase_subset <- data_filtered_agg_Change[[Change]] %>%
      filter(Phase == PhaseValue)
    
    data_filtered_agg_Change_Phase_subset <- data_filtered_agg_Change_Phase_subset %>%
      mutate(Group = ifelse(Group == "SIS" & SUS == FALSE, "RES", 
                            ifelse(Group == "SIS" & SUS == TRUE, "SUS", Group)))
    
    model <- lmerTest::lmer(ActivityIndex ~ Group + HalfHourElapsed + (1 | AnimalNum), data = data_filtered_agg_Change_Phase_subset)
    model_results[[paste0(Change, "_", PhaseValue)]] <- model
    
    mixed_model_results_file <- paste0(results_folder, "mixed_model_results_", Change, "_", PhaseValue, ".txt")
    model_summary <- capture.output(summary(model))
    writeLines(model_summary, con = mixed_model_results_file)
    
    cat("Mixed-effects model results for", Change, "-", PhaseValue, ":\n")
    print(summary(model))
  }
}

grid.arrange(grobs = plot_list, ncol = 2, top = "Activity Index by Cage Change")
