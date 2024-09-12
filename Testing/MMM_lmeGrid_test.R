# Load necessary libraries (if not already loaded)
required_packages <- c("readxl", "dplyr", "purrr", "stringr", "lubridate", "tidyr", 
                       "forcats", "ggplot2", "cowplot", "gridExtra", "lme4", 
                       "lmerTest", "emmeans")

# Check if each package is installed, install if not, and load it
for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}

# Define color palette
groupCols <- c("#1e3791", "#8aacdb", "#f49620")

##################### PLOT ACTIVITY INDEX #####################

# Create a single plot for the entire data
plot <- ggplot(data_filtered_agg, aes(x = HalfHourElapsed, y = ActivityIndex, group = Group, color = Group)) +
  geom_path(stat = "summary") +
  stat_summary(aes(fill = Group), fun = mean,
               fun.min = function(x) mean(x) - sd(x), 
               fun.max = function(x) mean(x) + sd(x), 
               geom = "ribbon", 
               alpha = 0.2, colour = NA) +
  scale_color_manual(name = NULL, values = groupCols) + 
  scale_fill_manual(name = NULL, values = groupCols) +
  scale_x_continuous(breaks = seq(0, max(data_filtered_agg$HalfHourElapsed), by = 24), 
                     labels = seq(0, max(data_filtered_agg$HalfHourElapsed), by = 24)/2) +
  theme_minimal_hgrid(12, rel_small = 1) +
  labs(title = bquote(~bold(.(paste("Activity Index")))),
       y = "Activity [a.u.]", x = "Time elapsed [h]") +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        plot.subtitle = element_text(hjust = 0.5, face = "plain"),
        legend.position = "top",
        legend.justification = "right",
        legend.text = element_text(size = 9),
        legend.box.spacing = unit(1, "pt"),
        axis.text.x = element_text(vjust = 1, size = 11, hjust = 0.5))

# Display the single plot
print(plot)

##################### MIXED MODELS #####################

# Function to perform model, emmeans, and plotting for each phase
perform_analysis <- function(PhaseValue, data_filtered_agg) {
  data_filtered_agg_Phase_subset <- data_filtered_agg %>%
    filter(Phase == PhaseValue)
  
  data_filtered_agg_Phase_subset <- data_filtered_agg_Phase_subset %>%
    mutate(Group = ifelse(Group == "SIS" & SUS == FALSE, "RES", 
                          ifelse(Group == "SIS" & SUS == TRUE, "SUS", Group)))
  
  if (PhaseValue == "Active") {
    model_formula <- ActivityIndex ~ Group + ConsecActive + (1 | AnimalNum)
    emmeans_formula <- ~ Group + ConsecActive
    pairwise_by <- "ConsecActive"
  } else if (PhaseValue == "Inactive") {
    model_formula <- ActivityIndex ~ Group + ConsecInactive + (1 | AnimalNum)
    emmeans_formula <- ~ Group + ConsecInactive
    pairwise_by <- "ConsecInactive"
  }
  
  model <- lmerTest::lmer(model_formula, data = data_filtered_agg_Phase_subset)
  emmeans_obj <- emmeans(model, emmeans_formula, data = data_filtered_agg_Phase_subset, 
                         cov.reduce = FALSE, adjust = "sidak", pbkrtest.limit = 10000)
  
  model_results <- summary(model)
  emmeans_results <- summary(emmeans_obj)
  pairwise_results <- pairs(emmeans_obj, by = pairwise_by, adjust = "tukey")
  
  # Save model results to file
  mixed_model_results_file <- paste0("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/Activity/gridActivity/mixed_model_results_", PhaseValue, ".txt")
  writeLines(capture.output(model_results), mixed_model_results_file)
  
  # Save emmeans results to file
  emmeans_results_file <- paste0("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/Activity/gridActivity/emmeans_results_", PhaseValue, ".txt")
  writeLines(capture.output(emmeans_results), emmeans_results_file)
  
  # Perform emmip plot (interaction plot) and save as SVG
  if (PhaseValue == "Active") {
    plot <- emmip(model, Group ~ ConsecActive, at = list(ConsecActive = c(2, 3, 4, 5)), CIs = TRUE, style = "factor", pbkrtest.limit = 10000, dodge = 0.5, width = 1) + 
      theme_minimal_hgrid(12, rel_small = 1) +
      scale_color_manual(name = NULL, values = groupCols) + 
      scale_fill_manual(name = NULL, values = groupCols) +
      labs(title = bquote(~bold(.(paste("Grid Activity")))),
           y = "Linear Prediction", x = "Active Phase") +
      theme(plot.title = element_text(hjust = 0.5, face = "plain"),
            plot.subtitle = element_text(hjust = 0.5, face = "plain"),
            legend.position = "top",
            legend.justification = "right",
            legend.text = element_text(size = 9),
            legend.box.spacing = unit(1, "pt"),
            axis.text.x = element_text(vjust = 1, size = 11, hjust = 0.5)
      )
  } else if (PhaseValue == "Inactive") {
    plot <- emmip(model, Group ~ ConsecInactive, at = list(ConsecInactive = c(0, 1, 2, 3, 4, 5)), CIs = TRUE, style = "factor", pbkrtest.limit = 10000, dodge = 0.5, width = 1) + 
      theme_minimal_hgrid(12, rel_small = 1) +
      scale_color_manual(name = NULL, values = groupCols) + 
      scale_fill_manual(name = NULL, values = groupCols) +
      labs(title = bquote(~bold(.(paste("Grid Activity")))),
           y = "Linear Prediction", x = "Inactive Phase") +
      theme(plot.title = element_text(hjust = 0.5, face = "plain"),
            plot.subtitle = element_text(hjust = 0.5, face = "plain"),
            legend.position = "top",
            legend.justification = "right",
            legend.text = element_text(size = 9),
            legend.box.spacing = unit(1, "pt"),
            axis.text.x = element_text(vjust = 1, size = 11, hjust = 0.5)
      )
  }
  
  svg_file <- paste0("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/Activity/gridActivity/emmip_plot_", PhaseValue, ".svg")
  svg(svg_file)
  print(plot)
  dev.off()  # Close the SVG device
  
  # Save pairwise results to file
  pairwise_results_file <- paste0("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/Activity/gridActivity/pairwise_results_", PhaseValue, ".txt")
  writeLines(capture.output(pairwise_results), pairwise_results_file)
  
  # Return model and emmeans results
  return(list(model_results = model_results, emmeans_results = emmeans_results))
}

# Perform analysis for Active and Inactive phases
for (PhaseValue in c("Active", "Inactive")) {
  analysis_results <- perform_analysis(PhaseValue, data_filtered_agg)
  
  cat("Mixed-effects model results for", PhaseValue, ":\n")
  print(analysis_results$model_results)
  
  cat("emmeans results for", PhaseValue, ":\n")
  print(analysis_results$emmeans_results)
}
