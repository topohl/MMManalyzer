library(ggplot2)
library(emmeans)
library(lmerTest)

activeToCompare <-
inactiveToCompare <- "2", "3"

# Create an empty list to store the models and results
model_results <- list()

for (PhaseValue in c("Active", "Inactive")) {
  data_filtered_agg_Phase_subset <- data_filtered_agg %>%
    filter(Phase == PhaseValue)
  
  data_filtered_agg_Phase_subset <- data_filtered_agg_Phase_subset %>%
    mutate(Group = ifelse(Group == "SIS" & SUS == FALSE, "RES", 
                          ifelse(Group == "SIS" & SUS == TRUE, "SUS", Group)))
  
  if (PhaseValue == "Active") {
    # Filter for ConsecActive levels 2, 3, 4, and 5
    data_filtered_agg_Phase_subset <- data_filtered_agg_Phase_subset %>%
      filter(ConsecActive %in% c(2, 3, 4, 5))
    
    model <- lmerTest::lmer(ActivityIndex ~ Group * ConsecActive + (1 | AnimalNum), data = data_filtered_agg_Phase_subset)
    emmeans_obj <- emmeans(model, ~ Group * ConsecActive, data = data_filtered_agg_Phase_subset, 
                           cov.reduce = FALSE, adjust = "sidak", pbkrtest.limit = 10000)
    pairwise_results <- pairs(emmeans_obj, by = c("ConsecActive"), adjust = "bonferroni")
  } else if (PhaseValue == "Inactive") {
    # Filter for ConsecInactive levels 2 and 3
    data_filtered_agg_Phase_subset <- data_filtered_agg_Phase_subset %>%
      filter(ConsecInactive %in% c())
    
    model <- lmerTest::lmer(ActivityIndex ~ Group * ConsecInactive + (1 | AnimalNum), data = data_filtered_agg_Phase_subset)
    emmeans_obj <- emmeans(model, ~ Group * ConsecInactive, data = data_filtered_agg_Phase_subset, 
                           cov.reduce = FALSE, adjust = "sidak", pbkrtest.limit = 10000)
    pairwise_results <- pairs(emmeans_obj, by = c("ConsecInactive"), adjust = "bonferroni")
  }
  
  model_results[[PhaseValue]] <- model
  
  mixed_model_results_file <- paste0("S:\\Lab_Member\\Tobi\\Experiments\\Exp9_Social-Stress\\Analysis\\Behavior\\RFID\\Activity\\gridActivity\\mixed_model_results_definedTimelines_", PhaseValue, ".txt")
  model_summary <- capture.output(summary(model))
  writeLines(model_summary, con = mixed_model_results_file)
  
  cat("Mixed-effects model results for", PhaseValue, ":\n")
  print(summary(model))
  
  cat("emmeans results for", PhaseValue, ":\n")
  print(emmeans_obj)
  
  # Perform pairwise comparisons with Tukey adjustment
  cat("Pairwise day comparisons for", PhaseValue, ":\n")
  print(pairwise_results)
  
  # Save pairwise results to a file
  pairwise_results_file <- paste0("S:\\Lab_Member\\Tobi\\Experiments\\Exp9_Social-Stress\\Analysis\\Behavior\\RFID\\Activity\\gridActivity\\pairwise_results_definedTimelines_", PhaseValue, ".txt")
  pairwise_summary <- capture.output(print(pairwise_results))
  writeLines(pairwise_summary, con = pairwise_results_file)
  
  # Save emmeans results to a file
  emmeans_results_file <- paste0("S:\\Lab_Member\\Tobi\\Experiments\\Exp9_Social-Stress\\Analysis\\Behavior\\RFID\\Activity\\gridActivity\\emmeans_results_definedTimelines_", PhaseValue, ".txt")
  emmeans_summary <- capture.output(summary(emmeans_obj))
  writeLines(emmeans_summary, con = emmeans_results_file)
  
  # Create scatter plots using emmip
  if (PhaseValue == "Active") {
    plot <- emmip(model, Group ~ ConsecActive, at = list(ConsecActive = c(2, 3, 4, 5)), CIs = TRUE, style = "factor", pbkrtest.limit = 10000, dodge = 0.5, width=1) + 
      theme_minimal_hgrid(12, rel_small = 1) +
      scale_color_manual(name = NULL, values = c("#1e3791", "#8aacdb", "#f49620")) + 
      scale_fill_manual(name = NULL, values = c("#1e3791", "#8aacdb", "#f49620")) +
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
    print(plot)
  } else if (PhaseValue == "Inactive") {
    plot <- emmip(model, Group ~ ConsecInactive, CIs = TRUE, style = "factor", pbkrtest.limit = 10000, dodge = 0.5, width=1) + 
      theme_minimal_hgrid(12, rel_small = 1) +
      scale_color_manual(name = NULL, values = c("#1e3791", "#8aacdb", "#f49620")) + 
      scale_fill_manual(name = NULL, values = c("#1e3791", "#8aacdb", "#f49620")) +
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
    print(plot)
  }
}