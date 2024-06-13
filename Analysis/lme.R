## Mixed models analysis to analyse both the active and inactive phase
#  compares susceptible and resilient groups to controls
#  will create results text files in defined folder

# List of packages to check and load
packages <- c("ggplot2", "dplyr", "tidyr", "gridExtra", "lme4", "lmerTest", "cowplot", "lsmeans", "emmeans")

# Check if each package is installed, install if not, and load it
for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}

# Define the folder to store the result files
results_dir <- "C:/Users/topohl/Documents/test/"
includeSex <- TRUE  # Set to TRUE or FALSE depending on whether you want to include sex

# Define the list to store plots
plot_list <- list()
heatmap_list <- list()

# Split data into separate data frames for each CC
data_filtered_agg_Change <- data_filtered_agg %>% 
  split(data_filtered_agg$Change)

# Create plots for each CC and Sex
plot_list <- lapply(sort(unique(data_filtered_agg$Change)), function(Change) {
  # Subset data for the current CC
  data_filtered_agg_Change_subset <- data_filtered_agg_Change[[Change]]
  
  # Create an empty list to store plots for each Sex
  plot_list_sex <- list()
  
  # Loop through each Sex
  for (SexValue in c("m", "f")) {
    # Filter data by Sex if includeSex is TRUE
    if (includeSex) {
      data_filtered_agg_Change_subset_sex <- data_filtered_agg_Change_subset %>% 
        filter(Sex == SexValue)
    } else {
      data_filtered_agg_Change_subset_sex <- data_filtered_agg_Change_subset
    }
    
    # Create the plot
    p <- ggplot(data_filtered_agg_Change_subset_sex, aes(x = HalfHourElapsed, y = ActivityIndex, group = Group, color = Group)) +
      geom_path(stat = "summary") +
      stat_summary(aes(fill = Group), fun = mean,
                   fun.min = function(x) mean(x) - sd(x), 
                   fun.max = function(x) mean(x) + sd(x), 
                   geom = "ribbon", 
                   alpha = 0.2, colour = NA) +
      scale_color_manual(name = NULL, values = c("#1e3791", "#8aacdb", "#f49620")) + 
      scale_fill_manual(name = NULL, values = c("#1e3791", "#8aacdb", "#f49620")) +
      scale_x_continuous(breaks = seq(0, max(data_filtered_agg_Change_subset_sex$HalfHourElapsed), by = 24), 
                         labels = seq(0, max(data_filtered_agg_Change_subset_sex$HalfHourElapsed), by = 24)/2) +
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
    
    # Save each plot as an SVG file with the Sex in the filename
    save_file <- paste0(results_dir, "line_plot_", Change, "_", SexValue, ".svg")
    ggsave(filename = save_file, plot = p, device = "svg", width = 4, height = 4)
    
    # Add the plot to the list
    plot_list_sex[[SexValue]] <- p
  }
  
  # Return the list of plots for each Sex
  return(plot_list_sex)
})

# Create the heatmap for each CC with sexes not overlapping
heatmap_list <- lapply(sort(unique(data_filtered_agg$Change)), function(Change) {
  # Subset data for the current CC
  data_filtered_agg_Change_subset <- data_filtered_agg_Change[[Change]]
  
  # Create an empty list to store heatmaps for each Sex
  heatmap_list_sex <- list()
  
  # Loop through each Sex
  for (SexValue in c("m", "f")) {
    # Filter data by Sex if includeSex is TRUE
    if (includeSex) {
      data_filtered_agg_Change_subset_sex <- data_filtered_agg_Change_subset %>% 
        filter(Sex == SexValue)
    } else {
      data_filtered_agg_Change_subset_sex <- data_filtered_agg_Change_subset
    }
    
    # Reorder levels of Group factor
    data_filtered_agg_Change_subset_sex <- data_filtered_agg_Change_subset_sex %>% 
      mutate(Group = factor(Group, levels = c("CON", "RES", "SUS")))
    
    # Create the heatmap
    p <- ggplot(data_filtered_agg_Change_subset_sex, aes(x = HalfHourElapsed, y = reorder(Group, -as.numeric(Group)), fill = ActivityIndex)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "darkblue", limits = c(0, max(data_filtered_agg$ActivityIndex))) +
      scale_x_continuous(breaks = seq(0, max(data_filtered_agg_Change_subset_sex$HalfHourElapsed), by = 24), 
                         labels = seq(0, max(data_filtered_agg_Change_subset_sex$HalfHourElapsed), by = 24)/2) +
      facet_grid(Sex ~ ., scales = "free") +  
      theme_minimal_hgrid(12, rel_small = 1) +
      labs(title = bquote(~bold(.(paste("Activity Index")))),
           subtitle = paste("Cage Change:", Change),
           y = "Group", x = "Time elapsed [h]") +
      theme(plot.title = element_text(hjust = 0.5, face = "plain"),
            plot.subtitle = element_text(hjust = 0.5, face = "plain"),
            legend.position = "top",
            legend.justification = "right",
            legend.text = element_text(size = 9),
            legend.box.spacing = unit(1, "pt"),
            axis.ticks = element_blank(),
            axis.text.x = element_text(vjust = 1, size = 11, hjust = 0.5),
            strip.text = element_text(size = 9))
    
    # Save each heatmap as an SVG file with the Sex in the filename
    save_file <- paste0(results_dir, "heatmap_", Change, "_", SexValue, ".svg")
    ggsave(filename = save_file, plot = p, device = "svg", width = 5, height = 5)
    
    # Add the heatmap to the list
    heatmap_list_sex[[SexValue]] <- p
  }
  
  # Add the list of heatmaps for each Sex to the main heatmap list
  heatmap_list[[Change]] <- heatmap_list_sex
  
  # Return the main heatmap list
  return(heatmap_list)
})

# Create the heatmap for each CC without sexes overlapping
heatmap_list <- lapply(sort(unique(data_filtered_agg$Change)), function(Change) {
  # Subset data for the current CC
  data_filtered_agg_Change_subset <- data_filtered_agg_Change[[Change]]
  
  # Create an empty list to store heatmaps for each Sex
  heatmap_list_sex <- list()
  
  # Loop through each Sex
  for (SexValue in c("m", "f")) {
    # Filter data by Sex if includeSex is TRUE
    if (includeSex) {
      data_filtered_agg_Change_subset_sex <- data_filtered_agg_Change_subset %>% 
        filter(Sex == SexValue)
    } else {
      data_filtered_agg_Change_subset_sex <- data_filtered_agg_Change_subset
    }
    
    # Reorder levels of Group factor
    data_filtered_agg_Change_subset_sex <- data_filtered_agg_Change_subset_sex %>% 
      mutate(Group = factor(Group, levels = c("CON", "RES", "SUS")))
    
    # Create the heatmap
    p <- ggplot(data_filtered_agg_Change_subset_sex, aes(x = HalfHourElapsed, y = reorder(Group, -as.numeric(Group)), fill = ActivityIndex)) +
      geom_tile() +
      scale_fill_gradientn(colors = c("#022859", "red", "yellow", "white"), 
                           limits = c(0, max(data_filtered_agg$ActivityIndex))) +
      scale_x_continuous(breaks = seq(0, max(data_filtered_agg_Change_subset_sex$HalfHourElapsed), by = 24), 
                         labels = seq(0, max(data_filtered_agg_Change_subset_sex$HalfHourElapsed), by = 24)/2) +
      theme_minimal_hgrid(12, rel_small = 1) +
      labs(title = bquote(~bold(.(paste("Activity Index")))),
           subtitle = paste("Cage Change:", Change),
           y = "Group", x = "Time elapsed [h]") +
      theme(plot.title = element_text(hjust = 0.5, face = "plain"),
            plot.subtitle = element_text(hjust = 0.5, face = "plain"),
            legend.position = "top",
            legend.justification = "right",
            legend.text = element_text(size = 9),
            legend.box.spacing = unit(1, "pt"),
            axis.ticks = element_blank(),
            axis.text.x = element_text(vjust = 1, size = 11, hjust = 0.5),
            strip.text = element_text(size = 9))
    
    # Save each heatmap as an SVG file with the Sex in the filename
    save_file <- paste0(results_dir, "heatmap_", Change, "_", SexValue, ".svg")
    ggsave(filename = save_file, plot = p, device = "svg", width = 5, height = 5)
    
    # Add the heatmap to the list
    heatmap_list_sex[[SexValue]] <- p
  }
  
  # Add the list of heatmaps for each Sex to the main heatmap list
  heatmap_list[[Change]] <- heatmap_list_sex
  
  # Return the main heatmap list
  return(heatmap_list_sex)
})

# Define data frames to store summary of lmer and posthoc results
lmer_summary_df <- data.frame()
posthoc_summary_df <- data.frame()

# Loop through each Cage Change and Phase
for (Change in sort(unique(data_filtered_agg$Change))) {
  for (PhaseValue in c("Active", "Inactive")) {
    data_filtered_agg_Change_Phase_subset <- data_filtered_agg_Change[[Change]] %>%
      filter(Phase == PhaseValue)
    
    # Loop through each Sex
    for (SexValue in c("m", "f")) {
      data_filtered_agg_Change_Phase_Sex_subset <- data_filtered_agg_Change_Phase_subset %>%
        filter(Sex == SexValue)

      # Set options for pbkrtest
      emm_options(pbkrtest.limit = 5474)   
      
      # Fit the lmer model
      model <- lmerTest::lmer(ActivityIndex ~ Group + HalfHourElapsed + (1 | AnimalNum), data = data_filtered_agg_Change_Phase_Sex_subset)
      
      # Get the fixed effects from lmer model
      model_summary <- summary(model)
      fixed_effects <- data.frame(Fixed_effect = rownames(model_summary$coefficients),
                                  model_summary$coefficients,
                                  CageChange = Change,
                                  Phase = PhaseValue,
                                  Sex = SexValue)
      
      # Append summary of lmer results to data frame
      lmer_summary_df <- rbind(lmer_summary_df, fixed_effects)
      
      cat("Mixed-effects model results for", ifelse(SexValue == "m", "male", "female"), Change, "-", PhaseValue, ":\n")
      print(model_summary)
      
      # Conduct post-hoc tests
      posthoc <- emmeans::emmeans(model, pairwise ~ Group)
      posthoc_summary <- summary(posthoc, infer = TRUE, adjust = "tukey")
      
      # Append posthoc results to data frame
      posthoc_df <- as.data.frame(posthoc_summary)
      posthoc_df$CageChange <- Change
      posthoc_df$Phase <- PhaseValue
      posthoc_df$Sex <- SexValue
      posthoc_summary_df <- rbind(posthoc_summary_df, posthoc_df)
      
      cat("Post-hoc test results for", ifelse(SexValue == "m", "male", "female"), Change, "-", PhaseValue, ":\n")
      print(posthoc_summary)
    }
  }
}

# Write summary of lmer results to CSV file
lmer_results_file <- paste0(results_dir, "lmer_results_summary.csv")
write.csv(lmer_summary_df, file = lmer_results_file, row.names = FALSE)

# Write post-hoc results to CSV file
posthoc_results_file <- paste0(results_dir, "posthoc_results.csv")
write.csv(posthoc_summary_df, file = posthoc_results_file, row.names = FALSE)

# Perform mixed-effects models
#model_results <- list()

#for (Change in sort(unique(data_filtered_agg$Change))) {
#  for (PhaseValue in c("Active", "Inactive")) {
#    data_filtered_agg_Change_Phase_subset <- data_filtered_agg_Change[[Change]] %>%
#      filter(Phase == PhaseValue)
#    
#    model <- lmerTest::lmer(ActivityIndex ~ Group + HalfHourElapsed + (1 | AnimalNum), data = data_filtered_agg_Change_Phase_subset)
#    model_results[[paste0(Change, "_", PhaseValue)]] <- model
#    
#    mixed_model_results_file <- paste0(results_dir, "mixed_model_results_", Change, "_", PhaseValue, ".txt")
#    model_summary <- capture.output(summary(model))
#    writeLines(model_summary, con = mixed_model_results_file)
#    
#    cat("Mixed-effects model results for", Change, "-", PhaseValue, ":\n")
#    print(summary(model))
#  }
#}