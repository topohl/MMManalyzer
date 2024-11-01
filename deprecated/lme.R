## Mixed models analysis to analyse both the active and inactive phase
#  compares susceptible and resilient groups to controls
#  will create results text files in defined folder

# List of packages to check and load
packages <- c("ggplot2", "dplyr", "tidyr", "gridExtra", "lme4", "lmerTest", "cowplot", "lsmeans", "emmeans", "Matrix")

# Check if each package is installed, install if not, and load it
for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}

# Define the folder to store the result files
results_dir <- "C:/Users/topohl/Documents/test/"
includeChange <- TRUE # Set to TRUE or FALSE depending on whether you want to include cage change in the analysis
includeSex <- TRUE  # Set to TRUE or FALSE depending on whether you want to include sex in the analysis
includePhase <- TRUE # Set to TRUE or FALSE depending on whether you want to include phase in the analysis

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
    
# Define a modern color palette
#color_palette <- c(
#  CON = "#007C92",  # Deep teal
#  RES = "#FFABAB",  # Soft peach
#  SUS = "#E8AAE6"   # Light lavender

# set color_palette to c("#1e3791", "#8aacdb", "#f49620")
color_palette <- c("#1e3791", "#8aacdb", "#f49620")

# Create the plot
p <- ggplot(data_filtered_agg_Change_subset_sex, aes(x = HalfHourElapsed, y = ActivityIndex, group = Group, color = Group)) +
  geom_path(stat = "summary", fun = mean, linewidth = 0.8) +  # Adjust line thickness for visibility
  stat_summary(aes(fill = Group), fun = mean,
               fun.min = function(x) mean(x) - sd(x), 
               fun.max = function(x) mean(x) + sd(x), 
               geom = "ribbon", 
               alpha = 0.3, colour = NA) +  # Shaded area representing ±1 SD
  
  # Use the defined color palette
  scale_color_manual(name = "Group", values = color_palette) + 
  scale_fill_manual(name = "Group", values = color_palette) +
  
  # Adjust the x-axis
  scale_x_continuous(breaks = seq(0, max(data_filtered_agg_Change_subset_sex$HalfHourElapsed), by = 24), 
                     labels = seq(0, max(data_filtered_agg_Change_subset_sex$HalfHourElapsed), by = 24) / 2,
                     expand = c(0, 0)) +  # Remove padding on the x-axis
  scale_y_continuous(expand = c(0, 0)) +  # Remove padding on the y-axis
  
  # Refined labels and captions
  labs(title = "Circadian Activity Index",  # More concise title
       subtitle = "Activity Levels Across Groups", 
       y = "Activity [a.u.]", x = "Time Elapsed [h]", 
       caption = "Shaded regions represent ±1 SD of the mean") +
  
  # Modern theme adjustments
  theme_minimal(base_size = 14) +  # Increase base font size for readability
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  # Center title
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic"),  # Italic for subtitle
    axis.text.x = element_text(size = 12),  # Adjust text sizes for clarity
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.ticks.x = element_line(size = 0.5),
    legend.position = "top",  # Place legend inside the plot area
    panel.background = element_blank()  # Clean panel background
  )

    # Save each plot as an SVG file with the Sex in the filename
    save_file <- paste0(results_dir, "line_plot_", Change, "_", SexValue, ".svg")
    ggsave(filename = save_file, plot = p, device = "svg", width = 7, height = 4)
    
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

# Modify data for visualization (selecting the necessary columns)
plot_data <- lmer_summary_df %>%
  filter(Fixed_effect %in% c("GroupRES", "GroupSUS")) %>%  # Only interested in RES and SUS effects compared to controls
  mutate(Group = ifelse(Fixed_effect == "GroupRES", "RES", "SUS"))

# Plot fixed effects for each Cage Change, Phase, and Sex
ggplot(plot_data, aes(x = Group, y = Estimate, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Estimate - Std..Error, ymax = Estimate + Std..Error), 
                position = position_dodge(width = 0.9), width = 0.2) +
  facet_grid(Sex ~ Phase, scales = "free") +  # Facet by Sex and Phase
  labs(title = "Mixed-Model Fixed Effects Comparison (RES & SUS vs. Controls)",
       x = "Group",
       y = "Estimate (Effect Size)") +
  scale_fill_manual(values = c("RES" = "#8aacdb", "SUS" = "#f49620")) +  # Custom colors
  theme_minimal() +
  theme(legend.position = "top", 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 12))

# Initialize data frames to store summaries
lmer_summary_df <- data.frame()
posthoc_summary_df <- data.frame()
emmeans_summary_df <- data.frame()  # New data frame to store EMMs

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
      
      # Conduct post-hoc tests and extract EMMs
      posthoc <- emmeans::emmeans(model, pairwise ~ Group)
      posthoc_summary <- summary(posthoc, infer = TRUE, adjust = "tukey")
      
      # Append posthoc results to data frame
      posthoc_df <- as.data.frame(posthoc_summary$contrasts)
      posthoc_df$CageChange <- Change
      posthoc_df$Phase <- PhaseValue
      posthoc_df$Sex <- SexValue
      posthoc_summary_df <- rbind(posthoc_summary_df, posthoc_df)
      
      # Extract EMMs and append to emmeans_summary_df
      emmeans_df <- as.data.frame(posthoc$emmeans)
      emmeans_df$CageChange <- Change
      emmeans_df$Phase <- PhaseValue
      emmeans_df$Sex <- SexValue
      emmeans_summary_df <- rbind(emmeans_summary_df, emmeans_df)
    }
  }
}

# Write summary of lmer results to CSV file
lmer_results_file <- paste0(results_dir, "lmer_results_summary.csv")
write.csv(lmer_summary_df, file = lmer_results_file, row.names = FALSE)

# Write post-hoc results to CSV file
posthoc_results_file <- paste0(results_dir, "posthoc_results.csv")
write.csv(posthoc_summary_df, file = posthoc_results_file, row.names = FALSE)

# Write EMMs to CSV file
emmeans_results_file <- paste0(results_dir, "emmeans_results.csv")
write.csv(emmeans_summary_df, file = emmeans_results_file, row.names = FALSE)

# Plotting the EMMs including CON
# Ensure Group is a factor with the desired order
emmeans_summary_df$Group <- factor(emmeans_summary_df$Group, levels = c("CON", "RES", "SUS"))

# Loop through each Cage Change and Sex to create and save plots
unique_changes <- unique(emmeans_summary_df$CageChange)
sex_labels <- c("m" = "Male", "f" = "Female")

for (Change in unique_changes) {
  for (SexValue in c("m", "f")) {
    plot_data <- emmeans_summary_df %>%
      filter(CageChange == Change, Sex == SexValue)
    
    # Create the plot for the specific Sex and Cage Change
    p <- ggplot(plot_data, aes(x = Group, y = emmean, fill = Group)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                    position = position_dodge(width = 0.9), width = 0.2) +
      facet_grid(Phase ~ ., scales = "free") +  # Separate by Phase (Active/Inactive)
      labs(title = paste("Estimated Marginal Means for", Change, "-", sex_labels[SexValue]),
           x = "Group",
           y = "Estimated Activity Index") +
      scale_fill_manual(values = c("CON" = "#1e3791", "RES" = "#8aacdb", "SUS" = "#f49620")) +  # Custom colors
      theme_minimal() +
      theme(legend.position = "none", 
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(size = 12))
    
    # Save the plot for each Cage Change and Sex
    save_file <- paste0(results_dir, "emmeans_plot_", Change, "_", sex_labels[SexValue], ".svg")
    ggsave(filename = save_file, plot = p, device = "svg", width = 5, height = 5)
    
    # Optionally, print the plot
    print(p)
  }
}


# Initialize data frames to store LME results for plotting
lme_results_df <- data.frame()

# Loop through each Cage Change and Phase
for (Change in sort(unique(data_filtered_agg$Change))) {
  for (PhaseValue in c("Active", "Inactive")) {
    # Loop through each Sex
    for (SexValue in c("m", "f")) {
      # Subset the data
      data_subset <- data_filtered_agg_Change[[Change]] %>%
        filter(Phase == PhaseValue, Sex == SexValue)

      # Fit the lmer model
      model <- lmerTest::lmer(ActivityIndex ~ Group + HalfHourElapsed + (1 | AnimalNum), data = data_subset)

      # Get fixed effects summary from the model
      model_summary <- summary(model)
      fixed_effects <- data.frame(Fixed_effect = rownames(model_summary$coefficients),
                                  Estimate = model_summary$coefficients[, "Estimate"],
                                  Std_Error = model_summary$coefficients[, "Std. Error"],
                                  Group = rownames(model_summary$coefficients),
                                  CageChange = Change,
                                  Phase = PhaseValue,
                                  Sex = SexValue)
      
      # Append results to the data frame
      lme_results_df <- rbind(lme_results_df, fixed_effects)
    }
  }
}

# Plotting the LME results
# Filter for RES and SUS groups only for plotting
plot_data_lme <- lme_results_df %>% 
  filter(Group %in% c("(Intercept)", "GroupRES", "GroupSUS"))

# Modify Group names for plotting
plot_data_lme$Group <- recode(plot_data_lme$Group, 
                               "(Intercept)" = "CON", 
                               "GroupRES" = "RES", 
                               "GroupSUS" = "SUS")

# Loop through each Cage Change and Sex to create and save LME plots
unique_changes <- unique(plot_data_lme$CageChange)
for (Change in unique_changes) {
  for (SexValue in c("m", "f")) {
    plot_data_sex <- plot_data_lme %>%
      filter(CageChange == Change, Sex == SexValue)

    # Create the plot for the specific Sex and Cage Change
    p_lme <- ggplot(plot_data_sex, aes(x = Group, y = Estimate, fill = Group)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = Estimate - Std_Error, ymax = Estimate + Std_Error),
                    position = position_dodge(width = 0.9), width = 0.2) +
      facet_grid(Phase ~ ., scales = "free") +  # Separate by Phase (Active/Inactive)
      labs(title = paste("LME Fixed Effects for", Change, "-", sex_labels[SexValue]),
           x = "Group",
           y = "Estimate (Effect Size)") +
      scale_fill_manual(values = c("CON" = "#1e3791", "RES" = "#8aacdb", "SUS" = "#f49620")) +  # Custom colors
      theme_minimal() +
      theme(legend.position = "none", 
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(size = 12))

    # Save the plot for each Cage Change and Sex
    save_file_lme <- paste0(results_dir, "lme_plot_", Change, "_", sex_labels[SexValue], ".svg")
    ggsave(filename = save_file_lme, plot = p_lme, device = "svg", width = 10, height = 8)
    
    # Optionally, print the plot
    print(p_lme)
  }
}