################################################################################
## Create the cookie plot
## Compare Means between Groups during 2 hours preceding the Active Phase
# This is used to quantify activity before the onset of the active phase
################################### L I N E  P L O T ###################################
# here, we plot the different groups on a minute bin basis and compute lme analysis on the data

# Load necessary libraries
requiredPackages <- c("cowplot", "ggsignif", "ggplot2", "colorspace", "dplyr", 
                      "openxlsx", "tidyr", "broom", "readxl", "rstatix", 
                      "ggpubr", "Hmisc", "lme4", "broom.mixed")

for (package in requiredPackages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  } else {
    library(package, character.only = TRUE)
  }
}

results_dir <- "C:/Users/topohl/Documents/test/cookie_analysis/"
# Define group colors
group_cols <- c("#1e3791", "#76A2E8", "#F79719")
# Define the list of SUS animals
sus_animals <- c(readLines(paste0("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/sus_animals.csv")))

# Add column named "Cookie" to the data frame, set it to true if DateTime is 17:00:00 and 18:00:00 and ConsecInactive "2"
data_filtered <- data_filtered %>%
  mutate(TimeOnly = format(DateTime, "%H:%M")) %>% # Extract only the time from DateTime
  mutate(Cookie = ifelse(TimeOnly >= "17:00" & TimeOnly <= "17:45" & ConsecInactive == 2, "TRUE", "FALSE"))

cookie_data <- data_filtered %>% 
  filter(Cookie == 'TRUE') %>%
  arrange(TimeOnly)  # Sort by time only

# Calculate the time difference from the beginning of the DateTime dataset
cookie_data$ElapsedMinutes <- as.numeric(cookie_data$DateTime - min(cookie_data$DateTime)) / 60

# Split the data into male and female datasets
cookie_data_male <- cookie_data %>% filter(Sex == "m")
cookie_data_female <- cookie_data %>% filter(Sex == "f")

# Initialize a list to store LME results
lme_results_list <- list()

# Calculate overall y-axis limits
y_min <- min(cookie_data_male$ActivityIndex, cookie_data_female$ActivityIndex)
y_max <- max(cookie_data_male$ActivityIndex, cookie_data_female$ActivityIndex)

# Function to perform analysis for a given dataset
perform_analysis <- function(data) {
  means_cookie <- data %>% 
    group_by(Group, AnimalNum) %>% 
    summarise(ActivityIndex = mean(ActivityIndex),
              sd_ActivityIndex = sd(ActivityIndex),
              .groups = 'drop') # Drop grouping
  
  # Fit linear mixed-effects models for the group using lmerTest for p-values
  lme_model <- lmerTest::lmer(ActivityIndex ~ Group + ElapsedMinutes + (1 | AnimalNum), data = data)
  
  # Get the fitted values from the LME model
  data$fitted_values <- fitted(lme_model)

  # Store the LME summary for exporting later
  lme_summary <- summary(lme_model)
  
  # Get fixed effects summary and convert to dataframe
  fixed_effects <- as.data.frame(lme_summary$coefficients)
  
  # Add Gender info
  fixed_effects$Gender <- unique(data$Sex)  
  
  # Store results in the list
  lme_results_list[[unique(data$Sex)]] <<- fixed_effects  # Use <<- to save it in the outer environment

  # Create plot for individual values
p_indiv_cookie <- ggplot(data, aes(x = Group, y = ActivityIndex, color = Group)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5) + # Add jittered dots
  scale_fill_manual(values = group_cols) + # Set fill colors
  scale_color_manual(values = group_cols) +
  scale_y_continuous(limits = c(y_min, y_max), expand=c(0.1,0.1)) + # Use scale_y_continuous instead
  labs(title = paste("All Activity -", unique(data$Sex)), x = "Group", y = "Activity Index [a.u.]") +
  theme_minimal_hgrid(12, rel_small = 1) +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        plot.subtitle = element_text(hjust = 0.5, face = "plain"),
        axis.text.x = element_text(angle = 0, vjust = 1, size = 11, hjust = 0.45),
        axis.title.x = element_blank(),
        legend.position = "none") 

# Create plot for means
p_means_cookie <- ggplot(means_cookie, aes(x = Group, y = ActivityIndex, fill = Group, colour = Group)) +
  geom_jitter(size=4, alpha=0.5, width=0.2, shape=16) + 
  geom_errorbar(aes(ymin = ActivityIndex - sd_ActivityIndex, ymax = ActivityIndex + sd_ActivityIndex), 
                position = position_dodge(width = 0.9), width = 0.2) +
  stat_summary(fun.min=function(z) {quantile(z,0.25)}, fun.max=function(z) {quantile(z,0.75)}, fun=median, color="black", size=1, shape=16) +
  scale_fill_manual(values = group_cols) +
  scale_color_manual(values = group_cols) +
  scale_y_continuous(limits = c(y_min, y_max), expand=c(0.1,0.1)) + # Use scale_y_continuous instead
  labs(title = paste("Individual Activity -", unique(data$Sex)), x = "Group", y = "Activity Index [a.u.]") +
  theme_minimal_hgrid(12, rel_small = 1) +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        plot.subtitle = element_text(hjust = 0.5, face = "plain"),
        axis.text.x = element_text(angle = 0, vjust = 1, size = 11, hjust = 0.45),
        axis.title.x = element_blank(),
        legend.position = "none") 
  
  # Arrange two plots side by side
  combined_plot <- plot_grid(p_indiv_cookie, p_means_cookie, ncol = 2, align = "v", labels = c("A", "B"))
  
  # Create cookie plot with adjusted summary functions
  cookie_plot <- ggplot(data, aes(x = TimeOnly, y = ActivityIndex, group = Group, color = Group)) +
    stat_summary(aes(group = Group), fun = mean, geom = "line", size = 1) + # Plot the mean as lines
    stat_summary(aes(fill = Group), fun = mean,
                 fun.min = function(x) mean(x) - sd(x), 
                 fun.max = function(x) mean(x) + sd(x), 
                 geom = "ribbon", 
                 alpha = 0.2, colour = NA) + # Add ribbons for SD
    scale_color_manual(name = NULL, values = group_cols) + 
    scale_fill_manual(name = NULL, values = group_cols) +
    theme_minimal_hgrid(12, rel_small = 1) +
    labs(title = bquote(~bold(.(paste("Cookie Challenge -", unique(data$Sex))))),
         y = "Activity [a.u.]", x = "Time", caption = "") + # Update x-axis label to "Time"
    theme(plot.title = element_text(hjust = 0.5, face = "plain"),
          plot.subtitle = element_text(hjust = 0.5, face = "plain"),
          legend.position = "top",
          legend.justification = "right",
          legend.text = element_text(size = 9),
          legend.box.spacing = unit(1, "pt"),
          axis.text.x = element_text(vjust = 1, size = 11, hjust = 0.5),
          axis.title.x = element_blank())

  # Save the plots
  ggsave(filename = paste0(results_dir, "cookie_plot_", unique(data$Sex), ".svg"), plot = cookie_plot, width = 8, height = 6, dpi = 300)
  ggsave(filename = paste0(results_dir, "combined_cookie_plot_", unique(data$Sex), ".svg"), plot = combined_plot, width = 8, height = 6, dpi = 300)
}

# Perform analysis for males and females separately
perform_analysis(cookie_data_male)
perform_analysis(cookie_data_female)

# Combine and save the LME results to CSV
lme_results_combined <- do.call(rbind, lme_results_list)
write.csv(lme_results_combined, file = paste0(results_dir, "lme_results.csv"), row.names = TRUE)