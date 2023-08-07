library(ggplot2)
library(gridExtra)

## Function to calculate sleep bouts per phase
calculate_total_sleep_info <- function(data) {
  total_sleep_info <- data %>%
    group_by(AnimalNum, Batch, Group, Change, Phase) %>%
    summarize(
      TotalSleepBouts = max(SleepBouts),
      TotalSleepingTime = sum(ActivityIndex == 0),  # Count occurrences of ActivityIndex == 0
      TotalSleepingTimeHours = sum(ActivityIndex == 0) / 60,  # Convert minutes to hours
      SleepingPercentage = (sum(ActivityIndex == 0) / n()) * 100,  # Percentage of sleeping
      AvgSleepBoutDuration = TotalSleepingTime / TotalSleepBouts  # Average duration of a sleeping bout
    ) %>%
    ungroup()
  
  return(total_sleep_info)
}

# Assuming 'data_filtered' is your data frame
total_sleep_info_per_change <- calculate_total_sleep_info(data_filtered)

print(total_sleep_info_per_change)


# Define group colors
group_cols <- c("#1e3791", "#76A2E8", "#F79719")

# Update Group column
total_sleep_info_per_change <- total_sleep_info_per_change %>%
    mutate(Group = if_else(AnimalNum %in% sus_animals, "SUS",
                           if_else(Group == "SIS", "RES", Group)))

# Define a function for summarizing data
summarize_data <- function(data, var_name) {
    data %>%
        group_by(Phase, AnimalNum, Group) %>%
        summarise(!!var_name := mean(!!sym(var_name)),
                  !!paste0("sd_", var_name) := sd(!!sym(var_name)),
                  .groups = "drop")
}

# Define a function for performing normality test
perform_normality_test <- function(data, var_name, group_name) {
    shapiro.test(data[[var_name]][data$Group == group_name])
}

# Define a function for performing ANOVA or Kruskal-Wallis
perform_anova_or_kruskal <- function(data, var_name) {
    if (all(sapply(unique(data$Group), function(group) {
        perform_normality_test(data, var_name, group)$p.value >= 0.05
    }))) {
        test_result <- aov(as.formula(paste(var_name, "~ Group")), data = data)
        test_stat <- "ANOVA"
    } else {
        test_result <- kruskal.test(as.formula(paste(var_name, "~ Group")), data = data)
        test_stat <- "Kruskal-Wallis"
    }
    list(test_result, test_stat = test_stat)
}

# Define a function for creating plots
create_plot <- function(data, var_name, y_label) {
    ggplot(data, aes(x = Group, y = !!sym(var_name), fill = Group, colour = Group)) +
        geom_jitter(size = 4, alpha = 0.5, width = 0.2, shape = 16) +
        stat_summary(fun.min = function(z) quantile(z, 0.25), fun.max = function(z) quantile(z, 0.75),
                     fun = median, geom = "point", color = "black", size = 3, shape = 16, position = position_dodge(width = 0.9)) +
        geom_errorbar(aes(ymin = !!sym(var_name) - !!sym(paste0("sd_", var_name)),
                          ymax = !!sym(var_name) + !!sym(paste0("sd_", var_name))),
                      width = 0.2) +
        scale_fill_manual(values = group_cols) +
        scale_color_manual(values = group_cols) +
        scale_y_continuous(expand = expansion(add = c(0.1, 0.1))) +
        labs(title = var_name, x = "Group", y = y_label) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "plain"),
              plot.subtitle = element_text(hjust = 0.5, face = "plain"),
              axis.text.x = element_text(angle = 0, vjust = 1, size = 11, hjust = 0.5),
              axis.title.x = element_blank(),
              legend.position = "none")
}

# Summarize data for each variable and phase
vars_to_summarize <- c("TotalSleepBouts", "SleepingPercentage", "AvgSleepBoutDuration", "TotalSleepingTime")

for (var in vars_to_summarize) {
    # For Active phase
    summary_data_active <- summarize_data(total_sleep_info_per_change %>% filter(Phase == "Active"), var)
    assign(paste0("means_", var, "_active"), summary_data_active)
    
    anova_kruskal_result_active <- perform_anova_or_kruskal(get(paste0("means_", var, "_active")), var)
    assign(paste0("test_result_", var, "_active"), anova_kruskal_result_active[[1]])
    assign(paste0("test_name_", var, "_active"), anova_kruskal_result_active[[2]])
    
    plot_active <- create_plot(get(paste0("means_", var, "_active")), var, "Activity Index [a.u.]")
    plot_active <- plot_active + labs(subtitle = "Active Phase")  # Add subtitle
    assign(paste0("p_means_", var, "_active"), plot_active)
    print(plot_active)
    
    # For Inactive phase
    summary_data_inactive <- summarize_data(total_sleep_info_per_change %>% filter(Phase == "Inactive"), var)
    assign(paste0("means_", var, "_inactive"), summary_data_inactive)
    
    anova_kruskal_result_inactive <- perform_anova_or_kruskal(get(paste0("means_", var, "_inactive")), var)
    assign(paste0("test_result_", var, "_inactive"), anova_kruskal_result_inactive[[1]])
    assign(paste0("test_name_", var, "_inactive"), anova_kruskal_result_inactive[[2]])
    
    plot_inactive <- create_plot(get(paste0("means_", var, "_inactive")), var, "Activity Index [a.u.]")
    plot_inactive <- plot_inactive + labs(subtitle = "Inactive Phase")  # Add subtitle
    assign(paste0("p_means_", var, "_inactive"), plot_inactive)
    print(plot_inactive)
}
