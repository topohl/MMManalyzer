# List of required packages
required_packages <- c("ggplot2", "gridExtra", "cowplot", "tidyr", "dplyr", "rstatix", "readxl", "svglite", "stringr", "openxlsx")

# Function to check and install missing packages
install_and_load <- function(packages) {
    for (package in packages) {
        if (!requireNamespace(package, quietly = TRUE)) {
            install.packages(package, dependencies = TRUE)
        }
        library(package, character.only = TRUE)
    }
}

# Install and load required packages
install_and_load(required_packages)

################################ Define constants for file paths, specific animals, and group colors ################################

# Directory paths for results
sleep_directory <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/Sleep"
graphs_directory <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/Sleep/plots"

# Define the working directory and source custom functions
working_directory <- "S:/Lab_Member/Anja/Git/AnjaIntern/code_cleanup"
source("C:/Users/topohl/Documents/GitHub/MMManalyzer/Analysis/MMM_functions.R")

# Load lists of SUS animals and excluded animals from CSV files
sus_animals <- readLines("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/sus_animals.csv")
excluded_animals <- readLines("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/BatchAnalysis/excludedAnimals.csv")

# Filter out excluded animals from the data
data_filtered <- data_filtered %>% filter(!AnimalNum %in% excluded_animals)

# Define color palette for groups
groupColors <- c("#1e3791", "#76A2E8", "#F79719")

# define the change to analyze
changeToAnalyze <- "CC4"

# add TRUE of FALSE to analysze specific changes
analyzeSingleChange <- TRUE

# Determine whether to include 'Phase' and 'Sex' factors in the analysis
include_phase <- incldeFactorExist("Phase", data_filtered, TRUE) 
include_sex <- incldeFactorExist("Sex", data_filtered, TRUE)
two_way_ANOVA <- FALSE

################################ Calculating and summarizing ################################

# Function to calculate sleep-related metrics
calculate_sleep_metrics <- function(data) {
    data %>%
        group_by_at(intersect(c("AnimalNum", "Batch", "Change", "Group", "Phase", "Sex"), names(data))) %>%
        summarize(
            SleepCount = sum(ActivityIndex == 0),
            TotalCount = n(),
            PercentageSleep = (SleepCount / TotalCount) * 100,
            TotalSleepingTime = sum(ActivityIndex == 0),
            SleepBouts = sum(ActivityIndex == 0 & lag(ActivityIndex, default = 1) != 0),
            AvgSleepBoutDuration = ifelse(SleepBouts == 0, 0, TotalSleepingTime / SleepBouts)
        )
}

# Calculate sleep-related metrics
total_sleep_info_per_change <- calculate_sleep_metrics(data_filtered)

total_sleep_info_per_change <- if (analyzeSingleChange) {
    total_sleep_info_per_change %>% filter(Change == changeToAnalyze)
} else {
    total_sleep_info_per_change
}

# Summarize the data to create an overview of sleep-activity for each individual
summarize_data <- function(data) {
    data %>%
        group_by_at(intersect(c("AnimalNum", "Phase", "Sex"), names(data))) %>%
        summarize(
            Batch = first(Batch),
            Group = first(Group),
            SleepBouts = sum(SleepBouts),
            TotalSleepingTime = sum(TotalSleepingTime),
            PercentageSleep = mean(PercentageSleep),
            AvgSleepBoutDuration = mean(AvgSleepBoutDuration)
        ) %>%
        ungroup()
}

# Summarize the data
overall_data <- summarize_data(total_sleep_info_per_change)

################################ Calculate coefficient of variation for sleep metrics ################################

# Function to calculate coefficient of variation
calculate_cv <- function(data) {
    data %>%
        group_by(AnimalNum, Batch, Group, Phase) %>%
        summarize(
            CV_SleepCount = sd(SleepCount) / mean(SleepCount),
            CV_TotalCount = sd(TotalCount) / mean(TotalCount),
            CV_PercentageSleep = sd(PercentageSleep) / mean(PercentageSleep),
            CV_TotalSleepingTime = sd(TotalSleepingTime) / mean(TotalSleepingTime),
            CV_SleepBouts = sd(SleepBouts) / mean(SleepBouts),
            CV_AvgSleepBoutDuration = sd(AvgSleepBoutDuration) / mean(AvgSleepBoutDuration)
        ) %>%
        ungroup()
}

# Calculate coefficient of variation only if analyzing multiple changes
if (!analyzeSingleChange) {
    cv_data <- calculate_cv(total_sleep_info_per_change)
    
    # Merge summarized data with coefficient of variation data
    overall_data <- merge(overall_data, cv_data, by = c("AnimalNum", "Batch", "Group", "Phase"))
}

################################ Execute tests and generate plots ################################

# Identify columns to plot, excluding non-numeric values
columns_to_plot <- setdiff(names(overall_data), c("AnimalNum", "Group", "Phase", "Batch", "Sex", "TotalCount", "CV_TotalCount"))

# Initialize lists to store results and plots
all_test_results <- list()
all_plots <- list()
all_posthoc_results <- list()

# Determine which phases and sexes to include in the analysis
phases <- if (include_phase) c("Active", "Inactive") else " "
sexes <- if (include_sex) c("m", "f") else " "

# Iterate through each variable, phase, and sex, and perform statistical tests
for (variable in columns_to_plot) {
    for (phase in phases) {
        for (sex in sexes) {
            result <- testAndPlotVariable(overall_data, variable, phase, sex)
            if (!is.null(result)) {
                all_test_results <- c(all_test_results, list(result$testResults))
                if (!is.null(result$posthocResults)) {
                    all_posthoc_results <- c(all_posthoc_results, list(result$posthocResults))
                }
                all_plots <- c(all_plots, list(result$plot))
            }
        }
    }
}

################################ Save the results ################################

# Convert list of test results to a data frame and save to CSV
all_test_results_df <- bind_rows(all_test_results)
write.csv(all_test_results_df, file = paste0(sleep_directory, "/test_results_Sleep.csv"), row.names = FALSE)

# Save post hoc results to CSV if available
if (length(all_posthoc_results) > 0) {
    all_posthoc_results_df <- bind_rows(all_posthoc_results)
    write.csv(all_posthoc_results_df, file = paste0(sleep_directory, "/posthoc_results_Sleep.csv"), row.names = FALSE)
}

# Save the generated plots
savePlotsInDir(all_test_results, all_plots, graphs_directory, ".svg")

################################ Display plots ################################

# Arrange plots into a grid
grid_plots <- grid.arrange(grobs = all_plots, ncol = 4)

# Save the arranged grid as SVG
ggsave(file = paste0(graphs_directory, "/allPlots.svg"), plot = grid_plots, width = 30, height = 20, device = "svg")

# Save individual and summarized data to CSV files
write.csv(overall_data, file = paste0(sleep_directory, "/overallData.csv"), row.names = FALSE)
write.csv(total_sleep_info_per_change, file = paste0(sleep_directory, "/total_sleep_info_per_change.csv"), row.names = FALSE)
write.csv(cv_data, file = paste0(sleep_directory, "/cv.csv"), row.names = FALSE)