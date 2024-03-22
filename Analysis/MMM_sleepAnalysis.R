# List of packages to check and load
packages <- c("ggplot2", "gridExtra", "cowplot", "tidyr", "dplyr", "rstatix", "readxl", "svglite")

# Check if each package is installed, install if not, and load it
for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
        install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
}

############## Define constants for file paths, sheet names, specific animals, and group colors ###########

# RESULT PATHS
## Sleep directory path for results 
sleep_directory <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/statistics/Sleep"
## Graphs directory path for result plots
graphs_directory <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/statistics/sleep/graphs"

# SOURCE PATHS
## Working directory path
working_directory <- "S:/Lab_Member/Anja/Git/AnjaIntern/code_cleanup"
# Include functions
source(paste0("C:/Users/topohl/Documents/GitHub/MMManalyzer/Analysis/MMM_functions.R"))

# Define SUS animals (csv file)
sus_animals <- readLines("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/sus_animals.csv")

# Define group colors
groupColors <- c("#1e3791", "#76A2E8", "#F79719")

# Define the factor to include/exclude
# Set to TRUE to include "Phase" or FALSE to exclude
# includeFactorExists prohibits the use of a factor that does not exist in data
include_phase <- incldeFactorExist("Phase", data_filtered, TRUE) 
include_sex <- incldeFactorExist("Sex", data_filtered, TRUE)
twoWayANOVA <- TRUE

#include_gender <- incldeFactorExist("Gender", data_filtered, TRUE)

################################ Calculating and summarizing #########################################################

# Add new columns (SleepCount, TotalCount...) to dataframe containing the required calculations
total_sleep_info_per_change <- data_filtered %>%
    group_by_at(intersect(c("AnimalNum", "Batch", "Change", "Group", "Phase", "Sex"), names(data_filtered))) %>%  # Check if all column names (esp. "Phase" and "Sex") exist in data_filtered
    summarize(
        SleepCount = sum(ActivityIndex == 0),
        TotalCount = n(),
        PercentageSleep = (SleepCount / TotalCount) * 100,
        TotalSleepingTime = sum(ActivityIndex == 0),
        SleepBouts = sum(ActivityIndex == 0 & lag(ActivityIndex, default = 1) != 0),
        AvgSleepBoutDuration = ifelse(SleepBouts == 0, 0, TotalSleepingTime / SleepBouts)
    )

# Update Group column based on SUS animals
# Overwrites some SIS and CON to SUS and RES groups
total_sleep_info_per_change <- total_sleep_info_per_change %>%
    mutate(
        Group = if_else(AnimalNum %in% sus_animals, "SUS", if_else(Group == "SIS", "RES", Group))
    )

# Summarize the data
# Creates an overview of the sleep-activity of each individual
summarized_data <- total_sleep_info_per_change %>%
    group_by_at(intersect(c("AnimalNum", "Phase", "Sex"), names(total_sleep_info_per_change))) %>%  # Check if "Phase", "AnimalNum" and "Sex" exist in total_sleep_info_per_change
    summarize(
        Batch = first(Batch),
        Group = first(Group),
        SleepBouts = sum(SleepBouts),
        TotalSleepingTime = sum(TotalSleepingTime),
        PercentageSleep = mean(PercentageSleep),
        AvgSleepBoutDuration = mean(AvgSleepBoutDuration)
    ) %>%
    ungroup()

# Rename
overallData <- summarized_data

###################### Execute tests and generate plots ###################################################

# Get the list of columns to plot (excluding non-numeric values, which are added manually)
columnsToPlot <- setdiff(names(overallData), c("AnimalNum", "Group", "Phase", "Batch", "Sex"))

# Run statistical tests on given data with wanted columns
# Generates three result dataframes

# Initialize empty lists
allTestResults <- list()
allPlots <- list()
allPosthocResults <- list()

# Declare vectors of the variables which are not always included
phases <-  " "
if (include_phase) phases <-  c("Active", "Inactive")
sexes <-  " "
if (include_sex) sexes <- c("m", "f")

# Iterate through each variable, phase, and sex, and perform tests
for (variable in columnsToPlot) {
    for (phase in phases) {
        for (sex in sexes) {
            result <- testAndPlotVariable(overallData, variable, phase, sex)
            # Add result list (containing the columns testResults, plot, posthocResults) to other fitting list
            if (!is.null(result)) {
                # posthocResults is always NULL for the Wilcoxon test
                if (is.null(result$posthocResults)) {
                    allTestResults <- c(allTestResults, list(result$testResults))
                } else {
                    allTestResults <- c(allTestResults, list(result$testResults))
                    allPosthocResults <- c(allPosthocResults, list(result$posthocResults))
                }
                # Add the plot
                allPlots <- c(allPlots, list(result$plot))
            }
        }
    }
}

####################### Saving the results #############################################################

# Convert the list of test results to a data frame
allTestResultsDf <- bind_rows(allTestResults)
# Save the test results data frame to a CSV file
write.csv(allTestResultsDf, file = paste0(sleep_directory, "/test_results_Sleep.csv"), row.names = FALSE)
# Save the post hoc results to a CSV file
if (!is.null(allPosthocResults) && length(allPosthocResults) > 0) {
    allPosthocResultsDf <- bind_rows(allPosthocResults)
    write.csv(allPosthocResultsDf, file = paste0(sleep_directory, "/posthoc_results_Sleep.csv"), row.names = FALSE)
}

# Save the plots
savePlotsInDir(allTestResults, allPlots, graphs_directory, ".svg")

############### Show plots in R ########################################################################

# Create a grid of plots
grid.arrange(grobs = allPlots, ncol = 4)
