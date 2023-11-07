## Read in data from Folders and xlsx data. The data is organized in subfolders. 
#  This will read in the data of different xlsx sheets and merge them based on the name of the folder. 
#  Also, the data will be sorted and formatted
#  Later, the data points will be plotted.

library(readxl)      # load readxl package for reading excel files
library(dplyr)       # load dplyr package for data manipulation functions
library(purrr)       # load purrr package for functional programming
library(stringr)     # load stringr package for string manipulation
library(lubridate)   # load lubridate package for date and time functions
library(tidyr)
library(forcats)

# set the working directory to the parent directory containing the subfolders
setwd("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/BatchAnalysis")

# get a list of all subfolders in the directory
subfolders <- list.dirs(".", recursive = FALSE)

# separate the Animal column into AnimalNumber and Cage columns
data <- separate(data, Animal, c("AnimalNum", "Cage"), sep = "_", remove = FALSE)

# convert the DateTime column to a datetime format
data$DateTime <- as.POSIXct(data$DateTime, format = "%d.%m.%Y %H:%M")

# rename ActivyIndex to ActivityIndex
data <- data %>%
  rename(ActivityIndex = ActivyIndex)

data <- data %>%
  mutate(
    Phase = ifelse(
      format(DateTime, "%H:%M") >= "18:30" | format(DateTime, "%H:%M") < "06:30",
      "Active",
      "Inactive"
    ),
    PriorActive = ifelse(
      format(DateTime, "%H:%M") >= "07:30" & format(DateTime, "%H:%M") <= "18:30",
      "TRUE",
      "FALSE"
    ),
    Batch = as.factor(Batch), # convert Batch to a factor variable
    Group = ifelse(
      AnimalNum %in% c("OR413", "OR414", "OR415", "OR416", "OR537", "OR538", "OR539", "OR540"),
      "CON",
      "SIS"
    )
  ) %>%
  group_by(Batch) %>%
  mutate(Hour = difftime(DateTime, first(DateTime), units = "hours")) %>%
  ungroup() 

# Create a new column to represent the consecutive Active phases for each animal
data$ConsecActive <- with(data, ave(Phase, AnimalNum, FUN=function(x) {
  cumsum(c(0, diff(ifelse(x == "Active", 1, 0))) == 1)
}))
data$ConsecActive <- as.numeric(data$ConsecActive)

## Cookie Habituation Challenge, only activate this when analyzing cookie data
#data <- data %>%
#  mutate(Cookie = ifelse(
#    ConsecActive == 1 & Phase == "Inactive" & format(DateTime, "%H:%M") >= "17:00" & format(DateTime, "%H:%M") <= "17:45",
#    "TRUE",
#    "FALSE"
#  ))

#remove last two active and inactive phases of CC4 due to grid within cage
data <- data %>%
  filter(!(Change == "CC4" & Phase %in% c("Active", "Inactive") & ConsecActive >= 15))

# exclude animals with non-complete datasets
excluded_animals <- c('OQ750', 'OQ751', 'OQ752', 'OQ753', '0001', '0002')
data <- data[!data$AnimalNum %in% excluded_animals,]


#remove last two active and inactive phases of CC4 due to grid within cage
data <- data %>%
  filter(!(AnimalNum == "CC4" & Phase %in% c("Active", "Inactive") & ConsecActive >= 15))


# Remove the first and last inactive phase for each Change and Batch
data_filtered <- data %>%
  group_by(Batch, Change, AnimalNum) %>% 
  mutate(ActivePeriods = ifelse(Phase == "Active", 1, 0),
         InactivePeriods = ifelse(Phase == "Inactive", 1, 0),
         TotalPeriods = sum(ActivePeriods, InactivePeriods),
         ConsecActive = ifelse(Phase == "Active", cumsum(c(1, diff(ifelse(Phase == "Inactive", 0, 1))) == 1), 0),
         ConsecInactive = ifelse(Phase == "Inactive", cumsum(c(1, diff(ifelse(Phase == "Active", 0, 1))) == 1), 0)) %>% 
  filter(!(ConsecInactive == max(ConsecInactive[Phase == "Inactive"]) & Phase == "Inactive" |
             ConsecInactive == min(ConsecInactive[Phase == "Inactive"]) & Phase == "Inactive")) %>% 
  ungroup()

# Add a new column for SleepBouts
data_filtered <- data_filtered %>%
  group_by(AnimalNum, Phase, Change) %>%
  mutate(
    SleepBouts = cumsum(ifelse(ActivityIndex == 0 & lag(ActivityIndex != 0, default = TRUE) | row_number() == 1 & ActivityIndex == 0, 1, 0))
  ) %>%
  ungroup()

# Round datetime down to the nearest half-hour
data_filtered$DateTime30Min <- format(as.POSIXct(trunc(as.numeric(as.POSIXct(data_filtered$DateTime, format = "%d.%m.%Y %H:%M")) / (30*60)) * (30*60), origin = "1970-01-01"), format = "%Y-%m-%d %H:%M:%S")

# Add column with shifted datetime rounded down to nearest half-hour
data_filtered$DateTime30MinShifted <- format(as.POSIXct(trunc(as.numeric(as.POSIXct(data_filtered$DateTime, format = "%d.%m.%Y %H:%M")) / (30*60)) * (30*60) + 30*60, origin = "1970-01-01"), format = "%Y-%m-%d %H:%M:%S")



# Aggregate data by AnimalNum, Batch, Group, Change, Phase, PriorActive, DateTime30Min, and then calculate the ActivityIndex
data_filtered_agg <- data_filtered %>%
  group_by(AnimalNum, Batch, Group, Change, Phase, PriorActive, DateTime30Min) %>%
  summarize(ActivityIndex = mean(ActivityIndex)) %>%
  ungroup()

# Create a new column 'SUS'
data_filtered_agg <- data_filtered_agg %>%
mutate(SUS = AnimalNum %in% c("0001", "0004", "OR750", "OR751", "OR755", "OR762", "OR764", "OR770", "OR771", "OR106", "OR111", "OR112", "OR113", "OR120", "OR134", 
                              "OR420",
                              "OR424",
                              "OR426",
                              "OR428",
                              "OR432",
                              "OR433",
                              "OR434",
                              "OR1545",
                              "OR1546",
                              "OR1547",
                              "OR529",
                              "OR530",
                              "OR531",
                              "OR550",
                              "OR551",
                              "OR552",
                              "OR553",
                              "OR554",
                              "OR555",
                              "OR556"))

# Combine the two datetime columns to form the interval
data_filtered_agg$DateTime30MinShifted <- lead(data_filtered_agg$DateTime30Min)
data_filtered_agg$TimeInterval <- paste0(data_filtered_agg$DateTime30Min, " to ", data_filtered_agg$DateTime30MinShifted)

# Calculate the number of half-hour periods elapsed since the first half-hour period for each Batch and AnimalNum combination
data_filtered_agg <- data_filtered_agg %>%
  group_by(Batch, AnimalNum) %>%
  mutate(HalfHourElapsed = as.numeric(difftime(DateTime30Min, first(DateTime30Min), units = "secs"))/1800) %>%
  ungroup()

#############################################################################################################################################################

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




# sus_animals <- c("755","764","771","111","750","753","0004") # replace with your specific animal IDs
sus_animals <- c("0001", "0004", "OQ750", "OQ751", "OQ755", "OQ762", "OQ764", "OQ770", "OQ771", "OR106", "OR111", "OR112", "OR113", "OR120", "OR134", "420",
                 "424",
                 "426",
                 "428",
                 "432",
                 "433",
                 "434",
                 "1545",
                 "1546",
                 "1547")


# Define group colors
group_cols <- c("#1e3791", "#76A2E8", "#F79719")
total_sleep_info_per_change <- total_sleep_info_per_change %>%
  mutate(Group = if_else(AnimalNum %in% sus_animals, "SUS",
                         if_else(Group == "SIS", "RES", Group)))

# Calculate the means and standard deviations of TotalSleepBouts for each group
means_TotalSleepBouts <- total_sleep_info_per_change %>% 
  group_by(AnimalNum, Group) %>% 
  summarise(TotalSleepBouts = mean(TotalSleepBouts),
            sd_TotalSleepBouts = sd(TotalSleepBouts),
            .groups = "drop")  # Drop grouping)

# Reset grouping
means <- ungroup(means_TotalSleepBouts)

# Perform Shapiro-Wilk normality test for each group

# Normality test for CON group
con_norm_TotalSleepBouts <- shapiro.test(means_TotalSleepBouts$TotalSleepBouts[means_TotalSleepBouts$Group == "CON"])

# Normality test for RES group
res_norm_TotalSleepBouts <- shapiro.test(means_TotalSleepBouts$TotalSleepBouts[means_TotalSleepBouts$Group == "RES"])

# Normality test for SUS group
sus_norm_TotalSleepBouts <- shapiro.test(means_TotalSleepBouts$TotalSleepBouts[means_TotalSleepBouts$Group == "SUS"])

# Perform ANOVA if all groups are normal, otherwise use Kruskal-Wallis
if (con_norm_TotalSleepBouts$p.value >= 0.05 & res_norm_TotalSleepBouts$p.value >= 0.05 & sus_norm_TotalSleepBouts$p.value >= 0.05) {
  # If all groups are normal, use ANOVA
  anova_test <- aov(TotalSleepBouts ~ Group, data = means_TotalSleepBouts)
  test_name <- "ANOVA"
  test_pval_TotalSleepBouts <- summary(anova_test)[[1]][["Pr(>F)"]][1]
  # Determine the significance level for ANOVA
  sig_levels_aov <- sprintf("%.3f", test_pval_TotalSleepBouts)
  pairwise_aov_TotalSleepBouts <- dunn_test(means_TotalSleepBouts, TotalSleepBouts~Group,
                            p.adjust.method = "holm"
  )
} else {
  # If at least one group is not normal, use Kruskal-Wallis test
  kruskal_test <- kruskal.test(TotalSleepBouts ~ Group, data = means_TotalSleepBouts)
  test_name <- "Kruskal-Wallis"
  test_pval_TotalSleepBouts <- kruskal_test$p.value
  # Determine the significance level for Kruskal-Wallis test
  sig_levels_kw_TotalSleepBouts <- sprintf("%.3f", p.adjust(test_pval_TotalSleepBouts, method = "BH"))
}

# Create plot for means
p_means_TotalSleepBouts <- ggplot(means_TotalSleepBouts, aes(x = Group, y = TotalSleepBouts, fill = Group, colour = Group)) +
  geom_jitter(size=4, alpha=0.5, width=0.2, shape=16) + # Add dodged columns for mean values
  geom_errorbar(aes(ymin = TotalSleepBouts - sd_TotalSleepBouts, ymax = TotalSleepBouts + sd_TotalSleepBouts), 
                position = position_dodge(width = 0.9), width = 0.2) + # Add error bars
  stat_summary(fun.min=function(z) {quantile(z,0.25)}, fun.max=function(z) {quantile(z,0.75)}, fun=median, color="black", size=1, shape=16) + # Add indication for mean and error bars
  scale_fill_manual(values = group_cols) + # Set fill colors
  scale_color_manual(values = group_cols) +
  scale_y_continuous(expand=c(0.1,0.1)) +
  labs(title = "TotalSleepBouts", x = "Group", y = "Activity Index [a.u.]") + # Add labels
  theme_minimal_hgrid(12, rel_small = 1) +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        plot.subtitle = element_text(hjust = 0.5, face = "plain"),
        axis.text.x = element_text(angle = 0, vjust = 1, size = 11, hjust = 0.45),
        axis.title.x = element_blank(),
        legend.position = "none") # Use a white and black theme

# Define group colors
group_cols <- c("#1e3791", "#76A2E8", "#F79719")

# Calculate the means and standard deviations of SleepingPercentage for each group
means_SleepingPercentage <- total_sleep_info_per_change %>% 
  group_by(AnimalNum, Group) %>% 
  summarise(SleepingPercentage = mean(SleepingPercentage),  # Updated column name
            sd_SleepingPercentage = sd(SleepingPercentage),  # Updated column name
            .groups = "drop")  # Drop grouping)

# Calculate the means and standard deviations of AvgSleepBoutDuration for each group
means_AvgSleepBoutDuration <- total_sleep_info_per_change %>% 
  group_by(AnimalNum, Group) %>% 
  summarise(AvgSleepBoutDuration = mean(AvgSleepBoutDuration),  # Updated column name
            sd_AvgSleepBoutDuration = sd(AvgSleepBoutDuration),  # Updated column name
            .groups = "drop")  # Drop grouping)

# Reset grouping
means <- ungroup(means_SleepingPercentage)  # Use the appropriate variable name

# Perform Shapiro-Wilk normality test for each group

# Normality test for CON group
con_norm_SleepingPercentage <- shapiro.test(means_SleepingPercentage$SleepingPercentage[means_SleepingPercentage$Group == "CON"])  # Updated variable name

# Normality test for RES group
res_norm_SleepingPercentage <- shapiro.test(means_SleepingPercentage$SleepingPercentage[means_SleepingPercentage$Group == "RES"])  # Updated variable name

# Normality test for SUS group
sus_norm_SleepingPercentage <- shapiro.test(means_SleepingPercentage$SleepingPercentage[means_SleepingPercentage$Group == "SUS"])  # Updated variable name

# Perform ANOVA if all groups are normal, otherwise use Kruskal-Wallis
if (con_norm_SleepingPercentage$p.value >= 0.05 & res_norm_SleepingPercentage$p.value >= 0.05 & sus_norm_SleepingPercentage$p.value >= 0.05) {
  # If all groups are normal, use ANOVA
  anova_test <- aov(SleepingPercentage ~ Group, data = means_SleepingPercentage)  # Updated variable name
  test_name <- "ANOVA"
  test_pval_SleepingPercentage <- summary(anova_test)[[1]][["Pr(>F)"]][1]
  # Determine the significance level for ANOVA
  sig_levels_aov <- sprintf("%.3f", test_pval_SleepingPercentage)
  pairwise_aov_SleepingPercentage <- dunn_test(means_SleepingPercentage, SleepingPercentage~Group,  # Updated variable name
                                               p.adjust.method = "holm"
  )
} else {
  # If at least one group is not normal, use Kruskal-Wallis test
  kruskal_test <- kruskal.test(SleepingPercentage ~ Group, data = means_SleepingPercentage)  # Updated variable name
  test_name <- "Kruskal-Wallis"
  test_pval_SleepingPercentage <- kruskal_test$p.value
  # Determine the significance level for Kruskal-Wallis test
  sig_levels_kw_SleepingPercentage <- sprintf("%.3f", p.adjust(test_pval_SleepingPercentage, method = "BH"))
}

# Create plot for means
p_means_SleepingPercentage <- ggplot(means_SleepingPercentage, aes(x = Group, y = SleepingPercentage, fill = Group, colour = Group)) +
  geom_jitter(size=4, alpha=0.5, width=0.2, shape=16) + # Add dodged columns for mean values
  geom_errorbar(aes(ymin = SleepingPercentage - sd_SleepingPercentage, ymax = SleepingPercentage + sd_SleepingPercentage), 
                position = position_dodge(width = 0.9), width = 0.2) + # Add error bars
  stat_summary(fun.min=function(z) {quantile(z,0.25)}, fun.max=function(z) {quantile(z,0.75)}, fun=median, color="black", size=1, shape=16) + # Add indication for mean and error bars
  scale_fill_manual(values = group_cols) + # Set fill colors
  scale_color_manual(values = group_cols) +
  scale_y_continuous(expand=c(0.1,0.1)) +
  labs(title = "SleepingPercentage", x = "Group", y = "Activity Index [a.u.]") + # Add labels
  theme_minimal_hgrid(12, rel_small = 1) +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        plot.subtitle = element_text(hjust = 0.5, face = "plain"),
        axis.text.x = element_text(angle = 0, vjust = 1, size = 11, hjust = 0.45),
        axis.title.x = element_blank(),
        legend.position = "none") # Use a white and black theme

# Create plot for AvgSleepBoutDuration
p_avg_sleep_bout_duration <- ggplot(means_AvgSleepBoutDuration, aes(x = Group, y = AvgSleepBoutDuration, fill = Group, colour = Group)) +
  geom_jitter(size=4, alpha=0.5, width=0.2, shape=16) + # Add dodged columns for mean values
  geom_errorbar(aes(ymin = AvgSleepBoutDuration - sd_AvgSleepBoutDuration, ymax = AvgSleepBoutDuration + sd_AvgSleepBoutDuration), 
                position = position_dodge(width = 0.9), width = 0.2) + # Add error bars
  stat_summary(fun.min=function(z) {quantile(z,0.25)}, fun.max=function(z) {quantile(z,0.75)}, fun=median, color="black", size=1, shape=16) + # Add indication for mean and error bars
  scale_fill_manual(values = group_cols) + # Set fill colors
  scale_color_manual(values = group_cols) +
  scale_y_continuous(expand=c(0.1,0.1)) +
  labs(title = "AvgSleepBoutDuration", x = "Group", y = "Duration [units]") + # Add labels
  theme_minimal_hgrid(12, rel_small = 1) +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        plot.subtitle = element_text(hjust = 0.5, face = "plain"),
        axis.text.x = element_text(angle = 0, vjust = 1, size = 11, hjust = 0.45),
        axis.title.x = element_blank(),
        legend.position = "none") # Use a white and black theme

# Define group colors
group_cols <- c("#1e3791", "#76A2E8", "#F79719")

# Calculate the means and standard deviations of TotalSleepingTime for each group
means_TotalSleepingTime <- total_sleep_info_per_change %>% 
  group_by(AnimalNum, Group) %>% 
  summarise(TotalSleepingTime = mean(TotalSleepingTime),  # Updated column name
            sd_TotalSleepingTime = sd(TotalSleepingTime),  # Updated column name
            .groups = "drop")  # Drop grouping)

# Reset grouping
means <- ungroup(means_TotalSleepingTime)  # Updated variable name

# Perform Shapiro-Wilk normality test for each group

# Normality test for CON group
con_norm_TotalSleepingTime <- shapiro.test(means_TotalSleepingTime$TotalSleepingTime[means_TotalSleepingTime$Group == "CON"])  # Updated variable name

# Normality test for RES group
res_norm_TotalSleepingTime <- shapiro.test(means_TotalSleepingTime$TotalSleepingTime[means_TotalSleepingTime$Group == "RES"])  # Updated variable name

# Normality test for SUS group
sus_norm_TotalSleepingTime <- shapiro.test(means_TotalSleepingTime$TotalSleepingTime[means_TotalSleepingTime$Group == "SUS"])  # Updated variable name

# Perform ANOVA if all groups are normal, otherwise use Kruskal-Wallis
if (con_norm_TotalSleepingTime$p.value >= 0.05 & res_norm_TotalSleepingTime$p.value >= 0.05 & sus_norm_TotalSleepingTime$p.value >= 0.05) {
  # If all groups are normal, use ANOVA
  anova_test <- aov(TotalSleepingTime ~ Group, data = means_TotalSleepingTime)  # Updated variable name
  test_name <- "ANOVA"
  test_pval_TotalSleepingTime <- summary(anova_test)[[1]][["Pr(>F)"]][1]
  # Determine the significance level for ANOVA
  sig_levels_aov <- sprintf("%.3f", test_pval_TotalSleepingTime)
  pairwise_aov_TotalSleepingTime <- dunn_test(means_TotalSleepingTime, TotalSleepingTime~Group,  # Updated variable name
                                              p.adjust.method = "holm"
  )
} else {
  # If at least one group is not normal, use Kruskal-Wallis test
  kruskal_test <- kruskal.test(TotalSleepingTime ~ Group, data = means_TotalSleepingTime)  # Updated variable name
  test_name <- "Kruskal-Wallis"
  test_pval_TotalSleepingTime <- kruskal_test$p.value
  # Determine the significance level for Kruskal-Wallis test
  sig_levels_kw_TotalSleepingTime <- sprintf("%.3f", p.adjust(test_pval_TotalSleepingTime, method = "BH"))
}

# Create plot for means
p_means_TotalSleepingTime <- ggplot(means_TotalSleepingTime, aes(x = Group, y = TotalSleepingTime, fill = Group, colour = Group)) +
  geom_jitter(size=4, alpha=0.5, width=0.2, shape=16) + # Add dodged columns for mean values
  geom_errorbar(aes(ymin = TotalSleepingTime - sd_TotalSleepingTime, ymax = TotalSleepingTime + sd_TotalSleepingTime), 
                position = position_dodge(width = 0.9), width = 0.2) + # Add error bars
  stat_summary(fun.min=function(z) {quantile(z,0.25)}, fun.max=function(z) {quantile(z,0.75)}, fun=median, color="black", size=1, shape=16) + # Add indication for mean and error bars
  scale_fill_manual(values = group_cols) + # Set fill colors
  scale_color_manual(values = group_cols) +
  scale_y_continuous(expand=c(0.1,0.1)) +
  labs(title = "TotalSleepingTime", x = "Group", y = "Activity Index [a.u.]") + # Add labels
  theme_minimal_hgrid(12, rel_small = 1) +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        plot.subtitle = element_text(hjust = 0.5, face = "plain"),
        axis.text.x = element_text(angle = 0, vjust = 1, size = 11, hjust = 0.45),
        axis.title.x = element_blank(),
        legend.position = "none") # Use a white and black theme










# Arrange two plots side by side
plot_grid(p_indiv, p_means, ncol = 2, align = "v", labels = c("A", "B"))









#############################################################################################################################################################
## Plot all cage changes in one graph
#  Plots data in line plot without separation of Changes
#  Groups CON and SIS are grouped and the mean and SD is plotted
#  Time (y axis) is plotted as the number of half hour intervals starting from the first active phase

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(forcats)
library(cowplot)

group_cols <- c("#1e3791", "#00ac8c")

# Create the plot
plot <- ggplot(data_filtered_agg, aes(x = HalfHourElapsed, y = ActivityIndex, group = Group, color = Group)) +
  geom_path(stat = "summary") +
  stat_summary(aes(fill = Group), fun = mean,
               fun.min = function(x) mean(x) - sd(x), 
               fun.max = function(x) mean(x) + sd(x), 
               geom = "ribbon", 
               alpha = 0.2, colour = NA) +
  scale_color_manual(name = NULL, values = group_cols) + 
  scale_fill_manual(name = NULL, values = group_cols) +
  theme_minimal_hgrid(12, rel_small = 1) +
  labs(title = bquote(~bold(.(paste("Activity Index")))),
       y = "Activity [a.u.]", x = "Day/Night", caption = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        plot.subtitle = element_text(hjust = 0.5, face = "plain"),
        legend.position = "top",
        legend.justification = "right",
        legend.text = element_text(size = 9),
        legend.box.spacing = unit(1, "pt"),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 11, hjust = 1),
        axis.title.x = element_blank())

plot




# Create the cookie plot
################################################################################
## Compare Means between Groups during 2 hours preceding the Active Phase
#  This is used to quantify activity before the onset of the active phase

# Create the cookie plot
################################################################################
## Compare Means between Groups during 2 hours preceding the Active Phase
#  This is used to quantify activity before the onset of the active phase

# Load necessary libraries
library(cowplot)
library(ggsignif)
library(ggplot2)
library(cowplot)
library(colorspace)
library(dplyr)
library(openxlsx)
library(tidyr)
library(broom)
library(readxl)
library(rstatix)
library(ggpubr)
library(Hmisc)
## Compare Means between Groups during 2 hours preceding the Active Phase

# Define the list of SUS animals
sus_animals <- c("0001", "0004", "OR750", "OR751", "OR755", "OR762", "OR764", "OR770", "OR771", "OR106", "OR111", "OR112", "OR113", "OR120", "OR134")

# Replace Group values with "SUS" for SUS animals, "RES" for SIS animals, and keep other groups as is
data <- data %>%
  mutate(Group = if_else(AnimalNum %in% sus_animals, "SUS",
                         if_else(Group == "SIS", "RES", Group)))

# Define group colors
group_cols <- c("#1e3791", "#76A2E8", "#F79719")

# Filter data to include only the rows with 'Cookie' value as 'TRUE'
cookie_data <- data %>% 
  filter(Cookie == 'TRUE')

# Calculate the means and standard deviations of ActivityIndex for each group
means_cookie <- cookie_data %>% 
  group_by(Group, AnimalNum) %>% 
  summarise(ActivityIndex = mean(ActivityIndex),
            sd_ActivityIndex = sd(ActivityIndex))

# Perform Shapiro-Wilk normality test for each group

# Normality test for CON group
con_norm_cookie <- shapiro.test(means_cookie$ActivityIndex[means_cookie$Group == "CON"])

# Normality test for RES group
res_norm_cookie <- shapiro.test(means_cookie$ActivityIndex[means_cookie$Group == "RES"])

# Normality test for SUS group
sus_norm_cookie <- shapiro.test(means_cookie$ActivityIndex[means_cookie$Group == "SUS"])

# Perform ANOVA if all groups are normal, otherwise use Kruskal-Wallis
if (con_norm_cookie$p.value >= 0.05 & res_norm_cookie$p.value >= 0.05 & sus_norm_cookie$p.value >= 0.05) {
  # If all groups are normal, use ANOVA
  anova_test <- aov(ActivityIndex ~ Group, data = means_cookie)
  test_name <- "ANOVA"
  test_pval <- summary(anova_test)[[1]][["Pr(>F)"]][1]
  # Determine the significance level for ANOVA
  sig_levels_aov <- sprintf("%.3f", test_pval)
  pairwise_aov <- dunn_test(means_cookie, ActivityIndex~Group,
                            p.adjust.method = "holm"
  )
} else {
  # If at least one group is not normal, use Kruskal-Wallis test
  kruskal_test <- kruskal.test(ActivityIndex ~ Group, data = means_cookie)
  test_name <- "Kruskal-Wallis"
  test_pval <- kruskal_test$p.value
  # Determine the significance level for Kruskal-Wallis test
  sig_levels_kw <- sprintf("%.3f", p.adjust(test_pval, method = "BH"))
}
# Create plot for individual values
p_indiv_cookie <- ggplot(cookie_data, aes(x = Group, y = ActivityIndex, color = Group)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5) + # Add jittered dots
  scale_color_manual(values = group_cols) + # Set color scheme
  scale_y_continuous(expand=c(0.1,0.1)) +
  labs(title = "All Activity", x = "Group", y = "Activity Index [a.u.]") + # Add labels
  theme_minimal_hgrid(12, rel_small = 1) +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        plot.subtitle = element_text(hjust = 0.5, face = "plain"),
        axis.text.x = element_text(angle = 0, vjust = 1, size = 11, hjust = 0.45),
        axis.title.x = element_blank(),
        legend.position = "none") # Use a white and black theme

# Create plot for means
p_means_cookie <- ggplot(means_cookie, aes(x = Group, y = ActivityIndex, fill = Group, colour = Group)) +
  geom_jitter(size=4, alpha=0.5, width=0.2, shape=16) + # Add dodged columns for mean values
  geom_errorbar(aes(ymin = ActivityIndex - sd_ActivityIndex, ymax = ActivityIndex + sd_ActivityIndex), 
                position = position_dodge(width = 0.9), width = 0.2) + # Add error bars
  stat_summary(fun.min=function(z) {quantile(z,0.25)}, fun.max=function(z) {quantile(z,0.75)}, fun=median, color="black", size=1, shape=16) + # Add indication for mean and error bars
  scale_fill_manual(values = group_cols) + # Set fill colors
  scale_color_manual(values = group_cols) +
  scale_y_continuous(expand=c(0.1,0.1)) +
  labs(title = "Individual Activity", x = "Group", y = "Activity Index [a.u.]") + # Add labels
  theme_minimal_hgrid(12, rel_small = 1) +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        plot.subtitle = element_text(hjust = 0.5, face = "plain"),
        axis.text.x = element_text(angle = 0, vjust = 1, size = 11, hjust = 0.45),
        axis.title.x = element_blank(),
        legend.position = "none") # Use a white and black theme


# Arrange two plots side by side
plot_grid(p_indiv_cookie, p_means_cookie, ncol = 2, align = "v", labels = c("A", "B"))



cookie_plot <- ggplot(cookie_data, aes(x = DateTime, y = ActivityIndex, group = Group, color = Group)) +
  geom_path(stat = "summary") +
  stat_summary(aes(fill = Group), fun = mean,
               fun.min = function(x) mean(x) - sd(x), 
               fun.max = function(x) mean(x) + sd(x), 
               geom = "ribbon", 
               alpha = 0.2, colour = NA) +
  scale_color_manual(name = NULL, values = group_cols) + 
  scale_fill_manual(name = NULL, values = group_cols) +
  theme_minimal_hgrid(12, rel_small = 1) +
  labs(title = bquote(~bold(.(paste("Cookie Challenge")))),
       y = "Activity [a.u.]", x = "Day/Night", caption = "") +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        plot.subtitle = element_text(hjust = 0.5, face = "plain"),
        legend.position = "top",
        legend.justification = "right",
        legend.text = element_text(size = 9),
        legend.box.spacing = unit(1, "pt"),
        axis.text.x = element_text(vjust = 1, size = 11, hjust = 0.5),
        axis.title.x = element_blank())

cookie_plot



## Plot graphs for group changes in separate graphs and group the plots together
#  Plots data in line plot and 2x2 grid with a separate plot for each change
#  Groups CON and SIS are grouped and the mean and SD is plotted
#  Time (y axis) is plotted as the number of half hour intervals starting from the first active phase

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(forcats)
library(cowplot)

group_cols <- c("#1e3791", "#00ac8c")

# Split data into separate data frames for each CC
data_filtered_agg_Change <- data_filtered_agg %>% 
  split(data_filtered_agg$Change)

# Create plots for each CC
plot_list <- lapply(sort(unique(data_filtered_agg$Change)), function(Change) {
  
  # Subset data for the current CC
  data_filtered_agg_Change_subset <- data_filtered_agg_Change[[Change]]
  
  # Create the plot
  ggplot(data_filtered_agg_Change_subset, aes(x = HalfHourElapsed, y = ActivityIndex, group = Group, color = Group)) +
    geom_path(stat = "summary") +
    stat_summary(aes(fill = Group), fun = mean,
                 fun.min = function(x) mean(x) - sd(x), 
                 fun.max = function(x) mean(x) + sd(x), 
                 geom = "ribbon", 
                 alpha = 0.2, colour = NA) +
    # Set color and fill scales
    scale_color_manual(name = NULL, values = group_cols) + 
    scale_fill_manual(name = NULL, values = group_cols) +
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

# Arrange plots in a grid
grid.arrange(grobs = plot_list, ncol = 2, top = "Activity Index by Cage Change")




# Subset data by phase and group
con_active <- subset(data_filtered_agg, Phase == "Active" & Group == "CON")
sis_active <- subset(data_filtered_agg, Phase == "Active" & Group == "SIS")
con_inactive <- subset(data_filtered_agg, Phase == "Inactive" & Group == "CON")
sis_inactive <- subset(data_filtered_agg, Phase == "Inactive" & Group == "SIS")

# Compare ActivityIndex between Con and SIS during Active phase
t.test(con_active$ActivityIndex, sis_active$ActivityIndex)

# Compare ActivityIndex between Con and SIS during Inactive phase
t.test(con_inactive$ActivityIndex, sis_inactive$ActivityIndex)

library(ggplot2)

# Plot for Phase = Active
ggplot(data = subset(data_filtered_agg, Phase == "Active"), aes(x = Group, y = ActivityIndex, fill = Group)) + 
  geom_boxplot() +
  labs(title = "Activity Index during Phase = Active",
       x = "Group", y = "Activity Index") +
  theme_minimal()

# Plot for Phase = Inactive
ggplot(data = subset(data_filtered_agg, Phase == "Inactive"), aes(x = Group, y = ActivityIndex, fill = Group)) + 
  geom_boxplot() +
  labs(title = "Activity Index during Phase = Inactive",
       x = "Group", y = "Activity Index") +
  theme_minimal()






################################################################################
## Compare Means between Groups during 2 hours preceding the Active Phase
#  This is used to quantify activity before the onset of the active phase

library(cowplot)
library(ggsignif)
library(ggstatsplot)


## Compare Means between Groups during 2 hours preceding the Active Phase

# Filter the data for Phase == Active
active_data <- data_filtered_agg %>% 
  filter(Phase == 'Active')

# Filter the data for Phase == Inactive
inactive_data <- data_filtered_agg %>% 
  filter(Phase == 'Inactive')

# Calculate the means and standard deviations of ActivityIndex for each group
means_active <- active_data %>% 
  group_by(Group, AnimalNum) %>% 
  summarise(ActivityIndex = mean(ActivityIndex),
            sd_ActivityIndex = sd(ActivityIndex))

# Calculate the means and standard deviations of ActivityIndex for each group
means_inactive <- inactive_data %>% 
  group_by(Group, AnimalNum) %>% 
  summarise(ActivityIndex = mean(ActivityIndex),
            sd_ActivityIndex = sd(ActivityIndex))

# Normality test for CTRL group
con_norm_active <- shapiro.test(means_active$ActivityIndex[means_active$Group == "CON"])
con_norm_active

# Normality test for SIS group
sis_norm_active <- shapiro.test(means_active$ActivityIndex[means_active$Group == "SIS"])
sis_norm_active

# Normality test for CTRL group
con_norm_inactive <- shapiro.test(means_inactive$ActivityIndex[means_inactive$Group == "CON"])
con_norm_inactive

# Normality test for SIS group
sis_norm_inactive <- shapiro.test(means_inactive$ActivityIndex[means_inactive$Group == "SIS"])
sis_norm_inactive

# Determine which test to use based on normality of both groups
if (con_norm_active$p.value >= 0.05 & sis_norm_active$p.value >= 0.05) {
  # If normal, use t-test
  t_test_active <- t.test(ActivityIndex ~ Group, data = means_active)
  test_name_active <- "t.test"
  test_pval_active <- t_test_active$p.value
  # Determine the significance level for t-test
  sig_levels_active <- sprintf("%.3f", test_pval_active)
} else {
  # If not normal, use Wilcoxon rank-sum test
  wilcox_test_active <- wilcox.test(ActivityIndex ~ Group, data = means_active)
  test_name_active <- "wilcox.test"
  test_pval_active <- wilcox_test_active$p.value
  # Determine the significance level for Wilcoxon rank-sum test
  sig_levels_active <- sprintf("%.3f", p.adjust(test_pval_active, method = "BH"))
}

# Determine which test to use based on normality of both groups
if (con_norm_inactive$p.value >= 0.05 & sis_norm_inactive$p.value >= 0.05) {
  # If normal, use t-test
  t_test_inactive <- t.test(ActivityIndex ~ Group, data = means_inactive)
  test_name_inactive <- "t.test"
  test_pval_inactive <- t_test_inactive$p.value
  # Determine the significance level for t-test
  sig_levels_inactive <- sprintf("%.3f", test_pval_inactive)
} else {
  # If not normal, use Wilcoxon rank-sum test
  wilcox_test_inactive <- wilcox.test(ActivityIndex ~ Group, data = means_inactive)
  test_name_inactive <- "wilcox.test"
  test_pval_inactive <- wilcox_test_inactive$p.value
  # Determine the significance level for Wilcoxon rank-sum test
  sig_levels_inactive <- sprintf("%.3f", p.adjust(test_pval_inactive, method = "BH"))
}

# Create plot for individual values
p_indiv_active <- ggplot(active_data, aes(x = Group, y = ActivityIndex, color = Group)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5) + # Add jittered dots
  scale_color_manual(values = group_cols) + # Set color scheme
  scale_y_continuous(expand=c(0.1,0.1)) +
  labs(title = "Activity", x = "Group", y = "Activity Index [a.u.]") + # Add labels
  theme_minimal_hgrid(12, rel_small = 1) +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        plot.subtitle = element_text(hjust = 0.5, face = "plain"),
        axis.text.x = element_text(angle = 0, vjust = 1, size = 11, hjust = 0.45),
        axis.title.x = element_blank(),
        legend.position = "none") # Use a white and black theme

# Create plot for means
p_means_active <- ggplot(means_active, aes(x = Group, y = ActivityIndex, fill = Group, colour = Group)) +
  geom_jitter(size=4, alpha=0.5, width=0.2, shape=16) + # Add dodged columns for mean values
  geom_errorbar(aes(ymin = ActivityIndex - sd_ActivityIndex, ymax = ActivityIndex + sd_ActivityIndex), 
                position = position_dodge(width = 0.9), width = 0.2) + # Add error bars
  stat_summary(fun.min=function(z) {quantile(z,0.25)}, fun.max=function(z) {quantile(z,0.75)}, fun=median, color="black", size=1, shape=16) + # Add indication for mean and error bars
  scale_fill_manual(values = group_cols) + # Set fill colors
  scale_color_manual(values = group_cols) +
  scale_y_continuous(expand=c(0.1,0.1)) +
  labs(title = "Mean Activity", x = "Group", y = "Activity Index [a.u.]") + # Add labels
  theme_minimal_hgrid(12, rel_small = 1) +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        plot.subtitle = element_text(hjust = 0.5, face = "plain"),
        axis.text.x = element_text(angle = 0, vjust = 1, size = 11, hjust = 0.45),
        axis.title.x = element_blank(),
        legend.position = "none") # Use a white and black theme

# Add significance asterisks
p_means_active <- p_means_active + geom_signif(comparisons = list(c("CON", "SIS")), 
                                 test = test_name_active,
                                 map_signif_level = TRUE, 
                                 textsize = 4,
                                 color="black",
                                 annotations = sig_levels_active,
                                 y_position = max(means_active$ActivityIndex)+5)

# Arrange two plots side by side
plot_grid(p_indiv_active, p_means_active, ncol = 2, align = "v", labels = c("A", "B"))



# Create plot for individual values
p_indiv_inactive <- ggplot(inactive_data, aes(x = Group, y = ActivityIndex, color = Group)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5) + # Add jittered dots
  scale_color_manual(values = group_cols) + # Set color scheme
  scale_y_continuous(expand=c(0.1,0.1)) +
  labs(title = "Activity", x = "Group", y = "Activity Index [a.u.]") + # Add labels
  theme_minimal_hgrid(12, rel_small = 1) +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        plot.subtitle = element_text(hjust = 0.5, face = "plain"),
        axis.text.x = element_text(angle = 0, vjust = 1, size = 11, hjust = 0.45),
        axis.title.x = element_blank(),
        legend.position = "none") # Use a white and black theme

# Create plot for means
p_means_inactive <- ggplot(means_inactive, aes(x = Group, y = ActivityIndex, fill = Group, colour = Group)) +
  geom_jitter(size=4, alpha=0.5, width=0.2, shape=16) + # Add dodged columns for mean values
  geom_errorbar(aes(ymin = ActivityIndex - sd_ActivityIndex, ymax = ActivityIndex + sd_ActivityIndex), 
                position = position_dodge(width = 0.9), width = 0.2) + # Add error bars
  stat_summary(fun.min=function(z) {quantile(z,0.25)}, fun.max=function(z) {quantile(z,0.75)}, fun=median, color="black", size=1, shape=16) + # Add indication for mean and error bars
  scale_fill_manual(values = group_cols) + # Set fill colors
  scale_color_manual(values = group_cols) +
  scale_y_continuous(expand=c(0.1,0.1)) +
  labs(title = "Mean Activity", x = "Group", y = "Activity Index [a.u.]") + # Add labels
  theme_minimal_hgrid(12, rel_small = 1) +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        plot.subtitle = element_text(hjust = 0.5, face = "plain"),
        axis.text.x = element_text(angle = 0, vjust = 1, size = 11, hjust = 0.45),
        axis.title.x = element_blank(),
        legend.position = "none") # Use a white and black theme

# Add significance asterisks
p_means_inactive <- p_means_inactive + geom_signif(comparisons = list(c("CON", "SIS")), 
                                               test = test_name_inactive,
                                               map_signif_level = TRUE, 
                                               textsize = 4,
                                               color="black",
                                               annotations = sig_levels_inactive,
                                               y_position = max(means_inactive$ActivityIndex)+5)

# Arrange two plots side by side
plot_grid(p_indiv_inactive, p_means_inactive, ncol = 2, align = "v", labels = c("A", "B"))






































################################################################################
## Line plot of Active and Inactive Phase based on Group
#  This is used to compare activity patterns during the active and inactive phase

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(forcats)

group_cols<-c("#1e3791","#00ac8c")

# Define the phases to loop through
phases <- c("Active", "Inactive")

# Create an empty list to store the plots
plots <- list()

# Loop through each phase and create a plot
for (phase in phases) {
  
  # Filter the data for the current phase
  phase_data <- nightly_data %>% 
    filter(Phase == phase) %>% 
    filter(RecentChange == FALSE) # Only keep rows where RecentChange is FALSE
  
  # Convert ConsecActive to factor
  phase_data <- phase_data %>% 
    mutate(ConsecActive = as.factor(ConsecActive))
    
  # Create the plot
  plot <- ggplot(phase_data, aes(x = fct_inorder(ConsecActive), ActivityIndex, group = Group, colour = Group)) +
    geom_line(stat = "summary") +
    stat_summary(aes(fill = Group), fun = mean,
                 fun.min = function(x) mean(x) - sd(x), 
                 fun.max = function(x) mean(x) + sd(x), 
                 geom = "ribbon", 
                 alpha = 0.2, colour = NA) +
    scale_color_manual(name = NULL, values = group_cols) + 
    scale_fill_manual(name = NULL, values = group_cols) +
    theme_minimal_hgrid(12, rel_small = 1) +
    labs(title = bquote(~bold(.(paste("Activity Index")))), subtitle = paste("", phase, "Phase"),
         y = "Activity [a.u.]", x = "Day/Night", caption = "") +
    theme(plot.title = element_text(hjust = 0.5, face = "plain"),
          plot.subtitle = element_text(hjust = 0.5, face = "plain"),
          legend.position = "top",
          legend.justification = "right",
          legend.text = element_text(size = 9),
          legend.box.spacing = unit(1, "pt"),
          axis.text.x = element_text(angle = 45, vjust = 1, size = 11, hjust = 1),
          axis.title.x = element_blank())
  
  # Add the plot to the list of plots
  plots[[phase]] <- plot
  
}

# Arrange the plots using grid.arrange
grid.arrange(grobs = plots, nrow = 2)


################################################################################
## Compare Means between Groups during 2 hours preceding the Active Phase
#  This is used to quantify activity before the onset of the active phase

library(cowplot)
library(ggsignif)

## Compare Means between Groups during 2 hours preceding the Active Phase

# Filter the data for PriorActive == TRUE
prior_active_data <- data_filtered_agg %>% 
  filter(PriorActive == TRUE)

# Calculate the means and standard deviations of ActivityIndex for each group
means_prioract <- prior_active_data %>% 
  group_by(Group, AnimalNum) %>% 
  summarise(ActivityIndex = mean(ActivityIndex),
            sd_ActivityIndex = sd(ActivityIndex))

# Normality test for CTRL group
con_norm_prioract <- shapiro.test(means_prioract$ActivityIndex[means_prioract$Group == "CON"])
con_norm_prioract

# Normality test for SIS group
sis_norm_prioract <- shapiro.test(means_prioract$ActivityIndex[means_prioract$Group == "SIS"])
sis_norm_prioract

# Determine which test to use based on normality of both groups
if (con_norm_prioract$p.value >= 0.05 & sis_norm_prioract$p.value >= 0.05) {
  # If normal, use t-test
  t_test_prioract <- t.test(ActivityIndex ~ Group, data = means_prioract)
  test_name_prioract <- "t.test"
  test_pval_prioract <- t_test_prioract$p.value
  # Determine the significance level for t-test
  sig_levels_prioract <- sprintf("%.3f", test_pval_prioract)
} else {
  # If not normal, use Wilcoxon rank-sum test
  wilcox_test_prioract <- wilcox.test(ActivityIndex ~ Group, data = means_prioract)
  test_name_prioract <- "wilcox.test"
  test_pval_prioract <- wilcox_test_prioract$p.value
  # Determine the significance level for Wilcoxon rank-sum test
  sig_levels_prioract <- sprintf("%.3f", p.adjust(test_pval_prioract, method = "BH"))
}

# Create plot for individual values
p_indiv_prioract <- ggplot(prior_active_data, aes(x = Group, y = ActivityIndex, color = Group)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5) + # Add jittered dots
  scale_color_manual(values = group_cols) + # Set color scheme
  scale_y_continuous(expand=c(0.1,0.1)) +
  labs(title = "All Activity", x = "Group", y = "Activity Index [a.u.]") + # Add labels
  theme_minimal_hgrid(12, rel_small = 1) +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        plot.subtitle = element_text(hjust = 0.5, face = "plain"),
        axis.text.x = element_text(angle = 0, vjust = 1, size = 11, hjust = 0.45),
        axis.title.x = element_blank(),
        legend.position = "none") # Use a white and black theme

# Create plot for means
p_means_prioract <- ggplot(means_prioract, aes(x = Group, y = ActivityIndex, fill = Group, colour = Group)) +
  geom_jitter(size=4, alpha=0.5, width=0.2, shape=16) + # Add dodged columns for mean values
  geom_errorbar(aes(ymin = ActivityIndex - sd_ActivityIndex, ymax = ActivityIndex + sd_ActivityIndex), 
                position = position_dodge(width = 0.9), width = 0.2) + # Add error bars
  stat_summary(fun.min=function(z) {quantile(z,0.25)}, fun.max=function(z) {quantile(z,0.75)}, fun=median, color="black", size=1, shape=16) + # Add indication for mean and error bars
  scale_fill_manual(values = group_cols) + # Set fill colors
  scale_color_manual(values = group_cols) +
  scale_y_continuous(expand=c(0.1,0.1)) +
  labs(title = "Individual Activity", x = "Group", y = "Activity Index [a.u.]") + # Add labels
  theme_minimal_hgrid(12, rel_small = 1) +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        plot.subtitle = element_text(hjust = 0.5, face = "plain"),
        axis.text.x = element_text(angle = 0, vjust = 1, size = 11, hjust = 0.45),
        axis.title.x = element_blank(),
        legend.position = "none") # Use a white and black theme

# Add significance asterisks
p_means_prioract <- p_means_prioract + geom_signif(comparisons = list(c("CON", "SIS")), 
                                 test = test_name_prioract,
                                 map_signif_level = TRUE, 
                                 textsize = 4,
                                 color="black",
                                 annotations = sig_levels_prioract,
                                 y_position = max(means_prioract$ActivityIndex)+5)

# Arrange two plots side by side
plot_grid(p_indiv_prioract, p_means_prioract, ncol = 2, align = "v", labels = c("A", "B",))

plot_grid(p_indiv_active, p_means_active,p_indiv_inactive, p_means_inactive,p_indiv_prioract, p_means_prioract, ncol = 2, align = "v", labels = c("A", "B","C","D","E","F"))


######################################################################################


# Load libraries
library(dplyr)
library(ggplot2)

# Create a new column with datetime format
data$DateTime <- as.POSIXct(data$DateTime, format = "%Y-%m-%d %H:%M:%S")

# Create a new column for datetime rounded to the nearest 30 minutes
data$DateTime30Min <- as.POSIXct(round(as.numeric(data$DateTime)/(30*60))*(30*60), origin="1970-01-01")

# Create a new column for the next datetime rounded to the nearest 30 minutes
data$DateTime30MinShifted <- c(data$DateTime30Min[-1], NA)

# Create a new column for the time interval
data$TimeInterval <- paste0(data$DateTime30Min, " to ", data$DateTime30MinShifted)

# Aggregate ActivityIndex by AnimalNum, Batch, Group, and TimeInterval
data_hourly <- aggregate(ActivityIndex ~ AnimalNum + Batch + Group + TimeInterval, data = data, FUN = sum)

# Create a new column for datetime rounded to the nearest hour
data_hourly$DateTimeHourly <- as.POSIXct(round(as.numeric(substr(data_hourly$TimeInterval, 1, 16))/(60*60))*(60*60), origin="1970-01-01")

# Create a new column for the Change
data_hourly$Change <- cut(data_hourly$DateTimeHourly, breaks = c(as.POSIXct("2022-10-20 00:00:00"), 
                                                                 as.POSIXct("2022-10-21 00:00:00"), 
                                                                 as.POSIXct("2022-10-22 00:00:00"), 
                                                                 as.POSIXct("2022-10-23 00:00:00"),
                                                                 as.POSIXct("2022-10-24 00:00:00")),
                          labels = c("CC1", "CC2", "CC3", "CC4"))

# Arrange data by Batch and DateTimeHourly
data_hourly <- data_hourly %>% 
  arrange(Batch, DateTimeHourly)

# Create a new column for the cumulative ActivityIndex
data_hourly$cumsum <- ave(data_hourly$ActivityIndex, data_hourly$AnimalNum, data_hourly$Batch, data_hourly$Change, FUN=cumsum)

# Create a line plot of ActivityIndex vs. Time of Day, faceted by Batch and Change
ggplot(data_hourly, aes(x = DateTimeHourly, y = ActivityIndex)) +
  geom_line() +
  facet_grid(Batch ~ Change, scales = "free_y") +
  labs(title = "Activity Index vs. Time of Day",
       x = "Time of Day",
       y = "Activity Index") +
  theme_bw()



## Run ANOVA for comparing CON, RES, SIS groups for each cage change.
#  Plot data for each group change

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(lme4)        # For mixed-effects models
library(lmerTest)   # For p-values

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
    # Set color and fill scales
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
# Create a list to store model results
model_results <- list()

# Loop through each CC
for (Change in sort(unique(data_filtered_agg$Change))) {
  # Subset data for the current CC
  data_filtered_agg_Change_subset <- data_filtered_agg_Change[[Change]]
  
  # Split "SIS" group into "RES" and "SUS" based on "SUS" column
  data_filtered_agg_Change_subset <- data_filtered_agg_Change_subset %>%
    mutate(Group = ifelse(Group == "SIS" & SUS == FALSE, "RES", 
                          ifelse(Group == "SIS" & SUS == TRUE, "SUS", Group)))
  
  # Fit the mixed-effects model
  model <- lmerTest::lmer(ActivityIndex ~ Group + HalfHourElapsed + (1 | AnimalNum), data = data_filtered_agg_Change_subset)
  model_results[[Change]] <- model
  
  # Save the mixed-effects model summary as a text file for each CC
  # change the folder as you prefer
  mixed_model_results_file <- paste0("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/Activity/mixed_model_results_", Change, ".txt")
  model_summary <- capture.output(summary(model))
  writeLines(model_summary, con = mixed_model_results_file)
  
  # Print the model summary
  cat("Mixed-effects model results for", Change, ":\n")
  print(summary(model))
}

# Arrange plots in a grid
grid.arrange(grobs = plot_list, ncol = 2, top = "Activity Index by Cage Change")








## an here is the same, just with phase filtered

library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(gridExtra)
library(lme4)
library(lmerTest)

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
    
    mixed_model_results_file <- paste0("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/Activity/mixed_model_results_", Change, "_", PhaseValue, ".txt")
    model_summary <- capture.output(summary(model))
    writeLines(model_summary, con = mixed_model_results_file)
    
    cat("Mixed-effects model results for", Change, "-", PhaseValue, ":\n")
    print(summary(model))
  }
}

grid.arrange(grobs = plot_list, ncol = 2, top = "Activity Index by Cage Change")

