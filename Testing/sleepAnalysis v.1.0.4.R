## Read in data from Folders and xlsx data. The data is organized in subfolders. 
#  This will read in the data of different xlsx sheets and merge them based on the name of the folder. 
#  Also, the data will be sorted and formatted
#  Later, the data points will be plotted.

# Define a vector of required packages
requiredPackages <- c("readxl", "dplyr", "purrr", "stringr", "lubridate", "tidyr", "forcats", "ggplot2", "cowplot", "gridExtra", "lme4", "lmerTest", "emmeans")

# Check if the required packages are installed, and install them if needed
for (package in requiredPackages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  } else {
    library(library_name, character.only = TRUE)
  }
}

# set the working directory to the parent directory containing the subfolders
setwd("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/BatchAnalysis")

# get a list of all subfolders in the directory
subfolders <- list.dirs(".", recursive = FALSE)

# Define SUS, excluded and sex of animals
con_animals <- readLines("con_animals.csv")
susAnimals <- c(readLines(paste0("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/sus_animals.csv")))
excluded_animals <- readLines("excludedAnimals.csv")
data <- data[!data$AnimalNum %in% excluded_animals,]
sexAnimals <- read_csv("ListSexes.csv", show_col_types = FALSE)

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
      format(DateTime, "%H:%M") >= "07:30" & format(DateTime, "%H:%M") < "18:30",
      "TRUE",
      "FALSE"
    ),
    Batch = as.factor(Batch),
    Group = ifelse(
      AnimalNum %in% con_animals,
      "CON",
      "SIS"
    )
  ) %>%
  group_by(Batch) %>%
  mutate(Hour = difftime(DateTime, first(DateTime), units = "hours")) %>%
  ungroup() 

# Create new columns to represent the consecutive Active and Inactive phases for each animal
data <- data %>%
  group_by(AnimalNum) %>%
  arrange(DateTime) %>%
  mutate(
    ConsecActive = cumsum(c(0, diff(ifelse(Phase == "Active", 1, 0))) == 1),
    ConsecInactive = cumsum(c(0, diff(ifelse(Phase == "Inactive", 1, 0))) == 1)
  ) %>%
  ungroup() %>%
  mutate(
    ConsecActive = as.numeric(ConsecActive),
    ConsecInactive = as.numeric(ConsecInactive)
  )

## Cookie Habituation Challenge, only activate this when analyzing cookie data
#data <- data %>%
#  mutate(Cookie = ifelse(
#    ConsecActive == 1 & Phase == "Inactive" & format(DateTime, "%H:%M") >= "17:00" & format(DateTime, "%H:%M") <= "17:45",
#    "TRUE",
#    "FALSE"
#  ))

#remove last two active and inactive phases of CC4 due to grid within cage
#data <- data %>%
#  filter(!(Change == "CC4" & Phase %in% c("Active", "Inactive") & ConsecActive >= 15))

#remove last two active and inactive phases of CC4 due to grid within cage
#data <- data %>%
#  filter(!(AnimalNum == "CC4" & Phase %in% c("Active", "Inactive") & ConsecActive >= 15))

# Filter the data to exclude rows where ConsecActive is less than 14
data <- data %>%
  filter(Change == "CC4")

# Remove the first and last inactive phase for each Change and Batch
data_filtered <- data %>%
  group_by(Batch, Change, AnimalNum) %>% 
  mutate(ConsecActive = ifelse(Phase == "Active", cumsum(c(0, diff(ifelse(Phase == "Inactive", 0, 1))) == 1), 0),
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
  group_by(AnimalNum, Batch, Group, Change, Phase, PriorActive, DateTime30Min, ConsecActive, ConsecInactive) %>%
  summarize(ActivityIndex = mean(ActivityIndex)) %>%
  ungroup()

# Create a new column 'SUS'
data_filtered_agg <- data_filtered_agg %>%
  mutate(SUS = AnimalNum %in% c(susAnimals))


# Combine the two datetime columns to form the interval
data_filtered_agg$DateTime30MinShifted <- lead(data_filtered_agg$DateTime30Min)
data_filtered_agg$TimeInterval <- paste0(data_filtered_agg$DateTime30Min, " to ", data_filtered_agg$DateTime30MinShifted)

# Calculate the number of half-hour periods elapsed since the first half-hour period for each Batch and AnimalNum combination
data_filtered_agg <- data_filtered_agg %>%
  group_by(Batch, AnimalNum) %>%
  mutate(HalfHourElapsed = as.numeric(difftime(DateTime30Min, first(DateTime30Min), units = "secs"))/1800) %>%
  ungroup()

##################### PLOT ACTIVITY INDEX #####################

# Split "SIS" group into "RES" and "SUS" based on "SUS" column
data_filtered_agg <- data_filtered_agg %>%
  mutate(Group = ifelse(Group == "SIS" & SUS == FALSE, "RES", 
                        ifelse(Group == "SIS" & SUS == TRUE, "SUS", Group)))

# Create a single plot for the entire data
plot <- ggplot(data_filtered_agg, aes(x = HalfHourElapsed, y = ActivityIndex, group = Group, color = Group)) +
  geom_path(stat = "summary") +
  stat_summary(aes(fill = Group), fun = mean,
               fun.min = function(x) mean(x) - sd(x), 
               fun.max = function(x) mean(x) + sd(x), 
               geom = "ribbon", 
               alpha = 0.2, colour = NA) +
  scale_color_manual(name = NULL, values = c("#1e3791", "#8aacdb", "#f49620")) + 
  scale_fill_manual(name = NULL, values = c("#1e3791", "#8aacdb", "#f49620")) +
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
# Perform mixed-effects models with pairwise comparisons

model_results <- list()
emmeans_results <- list()

for (PhaseValue in c("Active", "Inactive")) {
  data_filtered_agg_Phase_subset <- data_filtered_agg %>%
    filter(Phase == PhaseValue)
  
  data_filtered_agg_Phase_subset <- data_filtered_agg_Phase_subset %>%
    mutate(Group = ifelse(Group == "SIS" & SUS == FALSE, "RES", 
                          ifelse(Group == "SIS" & SUS == TRUE, "SUS", Group)))
  
  if (PhaseValue == "Active") {
    model <- lmerTest::lmer(ActivityIndex ~ Group * ConsecActive + (1 | AnimalNum), data = data_filtered_agg_Phase_subset)
    emmeans_obj <- emmeans(model, ~ Group * ConsecActive, data = data_filtered_agg_Phase_subset, 
                           cov.reduce = FALSE, adjust = "sidak", pbkrtest.limit = 10000)
    pairwise_results <- pairs(emmeans_obj, by = c("ConsecActive"), adjust = "bonferroni")
  } else if (PhaseValue == "Inactive") {
    model <- lmerTest::lmer(ActivityIndex ~ Group * ConsecInactive + (1 | AnimalNum), data = data_filtered_agg_Phase_subset)
    emmeans_obj <- emmeans(model, ~ Group * ConsecInactive, data = data_filtered_agg_Phase_subset, 
                           cov.reduce = FALSE, adjust = "sidak", pbkrtest.limit = 10000)
    pairwise_results <- pairs(emmeans_obj, by = c("ConsecInactive"), adjust = "bonferroni")
  }
  
  model_results[[PhaseValue]] <- model
  
  mixed_model_results_file <- paste0("S:\\Lab_Member\\Tobi\\Experiments\\Exp9_Social-Stress\\Analysis\\Behavior\\RFID\\Activity\\gridActivity\\mixed_model_results_", PhaseValue, ".txt")
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
  pairwise_results_file <- paste0("S:\\Lab_Member\\Tobi\\Experiments\\Exp9_Social-Stress\\Analysis\\Behavior\\RFID\\Activity\\gridActivity\\pairwise_results_", PhaseValue, ".txt")
  pairwise_summary <- capture.output(print(pairwise_results))
  writeLines(pairwise_summary, con = pairwise_results_file)
  
  # Save emmeans results to a file
  emmeans_results_file <- paste0("S:\\Lab_Member\\Tobi\\Experiments\\Exp9_Social-Stress\\Analysis\\Behavior\\RFID\\Activity\\gridActivity\\emmeans_results_", PhaseValue, ".txt")
  emmeans_summary <- capture.output(summary(emmeans_obj))
  writeLines(emmeans_summary, con = emmeans_results_file)
}

#################### GENERATE PLOT OF SPECIFIED TIMEFRAME #######################

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
    
    model <- lmerTest::lmer(ActivityIndex ~ Group * ConsecActive (1 | AnimalNum), data = data_filtered_agg_Phase_subset)
    emmeans_obj <- emmeans(model, ~ Group * ConsecActive, data = data_filtered_agg_Phase_subset, 
                           cov.reduce = FALSE, adjust = "sidak", pbkrtest.limit = 10000)
    pairwise_results <- pairs(emmeans_obj, by = c("ConsecActive"), adjust = "bonferroni")
  } else if (PhaseValue == "Inactive") {
    # Filter for ConsecInactive levels 2 and 3
    data_filtered_agg_Phase_subset <- data_filtered_agg_Phase_subset %>%
      filter(ConsecInactive %in% c(2, 3))
    
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
    plot <- emmip(model, Group ~ ConsecActive, at = list(ConsecActive = c(2, 3, 4, 5)), CIs = TRUE, style = "factor", pbkrtest.limit = 10000) + 
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
    plot <- emmip(model, Group ~ ConsecInactive, CIs = TRUE, style = "factor", pbkrtest.limit = 10000) + 
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

# print out list of unique AnimalNums
unique_animals <- unique(data_filtered_agg$AnimalNum)
print(unique_animals)

#print out list of SUS, RES and con animals
sus_animals_list <- unique(data_filtered_agg$AnimalNum[data_filtered_agg$SUS == TRUE])
print(sus_animals_list)
res_animals_list <- unique(data_filtered_agg$AnimalNum[data_filtered_agg$Group == "RES"])
print(res_animals_list)
con_animals_list <- unique(data_filtered_agg$AnimalNum[data_filtered_agg$Group == "CON"])
print(con_animals_list)