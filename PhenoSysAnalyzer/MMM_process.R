## Read in data from Folders and xlsx data. The data is organized in subfolders. 
#  This will read in the data of different xlsx sheets and merge them based on the name of the folder. 
#  Also, the data will be sorted and formatted
#  Later, the data points will be plotted.

library(readxl)      # load readxl package for reading excel files
library(dplyr)       # load dplyr package for data manipulation functions
library(purrr)       # load purrr package for functional programming
library(stringr)     # load stringr package for string manipulation
library(lubridate)   # load lubridate package for date and time functions
library(tidyr)       # load tidyr package for data reshaping
library(forcats)     # load forcats package for factor manipulation

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

# create a Phase column based on the time of day
data <- data %>%
  mutate(
    Phase = ifelse(
      format(DateTime, "%H:%M") >= "18:30" | format(DateTime, "%H:%M") < "06:30",
      "Active",
      "Inactive"
    ),
    PriorActive = ifelse(
      format(DateTime, "%H:%M") >= "16:30" & format(DateTime, "%H:%M") <= "18:30",
      "TRUE",
      "FALSE"
    ),
    Batch = as.factor(Batch), # convert Batch to a factor variable
    
    # Define Control animals
    Group = ifelse(
      AnimalNum %in% c("OR126", "OR127", "OR128", "OR129", "OQ761", "OQ763", "OQ765", "OQ760"),
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

#remove last two active and inactive phases of CC4 due to grid within cage
data <- data %>%
  filter(!(Change == "CC4" & Phase %in% c("Active", "Inactive") & ConsecActive >= 15))

# exclude animals with non-complete datasets
excluded_animals <- c('OQ750', 'OQ751', 'OQ752', 'OQ753', '0001', '0002')
data <- data[!data$AnimalNum %in% excluded_animals,]

#remove last two active and inactive phases of CC4 due to grid within cage
data <- data %>%
  filter(!(AnimalNum == "CC4" & Phase %in% c("Active", "Inactive") & ConsecActive >= 15))

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

# Round datetime down to the nearest half-hour
data_filtered$DateTime30Min <- format(as.POSIXct(trunc(as.numeric(as.POSIXct(data_filtered$DateTime, format = "%d.%m.%Y %H:%M")) / (30*60)) * (30*60), origin = "1970-01-01"), format = "%Y-%m-%d %H:%M:%S")

# Add column with shifted datetime rounded down to nearest half-hour
data_filtered$DateTime30MinShifted <- format(as.POSIXct(trunc(as.numeric(as.POSIXct(data_filtered$DateTime, format = "%d.%m.%Y %H:%M")) / (30*60)) * (30*60) + 30*60, origin = "1970-01-01"), format = "%Y-%m-%d %H:%M:%S")

# Aggregate data by AnimalNum, Batch, Change, Phase, and 30-minute interval
data_filtered_agg <- data_filtered %>%
  group_by(AnimalNum, Batch, Group, Change, Phase, PriorActive, DateTime30Min, DateTime30MinShifted) %>%
  summarize(ActivityIndex = mean(ActivityIndex)) %>%
  ungroup()

# Combine the two datetime columns to form the interval
data_filtered_agg$TimeInterval <- paste0(data_filtered_agg$DateTime30Min, " to ", data_filtered_agg$DateTime30MinShifted)

# Calculate the number of half-hour periods elapsed since the first half-hour period for each Batch, Cage, and AnimalNum combination
data_filtered_agg <- data_filtered_agg %>%
  group_by(Batch, AnimalNum) %>%
  mutate(HalfHourElapsed = as.numeric(difftime(DateTime30Min, first(DateTime30Min), units = "secs"))/1800) %>%
  ungroup()