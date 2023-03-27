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
  mutate(Phase = ifelse(
    format(DateTime, "%H:%M") >= "18:30" | format(DateTime, "%H:%M") < "06:30",
    "Active",
    "Inactive"
  ),
  PriorActive = ifelse(
    format(DateTime, "%H:%M") >= "16:30" & format(DateTime, "%H:%M") <= "18:30",
    "TRUE",
    "FALSE"
  ),
  Hour = floor(difftime(DateTime, first(DateTime), units = "hours")),
  RecentChange = Hour <= 2,
  # insert control animal IDs as defined in the provided csv sheets
  Group = ifelse(AnimalNum %in% c("OR126", "OR127", "OR128", "OR129", "OQ761", "OQ763", "OQ765", "OQ760"), "CON", "SIS")
  )

# remove intermediate variables
rm(subfolders)

# Create a new column to represent the consecutive Active phases for each animal
data$ConsecActive <- with(data, ave(Phase, AnimalNum, FUN=function(x) {
  cumsum(c(0, diff(ifelse(x == "Active", 1, 0))) == 1)
}))
data$ConsecActive <- as.numeric(data$ConsecActive)

# Aggregate data by AnimalNum, ConsecActive, and Group
hourly_data <- aggregate(ActivityIndex ~ AnimalNum + ConsecActive + Group + Phase + Hour + RecentChange + PriorActive, data = data, FUN = mean)

# Filter out the first and last inactive phases of each cage change
filtered_data <- hourly_data %>%
  group_by(AnimalNum, ConsecActive) %>%
  filter(
    !(Phase == "Inactive" & row_number() %in% c(1, n()))
  ) %>%
  ungroup()

# Aggregate data by AnimalNum, ConsecActive, and Group for each night
nightly_data <- aggregate(ActivityIndex ~ AnimalNum + ConsecActive + Group + Phase + RecentChange + PriorActive, data = data, FUN = mean)
