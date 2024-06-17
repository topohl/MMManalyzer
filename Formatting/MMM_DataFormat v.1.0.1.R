## Read in data from Folders and xlsx data. The data is organized in subfolders. 
#  This will read in the data of different xlsx sheets and merge them based on the name of the folder. 
#  Also, the data will be sorted and formatted

# List of packages to check and load
required_packages <- c("readxl", "dplyr", "purrr", "stringr", "lubridate", "readr", "openxlsx","tidyr")

# Check if each package is installed, install if not, and load it
for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}

# remove the last two active and inactive phases? (Grid in cage)
gridInCage <- FALSE
analyseGridInCage <- TRUE

# set the working directory to the parent directory containing the subfolders
setwd("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/BatchAnalysis/males/")

# read in excludedAnimal, conAnimals, and susAnimals from .csv files
excludedAnimals <- read.csv("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/BatchAnalysis/excludedAnimals.csv", header = FALSE, stringsAsFactors = FALSE)
conAnimals <- read.csv("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/BatchAnalysis/con_animals.csv", header = FALSE, stringsAsFactors = FALSE)
susAnimals <- read.csv("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/sus_animals.csv", header = FALSE, stringsAsFactors = FALSE)
sexAnimals <- read.csv("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/BatchAnalysis/ListSexes.csv", header = TRUE, stringsAsFactors = FALSE, sep = "\t")

# Extract the animal numbers from the first column
conAnimalNums <- conAnimals[[1]]
susAnimalNums <- susAnimals[[1]]
exclAnimalNums <- excludedAnimals[[1]]

# get a list of all subfolders in the directory
subfolders <- list.dirs(".", recursive = FALSE)

# separate the Animal column into AnimalNumber and Cage columns
print("Separating Animal column into AnimalNum and Cage columns...")
data <- separate(data, Animal, c("AnimalNum", "Cage"), sep = "_", remove = FALSE)

# convert the DateTime column to a datetime format
print("Converting DateTime column to datetime format...")
data$DateTime <- as.POSIXct(data$DateTime, format = "%d.%m.%Y %H:%M")

# Assign animals to groups
print("Assigning animals to groups...")
data <- data %>% mutate(Group = ifelse(AnimalNum %in% susAnimalNums, "SUS", ifelse(AnimalNum %in% conAnimalNums, "CON", "RES")))

# assign Sex to animals
print("Sexing animals...")
data <- data %>% mutate(Sex = ifelse(AnimalNum %in% sexAnimals$males, "m", ifelse(AnimalNum %in% sexAnimals$females, "f", "")))

# exclude animals with non-complete datasets
print("Exclude animals with non-complete datasets...")
data <- data[!data$AnimalNum %in% exclAnimalNums,]

# Check if ActivityIndex already exists
if (!exists("ActivityIndex")) {
  # Check if ActivyIndex exists
  if ("ActivyIndex" %in% colnames(data)) {
    # Rename ActivyIndex to ActivityIndex
    data <- data %>%
      rename(ActivityIndex = ActivyIndex)
  } else {
    print("Skipping renaming of ActivyIndex to ActivityIndex because ActivyIndex does not exist.")
  }
} else {
  print("Skipping renaming of ActivyIndex to ActivityIndex because ActivityIndex already exists.")
}

# create a Phase column based on the time of day and print message whats happening
print("Creating Phase column based on time of day...")
print("Define timeframes before Active Phase...")
print("Converting Batch to factor variable...")
print("Calculate the number of hours elapsed since the first timestamp for each Batch...")
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
  ) %>%
  group_by(Batch) %>%
  mutate(Hour = difftime(DateTime, first(DateTime), units = "hours")) %>%
  ungroup()

# create empty columns ConsecActive and ConsecInactive columns in data_filtered
data$ConsecActive <- 0
data$ConsecInactive <- 0
data$ConsecPhases <- 0

# Remove the first and last inactive phase for each Change and Batch
print("Create new columns ActivePeriods, InactivePeriods, TotalPeriods, ConsecActive, and ConsecInactive...")
print("Remove the first and last inactive phase for each Change and Batch...")
data_filtered <- data %>%
  group_by(Batch, Change, AnimalNum) %>% 
  mutate(
    ActivePeriods = ifelse(Phase == "Active", 1, 0),
    InactivePeriods = ifelse(Phase == "Inactive", 1, 0),
    TotalPeriods = sum(ActivePeriods, InactivePeriods),
    ConsecActive = ifelse(Phase == "Active", cumsum(c(0, diff(ifelse(Phase == "Inactive", 0, 1))) == 1), 0),
    ConsecInactive = ifelse(Phase == "Inactive", cumsum(c(1, diff(ifelse(Phase == "Active", 0, 1))) == 1), 0),
    ConsecPhases = cumsum(c(0, diff(as.numeric(factor(Phase))) != 0)) # Increment on phase switch
  ) %>% 
  filter(!(ConsecInactive == max(ConsecInactive[Phase == "Inactive"]) & Phase == "Inactive" |
           ConsecInactive == min(ConsecInactive[Phase == "Inactive"]) & Phase == "Inactive")) %>% 
  ungroup()

# Define a function to remove last two active and inactive phases
removePhases <- function(data) {
  if (gridInCage) {
    print("Remove last two active and inactive phases of CC4 due to grid within cage...")
    data <- data %>%
      filter(!(Change == "CC4" & Phase %in% c("Active", "Inactive") & ConsecActive >= 3 & ConsecInactive >= 3))
  } else {
    print("Skipping removal of last two active and inactive phases of CC4...")
  }
  return(data)
}

# Define function
analyseGrid <- function(data) {
  if (analyseGridInCage) {
    print("Extracting timeframe when grid is in cage...")
    data <- data %>% 
      filter(Change == "CC4" & ConsecPhases > 2)
  } else {
    print("Skipping extraction of timeframe when grid is in cage...")
  }
  return(data)
}

# Remove phases in data
data <- removePhases(data)
data <- analyseGrid(data)

# Remove phases in data_filtered
data_filtered <- removePhases(data_filtered)
data_filtered <- analyseGrid(data_filtered)

# Add a new column for SleepBouts
print("Add new column for SleepBouts...")
data_filtered <- data_filtered %>%
  group_by(AnimalNum, Phase, Change) %>%
  mutate(
    SleepBouts = cumsum(ifelse(ActivityIndex == 0 & lag(ActivityIndex != 0, default = TRUE) | row_number() == 1 & ActivityIndex == 0, 1, 0))
  ) %>%
  ungroup()

# Add a new column for SleepDur
print("Add new column for Sleep Duration...")
data_filtered <- data_filtered %>%
  group_by(AnimalNum, Phase, Change) %>%
  mutate(
    SleepDur = cumsum(ActivityIndex == 0)
  ) %>%
  ungroup()

# Round datetime down to the nearest half-hour
data_filtered$DateTime30Min <- format(as.POSIXct(trunc(as.numeric(as.POSIXct(data_filtered$DateTime, format = "%d.%m.%Y %H:%M")) / (30*60)) * (30*60), origin = "1970-01-01"), format = "%Y-%m-%d %H:%M:%S")

# Add column with shifted datetime rounded down to nearest half-hour
data_filtered$DateTime30MinShifted <- format(as.POSIXct(trunc(as.numeric(as.POSIXct(data_filtered$DateTime, format = "%d.%m.%Y %H:%M")) / (30*60)) * (30*60) + 30*60, origin = "1970-01-01"), format = "%Y-%m-%d %H:%M:%S")

# Aggregate data by AnimalNum, Batch, Change, Phase, and 30-minute interval
print("Aggregate data by AnimalNum, Batch, Change, Phase, and 30-minute interval...")
data_filtered_agg <- data_filtered %>%
  group_by(AnimalNum, Sex, Batch, Group, Change, Phase, ConsecActive, ConsecInactive, ConsecPhases, PriorActive, DateTime30Min, DateTime30MinShifted) %>%
  summarize(ActivityIndex = mean(ActivityIndex)) %>%
  ungroup()

# Combine the two datetime columns to form the interval
print("Forming time interval...")
data_filtered_agg$TimeInterval <- paste0(data_filtered_agg$DateTime30Min, " to ", data_filtered_agg$DateTime30MinShifted)

print("Assign susceptible animals to the SUS group...")
data_filtered_agg <- data_filtered_agg %>%
  mutate(SUS = AnimalNum %in% susAnimals)

# Calculate the number of half-hour periods elapsed since the first half-hour period for each Batch, Cage, and AnimalNum combination
print("Calculate the number of half-hour periods elapsed...")
data_filtered_agg <- data_filtered_agg %>%
  group_by(Batch, AnimalNum) %>%
  mutate(HalfHourElapsed = as.numeric(difftime(DateTime30Min, first(DateTime30Min), units = "secs"))/1800) %>%
  ungroup()

print("Done!")