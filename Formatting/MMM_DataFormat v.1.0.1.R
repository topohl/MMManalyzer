## merge files from different batches
### Read in data from Folders and xlsx data. The data is organized in subfolders. 
#  This will read in the data of different xlsx sheets and merge them based on the name of the folder. 
#  Also, the data will be sorted and formatted
#  Later, the data points will be plotted.

# List of packages to check and load
packages <- c("readxl", "dplyr", "purrr", "stringr", "lubridate", "readr", "openxlsx", "zoo", "tidyverse")

# Check if each package is installed, install if not, and load it
for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}

# set the working directory to the parent directory containing the subfolders an get a list of the B1 and B2 subfolders in the directory
setwd("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/BatchAnalysis")
subfolders <- c("males/B1", "males/B2", "females/B3", "females/B4")

# function to read and process a single CSV file, it processes all .csv files in the selected destination folder
process_file <- function(file_path) {
  cat(paste0("Processing ", file_path, "\n"))  # print file path
  read.csv(file_path, sep = ";") %>%     # read the csv file with semicolon delimiter
    mutate(filename = basename(file_path)) %>%     # add a column with the filename
    mutate(Batch = str_extract(filename, "B\\d")) %>%   # extract the Batch number from the filename
    mutate(Change = str_extract(filename, "CC\\d")) %>%   # extract the Change number from the filename
    select(-filename) # remove the filename column
}

# function to process and save as XLSX file
process_and_save_xlsx <- function(file_path) {
  file_base <- sub(".csv", "", basename(file_path))
  output_path <- paste0(dirname(file_path), "/", file_base, ".xlsx")
  
  if (file.exists(output_path)) {
    cat(paste0("File already exists: ", output_path, ". Skipping.\n"))
  } else {
    csv_data <- process_file(file_path)
    write.xlsx(csv_data, output_path, rowNames = FALSE)
    cat(paste0("Saved ", output_path, "\n"))  # print output file path
  }
}

# Filter files based on subsubfolders
sex <- c("males", "females")
files <- list.files(path = "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/BatchAnalysis", pattern = ".csv$", recursive = TRUE, full.names = TRUE)
files <- files[sapply(files, function(file) any(sapply(sex, function(subfolder) grepl(paste0("/", subfolder, "/"), file))))]

# Combine all files
purrr::walk(files, process_and_save_xlsx)

# Combine data from different subfolders into a single data frame
data <- map_dfr(subfolders, ~{
  # Get a list of subfolders in the current B1 or B2 folder
  subfolder_paths <- list.dirs(paste0(".", "/", .x), recursive = FALSE)
  
  # Get files matching the pattern for subfolders (SIS period)
  file_paths <- map(subfolder_paths, ~list.files(path = .x, pattern = c("E9_SIS_B\\d+_CC\\d_ActivityIndex.csv"), full.names = TRUE)) %>% 
    flatten()
  
  ################### cookie hab #####  only activate when analysing cookie habituation ###################
  # file_paths <- map(subfolder_paths, ~list.files(path = .x, pattern = c("E9_SIS_B2_EPMaftrecagechange_ActivityIndex.csv"), full.names = TRUE)) %>% 
  # flatten()
  
  # Process all files (add new columns) and combine into a single data frame
  all_data <- map(file_paths, ~process_file(.x)) %>% 
    bind_rows()
  
  all_data
})

## Read in data from Folders and xlsx data. The data is organized in subfolders. 
#  This will read in the data of different xlsx sheets and merge them based on the name of the folder. 
#  Also, the data will be sorted and formatted

# Set the working directory to the parent directory containing the subfolders
setwd("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/BatchAnalysis")

# Get a list of all subfolders in the directory
subfolders <- list.dirs(".", recursive = FALSE)

# Read the list of animals from a CSV file

message("Reading list of control and susceptible animals...")
con_animals <- readLines("con_animals.csv")
sus_animals <- readLines("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/sus_animals.csv")

message("Reading list of animal sexes...")
sexAnimals <- read_csv("ListSexes.csv", show_col_types = FALSE)

# Read the list of excluded animals from a CSV file
message("Reading list of excluded animals...")
excluded_animals <- readLines("excludedAnimals.csv")

# Separate the Animal column into AnimalNum and Cage columns
message("Separating the Animal column into AnimalNum and Cage columns...")
data <- separate(data, Animal, c("AnimalNum", "Cage"), sep = "_", remove = FALSE)

# Convert the DateTime column to a datetime format
message("Converting the DateTime column to a datetime format...")
data$DateTime <- as.POSIXct(data$DateTime, format = "%d.%m.%Y %H:%M")

# Rename ActivyIndex to ActivityIndex
data <- data %>%
  rename(ActivityIndex = ActivyIndex)

# create a Phase column based on the time of day
# This code block creates additional variables in the 'data' dataframe based on existing variables.
# It adds the following variables:
# - Sex: Assigns 'm' for males and 'f' for females based on the 'AnimalNum' column and a separate dataframe 'sexAnimals'.
# - Phase: Assigns 'Active' or 'Inactive' based on the time of day in the 'DateTime' column.
# - PriorActive: Assigns 'TRUE' or 'FALSE' based on the time of day in the 'DateTime' column.
# - Batch: Converts the 'Batch' column to a factor variable.
# - Group: Assigns 'CON' or 'SIS' based on the 'AnimalNum' column and a separate vector 'con_animals'.

  # Assign sexes, phases, prior active phases, and groups
  message("Assigning sexes, phases, prior active phases, and groups...")
  data <- data %>%
    mutate(
      Sex = ifelse(AnimalNum %in% sexAnimals$males, "m", "f"),
      Phase = ifelse(format(DateTime, "%H:%M") >= "18:30" | format(DateTime, "%H:%M") < "06:30", "Active", "Inactive"),
      PriorActive = ifelse(format(DateTime, "%H:%M") >= "16:30" & format(DateTime, "%H:%M") <= "18:30", "TRUE", "FALSE"),
      Batch = as.factor(Batch),
      Group = ifelse(AnimalNum %in% con_animals, "CON", "SIS"),
      Hour = difftime(DateTime, first(DateTime), units = "hours")
    ) %>%
    ungroup()

  # Create a new column to represent the consecutive Active phases for each animal
  data$ConsecActive <- with(data, ave(Phase, AnimalNum, FUN=function(x) {
    cumsum(c(0, diff(ifelse(x == "Active", 1, 0))) == 1)
  }))
  data$ConsecActive <- as.numeric(data$ConsecActive)

  # Remove last two active and inactive phases of CC4 due to grid within cage
  message("Removing last two active and inactive phases of CC4 due to grid within cage...")
  data <- data %>%
    filter(!(Change == "CC4" & Phase %in% c("Active", "Inactive") & ConsecActive >= 15))

  # Exclude animals with non-complete datasets
  message("Excluding animals with non-complete datasets...")
  data <- data[!data$AnimalNum %in% excluded_animals,]

  # Remove the first and last inactive phase for each Change and Batch
  message("Removing first and last inactive phase for each Change and Batch...")
  data_filtered <- data %>%
    group_by(Batch, Change, AnimalNum) %>% 
    mutate(
      ActivePeriods = ifelse(Phase == "Active", 1, 0),
      InactivePeriods = ifelse(Phase == "Inactive", 1, 0),
      TotalPeriods = sum(ActivePeriods, InactivePeriods),
      ConsecActive = ifelse(Phase == "Active", cumsum(c(1, diff(ifelse(Phase == "Inactive", 0, 1))) == 1), 0)
    )

# Create a new column to represent the consecutive Active phases for each animal
data$ConsecActive <- with(data, ave(Phase, AnimalNum, FUN=function(x) {
  cumsum(c(0, diff(ifelse(x == "Active", 1, 0))) == 1)
}))
data$ConsecActive <- as.numeric(data$ConsecActive)

#remove last two active and inactive phases of CC4 due to grid within cage
message("Removing last two active and inactive phases of CC4 due to grid within cage...")
data <- data %>%
  filter(!(Change == "CC4" & Phase %in% c("Active", "Inactive") & ConsecActive >= 15))

# exclude animals with non-complete datasets
message("Excluding animals with non-complete datasets...")
data <- data[!data$AnimalNum %in% excluded_animals,]

# Remove the first and last inactive phase for each Change and Batch
message("Removing first and last inactive phase for each Change and Batch...")
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

# Calculate sleep bouts
message("Calculating Sleep Bouts...")
data_filtered <- data_filtered %>%
  group_by(AnimalNum, Phase, Change) %>%
  mutate(
    SleepBouts = cumsum(ifelse(ActivityIndex == 0 & lag(ActivityIndex != 0, default = TRUE) | row_number() == 1 & ActivityIndex == 0, 1, 0))
  ) %>%
  ungroup()

# Add a new column for SleepDur
message("Calculating Sleep Duration...")
data_filtered <- data_filtered %>%
  group_by(AnimalNum, Phase, Change, Sex) %>%
  mutate(
    SleepDur = cumsum(ActivityIndex == 0)
  ) %>%
  ungroup()

# Round datetime down to the nearest half-hour
data_filtered$DateTime30Min <- format(as.POSIXct(trunc(as.numeric(as.POSIXct(data_filtered$DateTime, format = "%d.%m.%Y %H:%M")) / (30*60)) * (30*60), origin = "1970-01-01"), format = "%Y-%m-%d %H:%M:%S")

# Add column with shifted datetime rounded down to nearest half-hour
data_filtered$DateTime30MinShifted <- format(as.POSIXct(trunc(as.numeric(as.POSIXct(data_filtered$DateTime, format = "%d.%m.%Y %H:%M")) / (30*60)) * (30*60) + 30*60, origin = "1970-01-01"), format = "%Y-%m-%d %H:%M:%S")

# Aggregate data by AnimalNum, Batch, Change, Phase, and 30-minute interval
message("Aggregating data...")
data_filtered_agg <- data_filtered %>%
  group_by(AnimalNum, Batch, Group, Sex, Change, Phase, PriorActive, DateTime30Min, DateTime30MinShifted) %>%
  summarize(ActivityIndex = mean(ActivityIndex)) %>%
  ungroup()

# Combine the two datetime columns to form the interval
data_filtered_agg$TimeInterval <- paste0(data_filtered_agg$DateTime30Min, " to ", data_filtered_agg$DateTime30MinShifted)

data_filtered_agg <- data_filtered_agg %>%
  mutate(SUS = AnimalNum %in% sus_animals)

# Calculate the number of half-hour periods elapsed since the first half-hour period for each Batch, Cage, and AnimalNum combination
data_filtered_agg <- data_filtered_agg %>%
  group_by(Batch, AnimalNum, Sex) %>%
  mutate(HalfHourElapsed = as.numeric(difftime(DateTime30Min, first(DateTime30Min), units = "secs"))/1800) %>%
  ungroup()

message("Done.")