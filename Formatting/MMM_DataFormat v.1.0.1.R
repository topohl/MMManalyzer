## Read in data from Folders and xlsx data. The data is organized in subfolders. 
#  This will read in the data of different xlsx sheets and merge them based on the name of the folder. 
#  Also, the data will be sorted and formatted

# ===== Packages =====
required_packages <- c("readxl", "dplyr", "purrr", "stringr", "lubridate",
                       "readr", "openxlsx", "tidyr")

for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}

# ===== Toggles =====
gridInCage <- TRUE
analyseGridInCage <- FALSE
analyseCookie <- FALSE

# ===== Working directory =====
setwd("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/BatchAnalysis/males/")

# ===== Read CSV meta =====
excludedAnimals <- read.csv("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/BatchAnalysis/excludedAnimals.csv",
  header = FALSE, stringsAsFactors = FALSE
)
conAnimals <- read.csv("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/BatchAnalysis/con_animals.csv",
  header = FALSE, stringsAsFactors = FALSE
)
susAnimals <- read.csv("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/sus_animals.csv",
  header = FALSE, stringsAsFactors = FALSE
)

# Create organized output directory structure
output_base <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/BatchAnalysis/"
output_dirs <- c(
  file.path(output_base, "processed_data"),
  file.path(output_base, "processed_data/aggregated"),
  file.path(output_base, "processed_data/filtered"),
  file.path(output_base, "processed_data/raw")
)

# Create directories if they don't exist
for (dir in output_dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    print(paste("Created directory:", dir))
  }
}

# IMPORTANT: your pasted sex_list.csv shows comma separation, not tabs
sexAnimals <- read.csv("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/BatchAnalysis/sex_list.csv",
  header = TRUE, stringsAsFactors = FALSE, sep = ","
)

# Sanity check: must contain columns "males" and "females"
stopifnot(all(c("males", "females") %in% names(sexAnimals)))

# ===== Extract IDs =====
conAnimalNums <- conAnimals[[1]]
susAnimalNums <- susAnimals[[1]]
exclAnimalNums <- excludedAnimals[[1]]

# ===== Helpers to normalize IDs =====
clean_id <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    toupper()
}
pad4 <- function(x) ifelse(grepl("^[0-9]+$", x), stringr::str_pad(x, 4, pad = "0"), x)

# ===== Build tidy Sex lookup (pivot and clean) =====
sex_map <- sexAnimals %>%
  tidyr::pivot_longer(
    cols = c(males, females),
    names_to = "sex_col",
    values_to = "AnimalNum"
  ) %>%
  dplyr::mutate(
    AnimalNum = clean_id(AnimalNum),
    AnimalNum = pad4(AnimalNum),
    Sex = dplyr::case_when(
      sex_col == "males"   ~ "m",
      sex_col == "females" ~ "f"
    )
  ) %>%
  dplyr::select(AnimalNum, Sex) %>%
  dplyr::distinct()

# ===== Load or construct 'data' before this step =====
# Assumes 'data' already exists. If you read it from files, do so above.

# ===== Split Animal, normalize IDs, and join Sex =====
print("Separating Animal column into AnimalNum and Cage columns...")
data <- tidyr::separate(data, Animal, c("AnimalNum", "Cage"), sep = "_", remove = FALSE)

data <- data %>%
  dplyr::mutate(
    AnimalNum = clean_id(AnimalNum),
    AnimalNum = pad4(AnimalNum)
  ) %>%
  dplyr::left_join(sex_map, by = "AnimalNum")

# ===== Date/time conversion =====
print("Converting DateTime column to datetime format...")
data$DateTime <- as.POSIXct(data$DateTime, format = "%d.%m.%Y %H:%M")

# ===== Group assignment =====
print("Assigning animals to groups...")
data <- data %>%
  dplyr::mutate(
    Group = dplyr::case_when(
      AnimalNum %in% clean_id(pad4(susAnimalNums)) ~ "SUS",
      AnimalNum %in% clean_id(pad4(conAnimalNums)) ~ "CON",
      TRUE ~ "RES"
    )
  )

# ===== Exclude incomplete datasets =====
print("Exclude animals with non-complete datasets...")
data <- data[!data$AnimalNum %in% clean_id(pad4(exclAnimalNums)), ]

# ===== Fix ActivityIndex column name if needed =====
if (!exists("ActivityIndex")) {
  if ("ActivyIndex" %in% colnames(data)) {
    data <- data %>% dplyr::rename(ActivityIndex = ActivyIndex)
  } else {
    print("Skipping renaming of ActivyIndex to ActivityIndex because ActivyIndex does not exist.")
  }
} else {
  print("Skipping renaming of ActivyIndex to ActivityIndex because ActivityIndex already exists.")
}

# ===== Phase, timing, and hour calc =====
print("Creating Phase column based on time of day...")
print("Define timeframes before Active Phase...")
print("Converting Batch to factor variable...")
print("Calculate the number of hours elapsed since the first timestamp for each Batch...")
data <- data %>%
  dplyr::mutate(
    Phase = ifelse(
      format(DateTime, "%H:%M") >= "18:30" | format(DateTime, "%H:%M") < "06:30",
      "Active", "Inactive"
    ),
    PriorActive = ifelse(
      format(DateTime, "%H:%M") >= "16:30" & format(DateTime, "%H:%M") < "18:30",
      "TRUE", "FALSE"
    ),
    Batch = as.factor(Batch)
  ) %>%
  dplyr::group_by(Batch) %>%
  dplyr::mutate(Hour = difftime(DateTime, dplyr::first(DateTime), units = "hours")) %>%
  dplyr::ungroup()

# ===== Init counters =====
data$ConsecActive <- 0
data$ConsecInactive <- 0
data$ConsecPhases <- 0

# ===== Filtered with phase counters =====
print("Create new columns ActivePeriods, InactivePeriods, TotalPeriods, ConsecActive, and ConsecInactive...")
print("Remove the first and last inactive phase for each Change and Batch...")
data_filtered <- data %>%
  dplyr::group_by(Batch, Change, AnimalNum) %>%
  dplyr::mutate(
    ActivePeriods = ifelse(Phase == "Active", 1, 0),
    InactivePeriods = ifelse(Phase == "Inactive", 1, 0),
    TotalPeriods = sum(ActivePeriods, InactivePeriods),
    ConsecActive = ifelse(Phase == "Active", cumsum(c(0, diff(ifelse(Phase == "Inactive", 0, 1))) == 1), 0),
    ConsecInactive = ifelse(Phase == "Inactive", cumsum(c(1, diff(ifelse(Phase == "Active", 0, 1))) == 1), 0),
    ConsecPhases = cumsum(c(0, diff(as.numeric(factor(Phase))) != 0))
  ) %>%
  dplyr::filter(!(ConsecInactive == max(ConsecInactive[Phase == "Inactive"]) & Phase == "Inactive" |
                  ConsecInactive == min(ConsecInactive[Phase == "Inactive"]) & Phase == "Inactive")) %>%
  dplyr::ungroup()

# ===== Helpers: optional removals =====
removePhases <- function(data) {
  if (gridInCage) {
    print("Remove last two active and inactive phases of CC4 due to grid within cage...")
    data <- data %>%
      dplyr::filter(!(Change == "CC4" &
                      ((Phase == "Active" & ConsecActive > 2) |
                       (Phase == "Inactive" & ConsecInactive > 2))))
  } else {
    print("Skipping removal of last two active and inactive phases of CC4...")
  }
  return(data)
}

analyseGrid <- function(data) {
  if (analyseGridInCage) {
    print("Extracting timeframe when grid is in cage...")
    data <- data %>% dplyr::filter(Change == "CC4" & ConsecPhases > 2)
  } else {
    print("Skipping extraction of timeframe when grid is in cage...")
  }
  return(data)
}

# ===== Apply removals =====
data <- removePhases(data)
data <- analyseGrid(data)
data_filtered <- removePhases(data_filtered)
data_filtered <- analyseGrid(data_filtered)

# ===== Sleep metrics =====
print("Add new column for SleepBouts...")
data_filtered <- data_filtered %>%
  dplyr::group_by(AnimalNum, Phase, Change) %>%
  dplyr::mutate(
    SleepBouts = cumsum(ifelse(ActivityIndex == 0 & dplyr::lag(ActivityIndex != 0, default = TRUE) |
                                 dplyr::row_number() == 1 & ActivityIndex == 0, 1, 0))
  ) %>%
  dplyr::ungroup()

print("Add new column for Sleep Duration...")
data_filtered <- data_filtered %>%
  dplyr::group_by(AnimalNum, Phase, Change) %>%
  dplyr::mutate(
    SleepDur = cumsum(ActivityIndex == 0)
  ) %>%
  dplyr::ungroup()

# ===== Round to 30-min bins and shifted bins =====
data_filtered$DateTime30Min <- format(
  as.POSIXct(trunc(as.numeric(as.POSIXct(data_filtered$DateTime, format = "%d.%m.%Y %H:%M")) / (30*60)) * (30*60),
             origin = "1970-01-01"),
  format = "%Y-%m-%d %H:%M:%S"
)
data_filtered$DateTime30MinShifted <- format(
  as.POSIXct(trunc(as.numeric(as.POSIXct(data_filtered$DateTime, format = "%d.%m.%Y %H:%M")) / (30*60)) * (30*60) + 30*60,
             origin = "1970-01-01"),
  format = "%Y-%m-%d %H:%M:%S"
)


# ===== Aggregate per half-hour interval =====
print("Aggregate data by AnimalNum, Batch, Change, Phase, and 30-minute interval...")
data_filtered_agg <- data_filtered %>%
  dplyr::group_by(AnimalNum, Sex, Batch, Group, Change, Phase, DateTime30Min, DateTime30MinShifted) %>%
  dplyr::summarize(ActivityIndex = mean(ActivityIndex), .groups = "drop")

# ===== Correct DateTime Conversion and Ordering =====
if (is.factor(data_filtered_agg$DateTime30Min)) {
  data_filtered_agg$DateTime30Min <- as.character(data_filtered_agg$DateTime30Min)
}
data_filtered_agg$DateTime30Min <- as.POSIXct(data_filtered_agg$DateTime30Min)

data_filtered_agg <- data_filtered_agg %>%
  dplyr::arrange(AnimalNum, Batch, DateTime30Min)

# ===== Interval label =====
print("Forming time interval...")
data_filtered_agg$TimeInterval <- paste0(data_filtered_agg$DateTime30Min, " to ", data_filtered_agg$DateTime30MinShifted)

# ===== SUS flag =====
print("Assign susceptible animals to the SUS group...")
data_filtered_agg <- data_filtered_agg %>%
  dplyr::mutate(SUS = AnimalNum %in% clean_id(pad4(susAnimalNums)))

# ===== Half-hour elapsed within Batch =====
print("Calculate the number of half-hour periods elapsed...")
data_filtered_agg <- data_filtered_agg %>%
  dplyr::group_by(Batch, AnimalNum) %>%
  dplyr::mutate(HalfHourElapsed = as.numeric(difftime(DateTime30Min, dplyr::first(DateTime30Min), units = "secs"))/1800) %>%
  dplyr::ungroup()

# ====== Save data files to organized folders =====
write.csv(data_filtered_agg,
          file.path(output_base, "processed_data/aggregated/data_filtered_agg.csv"),
          row.names = FALSE)
write.csv(data_filtered,
          file.path(output_base, "processed_data/filtered/data_filtered.csv"),
          row.names = FALSE)
write.csv(data,
          file.path(output_base, "processed_data/raw/data.csv"),
          row.names = FALSE)

print("Data files saved successfully!")
print("Done!")

# ===== Optional diagnostics =====
# print(names(sexAnimals))
# print(head(sex_map))
# print(table(data$Sex, useNA = "ifany"))
# print(head(dplyr::select(data, AnimalNum, Sex)))
# print(sum(!is.na(data$Sex)))
