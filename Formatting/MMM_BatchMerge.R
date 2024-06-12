## merge files from different batches
### Read in data from Folders and xlsx data. The data is organized in subfolders. 
#  This will read in the data of different xlsx sheets and merge them based on the name of the folder. 
#  Also, the data will be sorted and formatted
#  Later, the data points will be plotted.

# List of packages to check and load
packages <- c("readxl", "dplyr", "purrr", "stringr", "lubridate", "readr", "openxlsx")

# Check if each package is installed, install if not, and load it
for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
        install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
}

# set the working directory to the parent directory containing the subfolders and get a list of the B1 and B2 subfolders in the directory
setwd("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/BatchAnalysis/")
subfolders <- c("males/B1", "males/B2", "females/B3", "females/B4", "males/B5", "females/B6")

file_pattern <- "E9_SIS_B\\d+_CC\\d_ActivityIndex.csv"

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
    csv_data <- process_file(file_path)
    write.xlsx(csv_data, output_path, rowNames = FALSE)
    cat(paste0("Saved ", output_path, "\n"))  # print output file path
}

# read all CSV files in the subfolders, transform them, and save as XLSX files
#files <- list.files(path = c("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/BatchAnalysis/males",
#                             "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/BatchAnalysis/females"),
#                    pattern = ".csv$", recursive = TRUE, full.names = TRUE)
#purrr::walk(files, process_and_save_xlsx)

# read all CSV files in the B1 and B2 subfolders, convert them to xlsx, and combine them into a single data frame
data <- map_dfr(subfolders, ~{
    # get a list of subfolders in the current B1 or B2 folder
    subfolder_paths <- list.dirs(paste0(".", "/", .x), recursive = FALSE)
    
    # get files matching the pattern for Batch 1 and Batch 2
    all_files <- map(subfolder_paths, ~list.files(path = .x, pattern = file_pattern, full.names = TRUE)) %>% 
        flatten()
    
    # process all files and combine into a single data frame
    all_data <- map(all_files, ~process_file(.x)) %>% 
        bind_rows()
    
    all_data
})

# combine all data frames into a single data frame
combined_data <- bind_rows(data)


