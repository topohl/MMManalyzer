#' Merge and Process CSV Files from Different Experimental Batches
#'
#' @Author Tobias Pohl
#' @Date 2025-01-29
#' 
#' @description
#' This script reads, processes, and merges CSV files from different experimental batches.
#' It extracts relevant metadata (e.g., batch number, change number) from filenames,
#' reformats the data, and saves the processed results as Excel (.xlsx) files.
#' 
#' The processed data is then combined into a single data frame for further analysis.
#'
#' @section Dependencies:
#' The script requires the following R packages: `readxl`, `dplyr`, `purrr`, `stringr`, `lubridate`, `readr`, `openxlsx`.
#' If these packages are not installed, they will be installed automatically.
#'
#' @section Input:
#' - CSV files stored in subfolders corresponding to different experimental batches.
#' - Filenames contain metadata regarding batch and condition (e.g., "B1", "CC1").
#'
#' @section Output:
#' - Processed and reformatted data saved as .xlsx files.
#' - A combined data frame (`combined_data`) containing merged results from all batches.

# ================================================
# Dependencies
# ================================================

# List of required packages
packages <- c("readxl", "dplyr", "purrr", "stringr", "lubridate", "readr", "openxlsx")

# Install missing packages and load them
for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
        install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
}

# ================================================
# Input Parameters
# ================================================

# Set working directory (adjust path accordingly)
setwd("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Raw Data/Behavior/RFID/BatchAnalysis/")

# Define the subfolders containing batch data
subfolders <- c("males/B1", "males/B2", "females/B3", "females/B4", "males/B5", "females/B6")

# Define the file naming pattern to filter relevant CSV files
file_pattern <- "E9_SIS_B\\d+_CC\\d_ActivityIndex.csv"
#file_pattern <- "E9_SIS_B\\d+_EPMaftercagechange_ActivityIndex.csv"

# ================================================
# Functions for processing and merging CSV files
# ================================================

#' @title process_file
#' @description Process a CSV file by reading, transforming, and adding metadata columns.
#' @param file_path Character. Path to the CSV file.
#' @return A tibble containing the processed data with additional metadata columns.

process_file <- function(file_path) {
    cat(paste0("Processing ", file_path, "\n"))  # print file path
    read.csv(file_path, sep = ";") %>%     # read the csv file with semicolon delimiter
        mutate(filename = basename(file_path)) %>%     # add a column with the filename
        mutate(Batch = str_extract(filename, "B\\d")) %>%   # extract the Batch number from the filename
        mutate(Change = str_extract(filename, "CC\\d")) %>%   # extract the Change number from the filename
        select(-filename) # remove the filename column
}

#' @title process_and_save_xlsx
#' @description Process a CSV file and save the results as an XLSX file.
#' @param file_path Character. Path to the CSV file.
#' @return Saves an XLSX file in the same directory as the original CSV.

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

# ================================================
# Read and merge CSV files from subfolders
# ================================================
#' @description Read and merge CSV files from subfolders into a single data frame.
#' 
#' @details This function reads all CSV files from the specified subfolders, processes them,
#' and combines the results into a single data frame.
#' 
#' @param subfolders Character vector. Paths to subfolders containing CSV files.
#' @param file_pattern Character. Regular expression pattern to match relevant CSV files.
#' @return A tibble containing the combined dataset from all batches.

data <- map_dfr(subfolders, ~{
    # Get a list of subfolders for each batch
    subfolder_paths <- list.dirs(paste0(".", "/", .x), recursive = FALSE)
    
    # Retrieve all matching CSV files from subfolders
    all_files <- map(subfolder_paths, ~list.files(path = .x, pattern = file_pattern, full.names = TRUE)) %>% 
        flatten()
    
    # Process all files and merge into a single data frame
    all_data <- map(all_files, ~process_file(.x)) %>% 
        bind_rows()
    
    all_data
})

# Combine all data frames into a single merged data set
combined_data <- bind_rows(data)