# Data Preparation Script for Swiss Patterns of Democracy

# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# Function to read and prepare the Swiss cantonal democracy dataset
prepare_cantonal_data <- function() {
  # Read the Excel file
  file_path <- "data/Kantonale DM_1979–2023.xlsx"
  
  # First read the Excel sheet names to identify the data structure
  sheets <- readxl::excel_sheets(file_path)
  
  # Print sheets for debugging
  cat("Available sheets:", paste(sheets, collapse = ", "), "\n")
  
  # Attempt to read data from the first sheet (adjust as needed based on sheet structure)
  cantonal_data <- readxl::read_excel(
    file_path,
    sheet = 1,  # Adjust if necessary
    na = c("", "NA", "N/A")
  )
  
  # Basic data cleaning
  cantonal_data <- cantonal_data %>%
    # Convert column names to lowercase and replace spaces with underscores
    rename_all(~str_to_lower(str_replace_all(., " ", "_"))) %>%
    # Remove any completely empty rows or columns
    janitor::remove_empty("rows") %>%
    janitor::remove_empty("cols")
  
  # Print column names for debugging
  cat("Columns in dataset:", paste(names(cantonal_data), collapse = ", "), "\n")
  
  # Save the processed data as RDS for faster loading in the app
  saveRDS(cantonal_data, "data/cantonal_democracy_data.rds")
  
  # Also save as CSV for easier external use
  write.csv(cantonal_data, "data/cantonal_democracy_data.csv", row.names = FALSE)
  
  # Return the processed data
  return(cantonal_data)
}

# Function to create summaries by canton and year
create_summary_data <- function(cantonal_data) {
  # This function will be expanded once we understand the data structure
  # For now, we set up a placeholder
  
  # First check if the dataset has canton and year columns
  has_canton <- any(grepl("canton", names(cantonal_data), ignore.case = TRUE))
  has_year <- any(grepl("year|jahr", names(cantonal_data), ignore.case = TRUE))
  
  if(has_canton && has_year) {
    # Identify the actual column names
    canton_col <- names(cantonal_data)[grep("canton", names(cantonal_data), ignore.case = TRUE)[1]]
    year_col <- names(cantonal_data)[grep("year|jahr", names(cantonal_data), ignore.case = TRUE)[1]]
    
    # Create summary by canton and year
    summary_data <- cantonal_data %>%
      group_by(across(all_of(c(canton_col, year_col)))) %>%
      summarize(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")
    
    # Save summary data
    saveRDS(summary_data, "data/cantonal_summary.rds")
    return(summary_data)
  } else {
    warning("Cannot create summary: canton or year columns not found")
    return(NULL)
  }
}

# Execute the data preparation
cantonal_data <- prepare_cantonal_data()
summary_data <- create_summary_data(cantonal_data)

cat("Data preparation complete!\n")
cat("Processed data saved as:\n")
cat("- data/cantonal_democracy_data.rds\n")
cat("- data/cantonal_democracy_data.csv\n")
if(!is.null(summary_data)) {
  cat("- data/cantonal_summary.rds\n")
} 