# Script to identify variables that are constant within each Kanton-Jahr combination

# Load required libraries
library(dplyr)
library(tidyr)
library(readr)

# Load the data
data <- readRDS("data/cantonal_democracy_data.rds")

# If RDS doesn't work, try CSV
if (!exists("data")) {
  data <- read.csv("data/cantonal_democracy_data.csv")
}

# Print the column names to understand the data structure
cat("Column names in the dataset:\n")
print(names(data))

# Identify the canton and year columns
canton_col <- names(data)[grep("^kanton$", names(data), ignore.case = TRUE)[1]]
if (is.na(canton_col)) {
  canton_col <- names(data)[grep("canton|kanton", names(data), ignore.case = TRUE)[1]]
}

year_col <- names(data)[grep("year|jahr", names(data), ignore.case = TRUE)[1]]

cat("\nUsing canton column:", canton_col, "\n")
cat("Using year column:", year_col, "\n")

# Function to check if a variable is constant within each Kanton-Jahr combination
check_constant_by_group <- function(data, group_cols, check_col) {
  result <- data %>%
    group_by(across(all_of(group_cols))) %>%
    summarize(
      unique_values = n_distinct(!!sym(check_col), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    summarize(
      all_constant = all(unique_values == 1),
      max_unique = max(unique_values)
    )
  
  return(result$all_constant)
}

# Get all numeric and character columns (excluding grouping columns)
all_cols <- names(data)
group_cols <- c(canton_col, year_col)
check_cols <- setdiff(all_cols, group_cols)

# Check each column
constant_vars <- character(0)
non_constant_vars <- character(0)

cat("\nChecking which variables are constant within each Kanton-Jahr combination...\n")

for (col in check_cols) {
  # Skip columns with all NA values
  if (all(is.na(data[[col]]))) {
    cat("Skipping column with all NA values:", col, "\n")
    next
  }
  
  is_constant <- check_constant_by_group(data, group_cols, col)
  
  if (is_constant) {
    constant_vars <- c(constant_vars, col)
  } else {
    non_constant_vars <- c(non_constant_vars, col)
  }
}

# Print results
cat("\nVariables that are constant within each Kanton-Jahr combination:\n")
if (length(constant_vars) > 0) {
  for (var in constant_vars) {
    cat("- ", var, "\n")
  }
} else {
  cat("No variables are constant within each Kanton-Jahr combination.\n")
}

cat("\nVariables that vary within at least one Kanton-Jahr combination:\n")
if (length(non_constant_vars) > 0) {
  for (var in non_constant_vars) {
    cat("- ", var, "\n")
  }
} else {
  cat("All variables are constant within each Kanton-Jahr combination.\n")
}

# Save results to a file
results <- list(
  constant_variables = constant_vars,
  non_constant_variables = non_constant_vars
)

saveRDS(results, "constant_variables_results.rds")
write.csv(data.frame(
  variable = constant_vars,
  is_constant = TRUE
), "constant_variables.csv", row.names = FALSE)

cat("\nResults saved to constant_variables_results.rds and constant_variables.csv\n") 