# Load libraries
library(dplyr)

# Load data
data <- read.csv("data/cantonal_democracy_data.csv")

# Find canton and year columns
canton_col <- "kanton"
year_col <- "jahr"

# Print column names
cat("Columns in dataset:", paste(names(data), collapse=", "), "\n\n")

# Function to check if a variable is constant within each canton-year group
check_constant <- function(data, col_name) {
  result <- data %>%
    group_by(!!sym(canton_col), !!sym(year_col)) %>%
    summarize(
      distinct_values = n_distinct(!!sym(col_name), na.rm = TRUE),
      .groups = "drop"
    )
  
  # If all groups have only 1 distinct value, the variable is constant
  return(all(result$distinct_values <= 1))
}

# Check each column
constant_vars <- c()
for (col in names(data)) {
  if (col != canton_col && col != year_col) {
    if (check_constant(data, col)) {
      constant_vars <- c(constant_vars, col)
    }
  }
}

# Print results
cat("Variables constant within each canton-year combination:\n")
cat(paste("- ", constant_vars, collapse = "\n"), "\n")

# Save results
write.csv(data.frame(constant_variables = constant_vars), 
          "constant_variables.csv", 
          row.names = FALSE) 