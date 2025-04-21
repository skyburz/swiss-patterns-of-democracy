# Load libraries
library(dplyr)

# Load data
data <- read.csv("data/cantonal_democracy_data.csv")

# Find canton and year columns
canton_col <- "kanton"
year_col <- "jahr"

# Function to check if a variable is NOT constant within each canton-year group
check_non_constant <- function(data, col_name) {
  result <- data %>%
    group_by(!!sym(canton_col), !!sym(year_col)) %>%
    summarize(
      distinct_values = n_distinct(!!sym(col_name), na.rm = TRUE),
      .groups = "drop"
    )
  
  # If any group has more than 1 distinct value, the variable is not constant
  return(any(result$distinct_values > 1))
}

# Check each column
non_constant_vars <- c()
for (col in names(data)) {
  if (col != canton_col && col != year_col) {
    if (check_non_constant(data, col)) {
      non_constant_vars <- c(non_constant_vars, col)
    }
  }
}

# Print results
cat("Variables NOT constant within each canton-year combination:\n")
if (length(non_constant_vars) > 0) {
  cat(paste("- ", non_constant_vars, collapse = "\n"), "\n")
} else {
  cat("All variables are constant within each canton-year combination.\n")
}

# Save results
write.csv(data.frame(non_constant_variables = non_constant_vars), 
          "non_constant_variables.csv", 
          row.names = FALSE) 