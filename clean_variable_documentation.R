# Script to clean the variable documentation CSV
library(tidyverse)

# Read the CSV file
var_doc <- read.csv("data/variable_documentation.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

# Define pattern for variable identifiers
var_pattern <- "^[a-zA-Z][a-zA-Z0-9_]*(_[a-zA-Z0-9_]+)*$"

# Preprocess to identify actual variables
var_doc <- var_doc %>%
  # Skip initial rows that aren't part of the table
  filter(row_number() > 2) %>%
  # Add a flag for rows that start a new variable definition
  mutate(
    is_variable = grepl(var_pattern, Indikator) | 
                  (grepl("^[a-zA-Z]", Indikator) & nchar(Indikator) > 2),
    variable_group = cumsum(is_variable)
  )

# Process each variable group
result <- data.frame(
  Variable = character(),
  Source = character(),
  Description = character(),
  stringsAsFactors = FALSE
)

for (i in unique(var_doc$variable_group)) {
  group_rows <- var_doc %>% filter(variable_group == i)
  
  if (nrow(group_rows) > 0) {
    # Get the variable name (first row's Indikator if it's a variable, otherwise "Unknown")
    var_name <- if (group_rows$is_variable[1]) group_rows$Indikator[1] else "Unknown"
    
    # Combine all non-empty text from each column
    source_text <- paste(group_rows$Quellen[group_rows$Quellen != ""], collapse = " ")
    description_text <- paste(group_rows$Bemerkungen[group_rows$Bemerkungen != ""], collapse = " ")
    
    # Add to result if it's a valid variable
    if (var_name != "Unknown" && var_name != "Indikator") {
      result <- rbind(result, data.frame(
        Variable = var_name,
        Source = source_text,
        Description = description_text,
        stringsAsFactors = FALSE
      ))
    }
  }
}

# Clean up text fields
result <- result %>%
  mutate(
    Source = gsub("\\s+", " ", Source),
    Description = gsub("\\s+", " ", Description),
    Source = trimws(Source),
    Description = trimws(Description)
  )

# Write the cleaned CSV
write.csv(result, "data/cleaned_variable_documentation.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Try to install writexl if it's not available
if (!requireNamespace("writexl", quietly = TRUE)) {
  message("Installing writexl package for Excel output...")
  install.packages("writexl")
}

# Export to Excel if possible
if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(result, "data/cleaned_variable_documentation.xlsx")
  message("Excel file created: data/cleaned_variable_documentation.xlsx")
} else {
  message("Package 'writexl' not installed or installation failed. Excel output not created.")
}

# Print summary
message("Cleaned variable documentation saved to data/cleaned_variable_documentation.csv")
message("Original rows: ", nrow(var_doc), ", Cleaned rows: ", nrow(result))
message("First few entries:")
print(head(result)) 