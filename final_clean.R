# Script to extract variable documentation directly from PDF text
library(pdftools)
library(tidyverse)

# Extract text from PDF
pdf_text <- pdf_text("data/Kantonale DM_1979–2023_Codebuch.pdf")

# Combine all pages into one text
all_text <- paste(pdf_text, collapse = "\n")

# Write the full extracted text to a file
writeLines(all_text, "data/full_pdf_text.txt")

# Create a data frame to store variable information
variables <- data.frame(
  Variable = character(),
  Description = character(),
  stringsAsFactors = FALSE
)

# Look for common variable patterns in the text
# We'll use a regex to identify variable names and their descriptions
variable_pattern <- "(^|\\s)([a-zA-Z][a-zA-Z0-9_]*(_[a-zA-Z0-9_]+)*)\\s+(.+?)(?=(^|\\s)[a-zA-Z][a-zA-Z0-9_]*(_[a-zA-Z0-9_]+)*\\s|$)"
matches <- gregexpr(variable_pattern, all_text, perl = TRUE)

if (length(matches) > 0 && matches[[1]][1] != -1) {
  match_text <- regmatches(all_text, matches)
  
  # Extract variable names and descriptions
  for (match in match_text[[1]]) {
    # Extract the variable name and description
    var_parts <- strsplit(match, "\\s+", perl = TRUE)[[1]]
    if (length(var_parts) >= 2) {
      var_name <- var_parts[1]
      var_desc <- paste(var_parts[2:length(var_parts)], collapse = " ")
      
      # Add to variables data frame
      variables <- rbind(variables, data.frame(
        Variable = var_name,
        Description = var_desc,
        stringsAsFactors = FALSE
      ))
    }
  }
}

# Filter out entries that don't look like variables
variables <- variables %>%
  filter(grepl("^[a-zA-Z][a-zA-Z0-9_]*(_[a-zA-Z0-9_]+)*$", Variable)) %>%
  arrange(Variable)

# Write to CSV
write.csv(variables, "data/variables_from_pdf.csv", row.names = FALSE)

# Also create a manual list of variables from the PDF text file
# This is a backup approach that you can edit manually
# Search for lines that might contain variable names
pdf_lines <- strsplit(all_text, "\n")[[1]]
potential_vars <- pdf_lines[grepl("^[a-zA-Z][a-zA-Z0-9_]*(_[a-zA-Z0-9_]+)*\\s", pdf_lines)]
writeLines(potential_vars, "data/potential_variables.txt")

message("Extracted text saved to data/full_pdf_text.txt")
message("Potential variables saved to data/potential_variables.txt")
message("Variable data saved to data/variables_from_pdf.csv")
message("Number of variables found: ", nrow(variables))
message("First few variables:")
print(head(variables)) 