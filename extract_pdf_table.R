# Script to extract text from PDF documentation
# Install required packages if not already installed
if (!requireNamespace("pdftools", quietly = TRUE)) {
  install.packages("pdftools")
}
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

library(pdftools)
library(tidyverse)

# Path to the PDF file
pdf_path <- "data/Kantonale DM_1979–2023_Codebuch.pdf"

# Extract all text from the PDF
message("Extracting text from PDF...")
pdf_text <- pdf_text(pdf_path)

# Save the raw text to a file
write(pdf_text, "data/pdf_extracted_text.txt")
message("Raw text saved to data/pdf_extracted_text.txt")

# Try to parse the text to identify table sections
# This is a basic approach - the accuracy depends on the PDF structure
message("Attempting to parse table structure...")

# Function to identify potential table rows
is_table_row <- function(line) {
  # Check if line contains tab-like patterns or multiple spaces
  grepl("\t|\\s{2,}", line) && nchar(line) > 10 && !grepl("^\\s*$", line)
}

# Process each page to extract potential table data
all_table_lines <- character(0)
for(i in seq_along(pdf_text)) {
  page_lines <- strsplit(pdf_text[i], "\n")[[1]]
  
  # Filter for lines that might be table rows
  table_lines <- page_lines[sapply(page_lines, is_table_row)]
  all_table_lines <- c(all_table_lines, table_lines)
}

# Try to parse the potential table lines into a CSV structure
if(length(all_table_lines) > 0) {
  # Split each line into columns based on multiple spaces or tabs
  table_data <- lapply(all_table_lines, function(line) {
    # Clean up: trim whitespace and normalize spaces
    line <- trimws(line)
    # Split on multiple spaces or tabs
    cols <- strsplit(line, "\\s{2,}|\\t")[[1]]
    # Ensure we have exactly 3 columns (Indikator, Quellen, Bemerkungen)
    if(length(cols) >= 3) {
      # If more than 3, combine extra columns into the last one
      if(length(cols) > 3) {
        cols <- c(cols[1], cols[2], paste(cols[3:length(cols)], collapse = " "))
      }
      return(cols)
    } else if(length(cols) == 2) {
      # If only 2 columns, add an empty third column
      return(c(cols, ""))
    } else if(length(cols) == 1) {
      # If only 1 column, likely a continuation of previous row
      return(c(cols, "", ""))
    }
    return(NULL)
  })
  
  # Remove NULL entries
  table_data <- table_data[!sapply(table_data, is.null)]
  
  # Convert to data frame
  if(length(table_data) > 0) {
    df <- do.call(rbind, table_data)
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    names(df) <- c("Indikator", "Quellen", "Bemerkungen")
    
    # Write to CSV
    write.csv(df, "data/variable_documentation.csv", row.names = FALSE)
    message("Extracted table saved to data/variable_documentation.csv")
  } else {
    message("No table data could be identified")
  }
} else {
  message("No table data could be identified")
}

# Try to open Firefox with a PDF table extraction tool
tryCatch({
  message("Opening Firefox with an online PDF table extraction tool...")
  system('start firefox "https://tabula.technology/"')
}, error = function(e) {
  message("Could not open Firefox: ", e$message)
}) 