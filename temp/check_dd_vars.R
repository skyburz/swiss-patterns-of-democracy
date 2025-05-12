# Script to analyze direct democracy data
data <- read.csv("data/cantonal_democracy_data.csv", stringsAsFactors = FALSE)

# Check the variables
dd_vars <- c('ddr_stutz', 'gir', 'vir', 'grr', 'frr')

cat("Variable check:\n")
for (var in dd_vars) {
  cat(paste(var, "exists in data:", var %in% names(data), "\n"))
  if (var %in% names(data)) {
    missing <- sum(is.na(data[[var]]) | data[[var]] == ".")
    cat(paste("  Missing values:", missing, "out of", nrow(data), "\n"))
    
    # Check years with data
    years_with_data <- sort(unique(data$jahr[!is.na(data[[var]]) & data[[var]] != "."]))
    cat(paste("  Years with data:", paste(years_with_data, collapse=", "), "\n"))
    
    # Show some sample data
    if (length(years_with_data) > 0) {
      latest_year <- max(years_with_data)
      sample <- data[data$jahr == latest_year, c("kanton", var)]
      cat(paste("  Sample data from", latest_year, ":\n"))
      print(head(sample, 5))
    }
  }
  cat("\n")
}

# Check if the canton references match up
source("R/canton_reference.R")

cat("\nKanton reference check:\n")
cantons_in_data <- unique(data$kanton)
cat(paste("Number of unique kantons in data:", length(cantons_in_data), "\n"))
cat(paste("Kantons in data:", paste(cantons_in_data, collapse=", "), "\n\n"))

# Check each canton name against the reference
cat("Canton standardization check:\n")
for (canton in cantons_in_data) {
  std_canton <- standardize_canton(canton)
  cat(paste(canton, "-->", std_canton, "\n"))
}

# Check for specific problematic year and variables
cat("\nChecking data for 2018 (often used for these indexes):\n")
dd_data_2018 <- data[data$jahr == 2018, c("kanton", dd_vars)]
print(head(dd_data_2018, 10))

# Check for types of values
cat("\nChecking value types:\n")
for (var in dd_vars) {
  if (var %in% names(data)) {
    non_na_values <- data[[var]][!is.na(data[[var]]) & data[[var]] != "."]
    if (length(non_na_values) > 0) {
      cat(paste(var, "sample values:", paste(head(non_na_values, 5), collapse=", "), "\n"))
      cat(paste(var, "value class:", class(non_na_values), "\n"))
      
      # Try numeric conversion
      numeric_values <- suppressWarnings(as.numeric(non_na_values))
      cat(paste(var, "can be converted to numeric:", !any(is.na(numeric_values)), "\n"))
    } else {
      cat(paste(var, "has no non-NA values\n"))
    }
  }
  cat("\n")
} 