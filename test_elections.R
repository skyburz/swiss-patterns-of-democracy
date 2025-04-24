# Test script for prepare_election_data function
library(dplyr)
library(tidyr)

# Source the canton reference
source("R/canton_reference.R")

# Source the elections module
source("R/elections_module.R")

# Read the data
data <- read.csv("data/cantonal_democracy_data.csv")

# Test the function with year 1979
result <- prepare_election_data(data, 1979, "parlament_sitze")

# Print the first few rows
print("First few rows of the result:")
print(head(result))

# Print summary of sitze values
print("Summary of sitze values:")
print(summary(result$sitze))

# Check for any NA values
print("Number of NA values in sitze:")
print(sum(is.na(result$sitze)))

# Check for any remaining "." values
print("Checking for any remaining '.' values:")
print(any(result$sitze == ".", na.rm = TRUE))

# Print data for Bern (canton_nr = 2)
print("Data for Bern (canton_nr = 2):")
print(result %>% filter(canton_nr == 2)) 