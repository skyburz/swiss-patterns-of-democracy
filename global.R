# Swiss Patterns of Democracy - Global Settings
# This file loads packages, global functions, and data that are used throughout the app

# Load required packages
library(shiny)
library(bslib)       # For Bootstrap 5 UI components
library(dplyr)       # For data manipulation
library(ggplot2)     # For creating visualizations
library(plotly)      # For interactive plots
library(DT)          # For interactive tables

# Additional packages for the canton map
library(sf)          # For spatial features
library(leaflet)     # For interactive maps
library(jsonlite)    # For JSON handling
library(geojsonio)    # For handling GeoJSON/TopoJSON data

# Set global options
options(shiny.autoreload = TRUE)  # Auto reload app during development

# Helper function to list available datasets in the data folder
list_data_files <- function() {
  # Only return CSV files, excluding the Excel file
  files <- list.files("data", pattern = "\\.csv$", full.names = FALSE)
  # Make sure we're not including any Excel files
  files <- files[!grepl("\\.xlsx$", files)]
  return(files)
}

# Function to load a dataset based on file extension
load_dataset <- function(filename) {
  filepath <- file.path("data", filename)
  
  # Only handle CSV files
  if (grepl("\\.csv$", filepath)) {
    data <- read.csv(filepath)
    # Convert any "." values to NA
    data <- data.frame(lapply(data, function(x) ifelse(x == ".", NA, x)))
    return(data)
  } else {
    stop("Only CSV files are supported")
  }
}

# Load cantonal data from CSV
cantonal_data <- read.csv("data/cantonal_democracy_data.csv")

# Add custom theme settings
custom_theme <- bs_theme(
  version = 5,                # Use Bootstrap 5
  bootswatch = "flatly",     # Use Flatly Bootswatch theme
  primary = "#2C3E50",       # Set primary color
  "font-size-base" = "1rem"  # Base font size
) 