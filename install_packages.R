# Install required packages for the Swiss Patterns of Democracy Shiny application

# List of required packages
packages <- c(
  # Core Shiny packages
  "shiny",      # Web application framework
  "bslib",      # Bootstrap styling for Shiny
  "shinyjs",    # JavaScript operations in Shiny
  
  # Data manipulation
  "dplyr",      # Data manipulation grammar
  "tidyr",      # Data tidying
  "readr",      # Data import
  "readxl",     # Excel file import
  "DT",         # Interactive data tables
  
  # Visualization
  "ggplot2",    # Data visualization
  "plotly",     # Interactive plots
  "viridis",    # Color palettes
  
  # Date handling
  "lubridate",  # Date-time manipulation
  
  # Maps
  "sf",        # For map visualizations
  "leaflet",   # For interactive maps
  "geojsonio", # For handling GeoJSON data
  
  # Other utilities
  "jsonlite",   # JSON handling
  "magrittr",   # Pipe operator
  "tools",      # File utilities
  "glue"        # String interpolation
)

# Install missing packages
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))

# Print message
cat("All required packages are installed and loaded.\n") 