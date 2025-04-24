# Check available years for sitzparl variable
data <- read.csv('data/cantonal_democracy_data.csv')
years <- sort(unique(data$jahr))
print("Available years:")
print(years)

# Check if sitzparl exists in the data
if("sitzparl" %in% names(data)) {
  print("sitzparl variable exists")
  # Check which years have sitzparl data
  sitzparl_years <- sort(unique(data$jahr[!is.na(data$sitzparl)]))
  print("Years with sitzparl data:")
  print(sitzparl_years)
} else {
  print("sitzparl variable does not exist")
} 