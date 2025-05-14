# Run the Swiss Patterns of Democracy Shiny app with Firefox
options(browser = "C:/Program Files/Mozilla Firefox/firefox.exe")

# Change to the app directory and run the app
setwd("app")
shiny::runApp(launch.browser = TRUE) 