# Canton Map Module
# This module creates an interactive map of Swiss cantons using leaflet

library(shiny)
library(bslib)
library(dplyr)
library(leaflet)
library(sf)
library(jsonlite)
library(geojsonio)  # For handling GeoJSON/TopoJSON data
library(viridis)

# Source the canton mapping functions
source("R/canton_mapping.R")

#' UI function for canton map module
#' @param id The module ID
#' @return A Shiny UI element
canton_map_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Add custom JavaScript to format slider values without commas
    tags$script(HTML(sprintf("
      $(document).ready(function() {
        // Format the year slider values without commas
        setTimeout(function() {
          $('.js-range-slider').each(function() {
            $(this).on('input', function() {
              var value = $(this).val();
              $(this).next('.irs-single').text(value);
            });
          });
        }, 100);
      });
    "))),
    fluidRow(
      column(3,
        card(
          card_header("Steuerung"),
          card_body(
            selectInput(ns("variable"), "Variable", 
                        choices = c("Parlamentssitze" = "sitzparl", "Regierungssitze" = "sitzreg"), 
                        selected = "sitzparl"),
            sliderInput(ns("year"), "Jahr", min = 1979, max = 2023, value = 2023, step = 1, 
                        ticks = TRUE, sep = "", animate = TRUE),
            checkboxInput(ns("show_labels"), "Kantonsnamen anzeigen", value = TRUE)
          )
        )
      ),
      column(9,
        card(
          card_header(uiOutput(ns("map_title"))),
          card_body(
            leafletOutput(ns("map"), height = "600px")
          )
        )
      )
    )
  )
}

#' Server function for canton map module
#' @param id The module ID
#' @param data The cantonal democracy data
#' @return A Shiny server function
canton_map_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Load Swiss map data
    swiss_map <- reactive({
      tryCatch({
        # Read the TopoJSON file and convert to SF object
        cantons <- topojson_read("data/swiss-maps.json", "cantons")
        
        # Convert full canton names to abbreviations
        cantons$kanton_abbr <- sapply(cantons$name, function(name) {
          abbr <- get_canton_abbr(name)
          if (is.null(abbr)) {
            warning(paste("Could not find abbreviation for canton:", name))
            return(NA)  # Return NA instead of NULL for better compatibility
          }
          return(abbr)
        })
        
        # Print debug information about the mapping
        print("Canton name mapping:")
        print(data.frame(
          full_name = cantons$name,
          abbreviation = cantons$kanton_abbr
        ))
        
        return(cantons)
      }, error = function(e) {
        showNotification(
          paste("Error loading map data:", e$message),
          type = "error",
          duration = 10
        )
        return(NULL)
      })
    })
    
    # Prepare data for mapping
    map_data <- reactive({
      req(input$variable, swiss_map(), data)
      
      # Filter data for selected year
      year_data <- data %>%
        filter(jahr == input$year) %>%
        select(kanton, !!sym(input$variable))
      
      # Print debug information
      print("Cantons in data:")
      print(unique(year_data$kanton))
      print("Cantons in map:")
      print(unique(swiss_map()$kanton_abbr))
      
      # Join with map data using abbreviations
      map_with_data <- swiss_map() %>%
        left_join(year_data, by = c("kanton_abbr" = "kanton"))
      
      # Check for unmatched cantons
      unmatched <- setdiff(year_data$kanton, map_with_data$kanton_abbr)
      if (length(unmatched) > 0) {
        warning("Some cantons could not be matched: ", paste(unmatched, collapse = ", "))
      }
      
      # Check for map regions without data
      missing_data <- map_with_data$kanton_abbr[is.na(map_with_data[[input$variable]])]
      if (length(missing_data) > 0) {
        warning("Map regions without data: ", paste(missing_data, collapse = ", "))
      }
      
      # Print the first few rows of the joined data to debug
      print("First few rows of joined data:")
      print(head(map_with_data))
      
      return(map_with_data)
    })
    
    # Create the map title
    output$map_title <- renderUI({
      req(input$variable)
      title_text <- if(input$variable == "sitzparl") "Kantonskarte - Parlamentssitze" else "Kantonskarte - Regierungssitze"
      title_text
    })
    
    # Create the map
    output$map <- renderLeaflet({
      req(map_data())
      
      # Get the values for the selected variable, excluding NA
      values <- map_data()[[input$variable]]
      valid_values <- values[!is.na(values)]
      
      # Print debug information
      print("Valid values for color palette:")
      print(valid_values)
      
      # Create color palette based on the selected variable
      if (input$variable == "sitzreg") {
        # For Regierungssitze, use discrete values (5, 7, 9)
        # Create a factor with these levels
        discrete_values <- factor(c(5, 7, 9), levels = c(5, 7, 9))
        
        # Create a color palette for discrete values with darker shades of blue
        pal <- colorFactor(
          palette = c("#C6DBEF", "#6BAED6", "#2171B5"),  # Darker shades of blue
          domain = discrete_values,
          na.color = "#808080"
        )
        
        # Create the map with discrete values
        leaflet(map_data()) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          setView(lng = 8.2275, lat = 46.8182, zoom = 8) %>%
          addPolygons(
            fillColor = ~ifelse(is.na(get(input$variable)), 
                              "#808080",
                              pal(factor(get(input$variable), levels = c(5, 7, 9)))),
            weight = 1,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            highlight = highlightOptions(
              weight = 2,
              color = "#666",
              dashArray = "",
              fillOpacity = 0.7,
              bringToFront = TRUE
            ),
            popup = ~paste(
              "<strong>", name, "</strong><br>",
              "Regierungssitze: ", 
              ifelse(is.na(get(input$variable)), 
                     "Keine Daten", 
                     round(get(input$variable), 0))
            ),
            label = if(input$show_labels) ~name else NULL,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "12px",
              direction = "auto"
            )
          ) %>%
          addLegend(
            pal = pal,
            values = discrete_values,
            opacity = 0.7,
            title = paste("Regierungssitze", input$year),
            position = "bottomright",
            na.label = "Keine Daten"
          )
      } else {
        # For Parlamentssitze, use continuous scale
        pal <- colorNumeric(
          palette = "Blues",
          domain = valid_values,
          na.color = "#808080"
        )
        
        # Create the map with continuous values
        leaflet(map_data()) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          setView(lng = 8.2275, lat = 46.8182, zoom = 8) %>%
          addPolygons(
            fillColor = ~ifelse(is.na(get(input$variable)), 
                              "#808080",
                              pal(get(input$variable))),
            weight = 1,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            highlight = highlightOptions(
              weight = 2,
              color = "#666",
              dashArray = "",
              fillOpacity = 0.7,
              bringToFront = TRUE
            ),
            popup = ~paste(
              "<strong>", name, "</strong><br>",
              "Parlamentssitze: ", 
              ifelse(is.na(get(input$variable)), 
                     "Keine Daten", 
                     round(get(input$variable), 0))
            ),
            label = if(input$show_labels) ~name else NULL,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "12px",
              direction = "auto"
            )
          ) %>%
          addLegend(
            pal = pal,
            values = ~get(input$variable),
            opacity = 0.7,
            title = paste("Parlamentssitze", input$year),
            position = "bottomright",
            na.label = "Keine Daten"
          )
      }
    })
  })
} 