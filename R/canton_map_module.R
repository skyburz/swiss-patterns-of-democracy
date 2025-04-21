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

#' UI function for canton map module
#' @param id The module ID
#' @return A Shiny UI element
canton_map_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3,
        card(
          card_header("Steuerung"),
          card_body(
            selectInput(ns("variable"), "Variable", choices = NULL),
            selectInput(ns("year"), "Jahr", choices = NULL),
            checkboxInput(ns("show_labels"), "Kantonsnamen anzeigen", value = FALSE)
          )
        )
      ),
      column(9,
        card(
          card_header("Kantonskarte"),
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
        
        # Ensure canton names are standardized
        cantons$name <- toupper(cantons$name)
        
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
    
    # Update UI choices based on data
    observe({
      req(data)
      
      # Update variable choices - only numeric columns
      numeric_cols <- names(data)[sapply(data, is.numeric)]
      numeric_cols <- setdiff(numeric_cols, "jahr")  # Exclude year column
      
      updateSelectInput(session, "variable",
        choices = setNames(numeric_cols, numeric_cols),
        selected = numeric_cols[1]
      )
      
      # Update year choices
      years <- sort(unique(data$jahr))
      updateSelectInput(session, "year",
        choices = years,
        selected = max(years)
      )
    })
    
    # Prepare data for mapping
    map_data <- reactive({
      req(input$variable, input$year, swiss_map(), data)
      
      # Filter data for selected year and standardize canton names
      year_data <- data %>%
        filter(jahr == input$year) %>%
        mutate(kanton = toupper(kanton)) %>%
        select(kanton, !!sym(input$variable))
      
      # Join with map data
      map_with_data <- swiss_map() %>%
        left_join(year_data, by = c("name" = "kanton"))
      
      # Check for unmatched cantons
      unmatched <- setdiff(year_data$kanton, map_with_data$name)
      if (length(unmatched) > 0) {
        warning("Some cantons could not be matched: ", paste(unmatched, collapse = ", "))
      }
      
      return(map_with_data)
    })
    
    # Create the map
    output$map <- renderLeaflet({
      req(map_data())
      
      # Get the values for the selected variable, excluding NA
      values <- map_data()[[input$variable]]
      valid_values <- values[!is.na(values)]
      
      # Only create color palette if we have valid values
      if (length(valid_values) > 0) {
        pal <- colorNumeric(
          palette = "viridis",
          domain = valid_values,
          na.color = "#808080"  # Grey for missing values
        )
      } else {
        # If no valid values, use a default palette
        pal <- colorFactor(
          palette = "viridis",
          domain = "No Data",
          na.color = "#808080"
        )
      }
      
      # Create the map
      leaflet(map_data()) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%  # Use a nicer base map
        setView(lng = 8.2275, lat = 46.8182, zoom = 8) %>%  # Center on Switzerland
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
            input$variable, ": ", 
            ifelse(is.na(get(input$variable)), 
                   "Keine Daten", 
                   round(get(input$variable), 2))
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
          title = input$variable,
          position = "bottomright",
          na.label = "Keine Daten"
        )
    })
  })
} 