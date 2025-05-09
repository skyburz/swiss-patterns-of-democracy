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

# Source the canton reference functions
source("R/canton_reference.R")

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
            # Category selection dropdown
            selectInput(ns("category"), "Kategorie", 
                      choices = c(
                        "Demokratische Institutionen" = "institutions",
                        "Direkte Demokratie" = "direct_democracy",
                        "Parlamentswahlen" = "parliament",
                        "Regierungswahlen" = "government",
                        "Gemeinden" = "municipalities"
                      ), 
                      selected = "institutions"),
            
            # Variable selection dropdown (will be updated based on category)
            selectInput(ns("variable"), "Variable", choices = NULL),
            
            # Year selector
            sliderInput(ns("year"), "Jahr", min = 1979, max = 2023, value = 2023, step = 1, 
                        ticks = TRUE, sep = "", animate = TRUE),
            
            # Option to show canton labels
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
    
    # Variable mappings for different categories
    variable_mappings <- reactiveVal(list(
      # Democratic Institutions variables - only confirmed variables
      institutions = c(
        "Regierungssitze" = "sitzreg",
        "Parlamentssitze" = "sitzparl",
        "Proporz im Parlament" = "proporz",
        "Amtsdauer des Parlaments" = "parlegisl",
        "Disproportionalität des Wahlsystems" = "gallagher",
        "Proportionalität des Wahlsystems bei Parlamentswahlen" = "proporz4",
        "Proportionalität der Wahlsysteme bei Parlaments- und Regierungswahlen" = "proporz3reg",
        "Wahlverfahren bei Regierungsratswahlen (Dummy)" = "reg_proporz"
      ),
      
      # Direct Democracy variables
      direct_democracy = c(
        "Direktdemokratische Rechte (Index)" = "ddr_stutz",
        "Gesetzesinitiativrecht (Index)" = "gir",
        "Verfassungsinitiativrecht (Index)" = "vir", 
        "Gesetzesreferendumsrecht (Index)" = "grr",
        "Finanzreferendumsrecht (Index)" = "frr",
        "Jährliche Anzahl Abstimmungen" = "abst_total",
        "Stimmbeteiligung bei kantonalen Volksabstimmungen" = "turnout_v"
      ),
      
      # Parliament election variables
      parliament = c(
        "Parlamentssitze" = "sitzparl",
        "Wahlbeteiligung Parlament" = "turnout_p",
        "Effektive Parteienzahl im Parlament" = "eff_part_parl",
        "Volatilität im Parlament" = "volat_parl"
      ),
      
      # Government election variables
      government = c(
        "Regierungssitze" = "sitzreg",
        "Wahlbeteiligung Regierung" = "turnout_r",
        "Effektive Parteienzahl in der Regierung" = "eff_part_reg",
        "Volatilität in der Regierung" = "volat_reg"
      ),
      
      # Municipalities variables
      municipalities = c(
        "Ausmass kantonaler Vorgaben zu obligatorischen Referenden" = "ddr_gde_obl",
        "Ausmass kantonaler Vorgaben zu fakultativen Referenden" = "ddr_gde_fak",
        "Ausmass kantonaler Vorgaben zu kommunalen Initiativen" = "ddr_gde_init",
        "Policy-Dezentralisierung" = "dez_policy",
        "Polity-Dezentralisierung" = "dez_polity",
        "Politics-Dezentralisierung" = "dez_politics"
      )
    ))
    
    # Update variable selection based on chosen category
    observe({
      req(input$category)
      variables <- variable_mappings()[[input$category]]
      
      updateSelectInput(
        session,
        "variable",
        choices = variables,
        selected = variables[1]
      )
    })
    
    # Load Swiss map data
    swiss_map <- reactive({
      tryCatch({
        # Read the TopoJSON file and convert to SF object
        cantons <- topojson_read("data/swiss-maps.json", "cantons")
        
        # Create a custom mapping function for the special cases in the map data
        map_canton_name <- function(name) {
          # Create a mapping for the special multilingual canton names
          special_cases <- list(
            "Bern / Berne" = "BE",
            "Fribourg / Freiburg" = "FR",
            "Graubünden / Grigioni / Grischun" = "GR",
            "Valais / Wallis" = "VS"
          )
          
          # Check if the name is in our special cases
          if (name %in% names(special_cases)) {
            return(special_cases[[name]])
          }
          
          # If not a special case, use standardize_canton from canton_reference.R
          abbr <- standardize_canton(name)
          if (is.na(abbr)) {
            warning(paste("Could not find abbreviation for canton:", name))
            return(NA)
          }
          return(abbr)
        }
        
        # Apply our custom mapping function
        cantons$kanton_abbr <- sapply(cantons$name, map_canton_name)
        
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
      
      # Check if variable exists in the dataset
      if(!input$variable %in% names(data())) {
        print(paste("Variable", input$variable, "not found in dataset"))
        # Return empty map data
        return(swiss_map() %>% mutate(!!sym(input$variable) := NA))
      }
      
      # Special debug for gallagher variable
      if(input$variable == "gallagher") {
        print("GALLAGHER DEBUG:")
        print(paste("Class of gallagher:", class(data()$gallagher)))
        print(paste("First few values:", paste(head(data()$gallagher), collapse=", ")))
        print(paste("Number of NA values:", sum(is.na(data()$gallagher))))
        print(paste("Number of dots:", sum(data()$gallagher == ".", na.rm=TRUE)))
        
        # Try to convert to numeric and check results
        gallagher_numeric <- suppressWarnings(as.numeric(data()$gallagher))
        print(paste("After numeric conversion - NAs:", sum(is.na(gallagher_numeric))))
        print(paste("First few numeric values:", paste(head(na.omit(gallagher_numeric)), collapse=", ")))
        
        # Create a clean version of the data
        data_clean <- data()
        data_clean$gallagher <- ifelse(data_clean$gallagher == ".", NA, data_clean$gallagher)
        data_clean$gallagher <- suppressWarnings(as.numeric(data_clean$gallagher))
      }
      
      # Filter data for selected year
      year_data <- data() %>%
        filter(jahr == input$year)
      
      # For gallagher, use cleaned numeric version
      if(input$variable == "gallagher") {
        year_data$gallagher <- ifelse(year_data$gallagher == ".", NA, year_data$gallagher)
        year_data$gallagher <- suppressWarnings(as.numeric(year_data$gallagher))
        print("After cleaning for selected year:")
        print(paste("Number of rows:", nrow(year_data)))
        print(paste("Number of NA values:", sum(is.na(year_data$gallagher))))
        if(sum(!is.na(year_data$gallagher)) > 0) {
          print(paste("Available values:", paste(na.omit(year_data$gallagher), collapse=", ")))
        } else {
          print("No available values after cleaning")
        }
      }
      
      # Check if we have data for this year
      if(nrow(year_data) == 0) {
        print(paste("No data for year", input$year))
        return(swiss_map() %>% mutate(!!sym(input$variable) := NA))
      }
      
      # Select kanton and variable column
      if(input$variable %in% names(year_data)) {
        year_data <- year_data %>% select(kanton, !!sym(input$variable))
      } else {
        print(paste("Variable", input$variable, "not found in filtered data"))
        return(swiss_map() %>% mutate(!!sym(input$variable) := NA))
      }
      
      # Print some debug info
      print(paste("Cantons with data:", paste(unique(year_data$kanton), collapse=", ")))
      if(input$variable == "gallagher") {
        print("Final gallagher values:")
        if(nrow(year_data) > 0) {
          for(i in 1:nrow(year_data)) {
            print(paste(year_data$kanton[i], ":", year_data[[input$variable]][i]))
          }
        }
      } else {
        print(paste("Variable values:", paste(head(year_data[[input$variable]]), collapse=", ")))
      }
      
      # Join with map data using abbreviations
      map_with_data <- swiss_map() %>%
        left_join(year_data, by = c("kanton_abbr" = "kanton"))
      
      return(map_with_data)
    })
    
    # Get the variable label based on the variable code
    get_var_label <- reactive({
      req(input$category, input$variable)
      
      # Get the variable mapping for the current category
      category_vars <- variable_mappings()[[input$category]]
      
      # Find the label for the selected variable
      var_label <- names(category_vars)[category_vars == input$variable]
      
      # Return the label or fallback to the variable code
      if(length(var_label) > 0) return(var_label) else return(input$variable)
    })
    
    # Create the map title
    output$map_title <- renderUI({
      req(input$variable, get_var_label())
      
      # Get the category label
      category_labels <- c(
        "institutions" = "Demokratische Institutionen",
        "direct_democracy" = "Direkte Demokratie",
        "parliament" = "Parlamentswahlen",
        "government" = "Regierungswahlen",
        "municipalities" = "Gemeinden"
      )
      
      category_label <- category_labels[input$category]
      var_label <- get_var_label()
      
      title_text <- paste0("Kantonskarte - ", category_label, " - ", var_label)
      title_text
    })
    
    # Create the map
    output$map <- renderLeaflet({
      req(map_data(), input$variable)
      
      # Get the values for the selected variable, excluding NA
      values <- map_data()[[input$variable]]
      
      # For gallagher variable, ensure it's numeric
      if(input$variable == "gallagher") {
        values <- suppressWarnings(as.numeric(values))
      }
      
      valid_values <- values[!is.na(values)]
      
      # Print debug info about valid values
      print(paste("Number of valid values:", length(valid_values)))
      if(length(valid_values) > 0) {
        print(paste("Sample values:", paste(head(valid_values), collapse=", ")))
      }
      
      # Skip rendering if no valid values
      if(length(valid_values) == 0) {
        return(leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          setView(lng = 8.2275, lat = 46.8182, zoom = 8) %>%
          addControl(
            html = "<div style='background-color: white; padding: 10px;'>Keine Daten für diese Variable verfügbar</div>",
            position = "topright"
          )
        )
      }
      
      # Determine if the variable is discrete or continuous
      is_discrete <- FALSE
      
      # Check if the variable is numeric
      is_numeric <- is.numeric(valid_values)
      
      # Discrete handling for specific variables
      if(input$variable == "sitzreg") {
        is_discrete <- TRUE
        discrete_values <- c(5, 7, 9)
      } else if(input$variable %in% c("ddr_gde_obl", "ddr_gde_fak", "ddr_gde_init")) {
        is_discrete <- TRUE
        discrete_values <- sort(unique(valid_values))
      }
      
      # Get the variable label for display
      var_label <- get_var_label()
      
      # Create the map
      if(is_discrete) {
        # Create a discrete color palette
        pal <- colorFactor(
          palette = viridis(length(discrete_values), option = "D"),
          domain = discrete_values,
          na.color = "#808080"
        )
        
        # Create a custom legend HTML
        legend_html <- paste0(
          "<div class='info legend'>",
          "<div style='margin-bottom: 5px;'><strong>", paste(var_label, input$year), "</strong></div>"
        )
        
        # Add color boxes and labels for each value
        for (i in 1:length(discrete_values)) {
          color <- viridis(length(discrete_values), option = "D")[i]
          legend_html <- paste0(
            legend_html,
            "<div style='display: flex; align-items: center; margin-bottom: 3px;'>",
            "<i style='background:", color, "; width: 18px; height: 18px; display: inline-block; margin-right: 5px;'></i>",
            "<span>", discrete_values[i], "</span>",
            "</div>"
          )
        }
        
        # Add the "Keine Daten" item with extra spacing
        legend_html <- paste0(
          legend_html,
          "<div style='display: flex; align-items: center; margin-top: 10px;'>",
          "<i style='background: #808080; width: 18px; height: 18px; display: inline-block; margin-right: 15px;'></i>",
          "<span style='margin-left: 10px;'>Keine Daten</span>",
          "</div>",
          "</div>"
        )
        
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
              var_label, ": ", 
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
          addControl(
            html = legend_html,
            position = "bottomright",
            className = "custom-legend"
          )
      } else {
        # For continuous variables
        pal <- colorNumeric(
          palette = viridis(10, option = "D"),
          domain = valid_values,
          na.color = "#808080"
        )
        
        # Special handling for percentages (turnout variables)
        decimal_places <- if(grepl("turnout|beteiligung", tolower(var_label))) 1 else 2
        
        # For gallagher variable, ensure numeric before rendering
        if(input$variable == "gallagher") {
          map_data_fixed <- map_data()
          map_data_fixed[[input$variable]] <- suppressWarnings(as.numeric(map_data_fixed[[input$variable]]))
          
          # Create gradient legend colors
          colors <- viridis(8, option = "D")
          
          # Find min/max values for legend
          min_val <- min(valid_values, na.rm = TRUE)
          max_val <- max(valid_values, na.rm = TRUE)
          
          # Create a custom legend HTML with gradient for continuous values
          legend_html <- paste0(
            "<div class='info legend'>",
            "<div style='margin-bottom: 5px;'><strong>", paste(var_label, input$year), "</strong></div>",
            "<div style='display: flex; align-items: center; margin-bottom: 10px;'>",
            "<div style='background: linear-gradient(to right, ", paste(colors, collapse = ", "), "); width: 150px; height: 20px;'></div>",
            "</div>",
            "<div style='display: flex; justify-content: space-between; width: 150px;'>",
            "<span>", round(min_val, decimal_places), "</span>",
            "<span>", round(max_val, decimal_places), "</span>",
            "</div>",
            "<div style='display: flex; align-items: center; margin-top: 15px;'>",
            "<i style='background: #808080; width: 18px; height: 18px; display: inline-block; margin-right: 15px;'></i>",
            "<span style='margin-left: 40px;'>Keine Daten</span>",
            "</div>",
            "</div>"
          )
          
          leaflet(map_data_fixed) %>%
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
                var_label, ": ", 
                ifelse(is.na(get(input$variable)), 
                      "Keine Daten", 
                      round(get(input$variable), decimal_places))
              ),
              label = if(input$show_labels) ~name else NULL,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "12px",
                direction = "auto"
              )
            ) %>%
            addControl(
              html = legend_html,
              position = "bottomright",
              className = "custom-legend"
            )
        } else {
          # Create gradient legend colors
          colors <- viridis(8, option = "D")
          
          # Find min/max values for legend
          min_val <- min(valid_values, na.rm = TRUE)
          max_val <- max(valid_values, na.rm = TRUE)
          
          # Create a custom legend HTML with gradient for continuous values
          legend_html <- paste0(
            "<div class='info legend'>",
            "<div style='margin-bottom: 5px;'><strong>", paste(var_label, input$year), "</strong></div>",
            "<div style='display: flex; align-items: center; margin-bottom: 10px;'>",
            "<div style='background: linear-gradient(to right, ", paste(colors, collapse = ", "), "); width: 150px; height: 20px;'></div>",
            "</div>",
            "<div style='display: flex; justify-content: space-between; width: 150px;'>",
            "<span>", round(min_val, decimal_places), "</span>",
            "<span>", round(max_val, decimal_places), "</span>",
            "</div>",
            "<div style='display: flex; align-items: center; margin-top: 15px;'>",
            "<i style='background: #808080; width: 18px; height: 18px; display: inline-block; margin-right: 15px;'></i>",
            "<span style='margin-left: 40px;'>Keine Daten</span>",
            "</div>",
            "</div>"
          )
          
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
                var_label, ": ", 
                ifelse(is.na(get(input$variable)), 
                      "Keine Daten", 
                      round(get(input$variable), decimal_places))
              ),
              label = if(input$show_labels) ~name else NULL,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "12px",
                direction = "auto"
              )
            ) %>%
            addControl(
              html = legend_html,
              position = "bottomright",
              className = "custom-legend"
            )
        }
      }
    })
  })
} 