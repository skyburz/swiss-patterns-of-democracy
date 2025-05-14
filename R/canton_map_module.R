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
        "Proportionalitätsgrad der Wahlsysteme bei Parlaments- und Regierungsratswahlen" = "proporz3reg",
        "Wahlverfahren bei Regierungsratswahlen (Dummy)" = "reg_proporz"
      ),
      
      # Direct Democracy variables
      direct_democracy = c(
        "Direktdemokratische Rechte (Index)" = "ddr_stutz",
        "Gesetzesinitiativrecht (Index)" = "gir",
        "Verfassungsinitiativrecht (Index)" = "vir", 
        "Gesetzesreferendumsrecht (Index)" = "grr",
        "Finanzreferendumsrecht (Index)" = "frr",
        "Sub-National Direct Democracy Index" = "ddr_snddi",
        "Jährliche Anzahl Abstimmungen" = "abst_total",
        "Jährliche Anzahl Abstimmungen über Volksinitiativen" = "init_total",
        "Jährliche Anzahl Abstimmungen über Referenden" = "ref_total",
        "Jährliche Anzahl Abstimmungen über obligatorische Referenden" = "ref_obl",
        "Jährliche Anzahl Abstimmungen über fakultative Referenden" = "ref_fak",
        "Jährliche Anzahl Volksinitiativen" = "volksinit",
        "Jährliche Anzahl Gegenentwürfe zu Initiativen" = "gegenvor_init",
        "Behörden- und Gemeindeinitiativen" = "sonstige_init",
        "Jährliche Anzahl obligatorische Finanzreferenden" = "obl_finref",
        "Jährliche Anzahl fakultative Finanzreferenden" = "fak_finref",
        "Stimmbeteiligung bei kantonalen Volksabstimmungen" = "turnout_v"
      ),
      
      # Parliament election variables
      parliament = c(
        "Parlamentssitze" = "sitzparl",
        "Wahlbeteiligung Parlamentswahlen" = "turnout_e",
        "Amtsdauer des Parlaments in Jahren" = "parlegisl",
        "Parlamentswahl im Jahr" = "parl_election",
        "Rae-Index der Parteienfraktionalisierung" = "rae",
        "Parteifraktionalisierung: Effektive Parteienzahl" = "partfrakt",
        "Anzahl Parlamentsparteien" = "parl_party",
        "Anzahl der Sitze der stärksten Partei" = "max_sitzzahl_parl",
        "Linkes Lager" = "parl_left",
        "Mitte-Lager" = "parl_cent",
        "Rechtes Lager" = "parl_right",
        "Katholisches Lager" = "parl_kath",
        "Grünes Lager" = "parl_gruene",
        "100 Prozent minus Wähleranteil stärkste Partei" = "wett_parl_se",
        "Unterschied grösste–zweitgrösste Partei" = "wett_parl2_vo",
        "Anzahl stärkster/bestplatzierter Parteien" = "n_bestplatzierte_sitzzahl_parl",
        "Parl. Volatilität: Nettoveränderung der Sitzanteile" = "volatilitaet_se_election",
        "Parl. Volatilität: Nettoveränderung (standardisiert)" = "volatilitaet_se_year",
        "Wählervolatilität: Nettoveränderung der Wähleranteile" = "volatilitaet_vo",
        "Effektive Disproportionalität des Wahlsystems" = "gallagher"
      ),
      
      # Government election variables
      government = c(
        "Regierungssitze" = "sitzreg",
        "Regierungsratswahlen im entsprechenden Jahr" = "reg_election",
        "Summierte Wähleranteile der Regierungsparteien" = "reg_konk",
        "Konkordanz" = "konk_2",
        "Linkes Lager" = "reg_left",
        "Mitte-Lager" = "reg_cent",
        "Rechtes Lager" = "reg_right",
        "Katholisches Lager" = "reg_kath",
        "Grünes Lager" = "reg_gruene",
        "Parteipolitische Spannweite der Regierungskoalition" = "spann",
        "100 Prozent minus Sitzanteil stärkste Partei" = "wett_reg_se",
        "Unterschied grösste–zweitgrösste Partei (Reg.) in Prozent aller Sitze" = "wett_reg2_se",
        "Anzahl der Sitze der stärksten Regierungspartei" = "max_sitzzahl_reg",
        "Anzahl stärkster/bestplatzierter Regierungsparteien" = "n_bestplatzierte_sitzzahl_reg",
        "Grösse der typischen Regierungspartei" = "g",
        "Grösse der typischen Oppositionspartei" = "o",
        "Index of Effective Opposition" = "ieo",
        "Index of Competitiveness" = "balance"
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
    
    # Helper function to process year data and join with Swiss map
    process_year_data <- function(year_data, swiss_map, variable_name) {
      print(paste("Processing year data for variable:", variable_name))
      
      # Check if we have data
      if(nrow(year_data) == 0) {
        print("No data available for the selected year")
        return(swiss_map %>% mutate(!!sym(variable_name) := NA))
      }
      
      # Check if variable exists in the data
      if(!(variable_name %in% names(year_data))) {
        print(paste("Variable", variable_name, "not found in year data"))
        return(swiss_map %>% mutate(!!sym(variable_name) := NA))
      }
      
      # Select kanton and variable column
      year_data <- year_data %>% select(kanton, !!sym(variable_name))
      
      # Print debug info
      print(paste("Number of rows in year data:", nrow(year_data)))
      print(paste("Number of NAs in variable:", sum(is.na(year_data[[variable_name]]))))
      print(paste("Number of valid values:", sum(!is.na(year_data[[variable_name]]))))
      
      # Print a sample of the data
      print("Sample of year data:")
      print(head(year_data))
      
      # Join with map data using abbreviations
      map_with_data <- swiss_map %>%
        left_join(year_data, by = c("kanton_abbr" = "kanton"))
      
      # Print info about the joined data
      print(paste("Final map data created with", nrow(map_with_data), "features"))
      print(paste("Number of NAs in final map:", sum(is.na(map_with_data[[variable_name]]))))
      
      return(map_with_data)
    }
    
    # Update variable selection based on chosen category
    observe({
      req(input$category)
      variables <- variable_mappings()[[input$category]]
      
      # Get default selected variable
      default_selected <- variables[1]
      
      # For direct democracy indices, default to 2018 (the year they have most complete data)
      if(input$category == "direct_democracy") {
        dd_indices <- c("ddr_stutz", "gir", "vir", "grr", "frr", "ddr_snddi")
        if(default_selected %in% dd_indices) {
          updateSliderInput(
            session,
            "year",
            value = 2018
          )
        }
      }
      
      updateSelectInput(
        session,
        "variable",
        choices = variables,
        selected = default_selected
      )
    })
    
    # Observer for when the variable changes to a direct democracy index
    observeEvent(input$variable, {
      # Special year handling for direct democracy indices
      dd_indices <- c("ddr_stutz", "gir", "vir", "grr", "frr", "ddr_snddi")
      if(input$variable %in% dd_indices) {
        updateSliderInput(
          session,
          "year",
          value = 2018
        )
      }
      
      # Special year handling for municipality variables - set to 2019 only
      if(input$variable %in% c("ddr_gde_obl", "ddr_gde_fak", "ddr_gde_init")) {
        updateSliderInput(
          session,
          "year",
          value = 2019
        )
      }
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
      
      # Print debug info at the start
      print(paste("Preparing map data for variable:", input$variable))
      print(paste("Year selected:", input$year))
      
      # List of direct democracy indices that need special handling
      dd_indices <- c("ddr_stutz", "gir", "vir", "grr", "frr", "ddr_snddi")
      
      # Create a clean version of the data with proper type conversion
      data_clean <- data()
      
      # Special handling for municipality variables
      if(input$variable %in% c("ddr_gde_obl", "ddr_gde_fak", "ddr_gde_init", "dez_policy", "dez_polity", "dez_politics")) {
        print(paste("Special handling for", input$variable, "variable"))
        
        # Print debug info about dataset
        print(paste("Number of rows in original data:", nrow(data_clean)))
        print(paste("Column exists in data:", input$variable %in% names(data_clean)))
        
        # For these specific municipality variables, only show data for 2019
        if(input$variable %in% c("ddr_gde_obl", "ddr_gde_fak", "ddr_gde_init")) {
          # If year is not 2019, return empty data
          if(input$year != 2019) {
            print("Selected year is not 2019, showing no data for municipality variables")
            return(swiss_map() %>% mutate(!!sym(input$variable) := NA))
          }
          
          # Continue with 2019 data
          print("Using 2019 data for municipality variables")
          
          # Filter to 2019 data
          year_data <- data_clean %>%
            filter(jahr == 2019)
          
          # Check if there is data for 2019
          if(sum(!is.na(year_data[[input$variable]])) == 0) {
            print("No data for 2019, showing empty map")
            return(swiss_map() %>% mutate(!!sym(input$variable) := NA))
          }
          
          # Convert "." to NA and then to numeric
          year_data[[input$variable]] <- ifelse(year_data[[input$variable]] == "." | 
                                           year_data[[input$variable]] == "", 
                                           NA, 
                                           year_data[[input$variable]])
          
          # Convert to numeric
          year_data[[input$variable]] <- suppressWarnings(as.numeric(as.character(year_data[[input$variable]])))
          
          # Debug info about the 2019 data
          print(paste("Number of rows for 2019:", nrow(year_data)))
          print(paste("Number of non-NA values for 2019:", sum(!is.na(year_data[[input$variable]]))))
          
          # Select kanton and variable column
          year_data <- year_data %>% select(kanton, !!sym(input$variable))
          
          # Join with map data
          map_with_data <- swiss_map() %>%
            left_join(year_data, by = c("kanton_abbr" = "kanton"))
          
          # Final debug info
          print(paste("Final map data created with", nrow(map_with_data), "features"))
          
          return(map_with_data)
        }
        
        # For other municipality variables (policy, polity, politics), do normal processing
        # Convert "." to NA and then to numeric
        data_clean[[input$variable]] <- ifelse(data_clean[[input$variable]] == "." | 
                                             data_clean[[input$variable]] == "", 
                                             NA, 
                                             data_clean[[input$variable]])
        
        # Convert to numeric
        data_clean[[input$variable]] <- suppressWarnings(as.numeric(as.character(data_clean[[input$variable]])))
      }
      
      # Special handling for parl_election variable
      if(input$variable == "parl_election") {
        print("Special handling for parl_election variable")
        
        # Find the parl_election column (could be named parl_election...97 or similar)
        parl_election_col <- names(data_clean)[grep("parl_election", names(data_clean))]
        
        if(length(parl_election_col) > 0) {
          print(paste("Found parl_election column:", parl_election_col[1]))
          
          # Create a clean parl_election column
          data_clean$parl_election <- ifelse(data_clean[[parl_election_col[1]]] == "." | 
                                           data_clean[[parl_election_col[1]]] == "", 
                                           NA, 
                                           data_clean[[parl_election_col[1]]])
          
          # Convert to numeric
          data_clean$parl_election <- suppressWarnings(as.numeric(as.character(data_clean$parl_election)))
          
          # Print debug info
          print(paste("Number of rows in data:", nrow(data_clean)))
          non_na_values <- data_clean$parl_election[!is.na(data_clean$parl_election)]
          print(paste("Number of non-NA values:", length(non_na_values)))
          if(length(non_na_values) > 0) {
            print(paste("Sample values:", paste(head(non_na_values), collapse=", ")))
          }
        } else {
          print("Could not find parl_election column in the data")
          return(swiss_map() %>% mutate(parl_election = NA))
        }
      }
      
      # Special handling for reg_election variable
      if(input$variable == "reg_election") {
        print("Special handling for reg_election variable")
        
        # Find the reg_election column
        reg_election_col <- names(data_clean)[grep("reg_election", names(data_clean))]
        
        if(length(reg_election_col) > 0) {
          print(paste("Found reg_election column:", reg_election_col[1]))
          
          # Create a clean reg_election column
          data_clean$reg_election <- ifelse(data_clean[[reg_election_col[1]]] == "." | 
                                          data_clean[[reg_election_col[1]]] == "", 
                                          NA, 
                                          data_clean[[reg_election_col[1]]])
          
          # Convert to numeric
          data_clean$reg_election <- suppressWarnings(as.numeric(as.character(data_clean$reg_election)))
        } else {
          print("Could not find reg_election column in the data")
          return(swiss_map() %>% mutate(reg_election = NA))
        }
      }
      
      # Check if variable exists in the dataset
      if(!input$variable %in% names(data_clean)) {
        print(paste("Variable", input$variable, "not found in dataset"))
        # Return empty map data
        return(swiss_map() %>% mutate(!!sym(input$variable) := NA))
      }
      
      # List of variables that need special numeric handling
      numeric_vars <- c(
        # Direct democracy variables
        "gallagher", "ddr_stutz", "gir", "vir", "grr", "frr", "ddr_snddi", 
        "turnout_v", "ref_obl", "ref_fak", "volksinit", "gegenvor_init", 
        "sonstige_init", "obl_finref", "fak_finref",
        
        # Parliamentary election variables
        "turnout_e", "parlegisl", "parl_election", "rae", "partfrakt", "parl_party",
        "max_sitzzahl_parl", "parl_left", "parl_cent", "parl_right", "parl_kath", 
        "parl_gruene", "wett_parl_se", "wett_parl2_vo", "n_bestplatzierte_sitzzahl_parl",
        "volatilitaet_se_election", "volatilitaet_se_year", "volatilitaet_vo",
        
        # Government election variables
        "reg_election", "reg_konk", "konk_2", "reg_left", "reg_cent", 
        "reg_right", "reg_kath", "reg_gruene", "spann", "wett_reg_se", "wett_reg2_se", 
        "max_sitzzahl_reg", "n_bestplatzierte_sitzzahl_reg", "g", "o", "ieo", "balance",
        
        # Municipality variables
        "ddr_gde_obl", "ddr_gde_fak", "ddr_gde_init", "dez_policy", "dez_polity", "dez_politics"
      )
      
      # Clean all numeric variables
      for (var in numeric_vars) {
        if (var %in% names(data_clean) && var != "parl_election" && var != "reg_election") {
          # Already handled parl_election and reg_election above
          # Convert "." to NA and then to numeric
          data_clean[[var]] <- ifelse(data_clean[[var]] == "." | data_clean[[var]] == "", 
                                     NA, 
                                     data_clean[[var]])
          data_clean[[var]] <- suppressWarnings(as.numeric(as.character(data_clean[[var]])))
        }
      }
      
      # For direct democracy indices, use 2018 data if we're close to 2018 and there's no data for the current year
      if (input$variable %in% dd_indices) {
        year_data_count <- sum(!is.na(data_clean[[input$variable]][data_clean$jahr == input$year]))
        if (year_data_count == 0 && abs(input$year - 2018) <= 5) {
          print(paste("No data for", input$variable, "in year", input$year, "- using 2018 data instead"))
          year_data <- data_clean %>%
            filter(jahr == 2018)
        } else {
          # Use the regular selected year
          year_data <- data_clean %>%
            filter(jahr == input$year)
        }
      } else {
        # Filter data for selected year as normal
        year_data <- data_clean %>%
          filter(jahr == input$year)
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
      
      # Print debug info about the data
      print(paste("Number of rows in filtered data:", nrow(year_data)))
      print(paste("Number of NAs in variable:", sum(is.na(year_data[[input$variable]]))))
      print(paste("Number of valid values:", sum(!is.na(year_data[[input$variable]]))))
      
      # Join with map data using abbreviations
      map_with_data <- swiss_map() %>%
        left_join(year_data, by = c("kanton_abbr" = "kanton"))
      
      # Final debug info
      print(paste("Final map data created with", nrow(map_with_data), "features"))
      
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
      discrete_values <- NULL
      
      # Define variables that should use discrete legends with specific values
      if(input$variable == "sitzreg") {
        is_discrete <- TRUE
        discrete_values <- c(5, 7, 9)
      } else if(input$variable == "proporz") {
        # Proporz im Parlament: discrete values 1, 2, 3
        is_discrete <- TRUE
        discrete_values <- c(1, 2, 3)
      } else if(input$variable == "parlegisl") {
        # Amtsdauer des Parlaments in Jahren: discrete values 4, 5
        is_discrete <- TRUE
        discrete_values <- c(4, 5)
      } else if(input$variable == "proporz4") {
        # Proportionalität des Wahlsystems bei Parlamentswahlen: discrete values 1, 2, 3, 4
        is_discrete <- TRUE
        discrete_values <- c(1, 2, 3, 4)
      } else if(input$variable == "proporz3reg") {
        # Proportionalitätsgrad der Wahlsysteme bei Parlaments- und Regierungsratswahlen: discrete values 1, 2, 3, 4, 5, 6
        is_discrete <- TRUE
        discrete_values <- c(1, 2, 3, 4, 5, 6)
      } else if(input$variable == "reg_proporz") {
        # Wahlverfahren bei Regierungsratswahlen (Dummy): discrete values 0, 1
        is_discrete <- TRUE
        discrete_values <- c(0, 1)
      } else if(input$variable == "gegenvor_init") {
        # Discrete values 0-4
        is_discrete <- TRUE
        discrete_values <- c(0, 1, 2, 3, 4)
      } else if(input$variable == "sonstige_init") {
        # Discrete values 0-1
        is_discrete <- TRUE
        discrete_values <- c(0, 1)
      } else if(input$variable == "parl_election" || input$variable == "reg_election") {
        # Parliamentary/Government election in year: discrete values 0, 1
        is_discrete <- TRUE
        discrete_values <- c(0, 1)
      } else if(input$variable == "parl_party" || input$variable == "n_bestplatzierte_sitzzahl_parl" || 
                input$variable == "n_bestplatzierte_sitzzahl_reg") {
        # Number of parliamentary parties: likely <= 6 distinct integer values
        # Check if we have enough distinct values in the data for a discrete scale
        distinct_values <- sort(unique(valid_values))
        if(length(distinct_values) <= 6 && all(distinct_values == round(distinct_values))) {
          is_discrete <- TRUE
          discrete_values <- distinct_values
        } else {
          is_discrete <- FALSE
        }
      } else if(input$variable == "max_sitzzahl_parl" || input$variable == "max_sitzzahl_reg") {
        # Number of seats of strongest party: typically small integers
        distinct_values <- sort(unique(valid_values))
        if(length(distinct_values) <= 6 && all(distinct_values == round(distinct_values))) {
          is_discrete <- TRUE
          discrete_values <- distinct_values
        } else {
          is_discrete <- FALSE
        }
      } else if(input$variable == "ddr_gde_obl" || input$variable == "ddr_gde_fak" || 
                input$variable == "ddr_gde_init") {
        # Municipality direct democracy variables: Check if they have <= 6 distinct integer values
        distinct_values <- sort(unique(valid_values))
        if(length(distinct_values) <= 6 && all(distinct_values == round(distinct_values))) {
          is_discrete <- TRUE
          discrete_values <- distinct_values
        } else {
          is_discrete <- FALSE
        }
      } else if(input$variable == "dez_policy" || input$variable == "dez_polity" || 
                input$variable == "dez_politics") {
        # Municipality decentralization variables: Check if they have <= 6 distinct integer values
        distinct_values <- sort(unique(valid_values))
        if(length(distinct_values) <= 6 && all(distinct_values == round(distinct_values))) {
          is_discrete <- TRUE
          discrete_values <- distinct_values
        } else {
          is_discrete <- FALSE
        }
      } else {
        # Check if the variable is numeric for other variables
        is_numeric <- is.numeric(valid_values)
        is_discrete <- !is_numeric
        if(is_discrete) {
          discrete_values <- sort(unique(valid_values))
        }
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
          
          # Create description based on variable and value
          description <- discrete_values[i]
          
          # Add descriptions for specific government variables
          if (input$variable == "reg_proporz") {
            if (discrete_values[i] == 0) description <- "0: Majorz"
            if (discrete_values[i] == 1) description <- "1: Proporz"
          } else if (input$variable == "max_sitzzahl_reg") {
            description <- paste0(discrete_values[i], ": ", discrete_values[i], " Sitze")
          } else if (input$variable == "n_bestplatzierte_sitzzahl_reg") {
            if (discrete_values[i] == 1) description <- "1: Eine dominante Partei"
            else description <- paste0(discrete_values[i], ": ", discrete_values[i], " Parteien mit gleicher Stärke")
          } else if (input$variable == "spann") {
            # Parteipolitische Spannweite descriptions
            if (discrete_values[i] == 1) description <- "1: Gering"
            if (discrete_values[i] == 2) description <- "2: Mittel"
            if (discrete_values[i] == 3) description <- "3: Hoch"
          } else if (input$variable == "wett_reg_se") {
            # 100% minus Sitzanteil descriptions based on percentile ranges
            description <- paste0(discrete_values[i], "%: Restwählerpotential")
          } else if (input$variable == "wett_reg2_se") {
            # Unterschied zwischen größter und zweitgrößter Regierungspartei
            description <- paste0(discrete_values[i], "%: Unterschied")
          } else if (input$variable == "g") {
            # Grösse der typischen Regierungspartei
            if (discrete_values[i] < 1) description <- paste0(discrete_values[i], ": Klein")
            else if (discrete_values[i] < 2) description <- paste0(discrete_values[i], ": Mittel")
            else description <- paste0(discrete_values[i], ": Gross")
          } else if (input$variable == "o") {
            # Grösse der typischen Oppositionspartei
            if (discrete_values[i] < 1) description <- paste0(discrete_values[i], ": Klein")
            else if (discrete_values[i] < 2) description <- paste0(discrete_values[i], ": Mittel")
            else description <- paste0(discrete_values[i], ": Gross")
          } else if (input$variable == "ieo") {
            # Index of Effective Opposition
            if (discrete_values[i] < 0.2) description <- paste0(discrete_values[i], ": Sehr schwach")
            else if (discrete_values[i] < 0.4) description <- paste0(discrete_values[i], ": Schwach")
            else if (discrete_values[i] < 0.6) description <- paste0(discrete_values[i], ": Mittel")
            else if (discrete_values[i] < 0.8) description <- paste0(discrete_values[i], ": Stark")
            else description <- paste0(discrete_values[i], ": Sehr stark")
          } else if (input$variable == "balance") {
            # Index of Competitiveness
            if (discrete_values[i] < 0.2) description <- paste0(discrete_values[i], ": Sehr unausgeglichen")
            else if (discrete_values[i] < 0.4) description <- paste0(discrete_values[i], ": Unausgeglichen")
            else if (discrete_values[i] < 0.6) description <- paste0(discrete_values[i], ": Mittel")
            else if (discrete_values[i] < 0.8) description <- paste0(discrete_values[i], ": Ausgeglichen")
            else description <- paste0(discrete_values[i], ": Sehr ausgeglichen")
          } else if (input$variable == "parl_election") {
            if (discrete_values[i] == 0) description <- "0: Keine Wahl"
            if (discrete_values[i] == 1) description <- "1: Parlamentswahl"
          } else if (input$variable == "reg_election") {
            if (discrete_values[i] == 0) description <- "0: Keine Wahl"
            if (discrete_values[i] == 1) description <- "1: Regierungswahl"
          }
          
          legend_html <- paste0(
            legend_html,
            "<div style='display: flex; align-items: center; margin-bottom: 3px;'>",
            "<i style='background:", color, "; width: 18px; height: 18px; display: inline-block; margin-right: 5px;'></i>",
            "<span>", description, "</span>",
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