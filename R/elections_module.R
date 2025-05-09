# Elections Module
# This module creates visualizations for election data

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(tidyr)

# Source the canton reference
source("R/canton_reference.R")

#' Prepare election data in long format
#' @param data The input dataset
#' @param year The selected year
#' @param variable_type Either "parlament_sitze", "parlament_sitze_anteil", "parlament_votes", "parl_election", "regierung_sitze", "parlegisl", "turnout_e", "rae", "partfrakt", "parl_party", "max_sitzzahl_parl", "reg_election", "reg_konk", "konk_2", "reg_left", "reg_cent", "reg_right", "reg_kath", "reg_gruene", "parl_left", "parl_cent", "parl_right", "parl_kath", "parl_gruene", "wett_parl_se", "wett_parl2_vo", "n_bestplatzierte_sitzzahl_parl", "volatilitaet_se_election", "volatilitaet_se_year", "volatilitaet_vo", or "gallagher"
#' @return A dataframe in long format with election data
prepare_election_data <- function(data, year, variable_type = "parlament_sitze") {
  # Get column names
  year_col <- names(data)[grep("year|jahr", names(data), ignore.case = TRUE)[1]]
  
  # Get party columns based on variable type
  if(variable_type == "parl_election") {
    # For parliamentary elections, clean the data first
    election_data <- data %>%
      filter(!!sym(year_col) == year) %>%
      select(kantonnr, `parl_election...97`) %>%
      mutate(
        canton_abbrev = sapply(kantonnr, get_canton_abbr),
        canton_name = sapply(canton_abbrev, function(abbr) get_canton_name(abbr, "de")),
        # First convert "." to NA
        parl_election = ifelse(`parl_election...97` == ".", NA, `parl_election...97`),
        # Then convert to numeric (0 or 1)
        parl_election = as.numeric(parl_election),
        # Use the cleaned value
        parl_sitze_partei = parl_election
      ) %>%
      filter(!is.na(parl_election))  # Remove any NA values
    return(election_data)
  }
  
  # For government elections (reg_election)
  if(variable_type == "reg_election") {
    # For government elections, clean the data first
    election_data <- data %>%
      filter(!!sym(year_col) == year) %>%
      select(kantonnr, reg_election) %>%
      mutate(
        canton_abbrev = sapply(kantonnr, get_canton_abbr),
        canton_name = sapply(canton_abbrev, function(abbr) get_canton_name(abbr, "de")),
        # First convert "." to NA
        reg_election_cleaned = ifelse(reg_election == ".", NA, reg_election),
        # Then convert to numeric (0 or 1)
        reg_election_cleaned = as.numeric(reg_election_cleaned),
        # Use the cleaned value
        parl_sitze_partei = reg_election_cleaned
      ) %>%
      filter(!is.na(reg_election_cleaned))  # Remove any NA values
    return(election_data)
  }
  
  # For parliamentary term length (parlegisl)
  if(variable_type == "parlegisl") {
    term_data <- data %>%
      filter(!!sym(year_col) == year) %>%
      select(kantonnr, parlegisl) %>%
      mutate(
        canton_abbrev = sapply(kantonnr, get_canton_abbr),
        canton_name = sapply(canton_abbrev, function(abbr) get_canton_name(abbr, "de")),
        # Convert "." to NA
        parlegisl = ifelse(parlegisl == ".", NA, parlegisl),
        # Convert to numeric
        parlegisl = as.numeric(parlegisl),
        # For compatibility with existing code
        parl_sitze_partei = parlegisl
      ) %>%
      filter(!is.na(parlegisl))  # Remove any NA values
    return(term_data)
  }
  
  # For election turnout (turnout_e)
  if(variable_type == "turnout_e") {
    turnout_data <- data %>%
      filter(!!sym(year_col) == year) %>%
      select(kantonnr, turnout_e) %>%
      mutate(
        canton_abbrev = sapply(kantonnr, get_canton_abbr),
        canton_name = sapply(canton_abbrev, function(abbr) get_canton_name(abbr, "de")),
        # Convert "." to NA
        turnout_e = ifelse(turnout_e == ".", NA, turnout_e),
        # Convert to numeric
        turnout_e = as.numeric(turnout_e),
        # For compatibility with existing code
        parl_sitze_partei = turnout_e
      ) %>%
      filter(!is.na(turnout_e))  # Remove any NA values
    return(turnout_data)
  }
  
  # For additional party system measures (rae, partfrakt, parl_party, max_sitzzahl_parl)
  if(variable_type %in% c("rae", "partfrakt", "parl_party", "max_sitzzahl_parl")) {
    party_data <- data %>%
      filter(!!sym(year_col) == year) %>%
      select(kantonnr, !!sym(variable_type)) %>%
      mutate(
        canton_abbrev = sapply(kantonnr, get_canton_abbr),
        canton_name = sapply(canton_abbrev, function(abbr) get_canton_name(abbr, "de")),
        # Convert "." to NA
        value = ifelse(!!sym(variable_type) == ".", NA, !!sym(variable_type)),
        # Convert to numeric
        value = as.numeric(value),
        # For compatibility with existing code
        parl_sitze_partei = value
      ) %>%
      filter(!is.na(value))  # Remove any NA values
    return(party_data)
  }
  
  # For additional government variables (reg_konk, konk_2, reg_left, reg_cent, reg_right, reg_kath, reg_gruene)
  if(variable_type %in% c("reg_konk", "konk_2", "reg_left", "reg_cent", "reg_right", "reg_kath", "reg_gruene",
                          "parl_left", "parl_cent", "parl_right", "parl_kath", "parl_gruene",
                          "wett_parl_se", "wett_parl2_vo", "n_bestplatzierte_sitzzahl_parl", 
                          "volatilitaet_se_election", "volatilitaet_se_year", "volatilitaet_vo", "gallagher")) {
    # Special handling for n_bestplatzierte_sitzzahl_parl
    if(variable_type == "n_bestplatzierte_sitzzahl_parl") {
      gov_data <- data %>%
        filter(!!sym(year_col) == year) %>%
        select(kantonnr, n_bestplatzierte_sitzzahl_parl) %>%
        mutate(
          canton_abbrev = sapply(kantonnr, get_canton_abbr),
          canton_name = sapply(canton_abbrev, function(abbr) get_canton_name(abbr, "de")),
          # Convert "." to NA
          value = ifelse(n_bestplatzierte_sitzzahl_parl == ".", NA, n_bestplatzierte_sitzzahl_parl),
          # Convert to numeric and round to integer - this should be a count of parties
          value = round(as.numeric(value)),
          # For compatibility with existing code
          parl_sitze_partei = value
        ) %>%
        filter(!is.na(value))  # Remove any NA values
      return(gov_data)
    }
    # Special handling for gallagher
    else if(variable_type == "gallagher") {
      gov_data <- data %>%
        filter(!!sym(year_col) == year) %>%
        select(kantonnr, gallagher) %>%
        mutate(
          canton_abbrev = sapply(kantonnr, get_canton_abbr),
          canton_name = sapply(canton_abbrev, function(abbr) get_canton_name(abbr, "de")),
          # Convert "." to NA
          value = ifelse(gallagher == ".", NA, gallagher),
          # Convert to numeric, need precision for this index
          value = as.numeric(value),
          # For compatibility with existing code
          parl_sitze_partei = value
        ) %>%
        filter(!is.na(value))  # Remove any NA values
      return(gov_data)
    }
    # Standard handling for other variables
    else {
      gov_data <- data %>%
        filter(!!sym(year_col) == year) %>%
        select(kantonnr, !!sym(variable_type)) %>%
        mutate(
          canton_abbrev = sapply(kantonnr, get_canton_abbr),
          canton_name = sapply(canton_abbrev, function(abbr) get_canton_name(abbr, "de")),
          # Convert "." to NA
          value = ifelse(!!sym(variable_type) == ".", NA, !!sym(variable_type)),
          # Convert to numeric
          value = as.numeric(value),
          # For compatibility with existing code
          parl_sitze_partei = value
        ) %>%
        filter(!is.na(value))  # Remove any NA values
      return(gov_data)
    }
  }
  
  party_cols <- if(variable_type %in% c("parlament_sitze", "parlament_sitze_anteil", "parlament_votes")) {
    # Use columns ending with _parl_s for parliamentary seats and _parl_v for votes
    if(variable_type == "parlament_votes") {
      names(data)[grep("_parl_v$", names(data))]
    } else {
      names(data)[grep("_parl_s$", names(data))]
    }
  } else if(variable_type == "regierung_sitze") {
    # For government seats, use columns ending with _reg_s
    names(data)[grep("_reg_s$", names(data))]
  } else {
    # For other government-related variables
    names(data)[grep("_reg_[sv]$|_reg_sa$", names(data))]
  }
  
  # Debug print
  print("Data preparation steps:")
  print(paste("Year column:", year_col))
  print(paste("Number of party columns:", length(party_cols)))
  print("Party columns:")
  print(party_cols)
  
  # Transform data
  election_data <- data %>%
    # Filter year
    filter(!!sym(year_col) == year) %>%
    # Select relevant columns including kantonnr
    select(kantonnr, all_of(party_cols)) %>%
    # Replace "." with NA and then convert to numeric
    mutate(across(all_of(party_cols), ~ifelse(. == ".", NA, .))) %>%
    mutate(across(all_of(party_cols), ~as.numeric(.))) %>%
    # Convert to long format
    pivot_longer(
      cols = all_of(party_cols),
      names_to = "partei",
      values_to = "parl_sitze_partei"
    ) %>%
    # Clean party names and add canton information
    mutate(
      partei = toupper(gsub("_.*$", "", partei)),  # Extract party abbreviation from start
      partei = ifelse(partei == "PARLOS", "Parteilos", partei),  # Convert PARLOS to Parteilos
      partei = ifelse(partei == "UEBRIG", "Übrige", partei),  # Convert UEBRIG to Übrige
      canton_abbrev = sapply(kantonnr, get_canton_abbr),
      canton_name = sapply(canton_abbrev, function(abbr) get_canton_name(abbr, "de"))
    )
  
  # Calculate percentages if variable_type is parlament_sitze_anteil
  if (variable_type == "parlament_sitze_anteil") {
    election_data <- election_data %>%
      group_by(kantonnr) %>%
      mutate(
        total_sitze = sum(parl_sitze_partei, na.rm = TRUE),
        # Keep precise percentages for calculations
        parl_sitze_partei = 100 * parl_sitze_partei / total_sitze
      ) %>%
      ungroup()
  }
  
  # Debug print for Bern
  print("Data for Bern:")
  print(election_data %>% filter(kantonnr == 2))
  
  return(election_data)
}

#' UI function for elections module
#' @param id The module ID
#' @return A Shiny UI element
elections_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      style = "display: flex; min-height: 500px;",  # Reduced from 550px
      column(
        width = 3,
        class = "equal-height",
        card(
          card_header("Filteroptionen"),
          card_body(
            class = "filter-panel",  # Added class for height control
            div(
              class = "filters-container",
              div(
                class = "filter-options",
                # 0. Election Type selector (new addition)
                selectInput(
                  ns("election_type"),
                  "Wahltyp:",
                  choices = c(
                    "Parlamentswahlen" = "parliament", 
                    "Regierungswahlen" = "government"
                  ),
                  selected = "parliament"
                ),
                
                # 1. Election Variables selector (will be updated based on election type)
                selectInput(
                  ns("variables"),
                  "Wahlvariablen:",
                  choices = NULL,
                  selected = "parlament_sitze"
                ),
                
                # 2. Time selectors - conditionally shown based on active tab
                # Year range slider for Zeittrend tab
                conditionalPanel(
                  condition = paste0("input['", ns("main_tabs"), "'] == 'Zeittrend'"),
                  sliderInput(
                    ns("time_range"),
                    "Zeitperiode auswählen:",
                    min = 1979, 
                    max = 2023, 
                    value = c(1979, 2023),  # Default to full range
                    step = 1,
                    sep = "",
                    animate = TRUE
                  )
                ),
                
                # Single year slider for Kantonsvergleich tab
                conditionalPanel(
                  condition = paste0("input['", ns("main_tabs"), "'] == 'Kantonsvergleich' || input['", ns("main_tabs"), "'] == 'Institutionsdetails'"),
                  sliderInput(
                    ns("selected_year"),
                    "Jahr auswählen:",
                    min = 1979, 
                    max = 2023, 
                    value = 2023,  # Default to most recent year
                    step = 1,
                    sep = "",
                    animate = TRUE
                  )
                ),
                
                # 3. Third filter: Canton selection with exclusive "All Cantons" option
                div(
                  class = "canton-selection-container",
                  tags$label("Kanton(e) auswählen:"),
                  # Radio buttons for selection mode (All vs. Individual)
                  radioButtons(
                    ns("canton_mode"),
                    label = NULL,
                    choices = list(
                      "Alle Kantone" = "all",
                      "Auswahl von Kantonen" = "select"
                    ),
                    selected = "all"  # Default to all cantons
                  ),
                  # Conditional panel for showing individual canton selections
                  conditionalPanel(
                    condition = paste0("input['", ns("canton_mode"), "'] == 'select'"),
                    div(
                      id = ns("cantons_group_container"),
                      class = "canton-checkbox-group",
                      checkboxGroupInput(
                        ns("canton_select"),
                        label = "Kantone:",
                        choices = NULL
                      )
                    )
                  )
                )
              ),
              # Apply filters button at the bottom
              div(
                class = "filter-button",
                actionButton(
                  ns("apply_filters"),
                  "Filter anwenden",
                  class = "btn-primary w-100"
                )
              )
            )
          )
        )
      ),
      column(
        width = 9,
        class = "equal-height",
        card(
          card_header("Überblick über Wahlen"),
          card_body(
            tabsetPanel(
              id = ns("main_tabs"),  # Add ID to the tabsetPanel for observing tab changes
              tabPanel(
                "Zeittrend",
                div(
                  class = "plot-container",
                  plotlyOutput(ns("time_trend_plot"), height = "500px")  # Reduced height
                )
              ),
              tabPanel(
                "Kantonsvergleich",
                div(
                  class = "plot-container",
                  plotlyOutput(ns("canton_comparison_plot"), height = "500px")  # Reduced height
                )
              ),
              tabPanel(
                "Institutionsdetails",
                plotlyOutput(ns("institution_details_plot"), height = "400px") 
              )
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        card(
          card_header("Analyse"),
          card_body(
            tabsetPanel(
              tabPanel(
                "Datentabelle",
                DTOutput(ns("data_table"))
              ),
              tabPanel(
                "Korrelationskarte",
                plotlyOutput(ns("correlation_plot"), height = "600px")
              ),
              tabPanel(
                "Statistische Zusammenfassung",
                verbatimTextOutput(ns("statistical_summary"))
              )
            )
          )
        )
      )
    )
  )
}

#' Server function for elections module
#'
#' @param id The module ID
#' @param selected_data Reactive expression that returns the selected dataset
#' @return A server function
elections_server <- function(id, selected_data) {
  moduleServer(id, function(input, output, session) {
    
    # Define party colors at the top level of the server function
    party_colors <- c(
      # Main parties from first image
      "SP" = "#E85D63",      # Red (H3 S68 B94)
      "SVP" = "#365436",     # Dark green (H110 S55 B54)
      "FDP" = "#4646B4",     # Blue (H212 S70 B70)
      "CVP" = "#D66E1D",     # Orange/Brown (H32 S80 B84)
      "MITTE" = "#D66E1D",   # Same as CVP
      "GP" = "#5EA55E",      # Green (H87 S61 B71)
      "GLP" = "#C4C446",     # Yellow-green (H60 S69 B77)
      "EVP" = "#DEBA3D",     # Yellow (H43 S82 B87)
      "PDA" = "#CC5959",     # Red (H0 S70 B80)
      "EDU" = "#A65A42",     # Brown (H17 S60 B65)
      "CSP" = "#4DB3B3",     # Turquoise (H182 S70 B70)
      "MCR" = "#4798E8",     # Light blue (H205 S68 B91)
      "LEGA" = "#D459D4",    # Purple (H259 S70 B83)
      "BDP" = "#E6D632",     # Yellow (H51 S86 B90)
      "MCG" = "#707070",     # Dark grey for MCG in Geneva
      
      # Additional parties from second image
      "HPS" = "#E0A088",     # Coral (H19 S66 B88)
      "PNOS" = "#AB8F5C",    # Brown (H28 S64 B67)
      "PPS" = "#E8C168",     # Light orange (H38 S58 B91)
      "CSPO" = "#E3D373",    # Light yellow (H47 S84 B89)
      "TPS" = "#919157",     # Olive green (H83 S50 B57)
      "ECOPOP" = "#917157",  # Dark green (H136 S40 B57)
      "LDU" = "#55AB8F",     # Turquoise (H155 S84 B67)
      "GEM" = "#54D1D1",     # Light turquoise (H175 S62 B82)
      "IP" = "#47B3B3",      # Turquoise (H192 S80 B70)
      "VB" = "#3F9ECC",      # Blue (H209 S82 B62)
      "EAG" = "#AD5E8E",     # Purple (H323 S54 B68)
      "RCV" = "#C75F78",     # Red (H348 S70 B78)
      "KJ" = "#9B3C3C",      # Dark red (H353 S72 B61)
      "PC" = "#9B3C3C",      # Dark red (H353 S72 B61)
      "FPS" = "#878787",     # Grey (B53)
      "SD" = "#9E9E9E",      # Grey (B62)
      
      # Greyscale for any remaining parties
      "LPS" = "#808080",     # Grey
      "DSP" = "#909090",     # Light grey
      "DN" = "#707070",      # Dark grey
      "FGA" = "#A0A0A0",     # Medium grey
      "PSA" = "#B0B0B0",     # Light medium grey
      "PPN" = "#C0C0C0",     # Very light grey
      "PRR" = "#606060",     # Very dark grey
      "Parteilos" = "#D0D0D0",  # Almost white grey
      "POCH" = "#505050",    # Darker grey
      "SOL" = "#404040",     # Very dark grey
      "REP" = "#858585",     # Medium dark grey
      "CSPOW" = "#656565",   # Dark medium grey
      "Übrige" = "#555555"   # Dark grey
    )
    
    # Reactive values for storing filtered data and year range
    filtered_data <- reactiveVal(NULL)
    min_year <- reactiveVal(NULL)
    max_year <- reactiveVal(NULL)
    
    # Initialize the module when data is selected
    observe({
      req(selected_data())
      data <- selected_data()
      
      # Find potential election columns
      election_cols <- names(data)[grep("_parl_[sv]$|_parl_sa$|_reg_[sv]$|_reg_sa$", names(data), ignore.case = TRUE)]
      
      # Remove any NULL values and make sure the list is unique
      election_cols <- unique(election_cols[!is.null(election_cols)])
      
      # Create variables for parliamentary elections (default view)
      parl_choices <- c(
        "Anzahl Parlamentssitze der Parteien" = "parlament_sitze",
        "Anteil Parlamentssitze der Parteien" = "parlament_sitze_anteil",
        "Wähleranteil der Parteien (%)" = "parlament_votes",
        "Parlamentswahl im Jahr" = "parl_election",
        "Amtsdauer des Parlaments in Jahren" = "parlegisl",
        "Wahlbeteiligung Parlamentswahlen" = "turnout_e",
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
        "Parl. Volatilität: Nettoveränderung der Sitzanteile der Parteien" = "volatilitaet_se_election",
        "Parl. Volatilität: Nettoveränderung der Sitzanteile der Parteien (standardisiert)" = "volatilitaet_se_year",
        "Wählervolatilität: Nettoveränderung der prozentuellen Wähleranteile der Parteien" = "volatilitaet_vo",
        "Effektive Disproportionalität des Wahlsystems" = "gallagher"
      )
      
      # Create variables for government elections
      gov_choices <- c(
        "Anzahl Regierungssitze der Parteien" = "regierung_sitze",
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
        # Add more government-related variables if available
      )
      
      # Initialize with parliamentary choices
      updateSelectInput(
        session,
        "variables",
        choices = parl_choices,
        selected = "parlament_sitze"  # Set default selection
      )
      
      # Get unique canton numbers from the data
      canton_numbers <- sort(unique(data$kantonnr))
      
      # Create choices using the canton reference system
      choices <- setNames(
        canton_numbers,
        sapply(canton_numbers, function(id) {
          get_canton_name(get_canton_abbr(id), "de")
        })
      )
      
      # Update the canton checkbox group
      updateCheckboxGroupInput(
        session, 
        "canton_select", 
        choices = choices,
        selected = choices[1:3]  # Pre-select first 3 cantons
      )
      
      # Find year column and update sliders
      year_col <- names(data)[grep("year|jahr", names(data), ignore.case = TRUE)[1]]
      if (!is.na(year_col)) {
        years <- sort(unique(data[[year_col]]))
        
        # Store min and max years for later use
        min_year(min(years))
        max_year(max(years))
        
        # Update the time range slider for Zeittrend tab
        updateSliderInput(session, "time_range", 
                        min = min(years), 
                        max = max(years),
                        value = c(min(years), max(years)))  # Default to full range
        
        # Update the single year slider for Kantonsvergleich tab
        updateSliderInput(session, "selected_year", 
                        min = min(years), 
                        max = max(years),
                        value = max(years))  # Default to most recent year
      }
      
      # Initialize filtered_data with all data
      filtered_data(data)
    })
    
    # React to changes in election type dropdown
    observeEvent(input$election_type, {
      if(input$election_type == "parliament") {
        # Parliamentary election variables
        choices <- c(
          "Anzahl Parlamentssitze der Parteien" = "parlament_sitze",
          "Anteil Parlamentssitze der Parteien" = "parlament_sitze_anteil",
          "Wähleranteil der Parteien (%)" = "parlament_votes",
          "Parlamentswahl im Jahr" = "parl_election",
          "Amtsdauer des Parlaments in Jahren" = "parlegisl",
          "Wahlbeteiligung Parlamentswahlen" = "turnout_e",
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
          "Parl. Volatilität: Nettoveränderung der Sitzanteile der Parteien" = "volatilitaet_se_election",
          "Parl. Volatilität: Nettoveränderung der Sitzanteile der Parteien (standardisiert)" = "volatilitaet_se_year",
          "Wählervolatilität: Nettoveränderung der prozentuellen Wähleranteile der Parteien" = "volatilitaet_vo",
          "Effektive Disproportionalität des Wahlsystems" = "gallagher"
        )
        
        updateSelectInput(
          session,
          "variables",
          choices = choices,
          selected = "parlament_sitze"
        )
      } else {
        # Government election variables
        choices <- c(
          "Anzahl Regierungssitze der Parteien" = "regierung_sitze",
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
          # Add more government-related variables if available in the future
        )
        
        updateSelectInput(
          session,
          "variables",
          choices = choices,
          selected = "regierung_sitze"
        )
      }
    })
    
    # Update filtered data when inputs change
    update_filtered_data <- function() {
      data <- selected_data()
      req(data)
      
      # Find year column
      year_col <- names(data)[grep("year|jahr", names(data), ignore.case = TRUE)[1]]
      
      # Apply canton filter based on mode
      if (input$canton_mode == "select") {
        # Check if any cantons are selected
        if (is.null(input$canton_select) || length(input$canton_select) == 0) {
          # No cantons selected, use all cantons
          print("No cantons selected, using all cantons")
        } else {
          # Filter to only selected cantons using kantonnr
          print(paste("Filtering to cantons:", paste(input$canton_select, collapse = ", ")))
          data <- data %>% filter(kantonnr %in% as.numeric(input$canton_select))
          print(paste("After canton filtering, rows:", nrow(data)))
        }
      }
      
      # Apply time filter based on active tab
      if (input$main_tabs == "Zeittrend" && !is.null(input$time_range)) {
        data <- data %>% filter(
          !!sym(year_col) >= input$time_range[1] & 
          !!sym(year_col) <= input$time_range[2]
        )
      } else if (!is.null(input$selected_year) && 
                (input$main_tabs == "Kantonsvergleich" || input$main_tabs == "Institutionsdetails")) {
        data <- data %>% filter(!!sym(year_col) == input$selected_year)
      }
      
      # Store filtered data
      filtered_data(data)
    }
    
    # Initialize filtered data
    observe({
      req(selected_data())
      update_filtered_data()
    })
    
    # Apply filters button
    observeEvent(input$apply_filters, {
      update_filtered_data()
    })
    
    # Prepare election data in long format
    election_data <- reactive({
      req(filtered_data(), input$selected_year, input$variables)
      prepare_election_data(filtered_data(), input$selected_year, input$variables)
    })
    
    # Time trend plot
    output$time_trend_plot <- renderPlotly({
      req(filtered_data())
      
      data <- filtered_data()
      year_col <- names(data)[grep("year|jahr", names(data), ignore.case = TRUE)[1]]
      
      # Handle party system measures and other metrics
      if (input$variables %in% c("rae", "partfrakt", "parl_party", "max_sitzzahl_parl", "reg_konk", "konk_2", 
                                 "reg_left", "reg_cent", "reg_right", "reg_kath", "reg_gruene",
                                 "parl_left", "parl_cent", "parl_right", "parl_kath", "parl_gruene",
                                 "wett_parl_se", "wett_parl2_vo", "n_bestplatzierte_sitzzahl_parl",
                                 "volatilitaet_se_election", "volatilitaet_se_year", "volatilitaet_vo", "gallagher",
                                 "spann", "wett_reg_se", "wett_reg2_se", "max_sitzzahl_reg", "n_bestplatzierte_sitzzahl_reg",
                                 "g", "o", "ieo", "balance")) {
        
        # Get variable name and labels
        var_name <- input$variables
        
        # Define titles and labels based on variable
        if (var_name == "rae") {
          plot_title <- "Durchschnittlicher Rae-Index der Parteienfraktionalisierung über die Zeit"
          y_axis_title <- "Rae-Index"
          value_suffix <- ""
          color <- "#5EA55E"  # Green
        } else if (var_name == "partfrakt") {
          plot_title <- "Durchschnittliche effektive Parteienzahl über die Zeit"
          y_axis_title <- "Effektive Parteienzahl"
          value_suffix <- ""
          color <- "#4646B4"  # Blue
        } else if (var_name == "parl_party") {
          plot_title <- "Durchschnittliche Anzahl der Parlamentsparteien über die Zeit"
          y_axis_title <- "Anzahl Parteien"
          value_suffix <- ""
          color <- "#D66E1D"  # Orange
        } else if (var_name == "max_sitzzahl_parl") {
          plot_title <- "Durchschnittliche Anzahl der Sitze der stärksten Partei über die Zeit"
          y_axis_title <- "Anzahl Sitze"
          value_suffix <- ""
          color <- "#365436"  # Dark green
        } else if (var_name == "reg_konk") {
          plot_title <- "Durchschnittliche summierte Wähleranteile der Regierungsparteien über die Zeit"
          y_axis_title <- "Wähleranteile (%)"
          value_suffix <- "%"
          color <- "#C4C446"  # Yellow-green
        } else if (var_name == "konk_2") {
          plot_title <- "Durchschnittliche Konkordanz über die Zeit"
          y_axis_title <- "Konkordanz"
          value_suffix <- ""
          color <- "#AD5E8E"  # Purple
        } else if (var_name == "reg_left") {
          plot_title <- "Durchschnittliche Stärke des linken Lagers über die Zeit"
          y_axis_title <- "Stärke"
          value_suffix <- ""
          color <- "#E85D63"  # Red
        } else if (var_name == "reg_cent") {
          plot_title <- "Durchschnittliche Stärke des Mitte-Lagers über die Zeit"
          y_axis_title <- "Stärke"
          value_suffix <- ""
          color <- "#D66E1D"  # Orange
        } else if (var_name == "reg_right") {
          plot_title <- "Durchschnittliche Stärke des rechten Lagers über die Zeit"
          y_axis_title <- "Stärke"
          value_suffix <- ""
          color <- "#365436"  # Dark green
        } else if (var_name == "reg_kath") {
          plot_title <- "Durchschnittliche Stärke des katholischen Lagers über die Zeit"
          y_axis_title <- "Stärke"
          value_suffix <- ""
          color <- "#4646B4"  # Blue
        } else if (var_name == "reg_gruene") {
          plot_title <- "Durchschnittliche Stärke des grünen Lagers über die Zeit"
          y_axis_title <- "Stärke"
          value_suffix <- ""
          color <- "#5EA55E"  # Green
        } else if (var_name == "parl_left") {
          plot_title <- "Durchschnittliche Stärke des linken Lagers im Parlament über die Zeit"
          y_axis_title <- "Stärke"
          value_suffix <- ""
          color <- "#E85D63"  # Red
        } else if (var_name == "parl_cent") {
          plot_title <- "Durchschnittliche Stärke des Mitte-Lagers im Parlament über die Zeit"
          y_axis_title <- "Stärke"
          value_suffix <- ""
          color <- "#D66E1D"  # Orange
        } else if (var_name == "parl_right") {
          plot_title <- "Durchschnittliche Stärke des rechten Lagers im Parlament über die Zeit"
          y_axis_title <- "Stärke"
          value_suffix <- ""
          color <- "#365436"  # Dark green
        } else if (var_name == "parl_kath") {
          plot_title <- "Durchschnittliche Stärke des katholischen Lagers im Parlament über die Zeit"
          y_axis_title <- "Stärke"
          value_suffix <- ""
          color <- "#4646B4"  # Blue
        } else if (var_name == "parl_gruene") {
          plot_title <- "Durchschnittliche Stärke des grünen Lagers im Parlament über die Zeit"
          y_axis_title <- "Stärke"
          value_suffix <- ""
          color <- "#5EA55E"  # Green
        } else if (var_name == "wett_parl_se") {
          plot_title <- "Durchschnittliches Restwählerpotential über die Zeit"
          y_axis_title <- "100% - Wähleranteil stärkste Partei"
          value_suffix <- "%"
          color <- "#9B3C3C"  # Dark red
        } else if (var_name == "wett_parl2_vo") {
          plot_title <- "Durchschnittlicher Unterschied zwischen größter und zweitgrößter Partei über die Zeit"
          y_axis_title <- "Unterschied"
          value_suffix <- "%"
          color <- "#DEBA3D"  # Yellow
        } else if (var_name == "n_bestplatzierte_sitzzahl_parl") {
          plot_title <- "Durchschnittliche Anzahl der stärksten/bestplatzierten Parteien über die Zeit"
          y_axis_title <- "Anzahl Parteien"
          value_suffix <- ""
          color <- "#4DB3B3"  # Turquoise
        } else if (var_name == "volatilitaet_se_election") {
          plot_title <- "Durchschnittliche parlamentarische Volatilität (Sitzanteile) über die Zeit"
          y_axis_title <- "Volatilität"
          value_suffix <- ""
          color <- "#D459D4"  # Purple
        } else if (var_name == "volatilitaet_se_year") {
          plot_title <- "Durchschnittliche standardisierte parlamentarische Volatilität über die Zeit"
          y_axis_title <- "Volatilität (standardisiert)"
          value_suffix <- ""
          color <- "#878787"  # Grey
        } else if (var_name == "volatilitaet_vo") {
          plot_title <- "Durchschnittliche Wählervolatilität über die Zeit"
          y_axis_title <- "Wählervolatilität"
          value_suffix <- "%"
          color <- "#4798E8"  # Light blue
        } else if (var_name == "gallagher") {
          plot_title <- "Durchschnittliche effektive Disproportionalität des Wahlsystems über die Zeit"
          y_axis_title <- "Gallagher-Index"
          value_suffix <- ""
          color <- "#A65A42"  # Brown
        } else if (var_name == "spann") {
          plot_title <- "Durchschnittliche parteipolitische Spannweite der Regierungskoalition über die Zeit"
          y_axis_title <- "Spannweite"
          value_suffix <- ""
          color <- "#A65A42"  # Brown
        } else if (var_name == "wett_reg_se") {
          plot_title <- "Durchschnittliches Regierungsrestwählerpotential über die Zeit"
          y_axis_title <- "100% - Sitzanteil stärkste Partei"
          value_suffix <- "%"
          color <- "#9B3C3C"  # Dark red
        } else if (var_name == "wett_reg2_se") {
          plot_title <- "Durchschnittlicher Unterschied zwischen größter und zweitgrößter Regierungspartei über die Zeit"
          y_axis_title <- "Unterschied"
          value_suffix <- "%"
          color <- "#DEBA3D"  # Yellow
        } else if (var_name == "max_sitzzahl_reg") {
          plot_title <- "Durchschnittliche Anzahl der Sitze der stärksten Regierungspartei über die Zeit"
          y_axis_title <- "Anzahl Sitze"
          value_suffix <- ""
          color <- "#365436"  # Dark green
        } else if (var_name == "n_bestplatzierte_sitzzahl_reg") {
          plot_title <- "Durchschnittliche Anzahl der stärksten/bestplatzierten Regierungsparteien über die Zeit"
          y_axis_title <- "Anzahl Parteien"
          value_suffix <- ""
          color <- "#4DB3B3"  # Turquoise
        } else if (var_name == "g") {
          plot_title <- "Durchschnittliche Größe der typischen Regierungspartei über die Zeit"
          y_axis_title <- "Größe"
          value_suffix <- ""
          color <- "#458B74"  # Aquamarine
        } else if (var_name == "o") {
          plot_title <- "Durchschnittliche Größe der typischen Oppositionspartei über die Zeit"
          y_axis_title <- "Größe"
          value_suffix <- ""
          color <- "#CD5555"  # Indian red
        } else if (var_name == "ieo") {
          plot_title <- "Durchschnittlicher Index of Effective Opposition über die Zeit"
          y_axis_title <- "IEO"
          value_suffix <- ""
          color <- "#8B6508"  # Dark goldenrod
        } else if (var_name == "balance") {
          plot_title <- "Durchschnittlicher Index of Competitiveness über die Zeit"
          y_axis_title <- "Balance"
          value_suffix <- ""
          color <- "#9370DB"  # Medium purple
        }
        
        # Check if specific cantons are selected or using all cantons
        selected_cantons <- if(input$canton_mode == "select" && !is.null(input$canton_select) && length(input$canton_select) > 0) {
          as.numeric(input$canton_select)
        } else {
          NULL  # Use all cantons
        }
        
        # Define measurement-specific variables based on the selected metric
        if (var_name %in% c("wett_parl_se", "wett_parl2_vo", "max_sitzzahl_parl", "n_bestplatzierte_sitzzahl_parl", 
                           "volatilitaet_se_election", "volatilitaet_se_year", "volatilitaet_vo", "gallagher",
                           "spann", "wett_reg_se", "wett_reg2_se", "max_sitzzahl_reg", "n_bestplatzierte_sitzzahl_reg",
                           "g", "o", "ieo", "balance")) {
          
          # Special handling for the specified variables, showing individual canton values
          if (!is.null(selected_cantons)) {
            # Show individual canton values when specific cantons are selected
            canton_data <- data %>%
              # Convert "." to NA and then to numeric
              mutate(value = ifelse(!!sym(var_name) == ".", NA, !!sym(var_name)),
                     value = as.numeric(value)) %>%
              filter(!is.na(value), kantonnr %in% selected_cantons) %>%
              # Add canton information
              mutate(
                canton_abbrev = sapply(kantonnr, get_canton_abbr),
                canton_name = sapply(canton_abbrev, function(abbr) get_canton_name(abbr, "de"))
              )
            
            # Define canton colors - using a consistent scheme across the app
            canton_colors <- c(
              "ZH" = "#003087", "BE" = "#FF0000", "LU" = "#0066CC", "UR" = "#FFCC00", 
              "SZ" = "#FF0000", "OW" = "#FFFFFF", "NW" = "#FF0000", "GL" = "#FF0000", 
              "ZG" = "#0033CC", "FR" = "#000000", "SO" = "#FF0000", "BS" = "#000000", 
              "BL" = "#FF0000", "SH" = "#000000", "AR" = "#000000", "AI" = "#000000", 
              "SG" = "#009933", "GR" = "#999999", "AG" = "#000000", "TG" = "#009933", 
              "TI" = "#FF0000", "VD" = "#006600", "VS" = "#FF0000", "NE" = "#009900", 
              "GE" = "#CC0000", "JU" = "#FF0000"
            )
            
            # Create plot showing individual canton values over time
            p <- plot_ly() %>%
              layout(
                title = sub("Durchschnittliche", "Kantonsspezifische", plot_title),
                xaxis = list(
                  title = "Jahr",
                  tickmode = "linear",
                  dtick = 2
                ),
                yaxis = list(
                  title = y_axis_title,
                  rangemode = "tozero",
                  ticksuffix = value_suffix
                ),
                showlegend = TRUE,
                hovermode = "closest"
              )
            
            # Add a trace for each canton
            for (canton_nr in selected_cantons) {
              canton_data_filtered <- canton_data %>% filter(kantonnr == canton_nr)
              
              if (nrow(canton_data_filtered) > 0) {
                canton_abbrev <- canton_data_filtered$canton_abbrev[1]
                canton_name <- canton_data_filtered$canton_name[1]
                
                # Get color from canton_colors or use a default
                color <- if (!is.null(canton_colors[canton_abbrev])) {
                  canton_colors[canton_abbrev]
                } else {
                  "#808080"  # Default gray
                }
                
                # Add trace for this canton
                p <- p %>% add_trace(
                  data = canton_data_filtered,
                  x = ~get(year_col),
                  y = ~value,
                  name = canton_name,
                  type = "scatter",
                  mode = "lines+markers",
                  line = list(color = color, width = 2),
                  marker = list(color = color, size = 8),
                  hoverinfo = "text",
                  text = ~paste(canton_name, "<br>Jahr:", get(year_col), 
                               "<br>Wert:", round(value, 2), value_suffix)
                )
              }
            }
            
            return(p)
          } else {
            # For "all cantons" view, show the average across all cantons as before
          }
        }
        
        # Calculate average values per year (used for both "all cantons" view and non-special variables)
        party_data <- data %>%
          # Convert "." to NA and then to numeric
          mutate(value = ifelse(!!sym(var_name) == ".", NA, !!sym(var_name)),
                 value = as.numeric(value)) %>%
          filter(!is.na(value)) %>%
          group_by(!!sym(year_col)) %>%
          summarise(
            avg_value = mean(value, na.rm = TRUE),
            count = n(),
            .groups = 'drop'
          )
        
        # Convert to vectors for plotting
        years <- as.numeric(party_data[[year_col]])
        avg_values <- party_data$avg_value
        counts <- party_data$count
        
        # Determine appropriate decimal places
        decimal_places <- if(var_name %in% c("rae", "partfrakt")) 2 else 1
        
        # Create plot
        p <- plot_ly() %>%
          add_trace(
            x = years,
            y = avg_values,
            type = "scatter",
            mode = "lines+markers",
            line = list(color = color, width = 2),
            marker = list(color = color, size = 8),
            hoverinfo = "text",
            text = paste("Jahr:", years, 
                         "<br>Durchschnittlicher Wert:", round(avg_values, decimal_places), value_suffix,
                         "<br>Anzahl Kantone mit Daten:", counts)
          ) %>%
          layout(
            title = plot_title,
            xaxis = list(
              title = "Jahr",
              tickmode = "linear",
              dtick = 2
            ),
            yaxis = list(
              title = y_axis_title,
              rangemode = "tozero"
            ),
            showlegend = FALSE,
            hovermode = "closest"
          )
        
        return(p)
      }
      
      # Handle election turnout (turnout_e)
      if (input$variables == "turnout_e") {
        # Calculate average turnout per year
        turnout_data <- data %>%
          # Convert "." to NA and then to numeric
          mutate(turnout_e = ifelse(turnout_e == ".", NA, turnout_e),
                 turnout_e = as.numeric(turnout_e)) %>%
          filter(!is.na(turnout_e)) %>%
          group_by(!!sym(year_col)) %>%
          summarise(
            avg_turnout = mean(turnout_e, na.rm = TRUE),
            count = n(),
            .groups = 'drop'
          )
        
        # Convert to vectors for plotting
        years <- as.numeric(turnout_data[[year_col]])
        avg_turnout <- turnout_data$avg_turnout
        counts <- turnout_data$count
        
        # Create plot
        p <- plot_ly() %>%
          add_trace(
            x = years,
            y = avg_turnout,
            type = "scatter",
            mode = "lines+markers",
            line = list(color = "#E85D63", width = 2),  # Red color for turnout
            marker = list(color = "#E85D63", size = 8),
            hoverinfo = "text",
            text = paste("Jahr:", years, 
                         "<br>Durchschnittliche Wahlbeteiligung:", round(avg_turnout, 1), "%",
                         "<br>Anzahl Kantone mit Daten:", counts)
          ) %>%
          layout(
            title = "Durchschnittliche Wahlbeteiligung bei Parlamentswahlen über die Zeit",
            xaxis = list(
              title = "Jahr",
              tickmode = "linear",
              dtick = 2
            ),
            yaxis = list(
              title = "Wahlbeteiligung in %",
              rangemode = "tozero"
            ),
            showlegend = FALSE,
            hovermode = "closest"
          )
        
        return(p)
      }
      
      # Handle parliamentary term length (parlegisl)
      if (input$variables == "parlegisl") {
        # Calculate average term length per year
        term_data <- data %>%
          # Convert "." to NA and then to numeric
          mutate(parlegisl = ifelse(parlegisl == ".", NA, parlegisl),
                 parlegisl = as.numeric(parlegisl)) %>%
          filter(!is.na(parlegisl)) %>%
          group_by(!!sym(year_col)) %>%
          summarise(
            avg_term = mean(parlegisl, na.rm = TRUE),
            count = n(),
            .groups = 'drop'
          )
        
        # Convert to vectors for plotting
        years <- as.numeric(term_data[[year_col]])
        avg_terms <- term_data$avg_term
        counts <- term_data$count
        
        # Create plot
        p <- plot_ly() %>%
          add_trace(
            x = years,
            y = avg_terms,
            type = "scatter",
            mode = "lines+markers",
            line = list(color = "#4646B4", width = 2),
            marker = list(color = "#4646B4", size = 8),
            hoverinfo = "text",
            text = paste("Jahr:", years, 
                         "<br>Durchschnittliche Amtsdauer:", round(avg_terms, 1), "Jahre",
                         "<br>Anzahl Kantone mit Daten:", counts)
          ) %>%
          layout(
            title = "Durchschnittliche Amtsdauer des Parlaments über die Zeit",
            xaxis = list(
              title = "Jahr",
              tickmode = "linear",
              dtick = 2
            ),
            yaxis = list(
              title = "Amtsdauer in Jahren",
              rangemode = "tozero"
            ),
            showlegend = FALSE,
            hovermode = "closest"
          )
        
        return(p)
      }
      
      # Get party columns based on selected variable
      if (input$variables == "parl_election") {
        # For parliamentary elections, count the number of elections per year
        # Create a simpler approach to avoid complex expressions
        election_data <- data %>%
          # Fix: Use the correct column name parl_election...97
          mutate(parl_election = as.numeric(ifelse(`parl_election...97` == ".", NA, `parl_election...97`))) %>%
          filter(!is.na(parl_election)) %>%
          group_by(!!sym(year_col)) %>%
          summarise(
            election_count = sum(parl_election == 1, na.rm = TRUE)
          ) %>%
          ungroup()
        
        # Convert the year column to a simple numeric vector for plotting
        years <- as.numeric(election_data[[year_col]])
        counts <- election_data$election_count
        
        # Create plot with direct data values
        p <- plot_ly() %>%
          add_trace(
            x = years,
            y = counts,
            type = "scatter",
            mode = "lines+markers",
            line = list(color = "#4646B4", width = 2),
            marker = list(color = "#4646B4", size = 8),
          hoverinfo = "text",
            text = paste("Jahr:", years, "<br>Anzahl Wahlen:", counts)
        ) %>%
          layout(
            title = "Anzahl Parlamentswahlen pro Jahr",
            xaxis = list(
              title = "Jahr",
              tickmode = "linear",
              dtick = 2
            ),
            yaxis = list(
              title = "Anzahl Wahlen",
              rangemode = "tozero"
            ),
            showlegend = FALSE,
            hovermode = "closest"
          )
        
        return(p)
      }
      
      # Handle government elections (reg_election)
      if (input$variables == "reg_election") {
        # For government elections, count the number of elections per year
        election_data <- data %>%
          # Convert "." to NA and then to numeric
          mutate(reg_election_cleaned = as.numeric(ifelse(reg_election == ".", NA, reg_election))) %>%
          filter(!is.na(reg_election_cleaned)) %>%
          group_by(!!sym(year_col)) %>%
          summarise(
            election_count = sum(reg_election_cleaned == 1, na.rm = TRUE)
          ) %>%
          ungroup()
        
        # Convert the year column to a simple numeric vector for plotting
        years <- as.numeric(election_data[[year_col]])
        counts <- election_data$election_count
        
        # Create plot with direct data values
        p <- plot_ly() %>%
          add_trace(
            x = years,
            y = counts,
            type = "scatter",
            mode = "lines+markers",
            line = list(color = "#D66E1D", width = 2),  # Orange color for government elections
            marker = list(color = "#D66E1D", size = 8),
            hoverinfo = "text",
            text = paste("Jahr:", years, "<br>Anzahl Wahlen:", counts)
          ) %>%
          layout(
            title = "Anzahl Regierungsratswahlen pro Jahr",
            xaxis = list(
              title = "Jahr",
              tickmode = "linear",
              dtick = 2
            ),
            yaxis = list(
              title = "Anzahl Wahlen",
              rangemode = "tozero"
            ),
            showlegend = FALSE,
            hovermode = "closest"
          )
        
        return(p)
      }
      
      if (input$variables %in% c("parlament_sitze", "parlament_sitze_anteil", "parlament_votes", "regierung_sitze")) {
        if(input$variables == "parlament_votes") {
          party_cols <- names(data)[grep("_parl_v$", names(data))]
          title <- "Wähleranteil der Parteien (%)"
        } else if(input$variables == "regierung_sitze") {
          party_cols <- names(data)[grep("_reg_s$", names(data))]
          title <- "Anzahl Regierungssitze der Parteien"
        } else {
          party_cols <- names(data)[grep("_parl_s$", names(data))]
          title <- if(input$variables == "parlament_sitze") {
            "Anzahl Parlamentssitze der Parteien"
          } else {
            "Anteil Parlamentssitze der Parteien"
          }
        }
      } else {
        party_cols <- names(data)[grep("_reg_[sv]$|_reg_sa$", names(data))]
        title <- "Anzahl Regierungssitze der Parteien"
      }
      
      # Convert all party columns to numeric before creating long format
      data <- data %>%
        mutate(across(all_of(party_cols), ~ifelse(. == ".", NA, .))) %>%
        mutate(across(all_of(party_cols), as.numeric))
      
      # Create long format data
      long_data <- data %>%
        select(all_of(c(year_col, party_cols))) %>%
        pivot_longer(
          cols = all_of(party_cols),
          names_to = "partei",
          values_to = "parl_sitze_partei"
        ) %>%
        mutate(
          partei = toupper(gsub("_.*$", "", partei)),
          partei = ifelse(partei == "PARLOS", "Parteilos", partei),  # Convert PARLOS to Parteilos
          partei = ifelse(partei == "UEBRIG", "Übrige", partei),  # Convert UEBRIG to Übrige
          parl_sitze_partei = as.numeric(parl_sitze_partei)
        ) %>%
        filter(!is.na(parl_sitze_partei))
      
      # Calculate percentages if needed
      if (input$variables == "parlament_sitze_anteil") {
        long_data <- long_data %>%
          group_by(get(year_col)) %>%
          mutate(
            total_sitze = sum(parl_sitze_partei, na.rm = TRUE),
            # Keep precise percentages for calculations
            parl_sitze_partei = 100 * parl_sitze_partei / total_sitze
          ) %>%
          ungroup()
      }
      
      # Group by year and party, summing seats or averaging votes
      long_data <- long_data %>%
        group_by(get(year_col), partei) %>%
        summarise(
          parl_sitze_partei = if(input$variables == "parlament_votes") {
            mean(parl_sitze_partei, na.rm = TRUE)  # Average for vote shares
          } else {
            sum(parl_sitze_partei, na.rm = TRUE)   # Sum for seats
          }, 
          .groups = 'drop'
        )
      
      # Create plot based on variable type
      if (input$variables == "parlament_sitze_anteil") {
        # Create a copy of the data for display with rounded percentages
        display_data <- long_data %>%
          mutate(parl_sitze_partei = round(parl_sitze_partei, 1))
        
        # Stacked area chart for percentages
        p <- plot_ly(
          data = long_data,  # Use unrounded data for the actual plot
          x = ~`get(year_col)`,
          y = ~parl_sitze_partei,
          color = ~partei,
          type = "scatter",
          mode = "none",
          fill = "tonexty",
          stackgroup = "one",
          text = ~paste(partei, ": ", round(parl_sitze_partei, 1), "%", sep=""),  # Round only for display
          hoverinfo = "text",
          colors = party_colors,
          line = list(width = 0.5)
        ) %>%
          layout(
            title = title,
            xaxis = list(
              title = "Jahr",
              tickmode = "linear",
              dtick = 1
            ),
            yaxis = list(
              title = "Anteil (%)",
              rangemode = "tozero",
              ticksuffix = "%"
            ),
            showlegend = TRUE,
            legend = list(
              orientation = "h",
              xanchor = "center",
              x = 0.5,
              y = -0.2
            ),
            hovermode = "closest"
          )
      } else {
        # Line chart for absolute numbers and vote shares
        p <- plot_ly(
          data = long_data,
          x = ~`get(year_col)`,
          y = ~parl_sitze_partei,
          color = ~partei,
          type = "scatter",
          mode = "lines+markers",
          text = ~paste(partei, ": ", 
                       if(input$variables == "parlament_votes") {
                         paste(round(parl_sitze_partei, 1), "%")
                       } else {
                         paste(parl_sitze_partei, " Sitze")
                       }, 
                       sep=""),
          hoverinfo = "text",
          colors = party_colors,
          line = list(width = 2),
          marker = list(size = 8)
        ) %>%
          layout(
            title = title,
            xaxis = list(
              title = "Jahr",
              tickmode = "linear",
              dtick = 1
            ),
            yaxis = list(
              title = if(input$variables == "parlament_votes") "Wähleranteil (%)" else "Anzahl Sitze",
              rangemode = "tozero",
              ticksuffix = if(input$variables == "parlament_votes") "%" else ""
            ),
            showlegend = TRUE,
            legend = list(
              orientation = "h",
              xanchor = "center",
              x = 0.5,
              y = -0.2
            ),
            hovermode = "closest"
          )
      }
      
      p
    })
    
    # Canton comparison plot
    output$canton_comparison_plot <- renderPlotly({
      req(election_data())
      
      plot_data <- election_data()
      
      # Handle party system measures (rae, partfrakt, parl_party, max_sitzzahl_parl)
      if (input$variables %in% c("rae", "partfrakt", "parl_party", "max_sitzzahl_parl", "reg_konk", "konk_2",
                                "reg_left", "reg_cent", "reg_right", "reg_kath", "reg_gruene",
                                "parl_left", "parl_cent", "parl_right", "parl_kath", "parl_gruene",
                                "wett_parl_se", "wett_parl2_vo", "n_bestplatzierte_sitzzahl_parl",
                                "volatilitaet_se_election", "volatilitaet_se_year", "volatilitaet_vo", "gallagher",
                                "spann", "wett_reg_se", "wett_reg2_se", "max_sitzzahl_reg", "n_bestplatzierte_sitzzahl_reg",
                                "g", "o", "ieo", "balance")) {
        
        # Get variable name
        var_name <- input$variables
        
        # Define titles, labels and colors based on variable
        if (var_name == "rae") {
          plot_title <- paste("Rae-Index der Parteienfraktionalisierung nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Rae-Index"
          value_suffix <- ""
          color <- "#5EA55E"  # Green
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "partfrakt") {
          plot_title <- paste("Effektive Parteienzahl nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Effektive Parteienzahl"
          value_suffix <- ""
          color <- "#4646B4"  # Blue
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "parl_party") {
          plot_title <- paste("Anzahl der Parlamentsparteien nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Anzahl Parteien"
          value_suffix <- ""
          color <- "#D66E1D"  # Orange
          value_column <- "value"
          decimal_places <- 0
        } else if (var_name == "max_sitzzahl_parl") {
          plot_title <- paste("Sitze der stärksten Partei nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Anzahl Sitze"
          value_suffix <- ""
          color <- "#365436"  # Dark green
          value_column <- "value"
          decimal_places <- 0
        } else if (var_name == "reg_konk") {
          plot_title <- paste("Summierte Wähleranteile der Regierungsparteien nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Wähleranteile (%)"
          value_suffix <- "%"
          color <- "#C4C446"  # Yellow-green
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "konk_2") {
          plot_title <- paste("Konkordanz nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Konkordanz"
          value_suffix <- ""
          color <- "#AD5E8E"  # Purple
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "reg_left") {
          plot_title <- paste("Stärke des linken Lagers nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Stärke"
          value_suffix <- ""
          color <- "#E85D63"  # Red
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "reg_cent") {
          plot_title <- paste("Stärke des Mitte-Lagers nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Stärke"
          value_suffix <- ""
          color <- "#D66E1D"  # Orange
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "reg_right") {
          plot_title <- paste("Stärke des rechten Lagers nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Stärke"
          value_suffix <- ""
          color <- "#365436"  # Dark green
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "reg_kath") {
          plot_title <- paste("Stärke des katholischen Lagers nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Stärke"
          value_suffix <- ""
          color <- "#4646B4"  # Blue
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "reg_gruene") {
          plot_title <- paste("Stärke des grünen Lagers nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Stärke"
          value_suffix <- ""
          color <- "#5EA55E"  # Green
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "parl_left") {
          plot_title <- paste("Stärke des linken Lagers im Parlament nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Stärke"
          value_suffix <- ""
          color <- "#E85D63"  # Red
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "parl_cent") {
          plot_title <- paste("Stärke des Mitte-Lagers im Parlament nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Stärke"
          value_suffix <- ""
          color <- "#D66E1D"  # Orange
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "parl_right") {
          plot_title <- paste("Stärke des rechten Lagers im Parlament nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Stärke"
          value_suffix <- ""
          color <- "#365436"  # Dark green
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "parl_kath") {
          plot_title <- paste("Stärke des katholischen Lagers im Parlament nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Stärke"
          value_suffix <- ""
          color <- "#4646B4"  # Blue
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "parl_gruene") {
          plot_title <- paste("Stärke des grünen Lagers im Parlament nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Stärke"
          value_suffix <- ""
          color <- "#5EA55E"  # Green
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "wett_parl_se") {
          plot_title <- paste("Restwählerpotential nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "100% - Wähleranteil stärkste Partei"
          value_suffix <- "%"
          color <- "#9B3C3C"  # Dark red
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "wett_parl2_vo") {
          plot_title <- paste("Unterschied zwischen größter und zweitgrößter Partei nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Unterschied"
          value_suffix <- "%"
          color <- "#DEBA3D"  # Yellow
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "n_bestplatzierte_sitzzahl_parl") {
          plot_title <- paste("Anzahl der stärksten/bestplatzierten Parteien nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Anzahl Parteien"
          value_suffix <- ""
          color <- "#4DB3B3"  # Turquoise
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "volatilitaet_se_election") {
          plot_title <- paste("Parl. Volatilität (Sitzanteile) nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Volatilität"
          value_suffix <- ""
          color <- "#D459D4"  # Purple
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "volatilitaet_se_year") {
          plot_title <- paste("Standardisierte parl. Volatilität nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Volatilität (standardisiert)"
          value_suffix <- ""
          color <- "#878787"  # Grey
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "volatilitaet_vo") {
          plot_title <- paste("Wählervolatilität nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Wählervolatilität"
          value_suffix <- "%"
          color <- "#4798E8"  # Light blue
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "gallagher") {
          plot_title <- paste("Effektive Disproportionalität des Wahlsystems nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Gallagher-Index"
          value_suffix <- ""
          color <- "#A65A42"  # Brown
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "spann") {
          plot_title <- paste("Parteipolitische Spannweite der Regierungskoalition nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Spannweite"
          value_suffix <- ""
          color <- "#A65A42"  # Brown
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "wett_reg_se") {
          plot_title <- paste("Regierungsrestwählerpotential nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "100% - Sitzanteil stärkste Partei"
          value_suffix <- "%"
          color <- "#9B3C3C"  # Dark red
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "wett_reg2_se") {
          plot_title <- paste("Unterschied zwischen größter und zweitgrößter Regierungspartei nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Unterschied"
          value_suffix <- "%"
          color <- "#DEBA3D"  # Yellow
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "max_sitzzahl_reg") {
          plot_title <- paste("Anzahl der Sitze der stärksten Regierungspartei nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Anzahl Sitze"
          value_suffix <- ""
          color <- "#365436"  # Dark green
          value_column <- "value"
          decimal_places <- 0
        } else if (var_name == "n_bestplatzierte_sitzzahl_reg") {
          plot_title <- paste("Anzahl stärkster/bestplatzierter Regierungsparteien nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Anzahl Parteien"
          value_suffix <- ""
          color <- "#4DB3B3"  # Turquoise
          value_column <- "value"
          decimal_places <- 0
        } else if (var_name == "g") {
          plot_title <- paste("Größe der typischen Regierungspartei nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Größe"
          value_suffix <- ""
          color <- "#458B74"  # Aquamarine
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "o") {
          plot_title <- paste("Größe der typischen Oppositionspartei nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Größe"
          value_suffix <- ""
          color <- "#CD5555"  # Indian red
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "ieo") {
          plot_title <- paste("Index of Effective Opposition nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "IEO"
          value_suffix <- ""
          color <- "#8B6508"  # Dark goldenrod
          value_column <- "value"
          decimal_places <- 2
        } else if (var_name == "balance") {
          plot_title <- paste("Index of Competitiveness nach Kanton im Jahr", input$selected_year)
          y_axis_title <- "Balance"
          value_suffix <- ""
          color <- "#9370DB"  # Medium purple
          value_column <- "value"
          decimal_places <- 2
        }
        
        # Sort cantons alphabetically by abbreviation
        canton_order <- sort(unique(plot_data$canton_abbrev))
        plot_data$canton_abbrev <- factor(plot_data$canton_abbrev, levels = canton_order)
        
        # Create format function for hover text
        format_value <- function(x) {
          if (decimal_places == 0) {
            return(as.character(round(x)))
          } else {
            return(as.character(round(x, decimal_places)))
          }
        }
        
        # Create bar chart for the metric
        p <- plot_ly(
          data = plot_data,
          x = ~canton_abbrev,
          y = ~value,
          type = "bar",
          text = ~paste(canton_name, "<br>", y_axis_title, ": ", format_value(value), value_suffix),
          hoverinfo = "text",
          marker = list(color = color)
        ) %>%
          layout(
            title = plot_title,
            xaxis = list(
              title = "Kanton",
              categoryorder = "array",
              categoryarray = canton_order
            ),
            yaxis = list(
              title = y_axis_title,
              rangemode = "tozero"
            ),
            showlegend = FALSE,
            margin = list(b = 50)
          )
        
        return(p)
      }
      
      # Handle election turnout (turnout_e)
      if (input$variables == "turnout_e") {
        # Sort cantons alphabetically by abbreviation
        canton_order <- sort(unique(plot_data$canton_abbrev))
        plot_data$canton_abbrev <- factor(plot_data$canton_abbrev, levels = canton_order)
        
        # Create bar chart for turnout
        p <- plot_ly(
          data = plot_data,
          x = ~canton_abbrev,
          y = ~turnout_e,
          type = "bar",
          text = ~paste(canton_name, "<br>Wahlbeteiligung:", round(turnout_e, 1), "%"),
          hoverinfo = "text",
          marker = list(color = "#E85D63")  # Red color for turnout
        ) %>%
          layout(
            title = paste("Wahlbeteiligung bei Parlamentswahlen nach Kanton im Jahr", input$selected_year),
            xaxis = list(
              title = "Kanton",
              categoryorder = "array",
              categoryarray = canton_order
            ),
            yaxis = list(
              title = "Wahlbeteiligung in %",
              rangemode = "tozero"
            ),
            showlegend = FALSE,
            margin = list(b = 50)
          )
        
        return(p)
      }
      
      # Handle parliamentary term length (parlegisl)
      if (input$variables == "parlegisl") {
        # Sort cantons alphabetically by abbreviation
        canton_order <- sort(unique(plot_data$canton_abbrev))
        plot_data$canton_abbrev <- factor(plot_data$canton_abbrev, levels = canton_order)
        
        # Create bar chart for term lengths
        p <- plot_ly(
          data = plot_data,
          x = ~canton_abbrev,
          y = ~parlegisl,
          type = "bar",
          text = ~paste(canton_name, "<br>Amtsdauer:", parlegisl, "Jahre"),
          hoverinfo = "text",
          marker = list(color = "#4646B4")
        ) %>%
          layout(
            title = paste("Amtsdauer des Parlaments nach Kanton im Jahr", input$selected_year),
            xaxis = list(
              title = "Kanton",
              categoryorder = "array",
              categoryarray = canton_order
            ),
            yaxis = list(
              title = "Amtsdauer in Jahren",
              rangemode = "tozero"
            ),
            showlegend = FALSE,
            margin = list(b = 50)
          )
        
        return(p)
      }
      
      # Special handling for parliamentary elections
      if (input$variables == "parl_election") {
        # Get the list of all canton abbreviations for this year
        all_cantons <- filtered_data() %>%
          filter(!!sym(names(filtered_data())[grep("year|jahr", names(filtered_data()), ignore.case = TRUE)[1]]) == input$selected_year) %>%
          pull(kantonnr) %>%
          unique() %>%
          sapply(get_canton_abbr)
        
        # Create a complete data frame with all cantons
        all_canton_data <- data.frame(
          canton_abbrev = all_cantons,
          canton_name = sapply(all_cantons, function(abbr) get_canton_name(abbr, "de")),
          parl_sitze_partei = 0  # Default value for cantons without elections
        )
        
        # Update values for cantons with elections (those in plot_data)
        cantons_with_elections <- plot_data %>%
          filter(parl_sitze_partei == 1) %>%
          pull(canton_abbrev)
        
        # Set the value to 1 for cantons with elections
        all_canton_data$parl_sitze_partei[all_canton_data$canton_abbrev %in% cantons_with_elections] <- 1
        
        # Sort cantons alphabetically by abbreviation
        canton_order <- sort(unique(all_canton_data$canton_abbrev))
        all_canton_data$canton_abbrev <- factor(all_canton_data$canton_abbrev, levels = canton_order)
        
        # Create bar chart for all cantons, highlighting those with elections
        p <- plot_ly(
          data = all_canton_data,
          x = ~canton_abbrev,
          y = ~parl_sitze_partei,
          type = "bar",
          text = ~canton_name,  # Only show canton name in hover text
          hoverinfo = "text",
          marker = list(
            color = ~ifelse(parl_sitze_partei == 1, "#4646B4", "#E0E0E0")  # Blue for elections, light gray for no elections
          )
        ) %>%
          layout(
            title = paste("Kantone mit Parlamentswahl im Jahr", input$selected_year),
            xaxis = list(
              title = "Kanton",
              categoryorder = "array",
              categoryarray = canton_order
            ),
            yaxis = list(
              title = "Parlamentswahl",
              range = c(0, 1),
              showticklabels = FALSE
            ),
            showlegend = FALSE,
            margin = list(b = 50)
          )
        
        # Add "Ja" text annotations only for cantons with elections
        election_cantons <- all_canton_data %>% 
          filter(parl_sitze_partei == 1) %>%
          pull(canton_abbrev)
        
        if(length(election_cantons) > 0) {
          annotations <- lapply(election_cantons, function(abb) {
            list(
              x = abb,       # Use the canton abbreviation directly as x-coordinate
              y = 0.5,       # Position the text in the middle of the bar
              text = "Ja",
              showarrow = FALSE,
              font = list(color = "white", size = 12)
            )
          })
          
          p <- p %>% layout(annotations = annotations)
        }
        
        return(p)
      }
      
      # Special handling for government elections
      if (input$variables == "reg_election") {
        # Get the list of all canton abbreviations for this year
        all_cantons <- filtered_data() %>%
          filter(!!sym(names(filtered_data())[grep("year|jahr", names(filtered_data()), ignore.case = TRUE)[1]]) == input$selected_year) %>%
          pull(kantonnr) %>%
          unique() %>%
          sapply(get_canton_abbr)
        
        # Create a complete data frame with all cantons
        all_canton_data <- data.frame(
          canton_abbrev = all_cantons,
          canton_name = sapply(all_cantons, function(abbr) get_canton_name(abbr, "de")),
          parl_sitze_partei = 0  # Default value for cantons without elections
        )
        
        # Update values for cantons with elections (those in plot_data)
        cantons_with_elections <- plot_data %>%
          filter(parl_sitze_partei == 1) %>%
          pull(canton_abbrev)
        
        # Set the value to 1 for cantons with elections
        all_canton_data$parl_sitze_partei[all_canton_data$canton_abbrev %in% cantons_with_elections] <- 1
        
        # Sort cantons alphabetically by abbreviation
        canton_order <- sort(unique(all_canton_data$canton_abbrev))
        all_canton_data$canton_abbrev <- factor(all_canton_data$canton_abbrev, levels = canton_order)
        
        # Create bar chart for all cantons, highlighting those with elections
        p <- plot_ly(
          data = all_canton_data,
          x = ~canton_abbrev,
          y = ~parl_sitze_partei,
          type = "bar",
          text = ~canton_name,  # Only show canton name in hover text
          hoverinfo = "text",
          marker = list(
            color = ~ifelse(parl_sitze_partei == 1, "#D66E1D", "#E0E0E0")  # Orange for elections, light gray for no elections
          )
        ) %>%
          layout(
            title = paste("Kantone mit Regierungsratswahl im Jahr", input$selected_year),
            xaxis = list(
              title = "Kanton",
              categoryorder = "array",
              categoryarray = canton_order
            ),
            yaxis = list(
              title = "Regierungsratswahl",
              range = c(0, 1),
              showticklabels = FALSE
            ),
            showlegend = FALSE,
            margin = list(b = 50)
          )
        
        # Add "Ja" text annotations only for cantons with elections
        election_cantons <- all_canton_data %>% 
          filter(parl_sitze_partei == 1) %>%
          pull(canton_abbrev)
        
        if(length(election_cantons) > 0) {
          annotations <- lapply(election_cantons, function(abb) {
            list(
              x = abb,       # Use the canton abbreviation directly as x-coordinate
              y = 0.5,       # Position the text in the middle of the bar
              text = "Ja",
              showarrow = FALSE,
              font = list(color = "white", size = 12)
            )
          })
          
          p <- p %>% layout(annotations = annotations)
        }
        
        return(p)
      }
      
      # Sort cantons alphabetically by abbreviation
      canton_order <- sort(unique(plot_data$canton_abbrev))
      
      # Check if "partei" column exists in plot_data (only for party-based plots)
      if ("partei" %in% names(plot_data)) {
        # Create plot with enhanced tooltips for party-based data
      p <- plot_ly(
        data = plot_data,
        x = ~canton_abbrev,
        y = ~parl_sitze_partei,
        color = ~partei,
        type = "bar",
        text = ~paste(canton_name, "<br>", partei, ": ", 
                     if(input$variables == "parlament_votes") {
                       paste(round(parl_sitze_partei, 1), "%")
                     } else if(input$variables == "parlament_sitze_anteil") {
                       paste(round(parl_sitze_partei, 1), "%")
                     } else {
                       paste(parl_sitze_partei, " Sitze")
                     }, 
                     sep=""),
        textposition = "none",  # Remove text labels
        hoverinfo = "text",
        colors = party_colors
      ) %>%
        layout(
          title = paste(
            if(input$variables == "parlament_sitze") "Anzahl Parlamentssitze" 
            else if(input$variables == "parlament_sitze_anteil") "Anteil Parlamentssitze"
            else if(input$variables == "parlament_votes") "Wähleranteil"
              else if(input$variables == "regierung_sitze") "Anzahl Regierungssitze"
              else "Anzahl Sitze",
            "der Parteien nach Kanton im Jahr", 
            input$selected_year
          ),
          xaxis = list(
            title = "Kanton",
            categoryorder = "array",
            categoryarray = canton_order
          ),
          yaxis = list(
            title = if(input$variables %in% c("parlament_sitze_anteil", "parlament_votes")) "Anteil (%)" else "Anzahl Sitze",
            rangemode = "tozero",
            ticksuffix = if(input$variables %in% c("parlament_sitze_anteil", "parlament_votes")) "%" else ""
          ),
          barmode = "stack",
          showlegend = TRUE,
          legend = list(
            orientation = "h",
            xanchor = "center",
            x = 0.5,
            y = -0.2
          ),
          margin = list(b = 50)
        )
      } else {
        # For non-party based metrics, show simple bar chart by canton
        plot_data$canton_abbrev <- factor(plot_data$canton_abbrev, levels = canton_order)
        
        # Determine decimal places for display
        decimal_places <- if(input$variables %in% c("rae", "partfrakt", "volatilitaet_se_year", "gallagher")) 2 else 1
        
        # Determine y-axis suffix
        suffix <- if(input$variables %in% c("wett_parl_se", "wett_parl2_vo", "volatilitaet_vo")) "%" else ""
        
        # Create bar chart for the metric
        p <- plot_ly(
          data = plot_data,
          x = ~canton_abbrev,
          y = ~parl_sitze_partei,
          type = "bar",
          text = ~paste(canton_name, "<br>Wert:", round(parl_sitze_partei, decimal_places), suffix),
          hoverinfo = "text",
          marker = list(
            color = if(input$variables == "reg_left" || input$variables == "parl_left") "#E85D63"  # Red
                   else if(input$variables == "reg_cent" || input$variables == "parl_cent") "#D66E1D"  # Orange
                   else if(input$variables == "reg_right" || input$variables == "parl_right") "#365436"  # Dark green
                   else if(input$variables == "reg_kath" || input$variables == "parl_kath") "#4646B4"  # Blue
                   else if(input$variables == "reg_gruene" || input$variables == "parl_gruene") "#5EA55E"  # Green
                   else if(input$variables == "wett_parl_se" || input$variables == "wett_reg_se") "#9B3C3C"  # Dark red
                   else if(input$variables == "wett_parl2_vo" || input$variables == "wett_reg2_se") "#DEBA3D"  # Yellow
                   else if(input$variables == "n_bestplatzierte_sitzzahl_parl" || input$variables == "n_bestplatzierte_sitzzahl_reg") "#4DB3B3"  # Turquoise
                   else if(input$variables == "volatilitaet_se_election") "#D459D4"  # Purple
                   else if(input$variables == "volatilitaet_se_year") "#878787"  # Grey
                   else if(input$variables == "volatilitaet_vo") "#4798E8"  # Light blue
                   else if(input$variables == "gallagher") "#A65A42"  # Brown
                   else if(input$variables == "spann") "#A65A42"  # Brown
                   else if(input$variables == "max_sitzzahl_reg") "#365436"  # Dark green
                   else if(input$variables == "g") "#458B74"  # Aquamarine
                   else if(input$variables == "o") "#CD5555"  # Indian red
                   else if(input$variables == "ieo") "#8B6508"  # Dark goldenrod
                   else if(input$variables == "balance") "#9370DB"  # Medium purple
                   else "#4646B4"  # Default blue
          )
        ) %>%
          layout(
            title = paste(case_when(
              input$variables == "rae" ~ "Rae-Index der Parteienfraktionalisierung",
              input$variables == "partfrakt" ~ "Effektive Parteienzahl",
              input$variables == "parl_party" ~ "Anzahl der Parlamentsparteien",
              input$variables == "max_sitzzahl_parl" ~ "Sitze der stärksten Partei",
              input$variables == "reg_konk" ~ "Summierte Wähleranteile der Regierungsparteien",
              input$variables == "konk_2" ~ "Konkordanz",
              input$variables == "reg_left" ~ "Stärke des linken Lagers",
              input$variables == "reg_cent" ~ "Stärke des Mitte-Lagers",
              input$variables == "reg_right" ~ "Stärke des rechten Lagers",
              input$variables == "reg_kath" ~ "Stärke des katholischen Lagers",
              input$variables == "reg_gruene" ~ "Stärke des grünen Lagers",
              input$variables == "parl_left" ~ "Stärke des linken Lagers im Parlament",
              input$variables == "parl_cent" ~ "Stärke des Mitte-Lagers im Parlament",
              input$variables == "parl_right" ~ "Stärke des rechten Lagers im Parlament",
              input$variables == "parl_kath" ~ "Stärke des katholischen Lagers im Parlament",
              input$variables == "parl_gruene" ~ "Stärke des grünen Lagers im Parlament",
              input$variables == "wett_parl_se" ~ "Restwählerpotential",
              input$variables == "wett_parl2_vo" ~ "Unterschied zwischen größter und zweitgrößter Partei",
              input$variables == "n_bestplatzierte_sitzzahl_parl" ~ "Anzahl der stärksten/bestplatzierten Parteien",
              input$variables == "volatilitaet_se_election" ~ "Parlamentarische Volatilität (Sitzanteile)",
              input$variables == "volatilitaet_se_year" ~ "Standardisierte parlamentarische Volatilität",
              input$variables == "volatilitaet_vo" ~ "Wählervolatilität",
              input$variables == "gallagher" ~ "Effektive Disproportionalität des Wahlsystems",
              input$variables == "spann" ~ "Parteipolitische Spannweite der Regierungskoalition",
              input$variables == "wett_reg_se" ~ "Regierungsrestwählerpotential",
              input$variables == "wett_reg2_se" ~ "Unterschied zwischen größter und zweitgrößter Regierungspartei",
              input$variables == "max_sitzzahl_reg" ~ "Anzahl der Sitze der stärksten Regierungspartei",
              input$variables == "n_bestplatzierte_sitzzahl_reg" ~ "Anzahl stärkster/bestplatzierter Regierungsparteien",
              input$variables == "g" ~ "Größe der typischen Regierungspartei",
              input$variables == "o" ~ "Größe der typischen Oppositionspartei",
              input$variables == "ieo" ~ "Index of Effective Opposition",
              input$variables == "balance" ~ "Index of Competitiveness",
              TRUE ~ input$variables  # Default case
            ), "nach Kanton im Jahr", input$selected_year),
            xaxis = list(
              title = "Kanton",
              categoryorder = "array",
              categoryarray = canton_order
            ),
            yaxis = list(
              title = case_when(
                input$variables == "rae" ~ "Rae-Index",
                input$variables == "partfrakt" ~ "Effektive Parteienzahl",
                input$variables == "parl_party" ~ "Anzahl Parteien",
                input$variables == "max_sitzzahl_parl" ~ "Anzahl Sitze",
                input$variables == "reg_konk" ~ "Wähleranteile (%)",
                input$variables == "konk_2" ~ "Konkordanz",
                input$variables %in% c("reg_left", "reg_cent", "reg_right", "reg_kath", "reg_gruene", 
                                      "parl_left", "parl_cent", "parl_right", "parl_kath", "parl_gruene") ~ "Stärke",
                input$variables == "wett_parl_se" ~ "100% - Wähleranteil stärkste Partei",
                input$variables == "wett_parl2_vo" ~ "Unterschied",
                input$variables == "n_bestplatzierte_sitzzahl_parl" ~ "Anzahl Parteien",
                input$variables == "volatilitaet_se_election" ~ "Volatilität",
                input$variables == "volatilitaet_se_year" ~ "Volatilität (standardisiert)",
                input$variables == "volatilitaet_vo" ~ "Wählervolatilität",
                input$variables == "gallagher" ~ "Gallagher-Index",
                input$variables == "spann" ~ "Spannweite",
                input$variables == "wett_reg_se" ~ "100% - Sitzanteil stärkste Partei",
                input$variables == "wett_reg2_se" ~ "Unterschied",
                input$variables == "max_sitzzahl_reg" ~ "Anzahl Sitze",
                input$variables == "n_bestplatzierte_sitzzahl_reg" ~ "Anzahl Parteien",
                input$variables == "g" ~ "Größe",
                input$variables == "o" ~ "Größe",
                input$variables == "ieo" ~ "IEO",
                input$variables == "balance" ~ "Balance",
                TRUE ~ "Wert"  # Default case
              ),
              rangemode = "tozero",
              ticksuffix = suffix
            ),
            showlegend = FALSE,
            margin = list(b = 50)
          )
      }
      
      p
    })
    
    # Institution details plot
    output$institution_details_plot <- renderPlotly({
      req(filtered_data())
      req(input$variables)
      
      # Placeholder for now - will be implemented later
      plot_ly() %>% 
        layout(title = "Details zu Wahlen (Implementierung folgt)")
    })
    
    # Data table
    output$data_table <- renderDT({
      req(filtered_data())
      req(input$variables)
      
      # Placeholder for now - will be implemented later
      datatable(
        data.frame(Message = "Datentabelle für Wahlen (Implementierung folgt)"),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'ftip'
        ),
        rownames = FALSE,
        class = "compact stripe"
      )
    })
    
    # Correlation plot
    output$correlation_plot <- renderPlotly({
      req(filtered_data())
      req(input$variables)
      
      # Placeholder for now - will be implemented later
      plot_ly() %>% 
        layout(title = "Korrelationskarte für Wahlen (Implementierung folgt)")
    })
    
    # Statistical summary
    output$statistical_summary <- renderPrint({
      req(filtered_data())
      req(input$variables)
      
      # Placeholder for now - will be implemented later
      cat("Statistische Zusammenfassung für Wahlen (Implementierung folgt)")
    })
  })
} 