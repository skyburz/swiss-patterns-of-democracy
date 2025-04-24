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
#' @param variable_type Either "parlament_sitze" or "regierung_sitze"
#' @return A dataframe in long format with election data
prepare_election_data <- function(data, year, variable_type = "parlament_sitze") {
  # Get column names
  year_col <- names(data)[grep("year|jahr", names(data), ignore.case = TRUE)[1]]
  
  # Get party columns based on variable type
  party_cols <- if(variable_type == "parlament_sitze") {
    # Only use columns ending with _parl_s for parliamentary seats
    names(data)[grep("_parl_s$", names(data))]
  } else {
    # For government seats, keep all relevant columns
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
                # 1. First filter: Election Variables (placeholder for now)
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
      
      # Create named choices for better display - two options
      named_choices <- c(
        "Anzahl Parlamentssitze der Parteien" = "parlament_sitze",
        "Anzahl Regierungsratssitze der Parteien" = "regierung_sitze"
      )
      
      # Update the select input with the available election columns
      updateSelectInput(
        session,
        "variables",
        choices = named_choices,
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
      
      # Get party columns based on selected variable
      if (input$variables == "parlament_sitze") {
        party_cols <- names(data)[grep("_parl_s$", names(data))]
        title <- "Anzahl Parlamentssitze der Parteien"
      } else {
        party_cols <- names(data)[grep("_reg_[sv]$|_reg_sa$", names(data))]
        title <- "Anzahl Regierungsratssitze der Parteien"
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
      
      # Group by year and party, summing seats
      long_data <- long_data %>%
        group_by(get(year_col), partei) %>%
        summarise(parl_sitze_partei = sum(parl_sitze_partei, na.rm = TRUE), .groups = 'drop')
      
      # Create line chart with custom colors
      p <- plot_ly(
        data = long_data,
        x = ~`get(year_col)`,
        y = ~parl_sitze_partei,
        color = ~partei,
        type = "scatter",
        mode = "lines+markers",
        text = ~paste(partei, ": ", parl_sitze_partei, " Sitze", sep=""),
        hoverinfo = "text",
        colors = party_colors,  # Use the same party_colors defined at the top
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
            title = "Anzahl Sitze",
            rangemode = "tozero"
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
      
      p
    })
    
    # Canton comparison plot
    output$canton_comparison_plot <- renderPlotly({
      req(election_data())
      
      plot_data <- election_data()
      
      # Sort cantons alphabetically by abbreviation
      canton_order <- sort(unique(plot_data$canton_abbrev))
      
      # Create plot with enhanced tooltips
      p <- plot_ly(
        data = plot_data,
        x = ~canton_abbrev,
        y = ~parl_sitze_partei,
        color = ~partei,
        type = "bar",
        text = ~paste(canton_name, "<br>", partei, ": ", parl_sitze_partei, " Sitze", sep=""),
        textposition = "none",  # Remove text labels
        hoverinfo = "text",
        colors = party_colors
      ) %>%
        layout(
          title = paste(
            if(input$variables == "parlament_sitze") "Anzahl Parlamentssitze" else "Anzahl Regierungsratssitze",
            "der Parteien nach Kanton im Jahr", 
            input$selected_year
          ),
          xaxis = list(
            title = "Kanton",
            categoryorder = "array",
            categoryarray = canton_order
          ),
          yaxis = list(
            title = "Anzahl Sitze",
            rangemode = "tozero"
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