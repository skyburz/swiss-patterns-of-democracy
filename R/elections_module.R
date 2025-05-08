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
#' @param variable_type Either "parlament_sitze", "parlament_sitze_anteil", "parlament_votes", "parl_election", or "regierung_sitze"
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
  
  party_cols <- if(variable_type %in% c("parlament_sitze", "parlament_sitze_anteil", "parlament_votes")) {
    # Use columns ending with _parl_s for parliamentary seats and _parl_v for votes
    if(variable_type == "parlament_votes") {
      names(data)[grep("_parl_v$", names(data))]
    } else {
      names(data)[grep("_parl_s$", names(data))]
    }
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
        "Anteil Parlamentssitze der Parteien" = "parlament_sitze_anteil",
        "Wähleranteil der Parteien (%)" = "parlament_votes",
        "Parlamentswahl im Jahr" = "parl_election",
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
      
      if (input$variables %in% c("parlament_sitze", "parlament_sitze_anteil", "parlament_votes")) {
        if(input$variables == "parlament_votes") {
          party_cols <- names(data)[grep("_parl_v$", names(data))]
          title <- "Wähleranteil der Parteien (%)"
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
      
      # Sort cantons alphabetically by abbreviation
      canton_order <- sort(unique(plot_data$canton_abbrev))
      
      # Create plot with enhanced tooltips
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
            else "Anzahl Regierungsratssitze",
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