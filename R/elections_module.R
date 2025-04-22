# Elections Module
# This module creates visualizations for election data

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

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
                  selected = NULL
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
    
    # Reactive value to store filtered data
    filtered_data <- reactiveVal(NULL)
    
    # Reactive value to store the detected canton column name
    canton_col_name <- reactiveVal(NULL)
    
    # Reactive values to store min and max years from the data
    min_year <- reactiveVal(NULL)
    max_year <- reactiveVal(NULL)
    
    # Initialize UI when data becomes available
    observe({
      req(selected_data())
      data <- selected_data()
      
      # Find potential election columns
      election_cols <- names(data)[grep("wahl|election|vote|stimmen|partei|party",
                                       names(data), ignore.case = TRUE)]
      
      # Remove any NULL values and make sure the list is unique
      election_cols <- unique(election_cols[!is.null(election_cols)])
      
      # Create named choices for better display
      named_choices <- setNames(election_cols, election_cols)
      
      # Update the select input with the available election columns
      updateSelectInput(
        session,
        "variables",
        choices = named_choices,
        selected = ifelse(length(election_cols) > 0, election_cols[1], NULL)
      )
      
      # Find canton column
      canton_col <- names(data)[grep("^kanton$|canton|kanton", names(data), ignore.case = TRUE)[1]]
      canton_col_name(canton_col)
      
      if (!is.na(canton_col)) {
        # Get unique canton labels
        cantons <- sort(unique(data[[canton_col]]))
        
        # Update the canton checkbox group with named choices
        updateCheckboxGroupInput(
          session, 
          "canton_select", 
          choices = setNames(cantons, cantons),
          selected = cantons[1:3]  # Pre-select first 3 cantons for convenience
        )
      }
      
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
    
    # Function to update filtered data based on current inputs
    update_filtered_data <- function() {
      data <- selected_data()
      req(data)
      
      # Find canton and year columns
      canton_col <- canton_col_name()
      year_col <- names(data)[grep("year|jahr", names(data), ignore.case = TRUE)[1]]
      
      # Apply canton filter based on mode
      if (input$canton_mode == "select") {
        # Check if any cantons are selected
        if (is.null(input$canton_select) || length(input$canton_select) == 0) {
          # No cantons selected, use all cantons
          print("No cantons selected, using all cantons")
        } else {
          # Filter to only selected cantons
          print(paste("Filtering to cantons:", paste(input$canton_select, collapse = ", ")))
          
          # Check if the canton column exists
          if (is.null(canton_col) || !canton_col %in% names(data)) {
            print(paste("ERROR: Canton column not found. Available columns:", paste(names(data), collapse = ", ")))
            return()
          }
          
          # Filter the data to include only the selected cantons
          data <- data %>% filter(!!sym(canton_col) %in% input$canton_select)
          print(paste("After canton filtering, rows:", nrow(data)))
        }
      } else {
        # Using all cantons, no filtering needed
        print("Using all cantons (no canton filtering)")
      }
      
      # Apply time filter based on active tab
      if (input$main_tabs == "Zeittrend" && !is.null(input$time_range)) {
        # For Zeittrend tab, use the time range slider
        data <- data %>% filter(
          !!sym(year_col) >= input$time_range[1] & 
          !!sym(year_col) <= input$time_range[2]
        )
        print(paste("Filtering to time range:", input$time_range[1], "to", input$time_range[2]))
      } else if (!is.null(input$selected_year) && 
                (input$main_tabs == "Kantonsvergleich" || input$main_tabs == "Institutionsdetails")) {
        # For Kantonsvergleich and Institutionsdetails tabs, use the single year slider
        data <- data %>% filter(!!sym(year_col) == input$selected_year)
        print(paste("Filtering to year:", input$selected_year))
      }
      
      print(paste("After time filtering, rows:", nrow(data)))
      
      # Make sure filtered data is not empty
      if (nrow(data) == 0) {
        print("WARNING: No data after filtering!")
        return()
      }
      
      print(paste("Updated filtered data has", nrow(data), "rows"))
      
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
    
    # Time trend plot
    output$time_trend_plot <- renderPlotly({
      req(filtered_data())
      req(input$variables)
      
      # Placeholder for now - will be implemented later
      plot_ly() %>% 
        layout(title = "Zeittrend der Wahlen (Implementierung folgt)")
    })
    
    # Canton comparison plot
    output$canton_comparison_plot <- renderPlotly({
      req(filtered_data())
      req(input$variables)
      
      # Placeholder for now - will be implemented later
      plot_ly() %>% 
        layout(title = "Kantonsvergleich der Wahlen (Implementierung folgt)")
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