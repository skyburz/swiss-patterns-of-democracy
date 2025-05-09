# Modul für die Analyse demokratischer Institutionen

#' UI function for democratic institutions module
#'
#' @param id The module ID
#' @return A UI definition
democratic_institutions_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Custom CSS to ensure equal height columns
    tags$head(
      tags$style(HTML("
        .equal-height {
          display: flex;
          flex-direction: column;
          height: 100%;
        }
        .equal-height .card {
          flex: 1;
          display: flex;
          flex-direction: column;
        }
        .equal-height .card-body {
          flex: 1;
          display: flex;
          flex-direction: column;
        }
        .filters-container {
          display: flex;
          flex-direction: column;
          justify-content: space-between;
          height: 100%;
        }
        .filter-options {
          flex-grow: 1;
        }
        .filter-button {
          margin-top: auto;
        }
        .canton-selection-container {
          margin-top: 10px;
          margin-bottom: 10px;
        }
        .canton-checkbox-group {
          max-height: 150px;
          overflow-y: auto;
          border: 1px solid #ddd;
          padding: 5px;
          margin-top: 5px;
        }
        .disabled-canton-group {
          opacity: 0.5;
          pointer-events: none;
        }
        .time-trend-controls {
          padding: 10px 15px;
          background-color: #f8f9fa;
          border-radius: 4px;
          margin-bottom: 15px;
        }
        .plot-container {
          min-height: 500px;
        }
        .filter-panel {
          max-height: 500px;
          overflow-y: auto;
        }
      "))
    ),
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
                # 1. First filter: Democratic Institutions (changed to single-select dropdown)
                selectInput(
                  ns("institutions"),
                  "Demokratische Institutionen:",
                  choices = NULL,
                  selected = "sitzparl"
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
          card_header("Überblick über demokratische Institutionen"),
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
                  plotlyOutput(ns("canton_comparison_plot"), height = "500px")
                )
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
          card_header("Direkte Demokratie"),
          card_body(
            tabsetPanel(
              tabPanel(
                "Datentabelle",
                DTOutput(ns("data_table"))
              )
            )
          )
        )
      )
    )
  )
}

#' Server function for democratic institutions module
#'
#' @param id The module ID
#' @param selected_data Reactive expression that returns the selected dataset
#' @return A server function
democratic_institutions_server <- function(id, selected_data) {
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
      
      # Find potential institution columns, making sure to include sitzparl, sitzreg, proporz, parlegisl, gallagher, proporz4, proporz3reg, and reg_proporz
      institution_cols <- c(
        # First check if sitzparl exists in the dataset
        if("sitzparl" %in% names(data)) "sitzparl" else NULL,
        # Check if sitzreg exists in the dataset
        if("sitzreg" %in% names(data)) "sitzreg" else NULL,
        # Check if proporz exists in the dataset
        if("proporz" %in% names(data)) "proporz" else NULL,
        # Check if parlegisl exists in the dataset
        if("parlegisl" %in% names(data)) "parlegisl" else NULL,
        # Check if gallagher exists in the dataset
        if("gallagher" %in% names(data)) "gallagher" else NULL,
        # Check if proporz4 exists in the dataset
        if("proporz4" %in% names(data)) "proporz4" else NULL,
        # Check if proporz3reg exists in the dataset
        if("proporz3reg" %in% names(data)) "proporz3reg" else NULL,
        # Check if reg_proporz exists in the dataset
        if("reg_proporz" %in% names(data)) "reg_proporz" else NULL,
        # Then get other institutional variables
        names(data)[grep("institution|democracy|demokratie|vote|abstimmung|referendum|initiative",
                         names(data), ignore.case = TRUE)]
      )
      
      # Remove any NULL values and make sure the list is unique
      institution_cols <- unique(institution_cols[!is.null(institution_cols)])
      
      # Create named choices for better display
      named_choices <- setNames(
        institution_cols,
        ifelse(
          institution_cols == "sitzparl",
          "Parlamentssitze",
          ifelse(
            institution_cols == "sitzreg",
            "Sitze in der Regierung",
            ifelse(
              institution_cols == "proporz",
              "Proporzwahl",
              ifelse(
                institution_cols == "parlegisl",
                "Amtsdauer des Parlaments in Jahren",
                ifelse(
                  institution_cols == "gallagher",
                  "Effektive Disproportionalität des Wahlsystems",
                  ifelse(
                    institution_cols == "proporz4",
                    "Proportionalität des Wahlsystems bei Parlamentswahlen",
                    ifelse(
                      institution_cols == "proporz3reg",
                      "Proportionalität der Wahlsysteme bei Parlaments- und Regierungswahlen",
                      ifelse(
                        institution_cols == "reg_proporz",
                        "Wahlverfahren bei Regierungsratswahlen (Dummy)",
                        institution_cols
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
      
      # Update the institutions dropdown (now a single-select)
      updateSelectInput(session, "institutions", choices = named_choices, selected = "sitzparl")
      
      # Find canton column - specifically look for exact "kanton" column first
      canton_col <- names(data)[grep("^kanton$", names(data), ignore.case = TRUE)[1]]
      if (is.na(canton_col)) {
        # Fallback to any column with "canton" in the name
        canton_col <- names(data)[grep("canton|kanton", names(data), ignore.case = TRUE)[1]]
      }
      
      # Store the canton column name for later use
      canton_col_name(canton_col)
      print(paste("Detected canton column:", canton_col))
      
      if (!is.na(canton_col)) {
        # Get unique canton labels
        cantons <- sort(unique(data[[canton_col]]))
        print(paste("Available cantons:", paste(cantons, collapse = ", ")))
        
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
      } else if ((input$main_tabs == "Kantonsvergleich" || input$main_tabs == "Institutionsdetails") && !is.null(input$selected_year)) {
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
      
      # Convert gallagher to numeric if it exists
      if("gallagher" %in% names(data)) {
        data$gallagher <- as.numeric(data$gallagher)
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
    
    # Time trend plot
    output$time_trend_plot <- renderPlotly({
      req(filtered_data())
      req(input$institutions)
      req(input$time_range)
      
      data <- filtered_data()
      year_col <- names(data)[grep("year|jahr", names(data), ignore.case = TRUE)[1]]
      
      # Use the time range for the Zeittrend plot
      selected_year_min <- input$time_range[1]
      selected_year_max <- input$time_range[2]
      
      # Find canton column
      canton_col <- canton_col_name()
      
      # Get the selected institution
      selected_institution <- input$institutions
      
      # Ensure gallagher is numeric if it's the selected institution
      if(selected_institution == "gallagher" && "gallagher" %in% names(data)) {
        data$gallagher <- as.numeric(data$gallagher)
      }
      
      # Different plot behavior based on canton selection mode
      if (input$canton_mode == "all") {
        # For "Alle Kantone", show average across all cantons
        plot_data <- data %>%
          group_by(!!sym(year_col)) %>%
          summarize(value = mean(!!sym(selected_institution), na.rm = TRUE), .groups = "drop")
        
        # Customize title based on selected institution
        plot_title <- if(selected_institution == "sitzparl") {
          paste("Zeittrend der Parlamentssitze (", selected_year_min, "-", selected_year_max, ")", sep = "")
        } else if(selected_institution == "sitzreg") {
          paste("Zeittrend der Regierungssitze (", selected_year_min, "-", selected_year_max, ")", sep = "")
        } else if(selected_institution == "proporz") {
          paste("Zeittrend der Proporzwahl (", selected_year_min, "-", selected_year_max, ")", sep = "")
        } else if(selected_institution == "parlegisl") {
          paste("Zeittrend der Amtsdauer des Parlaments (", selected_year_min, "-", selected_year_max, ")", sep = "")
        } else if(selected_institution == "gallagher") {
          paste("Zeittrend der Disproportionalität des Wahlsystems (", selected_year_min, "-", selected_year_max, ")", sep = "")
        } else if(selected_institution == "proporz4") {
          paste("Zeittrend der Proportionalität des Wahlsystems bei Parlamentswahlen (", selected_year_min, "-", selected_year_max, ")", sep = "")
        } else if(selected_institution == "proporz3reg") {
          paste("Zeittrend der Proportionalität der Wahlsysteme bei Parlaments- und Regierungswahlen (", selected_year_min, "-", selected_year_max, ")", sep = "")
        } else if(selected_institution == "reg_proporz") {
          paste("Zeittrend des Wahlverfahrens bei Regierungsratswahlen (", selected_year_min, "-", selected_year_max, ")", sep = "")
        } else {
          paste("Zeittrend von ", selected_institution, " (", selected_year_min, "-", selected_year_max, ")", sep = "")
        }
        
        # Customize y-axis label based on selected institution
        y_label <- if(selected_institution == "sitzparl") {
          "Parlamentssitze"
        } else if(selected_institution == "sitzreg") {
          "Regierungssitze"
        } else if(selected_institution == "proporz") {
          "Proporzwahl"
        } else if(selected_institution == "parlegisl") {
          "Amtsdauer in Jahren"
        } else if(selected_institution == "gallagher") {
          "Disproportionalitätsindex"
        } else if(selected_institution == "proporz4") {
          "Proportionalitätsindex"
        } else if(selected_institution == "proporz3reg") {
          "Proportionalitätsindex"
        } else if(selected_institution == "reg_proporz") {
          "Wahlverfahren (0=Majorz, 1=Proporz)"
        } else {
          selected_institution
        }
        
        # Create plot for all cantons average
        p <- ggplot(plot_data, aes_string(x = year_col, y = "value")) +
          geom_line(linewidth = 1.2, color = "#2C3E50") +
          geom_point(color = "#2C3E50") +
          theme_minimal() +
          labs(
            title = plot_title,
            x = "Jahr",
            y = y_label
          )
      } else {
        # For "Auswahl von Kantonen", show individual lines for each canton
        # First check if we have any cantons selected
        if (is.null(input$canton_select) || length(input$canton_select) == 0) {
          # No cantons selected, show empty plot with message
          return(plot_ly() %>% 
                   layout(title = "Bitte wählen Sie mindestens einen Kanton aus"))
        }
        
        # Create a plot with one line per canton
        plot_data <- data %>%
          # No grouping by canton here - we want individual lines
          group_by(!!sym(year_col), !!sym(canton_col)) %>%
          summarize(
            value = mean(!!sym(selected_institution), na.rm = TRUE),
            .groups = "drop"
          )
        
        # Create plot with cantons as colors and legend on the right
        # Customize title based on selected institution
        plot_title <- if(selected_institution == "sitzparl") {
          paste("Zeittrend der Parlamentssitze nach Kanton (", 
                selected_year_min, "-", selected_year_max, ")", sep = "")
        } else if(selected_institution == "sitzreg") {
          paste("Zeittrend der Regierungssitze nach Kanton (", 
                selected_year_min, "-", selected_year_max, ")", sep = "")
        } else if(selected_institution == "proporz") {
          paste("Zeittrend der Proporzwahl nach Kanton (", 
                selected_year_min, "-", selected_year_max, ")", sep = "")
        } else if(selected_institution == "parlegisl") {
          paste("Zeittrend der Amtsdauer des Parlaments nach Kanton (", 
                selected_year_min, "-", selected_year_max, ")", sep = "")
        } else if(selected_institution == "gallagher") {
          paste("Zeittrend der Disproportionalität des Wahlsystems nach Kanton (", 
                selected_year_min, "-", selected_year_max, ")", sep = "")
        } else if(selected_institution == "proporz4") {
          paste("Zeittrend der Proportionalität des Wahlsystems bei Parlamentswahlen nach Kanton (", 
                selected_year_min, "-", selected_year_max, ")", sep = "")
        } else if(selected_institution == "proporz3reg") {
          paste("Zeittrend der Proportionalität der Wahlsysteme bei Parlaments- und Regierungswahlen nach Kanton (", 
                selected_year_min, "-", selected_year_max, ")", sep = "")
        } else if(selected_institution == "reg_proporz") {
          paste("Zeittrend des Wahlverfahrens bei Regierungsratswahlen nach Kanton (", 
                selected_year_min, "-", selected_year_max, ")", sep = "")
        } else {
          paste("Zeittrend von ", selected_institution, " nach Kanton (", 
                selected_year_min, "-", selected_year_max, ")", sep = "")
        }
        
        # Customize y-axis label based on selected institution
        y_label <- if(selected_institution == "sitzparl") {
          "Parlamentssitze"
        } else if(selected_institution == "sitzreg") {
          "Regierungssitze"
        } else if(selected_institution == "proporz") {
          "Proporzwahl"
        } else if(selected_institution == "parlegisl") {
          "Amtsdauer in Jahren"
        } else if(selected_institution == "gallagher") {
          "Disproportionalitätsindex"
        } else if(selected_institution == "proporz4") {
          "Proportionalitätsindex"
        } else if(selected_institution == "proporz3reg") {
          "Proportionalitätsindex"
        } else if(selected_institution == "reg_proporz") {
          "Wahlverfahren (0=Majorz, 1=Proporz)"
        } else {
          selected_institution
        }
        
        p <- ggplot(plot_data, aes_string(x = year_col, y = "value", color = canton_col, group = canton_col)) +
          geom_line(linewidth = 1.2) +
          geom_point() +
          theme_minimal() +
          labs(
            title = plot_title,
            x = "Jahr",
            y = y_label,
            color = "Kanton"  # Label for the legend
          ) +
          theme(
            legend.position = "right",
            legend.title = element_text(face = "bold"),
            plot.title = element_text(size = 14, face = "bold")
          )
      }
      
      # If we have a narrow time range, adjust the x-axis to show all years
      if (selected_year_max - selected_year_min < 10) {
        p <- p + scale_x_continuous(breaks = seq(selected_year_min, selected_year_max, by = 1))
      }
      
      ggplotly(p)
    })
    
    # Canton comparison plot
    output$canton_comparison_plot <- renderPlotly({
      req(filtered_data())
      req(input$institutions)
      req(input$selected_year)
      
      data <- filtered_data()
      
      # Make sure we have data to plot
      if (nrow(data) == 0) {
        return(plot_ly() %>% 
                 layout(title = "Keine Daten für die ausgewählten Filter verfügbar"))
      }
      
      # Find the appropriate canton column - specifically look for "kanton" which contains the labels (ZH, BE, etc.)
      # rather than canton numbers
      canton_col <- names(data)[grep("^kanton$", names(data), ignore.case = TRUE)[1]]
      if (is.na(canton_col)) {
        # Fallback to any column with "canton" in the name
        canton_col <- names(data)[grep("canton|kanton", names(data), ignore.case = TRUE)[1]]
      }
      
      year_col <- names(data)[grep("year|jahr", names(data), ignore.case = TRUE)[1]]
      
      # Get the selected year
      selected_year <- input$selected_year
      
      # Get the selected institution
      selected_institution <- input$institutions
      
      # Data should already be filtered by the year in filtered_data()
      # But let's make sure we're only using data from the selected year
      data <- data %>% filter(!!sym(year_col) == selected_year)
      
      # Get unique cantons in the filtered data
      unique_cantons <- unique(data[[canton_col]])
      
      # Debug message
      print(paste("Number of cantons in filtered data:", length(unique_cantons)))
      print(paste("Cantons:", paste(unique_cantons, collapse = ", ")))
      print(paste("Selected year:", selected_year))
      print(paste("Selected institution:", selected_institution))
      
      # Ensure gallagher is numeric if it's the selected institution
      if(selected_institution == "gallagher" && "gallagher" %in% names(data)) {
        data$gallagher <- as.numeric(data$gallagher)
      }
      
      # Now prepare the data for plotting, ensuring we don't lose any canton
      # For a single year, we don't need to calculate averages
      plot_data <- data %>%
        group_by(!!sym(canton_col)) %>%
        summarize(value = first(!!sym(selected_institution)), .groups = "drop")
      
      # Print the actual values for verification
      print("Values used for plotting:")
      print(plot_data)
      
      # Ensure we have data after aggregation
      if (nrow(plot_data) == 0) {
        return(plot_ly() %>% 
                 layout(title = "Keine Daten zum Anzeigen nach Aggregation"))
      }
      
      # Check if the selected institution is one of our known variables
      is_sitzparl <- selected_institution == "sitzparl"
      is_sitzreg <- selected_institution == "sitzreg"
      is_proporz <- selected_institution == "proporz"
      is_parlegisl <- selected_institution == "parlegisl"
      is_gallagher <- selected_institution == "gallagher"
      is_proporz4 <- selected_institution == "proporz4"
      is_proporz3reg <- selected_institution == "proporz3reg"
      is_reg_proporz <- selected_institution == "reg_proporz"
      
      # Set plot title based on the selected institution
      plot_title <- if(is_sitzparl) {
        paste("Anzahl Parlamentssitze pro Kanton (", selected_year, ")", sep = "")
      } else if(is_sitzreg) {
        paste("Anzahl Sitze in der Regierung pro Kanton (", selected_year, ")", sep = "")
      } else if(is_proporz) {
        paste("Proporzwahl nach Kanton (", selected_year, ")", sep = "")
      } else if(is_parlegisl) {
        paste("Amtsdauer des Parlaments nach Kanton (", selected_year, ")", sep = "")
      } else if(is_gallagher) {
        paste("Disproportionalität des Wahlsystems nach Kanton (", selected_year, ")", sep = "")
      } else if(is_proporz4) {
        paste("Proportionalität des Wahlsystems bei Parlamentswahlen nach Kanton (", selected_year, ")", sep = "")
      } else if(is_proporz3reg) {
        paste("Proportionalität der Wahlsysteme bei Parlaments- und Regierungswahlen nach Kanton (", selected_year, ")", sep = "")
      } else if(is_reg_proporz) {
        paste("Wahlverfahren bei Regierungsratswahlen nach Kanton (", selected_year, ")", sep = "")
      } else {
        paste("Wert von ", selected_institution, " nach Kanton (", selected_year, ")", sep = "")
      }
      
      # Sort cantons by the value for better visualization (highest to lowest)
      plot_data <- plot_data %>%
        arrange(desc(value))
      
      # Make canton a factor with levels ordered by the value to control the order in the plot
      # This ensures cantons are displayed from highest to lowest value (left to right)
      plot_data[[canton_col]] <- factor(
        plot_data[[canton_col]], 
        levels = plot_data[[canton_col]]
      )
      
      # Format the values for display
      # For integer-like values, show as integers
      # For decimal values, round to 1 decimal place
      plot_data$label_text <- if(is_sitzparl || is_sitzreg || is_proporz || is_parlegisl || is_reg_proporz) {
        as.character(round(plot_data$value))
      } else {
        format(round(plot_data$value, 1), nsmall = 1)
      }
      
      # Calculate a y-offset for the labels (5% above the bar height)
      max_value <- max(plot_data$value, na.rm = TRUE)
      y_offset <- max_value * 0.05  # 5% of the maximum value
      
      # Set y-axis label based on the selected institution
      y_axis_label <- if(is_sitzparl) {
        "Parlamentssitze"
      } else if(is_sitzreg) {
        "Regierungssitze"
      } else if(is_proporz) {
        "Proporzwahl"
      } else if(is_parlegisl) {
        "Amtsdauer in Jahren"
      } else if(is_gallagher) {
        "Disproportionalitätsindex"
      } else if(is_proporz4) {
        "Proportionalitätsindex"
      } else if(is_proporz3reg) {
        "Proportionalitätsindex"
      } else if(is_reg_proporz) {
        "Wahlverfahren (0=Majorz, 1=Proporz)"
      } else {
        selected_institution
      }
      
      # Create the bar chart with vertical bars
      p <- ggplot(plot_data, aes_string(x = canton_col, y = "value")) +
        geom_bar(stat = "identity", fill = "#2C3E50") +
        # Add value labels on top of each bar with absolute positioning
        geom_text(
          aes(label = label_text, 
              y = value + y_offset),  # Position above the bar with fixed offset
          size = 3.5,    # Text size
          color = "#2C3E50"  # Text color matching the bars
        ) +
        # Add extra space at the top of the plot to accommodate labels
        scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +  # Increased expansion
        theme_minimal() +
        labs(
          title = plot_title,
          x = "Kanton",
          y = y_axis_label
        ) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          plot.title = element_text(size = 14, face = "bold"),
          plot.margin = margin(t = 30, r = 10, b = 10, l = 10, unit = "pt")  # Add extra margin at the top
        )
      
      # Convert to plotly for interactivity
      ggplotly(p) %>%
        layout(
          xaxis = list(
            title = "Kanton",
            categoryorder = "array",
            categoryarray = levels(plot_data[[canton_col]]),
            tickangle = -45
          ),
          # Add more margin at the top to accommodate the labels
          margin = list(t = 100, b = 100)
        )
    })
    
    # Data table
    output$data_table <- renderDT({
      req(filtered_data())
      req(input$institutions)
      
      data <- filtered_data()
      # Select relevant columns for the table
      canton_col <- names(data)[grep("^kanton$|canton|kanton", names(data), ignore.case = TRUE)[1]]
      year_col <- names(data)[grep("year|jahr", names(data), ignore.case = TRUE)[1]]
      
      # Get the selected institution
      selected_institution <- input$institutions
      
      table_data <- data %>%
        select(!!sym(canton_col), !!sym(year_col), !!sym(selected_institution))
      
      # Rename the column for better display
      if (selected_institution == "sitzparl") {
        names(table_data)[3] <- "Parlamentssitze"
      } else if (selected_institution == "sitzreg") {
        names(table_data)[3] <- "Regierungssitze"
      } else if (selected_institution == "proporz") {
        names(table_data)[3] <- "Proporzwahl"
      } else if (selected_institution == "parlegisl") {
        names(table_data)[3] <- "Amtsdauer des Parlaments in Jahren"
      } else if (selected_institution == "gallagher") {
        names(table_data)[3] <- "Effektive Disproportionalität des Wahlsystems"
      } else if (selected_institution == "proporz4") {
        names(table_data)[3] <- "Proportionalität des Wahlsystems bei Parlamentswahlen"
      } else if (selected_institution == "proporz3reg") {
        names(table_data)[3] <- "Proportionalität der Wahlsysteme bei Parlaments- und Regierungswahlen"
      } else if (selected_institution == "reg_proporz") {
        names(table_data)[3] <- "Wahlverfahren bei Regierungsratswahlen (Dummy)"
      }
      
      datatable(
        table_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'ftip'
        ),
        rownames = FALSE,
        class = "compact stripe"
      )
    })
    
    # Correlation plot - replaced with a time series comparison
    output$correlation_plot <- renderPlotly({
      req(filtered_data())
      req(input$institutions)
      
      data <- filtered_data()
      year_col <- names(data)[grep("year|jahr", names(data), ignore.case = TRUE)[1]]
      
      # Get the selected institution
      selected_institution <- input$institutions
      
      # Ensure gallagher is numeric if it's the selected institution
      if(selected_institution == "gallagher" && "gallagher" %in% names(data)) {
        data$gallagher <- as.numeric(data$gallagher)
      }
      
      # Find other relevant variables to correlate with
      potential_correlates <- c(
        "sitzparl", "sitzreg", "proporz", "parlegisl", "gallagher", "proporz4", "proporz3reg", "reg_proporz",
        "turnout_e", "turnout_v", "rae", "partfrakt", "parl_party", 
        "volatilitaet_se", "volatilitaet_vo", "parl_left", "parl_cent", "parl_right"
      )
      
      # Remove the selected institution from potential correlates
      potential_correlates <- setdiff(potential_correlates, selected_institution)
      
      # Filter to only those that exist in the data
      correlates <- intersect(potential_correlates, names(data))
      
      # Ensure gallagher is numeric if it's in the correlates
      if("gallagher" %in% correlates) {
        data$gallagher <- as.numeric(data$gallagher)
      }
      
      # If we have at least one correlate, create a scatter plot
      if (length(correlates) > 0) {
        # Choose the first correlate
        correlate <- correlates[1]
        
        # Set title and axis labels based on the selected variables
        x_label <- if(selected_institution == "sitzparl") {
          "Parlamentssitze"
        } else if(selected_institution == "sitzreg") {
          "Regierungssitze"
        } else if(selected_institution == "proporz") {
          "Proporzwahl"
        } else if(selected_institution == "parlegisl") {
          "Amtsdauer des Parlaments in Jahren"
        } else if(selected_institution == "gallagher") {
          "Effektive Disproportionalität des Wahlsystems"
        } else if(selected_institution == "proporz4") {
          "Proportionalität des Wahlsystems bei Parlamentswahlen"
        } else if(selected_institution == "proporz3reg") {
          "Proportionalität der Wahlsysteme bei Parlaments- und Regierungswahlen"
        } else if(selected_institution == "reg_proporz") {
          "Wahlverfahren bei Regierungsratswahlen (Dummy)"
        } else {
          selected_institution
        }
        
        y_label <- if(correlate == "sitzparl") {
          "Parlamentssitze"
        } else if(correlate == "sitzreg") {
          "Regierungssitze"
        } else if(correlate == "proporz") {
          "Proporzwahl"
        } else if(correlate == "parlegisl") {
          "Amtsdauer des Parlaments in Jahren"
        } else if(correlate == "gallagher") {
          "Effektive Disproportionalität des Wahlsystems"
        } else if(correlate == "proporz4") {
          "Proportionalität des Wahlsystems bei Parlamentswahlen"
        } else if(correlate == "proporz3reg") {
          "Proportionalität der Wahlsysteme bei Parlaments- und Regierungswahlen"
        } else if(correlate == "reg_proporz") {
          "Wahlverfahren bei Regierungsratswahlen (Dummy)"
        } else {
          correlate
        }
        
        # Create scatter plot
        p <- ggplot(data, aes_string(x = selected_institution, y = correlate)) +
          geom_point(alpha = 0.7, color = "#2C3E50") +
          geom_smooth(method = "lm", color = "#E74C3C") +
          theme_minimal() +
          labs(
            title = paste("Korrelation zwischen", x_label, "und", y_label),
            x = x_label,
            y = y_label
          )
        
        ggplotly(p)
      } else {
        # If no correlates available, show a time trend
        plot_data <- data %>%
          group_by(!!sym(year_col)) %>%
          summarize(value = mean(!!sym(selected_institution), na.rm = TRUE), .groups = "drop")
        
        # Set title and y-axis label
        plot_title <- if(selected_institution == "sitzparl") {
          "Zeittrend der Parlamentssitze"
        } else if(selected_institution == "sitzreg") {
          "Zeittrend der Regierungssitze"
        } else if(selected_institution == "proporz") {
          "Zeittrend der Proporzwahl"
        } else if(selected_institution == "parlegisl") {
          "Zeittrend der Amtsdauer des Parlaments"
        } else if(selected_institution == "gallagher") {
          "Zeittrend der Disproportionalität des Wahlsystems"
        } else if(selected_institution == "proporz4") {
          "Zeittrend der Proportionalität des Wahlsystems bei Parlamentswahlen"
        } else if(selected_institution == "proporz3reg") {
          "Zeittrend der Proportionalität der Wahlsysteme bei Parlaments- und Regierungswahlen"
        } else if(selected_institution == "reg_proporz") {
          "Zeittrend des Wahlverfahrens bei Regierungsratswahlen"
        } else {
          paste("Zeittrend von", selected_institution)
        }
        
        y_label <- if(selected_institution == "sitzparl") {
          "Parlamentssitze"
        } else if(selected_institution == "sitzreg") {
          "Regierungssitze"
        } else if(selected_institution == "proporz") {
          "Proporzwahl"
        } else if(selected_institution == "parlegisl") {
          "Amtsdauer in Jahren"
        } else if(selected_institution == "gallagher") {
          "Disproportionalitätsindex"
        } else if(selected_institution == "proporz4") {
          "Proportionalitätsindex"
        } else if(selected_institution == "proporz3reg") {
          "Proportionalitätsindex"
        } else if(selected_institution == "reg_proporz") {
          "Wahlverfahren (0=Majorz, 1=Proporz)"
        } else {
          selected_institution
        }
        
        # Create time trend plot
        p <- ggplot(plot_data, aes_string(x = year_col, y = "value")) +
          geom_line(linewidth = 1.2, color = "#2C3E50") +
          geom_point(color = "#2C3E50") +
          theme_minimal() +
          labs(
            title = plot_title,
            x = "Jahr",
            y = y_label
          )
        
        ggplotly(p)
      }
    })
    
    # Statistical summary
    output$statistical_summary <- renderPrint({
      req(filtered_data())
      req(input$institutions)
      
      data <- filtered_data()
      
      # Get the selected institution
      selected_institution <- input$institutions
      
      # Calculate summary statistics for the selected institution
      summary_stats <- summary(data[[selected_institution]])
      
      # Print summary
      cat("Zusammenfassung für ", selected_institution, ":\n\n", sep = "")
      print(summary_stats)
      
      # Add additional statistics
      cat("\nStandardabweichung: ", sd(data[[selected_institution]], na.rm = TRUE), "\n")
      cat("Anzahl der Beobachtungen: ", sum(!is.na(data[[selected_institution]])), "\n")
      cat("Anzahl der fehlenden Werte: ", sum(is.na(data[[selected_institution]])), "\n")
    })
  })
} 