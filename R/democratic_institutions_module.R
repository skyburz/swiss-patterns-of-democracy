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
          card_header("Quellen & Daten"),
          card_body(
            style = "min-height: 600px; padding: 15px;",  # Increased minimum height
            tabsetPanel(
              tabPanel(
                "Quelle(n)",
                div(style = "overflow-y: auto; height: 100%;",
                    uiOutput(ns("source_info"))
                )
              ),
              tabPanel(
                "Daten",
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
    
    # Function to get consistent canton colors
    get_canton_colors <- function() {
      # Define custom colors for each canton
      canton_colors <- c(
        "ZH" = "#0038A8", # Blue
        "BE" = "#E30613", # Red
        "LU" = "#78B7E7", # Light Blue
        "UR" = "#FFCC00", # Yellow
        "SZ" = "#C8102E", # Dark Red
        "OW" = "#9D2235", # Crimson
        "NW" = "#7E2B3F", # Burgundy
        "GL" = "#000000", # Black
        "ZG" = "#002F6C", # Navy Blue
        "FR" = "#7F7F7F", # Gray
        "SO" = "#964B00", # Brown
        "BS" = "#001E62", # Dark Blue
        "BL" = "#E50075", # Pink/Magenta
        "SH" = "#FFD700", # Gold
        "AR" = "#C0C0C0", # Silver Gray
        "AI" = "#404040", # Dark Gray
        "SG" = "#009A44", # Green
        "GR" = "#BBBBBB", # Light Gray
        "AG" = "#008080", # Teal/Turquoise
        "TG" = "#006400", # Dark Green
        "TI" = "#FF6600", # Orange
        "VD" = "#89CF00", # Lime Green
        "VS" = "#9B111E", # Ruby Red
        "NE" = "#046A38", # Emerald Green
        "GE" = "#DA9100", # Yellow-Gold
        "JU" = "#6A0DAD"  # Purple
      )
      
      return(canton_colors)
    }
    
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
      
      # Specifically make sure we're using the column with abbreviations (ZH, BE, etc.), not numbers
      print(paste("Using canton column:", canton_col))
      # Check a sample of values to confirm we have abbreviations
      print(paste("Sample values from canton column:", paste(head(unique(data[[canton_col]])), collapse=", ")))
      
      # If we accidentally got kantonnr instead of kanton, fix it
      if (is.numeric(data[[canton_col]]) || 
          (is.character(data[[canton_col]]) && 
           grepl("^\\d+$", data[[canton_col]][1]))) {
        print("WARNING: Canton column contains numbers, switching to kanton column with abbreviations")
        # Find the actual kanton column with abbreviations
        potential_cols <- names(data)[grep("kanton", names(data), ignore.case = TRUE)]
        for (col in potential_cols) {
          # Check if this column has text abbreviations
          if (is.character(data[[col]]) && nchar(data[[col]][1]) == 2) {
            canton_col <- col
            print(paste("Switched to column:", canton_col))
            break
          }
        }
      }
      
      # Store the canton column name for later use
      canton_col_name(canton_col)
      print(paste("Detected canton column:", canton_col))
      
      if (!is.na(canton_col)) {
        # Get unique canton labels
        cantons <- sort(unique(data[[canton_col]]))
        print(paste("Available cantons:", paste(cantons, collapse = ", ")))
        
        # Convert canton abbreviations to full names using canton_reference
        # First source the canton reference if not already done
        source("R/canton_reference.R")
        
        # Create a mapping of abbreviations to full German names
        canton_full_names <- sapply(cantons, function(abbr) {
          name <- get_canton_name(abbr, "de")
          if(is.na(name)) return(abbr) # If not found, keep the original value
          return(name)
        })
        
        # Sort by full German names
        sorted_indices <- order(canton_full_names)
        sorted_cantons <- cantons[sorted_indices]
        sorted_names <- canton_full_names[sorted_indices]
        
        # Create named vector for choices
        choices <- setNames(sorted_cantons, sorted_names)
        
        # Update the canton checkbox group with named choices
        updateCheckboxGroupInput(
          session, 
          "canton_select", 
          choices = choices,
          selected = choices[1:3]  # Pre-select first 3 cantons for convenience
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
          geom_line(linewidth = 1.2, color = "#1b6d80") +
          geom_point(color = "#1b6d80") +
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
          "Proportionalität des Wahlsystems bei Parlamentswahlen"
        } else if(selected_institution == "proporz3reg") {
          "Proportionalität der Wahlsysteme bei Parlaments- und Regierungswahlen"
        } else if(selected_institution == "reg_proporz") {
          "Wahlverfahren (0=Majorz, 1=Proporz)"
        } else {
          selected_institution
        }
        
        p <- ggplot(plot_data, aes_string(x = year_col, y = "value", color = canton_col, group = canton_col)) +
          geom_line(linewidth = 1.2) +
          geom_point() +
          theme_minimal() +
          scale_color_manual(values = get_canton_colors()) +  # Use custom canton colors
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
        geom_bar(stat = "identity", fill = "#1b6d80") +  # Use consistent color #1b6d80 for all bars
        # Add value labels on top of each bar with absolute positioning
        geom_text(
          aes(label = label_text, 
              y = value + y_offset),  # Position above the bar with fixed offset
          size = 3.5,    # Text size
          color = "#1b6d80"  # Text color matching the bars
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
      
      # Get the full dataset (not filtered by year)
      data <- selected_data()
      
      # Select relevant columns for the table
      canton_col <- names(data)[grep("^kanton$|canton|kanton", names(data), ignore.case = TRUE)[1]]
      year_col <- names(data)[grep("year|jahr", names(data), ignore.case = TRUE)[1]]
      
      # Get the selected institution
      selected_institution <- input$institutions
      
      # Get variable display name
      var_display_name <- if (selected_institution == "sitzparl") {
        "Parlamentssitze"
      } else if (selected_institution == "sitzreg") {
        "Regierungssitze"
      } else if (selected_institution == "proporz") {
        "Proporzwahl"
      } else if (selected_institution == "parlegisl") {
        "Amtsdauer des Parlaments in Jahren"
      } else if (selected_institution == "gallagher") {
        "Effektive Disproportionalität des Wahlsystems"
      } else if (selected_institution == "proporz4") {
        "Proportionalität des Wahlsystems bei Parlamentswahlen"
      } else if (selected_institution == "proporz3reg") {
        "Proportionalität der Wahlsysteme bei Parlaments- und Regierungswahlen"
      } else if (selected_institution == "reg_proporz") {
        "Wahlverfahren bei Regierungsratswahlen (Dummy)"
      } else {
        selected_institution
      }
      
      # Make sure we're using the canton column with abbreviations
      # Check if we have a column that explicitly contains abbreviations like "ZH", "BE", etc.
      abbr_col <- canton_col  # Default to the current canton column
      potential_cols <- names(data)[grep("kanton", names(data), ignore.case = TRUE)]
      for (col in potential_cols) {
        if (is.character(data[[col]]) && nchar(data[[col]][1]) == 2) {
          abbr_col <- col
          print(paste("Using column for abbreviations:", abbr_col))
          break
        }
      }
      
      # Filter data to include only relevant columns
      table_data <- data %>%
        select(!!sym(abbr_col), !!sym(year_col), !!sym(selected_institution)) %>%
        # Make sure data is ordered by canton and year
        arrange(!!sym(abbr_col), !!sym(year_col))
      
      # Get unique canton values
      cantons <- unique(table_data[[abbr_col]])
      
      # Source the canton reference for names
      source("R/canton_reference.R")
      
      # BFS numbers for each canton
      bfs_numbers <- c(
        "ZH" = 1, "BE" = 2, "LU" = 3, "UR" = 4, "SZ" = 5, 
        "OW" = 6, "NW" = 7, "GL" = 8, "ZG" = 9, "FR" = 10,
        "SO" = 11, "BS" = 12, "BL" = 13, "SH" = 14, "AR" = 15,
        "AI" = 16, "SG" = 17, "GR" = 18, "AG" = 19, "TG" = 20,
        "TI" = 21, "VD" = 22, "VS" = 23, "NE" = 24, "GE" = 25,
        "JU" = 26
      )
      
      # Add BFS number and German canton name
      table_data <- table_data %>%
        mutate(
          bfs_nr = sapply(!!sym(abbr_col), function(abbr) {
            if (abbr %in% names(bfs_numbers)) {
              return(bfs_numbers[abbr])
            } else {
              return(NA)
            }
          }),
          canton_name = sapply(!!sym(abbr_col), function(abbr) {
            name <- get_canton_name(abbr, "de")
            print(paste("Converting", abbr, "to", name))  # Debug output
            if(is.na(name)) return(abbr)
            return(name)
          }),
          canton_abbr = !!sym(abbr_col)  # Store the original abbreviation
        )
      
      # Print debugging info for canton abbreviations
      print("Canton abbreviations being used for Kanton-Kürzel column:")
      print(head(table_data$canton_abbr))
      print(unique(table_data$canton_abbr))
      
      # Print the first few rows to verify canton names are present
      print("Canton name conversion result:")
      print(head(table_data))
      
      # Rename the value column to the display name
      names(table_data)[names(table_data) == selected_institution] <- var_display_name
      
      # Select and reorder columns
      table_data <- table_data %>%
        select(canton_name, canton_abbr, !!sym(year_col), !!var_display_name)
      
      # Rename columns for better display
      names(table_data)[names(table_data) == "canton_name"] <- "Kanton"
      names(table_data)[names(table_data) == "canton_abbr"] <- "Kanton-Kürzel"
      names(table_data)[names(table_data) == year_col] <- "Jahr"
      
      # Show the ACTUAL final table structure being sent to the datatable
      print("FINAL TABLE STRUCTURE:")
      print(names(table_data))
      print("First few rows of FINAL TABLE:")
      print(head(table_data))
      
      # Create the datatable with customized options for one canton per page
      dt <- datatable(
        table_data,
        options = list(
          pageLength = 45,  # Show enough rows for all years for one canton
          scrollX = TRUE,
          scrollY = "500px", # Make it scrollable vertically
          dom = '<"top"ip>rt<"bottom"ip><"clear">',  # Add pagination controls at top and bottom
          ordering = FALSE,  # Disable sorting to maintain the order
          lengthChange = FALSE,  # Don't allow changing number of entries shown
          language = list(
            paginate = list(
              previous = "Vorheriger Kanton",
              "next" = "Nächster Kanton"
            ),
            info = ""  # Remove the info text
          ),
          rowGroup = list(
            dataSrc = 0,  # Group by Kanton column (index 0)
            emptyDataGroup = "No canton data available"
          ),
          columnDefs = list(
            list(width = "140px", targets = 0),  # ID
            list(width = "100px", targets = 1),  # Kanton-Kürzel
            list(width = "80px", targets = 2),   # Jahr
            list(width = "140px", targets = 3)   # Value column
          )
        ),
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center; font-size: 16px; font-weight: bold;',
          ''  # Removed "Kantonsdaten" text
        ),
        rownames = FALSE,
        class = "display compact"  # No filter class
      )
      
      # Return the datatable
      dt
    })
    
    # Source information lookup
    output$source_info <- renderUI({
      req(filtered_data())
      req(input$institutions)
      
      # Get the selected institution
      selected_institution <- input$institutions
      
      # Function to search for variables in documentation
      find_variable_info <- function(variable_name) {
        # Try to load the structured documentation first
        structured_doc_path <- "data/structured_variable_documentation.csv"
        if (file.exists(structured_doc_path)) {
          structured_doc <- read.csv(structured_doc_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
          
          # Look for exact match of variable name
          var_index <- which(structured_doc$Variable == variable_name)
          
          if (length(var_index) > 0) {
            # Return the structured information
            return(list(
              source = structured_doc$Quelle.n.[var_index[1]],
              remarks = structured_doc$Bemerkungen[var_index[1]]
            ))
          }
        }
        
        # If the variable isn't found in the structured documentation or the file doesn't exist,
        # fall back to the specific variables defined below
        
        # Special handling for sitzparl with exact information
        if (variable_name == "sitzparl") {
          return(list(
            source = paste("Für 1979–2008:",
                          "BFS (diverse Jahrgänge),",
                          "APS (diverse Jahrgänge).",
                          "\n\nFür 2009–2023:",
                          "BFS: Kantonale Parlamentswahlen:",
                          "Mandatsverteilung nach Parteien und Kanton",
                          "(BFS-Nummer: je-d-17.02.05.01.03)",
                          "https://www.bfs.admin.ch/asset/de/24385274",
                          "(zuletzt heruntergeladen am: 16.05.2023)", sep = "\n"),
            remarks = paste("Anzahl Sitze im Parlament",
                           "\n\nHinweise:",
                           "Angaben und deren Änderungen gehen immer vom relevanten Wahljahr/-tag und",
                           "nicht der Legislaturperiode aus. Bezugsrahmen ist somit immer der Wahlzyklus.",
                           "\n\nBsp. AG: Das Wahljahr 2012 stimmt nicht mit der üblichen Legislaturperiode von vier",
                           "Jahren überein. So wurde 2009 eine Gesamterneuerungswahl durchgeführt, wobei",
                           "das darauffolgende Wahljahr bereits drei Jahre später war (2012 anstelle 2013). Die",
                           "Wahlen wurden demnach ab dem Jahr 2012 jeweils im Vorjahr der neuen",
                           "Legislaturperiode abgehalten.", sep = "\n")
          ))
        }
        
        # Special handling for sitzreg
        if (variable_name == "sitzreg") {
          return(list(
            source = paste("Für 1979–2008:",
                           "BFS (diverse Jahrgänge),",
                           "APS (diverse Jahrgänge).",
                           "\n\nFür 2009–2023:",
                           "BFS: Kantonale Regierungswahlen:",
                           "Mandatsverteilung nach Parteien und Kanton",
                           "(BFS-Nummer: je-d-17.02.06.01)",
                           "https://www.bfs.admin.ch/asset/de/24385222",
                           "(zuletzt heruntergeladen am: 16.05.2023)", sep = "\n"),
            remarks = paste("Anzahl Sitze in der Regierung",
                            "\n\nHinweise:",
                            "Der Regierungsrat besteht in allen Kantonen aus 5 oder 7 Mitgliedern, nur Kantone",
                            "ZH, BE und VD haben 9 Mitglieder, TI und NE waren bis 1995 bzw. 2005 bei 7;",
                            "VS bis 2009 bei 5, jetzt 7. Seit 2014 hat Appenzell Ausserrhoden nur noch 5 statt",
                            "7 Regierungsratsmitglieder.", sep = "\n")
          ))
        }
        
        # Special handling for proporz
        if (variable_name == "proporz") {
          return(list(
            source = paste("Für 1979–2008:",
                           "Eigene Erhebungen in Zusammenhang mit Vatter et al. (2012). Insbesondere",
                           "Lutz (2004: 281ff.); Kantonsparlamente.ch (2008: Zeile 1.4.5); für GR und AI: Huber/",
                           "Huber-Schlatter (1987: 170, 183); für Zusatzinfos: kantonale Verfassungen und",
                           "Parlamentsgesetze. AR: Mitte 1996 (APS) genaues Datum wohl 1.7.1995 (KV).",
                           "\n\nFür 2009–2023:",
                           "Kontrollen anhand von BFS oder APS sowie kantonale Verfassungen,",
                           "Parlamentsgesetze und Webseiten.", sep = "\n"),
            remarks = paste("Proporzwahl",
                           "\n\nKategorien:",
                           "1 = Alle Sitze per Proporz",
                           "0 = Mindestens ein Wahlkreis wählt mit Majorz",
                           "\n\nHinweise:",
                           "Differenzierte Zahlen (entsprechend dem Anteil Proporzmandate an allen Mandaten)",
                           "wären wünschbar, werden hier aber nicht vorgenommen, weil Zahlen zur Verteilung",
                           "von Proporz- und Majorzsitzen schwer auffindbar sind. Die Zahlen entsprechen dem",
                           "Zustand nach den letzten Gesamterneuerungswahlen.", sep = "\n")
          ))
        }
        
        # Special handling for parlegisl
        if (variable_name == "parlegisl") {
          return(list(
            source = paste("Für 1979–2008:",
                           "1998: Lutz/Strohmann (1998: 64)",
                           "2008: Kantonsparlamente.ch (2008: Zeile 1.4.1)",
                           "1986: Moser (1987: 44)",
                           "Verfassungen für AR, AI",
                           "\n\nFür 2009–2023:",
                           "BFS: Kantonale Parlamentswahlen:",
                           "Mandatsverteilung nach Parteien und Kanton",
                           "(BFS-Nummer: je-d-17.02.05.01.03)",
                           "https://www.bfs.admin.ch/asset/de/24385274",
                           "(zuletzt heruntergeladen am: 16.05.2023)", sep = "\n"),
            remarks = paste("Amtsdauer des Parlaments in Jahren", sep = "\n")
          ))
        }
        
        # Special handling for gallagher
        if (variable_name == "gallagher") {
          return(list(
            source = paste("Für 1979–2008:",
                           "Berechnet aus den vom BFS angegebenen Wähler- und Sitzanteilen durch",
                           "Vatter et al. (2012).",
                           "\n\nFür 2009–2023:",
                           "Berechnet aus den vom BFS angegebenen Wähler- und Sitzanteilen.",
                           "\n\nFormel: Gallagher = Wurzel aus [1/2 * Summe((vi-si)^2)]",
                           "vi = Wähleranteil der Partei i, si = Sitzanteil der Partei i", sep = "\n"),
            remarks = paste("Effektive Disproportionalität des Wahlsystems (Gallagher-Index)",
                           "\n\nBedeutung:",
                           "Der Gallagher-Index misst die Diskrepanz zwischen Wähleranteilen und Sitzanteilen.",
                           "Je höher der Indexwert, desto höher die Diskrepanz zwischen Stimm- und Sitzanteilen",
                           "der verschiedenen Parteien (d.h. desto disproportionaler das Wahlsystem).",
                           "\n\nFür Kantone mit Majorzsystem (GR, AI, AR; UR bis 1988, OW bis 1985, NW bis",
                           "1981; Teile von ZG - siehe entsprechende Bemerkungen bei _parl_v) liegen keine",
                           "Wähleranteile vor, daher kann auch kein entsprechender Wert für den",
                           "Disproportionalitätsindex berechnet werden.",
                           "\n\nFür Kantone mit Mischlisten (ZH, BS, GR, TG, VD, VS, GE): Bis und mit Ausgabe",
                           "2010 der BFS-Tabellen wurden Mischlisten bei der Berechnung der Wähleranteile nicht",
                           "auf einzelne Parteien aufgeteilt und der Gallagher-Index wurde auf der Basis",
                           "summierter Parteistärken (bei den «Übrigen» aufsummiert) und zugeordneter",
                           "Sitzanteile mit SPSS berechnet (durch Vatter et al., 2012). Ab 2012 stellt BFS diese",
                           "Zuteilung zur Verfügung (siehe oben bei [Partei(gruppe)]_parl_v).", sep = "\n")
          ))
        }
        
        # Special handling for proporz4
        if (variable_name == "proporz4") {
          return(list(
            source = paste("Für 1979–2008:",
                           "Eigene Konstruktion.",
                           "\n\nFür 2009–2023:",
                           "Eigene Konstruktion.",
                           "\n\nFormel: proporz4 = (1-proporz)*(1-gallagher)*0 + proporz*gallagher*1 +",
                           "proporz*(1-gallagher)*2 + (1-proporz)*gallagher*3", sep = "\n"),
            remarks = paste("Proportionalität des Wahlsystems bei Parlamentswahlen",
                           "\n\nOperationalisierung:",
                           "proporz4 ist eine Multiplikation der beiden Variablen proporz (Proporzwahlsystem ja/",
                           "nein, d.h. 1/0) und (1-gallagher), wobei letztere auf einen Wertebereich von 0 bis 1",
                           "standardisiert wurde und zur Anpassung an die Skalenrichtung von «1-gallagher»",
                           "kodiert ist.",
                           "\n\nDie Variable proporz4 nimmt somit folgendermassen operationalisierte Werte an:",
                           "0 = Majorz mit hoher Disproportionalität: Majorz führt formal zu hoher Disproportionalität",
                           "1 = Proporz mit hoher Disproportionalität: Formales Proporzwahlsystem, aber mit hoher",
                           "    effektiver Disproportionalität, z.B. aufgrund kleiner Wahlkreise",
                           "2 = Proporz mit tiefer Disproportionalität: Formales Proporzwahlsystem mit tiefer",
                           "    effektiver Disproportionalität, d.h. hoher Konkordanz zwischen Stimmen- und",
                           "    Sitzanteilen",
                           "3 = Majorz mit tiefer Disproportionalität: In der Praxis nicht zu beobachten", sep = "\n")
          ))
        }
        
        # Special handling for reg_proporz
        if (variable_name == "reg_proporz") {
          return(list(
            source = paste("1987: ",
                           "Moser (1987)",
                           "\n\n1998: ",
                           "Lutz/Strohmann (1998: 29)",
                           "\n\n2004: ",
                           "Bochsler et al. (2004: 51)",
                           "\n\n2007: ",
                           "Bochsler/Goridis (2009), Stand des Datensatzes im Dezember 2008)",
                           "\n\nRest, 1979–2008: ",
                           "extra-/intrapoliert",
                           "\n\n2009–2012: ",
                           "Kantonsverfassungen",
                           "\n\n2013–2018: ",
                           "Flick Witzig (2019)",
                           "\n\n2019–2023: ",
                           "Kantonsverfassungen", sep = ""),
            remarks = paste("Wahlverfahren bei Regierungsratswahlen (Dummy)",
                           "\n\nKategorien:",
                           "0 = Majorz",
                           "1 = Proporz",
                           "\n\nHinweise:",
                           "GE bis 2012: 1/3 der gültigen Stimmen im 1. Wahlgang reichen bereits für einen Sitzgewinn aus («majorité qualifiée»; siehe Art. 96 und Art. 98, «Loi sur l'exercice des droits politiques (LEDP)», in Kraft seit 1983; Kölz 1987: 3–4) mit 0.2 codiert.", sep = "\n")
          ))
        }
        
        # Special handling for proporz3reg
        if (variable_name == "proporz3reg") {
          return(list(
            source = paste("1979–1982: ", 
                           "Vatter (2002: 119f.); Kantonsverfassungen.",
                           "\n\nVatter et al. (2004), Kantonsverfassungen.",
                           "\n\nOW, NW und UR korrigiert gemäss Vatter (2002: 119) und APS 1992 (zu UR).",
                           "\n\nKantonsverfassungen",
                           "\n\nFlick Witzig (2019)",
                           "\n\nKantonsverfassungen", sep = ""),
            remarks = paste("Proportionalitätsgrad der Wahlsysteme bei Parlaments- und Regierungsratswahlen",
                           "\n\nKategorien: Kombinierter Indikator mit Maximalwert von sechs Punkten mit...",
                           "\n\n(a) maximal drei Punkte für Regierungsratswahlen:",
                           "\n0 = für Majorz mit erforderlichem Mehr von 50 Prozent im 1. Wahlgang.",
                           "\n1 = für Majorz mit erforderlichem Mehr von 33.3 Prozent im 1. Wahlgang (GE)",
                           "\n3 = für Proporz (ZG, TI)",
                           "\n\n(b) maximal drei Punkten für Parlamentswahlen:",
                           "\n0 = reiner Majorz.",
                           "\n2 = gemischt mit relativem Mehr",
                           "\n3 = reiner Proporz", sep = "")
          ))
        }
        
        # Fallback to the regular search method
        result <- list(
          source = "Keine Quelleninformation gefunden",
          remarks = "Keine Bemerkungen verfügbar"
        )
        
        # Try to gather information from multiple sources for completeness
        sources_info <- c()
        remarks_info <- c()
        
        # Try to read variable documentation from CSV
        var_doc_path <- "data/variable_documentation.csv"
        if (file.exists(var_doc_path)) {
          tryCatch({
            var_doc <- read.csv(var_doc_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
            
            # Look for the variable name (exact match)
            var_index <- which(var_doc$Indikator == variable_name)
            
            if (length(var_index) > 0) {
              # Store the source and remarks
              sources_info <- c(sources_info, paste("Aus variable_documentation.csv:", var_doc$Quellen[var_index[1]]))
              remarks_info <- c(remarks_info, paste("Aus variable_documentation.csv:", var_doc$Bemerkungen[var_index[1]]))
            }
            
            # If not found by exact match, try partial match
            if (length(var_index) == 0) {
              var_index <- which(grepl(variable_name, var_doc$Indikator, fixed = TRUE))
              
              if (length(var_index) > 0) {
                # Store the source and remarks
                sources_info <- c(sources_info, paste("Aus variable_documentation.csv (Partielle Übereinstimmung):", var_doc$Quellen[var_index[1]]))
                remarks_info <- c(remarks_info, paste("Aus variable_documentation.csv (Partielle Übereinstimmung):", var_doc$Bemerkungen[var_index[1]]))
              }
            }
          }, error = function(e) {
            print(paste("Error reading variable documentation:", e$message))
          })
        }
        
        # Try PDF extracted text (primary source for complete information)
        pdf_text_path <- "data/pdf_extracted_text.txt"
        if (file.exists(pdf_text_path)) {
          tryCatch({
            pdf_text <- readLines(pdf_text_path, warn = FALSE, encoding = "UTF-8")
            
            # Find all instances of the variable in the text
            var_lines <- grep(paste0("\\b", variable_name, "\\b"), pdf_text)
            
            if (length(var_lines) > 0) {
              # Use the first match as the primary instance
              var_line <- var_lines[1]
              
              # First look for section headers to identify the current section
              # We'll search backward from the found line to find section headers
              section_start <- max(1, var_line - 100)  # Look up to 100 lines back
              
              # Look for section headers - usually indicator, source, remarks columns
              header_pattern <- "Indikator.*Quelle.*Bemerkungen"
              header_lines <- grep(header_pattern, pdf_text[section_start:var_line], ignore.case = TRUE)
              
              if (length(header_lines) > 0) {
                # Found a header, use this as our starting point
                section_start <- section_start + header_lines[length(header_lines)] - 1
              }
              
              # Now, look forward to find the next variable or section
              section_end <- min(length(pdf_text), var_line + 150)  # Look up to 150 lines forward
              
              # Extract the entire section
              section_text <- pdf_text[section_start:section_end]
              
              # Search for the start of the variable description
              var_start_idx <- grep(paste0("\\b", variable_name, "\\b"), section_text)[1]
              
              if (!is.na(var_start_idx)) {
                # Now find where the next variable starts (to determine end of current variable)
                # Typical patterns for variable declarations:
                # 1. Variable names at start of line
                # 2. Often followed by sources and remarks
                
                # Look for lines that start with word characters followed by spaces and then "Für" or other patterns
                # that indicate the start of a new variable section
                next_var_pattern <- "^\\s*\\w+.*\\s+(Für|BFS|APS)"
                next_var_lines <- grep(next_var_pattern, section_text[(var_start_idx+1):length(section_text)])
                
                var_end_idx <- if (length(next_var_lines) > 0) {
                  var_start_idx + next_var_lines[1] - 1
                } else {
                  length(section_text)
                }
                
                # Extract just this variable's section
                var_section <- section_text[var_start_idx:var_end_idx]
                
                # Now try to separate source and remarks
                # Look for common patterns in the text to determine where source ends and remarks begin
                source_part <- var_section
                remarks_part <- c()
                
                # Common patterns indicating remarks section
                remarks_patterns <- c("Bemerkungen", "Hinweise", "Kategorien", "Anzahl")
                
                # Find lines with remarks patterns
                for (pattern in remarks_patterns) {
                  remarks_lines <- grep(pattern, var_section, ignore.case = TRUE)
                  if (length(remarks_lines) > 0) {
                    # Found a line with remarks pattern
                    remarks_line <- remarks_lines[1]
                    
                    # Split the section
                    source_part <- var_section[1:(remarks_line-1)]
                    remarks_part <- var_section[remarks_line:length(var_section)]
                    break
                  }
                }
                
                # If we found a clear separation, add this information
                if (length(remarks_part) > 0) {
                  # Clean up the source and remarks text
                  source_text <- paste(source_part, collapse = "\n")
                  remarks_text <- paste(remarks_part, collapse = "\n")
                  
                  # Store the information
                  sources_info <- c(sources_info, paste("Aus PDF-Extrakt:", source_text))
                  remarks_info <- c(remarks_info, paste("Aus PDF-Extrakt:", remarks_text))
                } else {
                  # If no clear separation, just use the whole section as context
                  context_text <- paste(var_section, collapse = "\n")
                  sources_info <- c(sources_info, paste("Aus PDF-Extrakt (Kontext):", context_text))
                }
              }
            }
          }, error = function(e) {
            print(paste("Error reading PDF extracted text:", e$message))
          })
        }
        
        # If we've gathered information from any source, use it
        if (length(sources_info) > 0) {
          result$source <- paste(sources_info, collapse = "\n\n---\n\n")
        }
        
        if (length(remarks_info) > 0) {
          result$remarks <- paste(remarks_info, collapse = "\n\n---\n\n")
        }
        
        # Return the compiled information
        return(result)
      }
      
      # Find information for the selected institution
      info <- find_variable_info(selected_institution)
      
      # Variable name mapping for display
      var_display_name <- if (selected_institution == "sitzparl") {
        "Parlamentssitze"
      } else if (selected_institution == "sitzreg") {
        "Regierungssitze"
      } else if (selected_institution == "proporz") {
        "Proporzwahl"
      } else if (selected_institution == "parlegisl") {
        "Amtsdauer des Parlaments in Jahren"
      } else if (selected_institution == "gallagher") {
        "Effektive Disproportionalität des Wahlsystems"
      } else if (selected_institution == "proporz4") {
        "Proportionalität des Wahlsystems bei Parlamentswahlen"
      } else if (selected_institution == "proporz3reg") {
        "Proportionalität der Wahlsysteme bei Parlaments- und Regierungswahlen"
      } else if (selected_institution == "reg_proporz") {
        "Wahlverfahren bei Regierungsratswahlen (Dummy)"
      } else {
        selected_institution
      }
      
      # Create UI elements with better styling
      tagList(
        div(style = "margin-top: 20px;"), # Extra space above
        h4(paste0("Variable: ", var_display_name, " (", selected_institution, ")"), 
           style = "color: #1b6d80; border-bottom: 1px solid #ddd; padding-bottom: 8px;"),
        
        div(class = "row", style = "margin-top: 15px;",
            div(class = "col-12",
                h5("Quellen:", style = "font-weight: bold; color: #1b6d80;"),
                div(
                  style = "white-space: pre-wrap; overflow-y: visible; 
                          padding: 10px; border: 1px solid #e0e0e0; border-radius: 4px; background-color: #f8f9fa;",
                  HTML(gsub("\n", "<br>", info$source))
                )
            )
        ),
        
        div(class = "row", style = "margin-top: 15px;",
            div(class = "col-12",
                h5("Bemerkungen:", style = "font-weight: bold; color: #1b6d80;"),
                div(
                  style = "white-space: pre-wrap; overflow-y: visible; 
                          padding: 10px; border: 1px solid #e0e0e0; border-radius: 4px; background-color: #f8f9fa;", 
                  HTML(gsub("\n", "<br>", info$remarks))
                )
            )
        ),
        
        div(class = "row", style = "margin-top: 15px;",
            div(class = "col-12",
                h5("Codebuch:", style = "font-weight: bold; color: #1b6d80;"),
                div(
                  style = "white-space: pre-wrap; overflow-y: visible; 
                          padding: 10px; border: 1px solid #e0e0e0; border-radius: 4px; background-color: #f8f9fa;",
                  tags$a(href = "Kantonale DM_1979–2023_Codebuch.pdf", 
                         target = "_blank",
                         "Codebuch Demokratiemuster in den Schweizer Kantonen, 1979–2023 (Datensatz)")
                )
            )
        )
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
          geom_point(alpha = 0.7, color = "#1b6d80") +
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
          geom_line(linewidth = 1.2, color = "#1b6d80") +
          geom_point(color = "#1b6d80") +
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