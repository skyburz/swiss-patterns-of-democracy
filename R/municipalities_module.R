# Municipalities Module

#' UI function for municipalities module
#'
#' @param id The module ID
#' @return A UI definition
municipalities_ui <- function(id) {
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
      style = "display: flex; min-height: 500px;",
      column(
        width = 3,
        class = "equal-height",
        card(
          card_header("Filteroptionen"),
          card_body(
            class = "filter-panel",
            div(
              class = "filters-container",
              div(
                class = "filter-options",
                # 1. First filter: Municipalities Variables
                selectInput(
                  ns("variables"),
                  "Gemeinden Variablen:",
                  choices = c(
                    "Ausmass kantonaler Vorgaben zu obligatorischen Referenden" = "ddr_gde_obl",
                    "Ausmass kantonaler Vorgaben zu fakultativen Referenden" = "ddr_gde_fak",
                    "Ausmass kantonaler Vorgaben zu kommunalen Initiativen" = "ddr_gde_init",
                    "Policy-Dezentralisierung" = "dez_policy",
                    "Polity-Dezentralisierung" = "dez_polity",
                    "Politics-Dezentralisierung" = "dez_politics"
                  ),
                  selected = "ddr_gde_obl"
                ),
                
                # 2. Time selectors - conditionally shown based on active tab
                # Year range slider for Zeittrend tab
                conditionalPanel(
                  condition = paste0("input['", ns("main_tabs"), "'] == 'Zeittrend'"),
                  sliderInput(
                    ns("time_range"),
                    "Zeitperiode auswählen:",
                    min = 2015, 
                    max = 2023, 
                    value = c(2015, 2023),
                    step = 1,
                    sep = "",
                    animate = TRUE
                  )
                ),
                
                # Single year slider for Kantonsvergleich tab
                conditionalPanel(
                  condition = paste0("input['", ns("main_tabs"), "'] == 'Kantonsvergleich'"),
                  sliderInput(
                    ns("selected_year"),
                    "Jahr auswählen:",
                    min = 2015, 
                    max = 2023, 
                    value = 2019,
                    step = 1,
                    sep = "",
                    animate = TRUE
                  )
                ),
                
                # 3. Third filter: Canton selection with exclusive "All Cantons" option
                div(
                  class = "canton-selection-container",
                  tags$label("Kanton(e) auswählen:"),
                  radioButtons(
                    ns("canton_mode"),
                    label = NULL,
                    choices = list(
                      "Alle Kantone" = "all",
                      "Auswahl von Kantonen" = "select"
                    ),
                    selected = "all"
                  ),
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
          card_header("Überblick über Gemeinden"),
          card_body(
            tabsetPanel(
              id = ns("main_tabs"),
              tabPanel(
                "Zeittrend",
                div(
                  class = "plot-container",
                  plotlyOutput(ns("time_trend_plot"), height = "500px")
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
            style = "min-height: 600px; padding: 15px;",  # Set minimum height
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

#' Server function for municipalities module
#'
#' @param id The module ID
#' @param full_dataset Reactive expression that returns the full dataset
#' @return A server function
municipalities_server <- function(id, full_dataset) {
  moduleServer(id, function(input, output, session) {
    
    # Create a cleaned dataset function
    cleaned_data <- reactive({
      data <- full_dataset()
      
      # Clean all municipality variables - replace dots and empty strings with NA
      municipality_vars <- c("ddr_gde_obl", "ddr_gde_fak", "ddr_gde_init", 
                           "dez_policy", "dez_polity", "dez_politics")
      
      for(var in municipality_vars) {
        if (var %in% names(data)) {
          data <- data %>%
            mutate(!!sym(var) := ifelse(!!sym(var) == "." | !!sym(var) == "" | is.na(!!sym(var)), 
                                     NA_real_, 
                                     as.numeric(as.character(!!sym(var)))))
        }
      }
      
      return(data)
    })
    
    # Create a reactive function to filter the data
    filtered_data <- reactive({
      # Get the cleaned dataset
      data <- cleaned_data()
      
      # Apply time filters based on active tab
      if (input$main_tabs == "Zeittrend" && !is.null(input$time_range)) {
        # For Zeittrend tab, use the time range slider
        data <- data %>% filter(jahr >= input$time_range[1] & jahr <= input$time_range[2])
      } else if (!is.null(input$selected_year) && input$main_tabs == "Kantonsvergleich") {
        # For Kantonsvergleich tab, use the single year slider
        data <- data %>% filter(jahr == input$selected_year)
      }
      
      # Filter by canton
      if (input$canton_mode == "select" && length(input$canton_select) > 0) {
        data <- data %>% filter(kanton %in% input$canton_select)
      }
      
      return(data)
    })
    
    # Update the input choices once data is loaded
    observe({
      data <- cleaned_data()
      
      # Get unique cantons
      cantons <- sort(unique(data$kanton))
      
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
      
      # Update the canton selection checkbox group
      updateCheckboxGroupInput(
        session,
        "canton_select",
        choices = choices,
        selected = choices[1:3]  # Select first three by default
      )
      
      # Get min and max years (for potential future use)
      years <- sort(unique(data$jahr))
      
      # Override with fixed range for time slider inputs (2015-2023)
      updateSliderInput(
        session, 
        "time_range", 
        min = 2015,
        max = 2023,
        value = c(2015, 2023)
      )
      
      # Update the single year slider for Kantonsvergleich tab
      updateSliderInput(
        session,
        "selected_year",
        min = 2015,
        max = 2023,
        value = 2019
      )
    })
    
    # Get variable label based on variable name
    get_variable_label <- function(var_name) {
      labels <- c(
        "ddr_gde_obl" = "Ausmass kantonaler Vorgaben zu obligatorischen Referenden",
        "ddr_gde_fak" = "Ausmass kantonaler Vorgaben zu fakultativen Referenden",
        "ddr_gde_init" = "Ausmass kantonaler Vorgaben zu kommunalen Initiativen",
        "dez_policy" = "Policy-Dezentralisierung",
        "dez_polity" = "Polity-Dezentralisierung",
        "dez_politics" = "Politics-Dezentralisierung"
      )
      
      return(labels[var_name])
    }
    
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
    
    # Time trend plot
    output$time_trend_plot <- renderPlotly({
      # Add explicit dependencies on inputs to ensure reactivity
      req(filtered_data(), input$variables, input$time_range, input$canton_mode)
      data <- filtered_data()
      
      # Get the selected variable
      selected_var <- input$variables
      selected_var_label <- get_variable_label(selected_var)
      
      # Check if there are valid data points to plot
      if (all(is.na(data[[selected_var]]))) {
        # Return an empty plot with a message if all values are NA
        return(plot_ly() %>% 
                 layout(title = "Keine Daten verfügbar für diese Variable",
                        xaxis = list(title = ""),
                        yaxis = list(title = "")))
      }
      
      # Create the plot based on canton selection mode
      if (input$canton_mode == "all") {
        # For "Alle Kantone", calculate average by year
        yearly_data <- data %>%
          group_by(jahr) %>%
          summarise(
            value = mean(!!sym(selected_var), na.rm = TRUE),
            n_vals = sum(!is.na(!!sym(selected_var))),
            n_total = n()
          )
        
        # Ensure value is numeric and create hover text
        yearly_data <- yearly_data %>%
          mutate(
            value = as.numeric(value),
            hover_text = paste0(
              "Jahr: ", jahr, "<br>",
              "Durchschnittlicher Wert: ", round(as.numeric(value), 2), "<br>",
              "Datengrundlage: ", n_vals, " von ", n_total, " Kantonen"
            )
          )
        
        # Check if there are any rows left after summarizing
        if (nrow(yearly_data) == 0) {
          return(plot_ly() %>% 
                   layout(title = "Keine Daten verfügbar für diese Kombination von Filtern",
                          xaxis = list(title = ""),
                          yaxis = list(title = "")))
        }
        
        # Create the plot as plotly directly
        p <- plot_ly(
          data = yearly_data,
          x = ~jahr,
          y = ~as.numeric(value),
          type = 'scatter',
          mode = 'lines+markers',
          line = list(color = '#1b6d80', width = 2),
          marker = list(color = '#1b6d80', size = 8),
          hoverinfo = 'text',
          text = ~hover_text
        ) %>%
        layout(
          title = paste0("Zeittrend des ", selected_var_label),
          xaxis = list(
            title = "Jahr",
            dtick = 1,  # Set tick spacing to 1 year
            tickmode = "linear",  # Force linear ticks
            tickformat = "d"  # Format as integer
          ),
          yaxis = list(
            title = selected_var_label,
            tickformat = ".2f"
          )
        )
        
        return(p)
      } else {
        # For "Auswahl von Kantonen", show individual canton values
        # Filter out NA values before plotting
        plot_data <- data %>% 
          filter(!is.na(!!sym(selected_var))) %>%
          # Ensure numeric conversion
          mutate(!!sym(selected_var) := as.numeric(!!sym(selected_var)))
        
        # Check if we have data to plot
        if(nrow(plot_data) == 0) {
          return(plot_ly() %>% 
                   layout(title = "Keine Daten verfügbar für diese Kombination von Filtern",
                          xaxis = list(title = ""),
                          yaxis = list(title = "")))
        }
        
        # Count total number of cantons and years
        total_cantons <- length(unique(data$kanton))
        total_years <- length(unique(data$jahr))
        total_possible <- total_cantons * total_years
        
        # Count number of non-NA values
        total_available <- sum(!is.na(data[[selected_var]]))
        
        # Calculate percentage of data available
        data_completeness <- round(100 * total_available / total_possible, 1)
        
        # Define canton colors - using a consistent scheme across the app
        canton_colors <- get_canton_colors()
        
        p <- plot_ly() %>%
          layout(
            title = paste0(
              "Zeittrend ", selected_var_label, "<br>",
              "<sup>Datenverfügbarkeit: ", data_completeness, "% aller möglichen Datenpunkte</sup>"
            ),
            xaxis = list(
              title = "Jahr",
              dtick = 1,  # Set tick spacing to 1 year
              tickmode = "linear",  # Force linear ticks
              tickformat = "d"  # Format as integer
            ),
            yaxis = list(
              title = selected_var_label,
              tickformat = ".2f"
            ),
            legend = list(title = list(text = "Kanton"))
          )
        
        # Add a trace for each canton
        for (canton in unique(plot_data$kanton)) {
          canton_data <- plot_data %>% filter(kanton == canton)
          
          # Use the exact canton abbreviation since that's what we're using as keys in get_canton_colors()
          # Get color from canton_colors or use a default
          color <- if (!is.null(canton_colors[canton])) {
            canton_colors[canton]
          } else {
            "#808080"  # Default gray
          }
          
          # Add trace for this canton
          p <- p %>% add_trace(
            data = canton_data,
            x = ~jahr,
            y = ~get(selected_var),
            name = canton,
            type = "scatter",
            mode = "lines+markers",
            line = list(color = color, width = 2),
            marker = list(color = color, size = 8),
            hoverinfo = "text",
            text = ~paste(canton, "<br>Jahr:", jahr, 
                         "<br>", selected_var_label, ":", round(get(selected_var), 2))
          )
        }
        
        return(p)
      }
    })
    
    # Canton comparison plot
    output$canton_comparison_plot <- renderPlotly({
      req(filtered_data(), input$variables)
      data <- filtered_data()
      
      # Get the selected variable
      selected_var <- input$variables
      selected_var_label <- get_variable_label(selected_var)
      
      # Check if there are valid data points to plot
      if (all(is.na(data[[selected_var]]))) {
        # Return an empty plot with a message if all values are NA
        return(plot_ly() %>% 
                 layout(title = "Keine Daten verfügbar für diese Variable",
                        xaxis = list(title = ""),
                        yaxis = list(title = "")))
      }
      
      # Filter out cantons that have all NA values for this variable
      valid_cantons <- data %>%
        group_by(kanton) %>%
        summarise(has_valid = !all(is.na(!!sym(selected_var)))) %>%
        filter(has_valid) %>%
        pull(kanton)
      
      # Keep only cantons with valid data
      data <- data %>% filter(kanton %in% valid_cantons)
      
      # Calculate average value by canton
      canton_avg <- data %>%
        group_by(kanton) %>%
        summarise(avg_value = mean(!!sym(selected_var), na.rm = TRUE))
      
      # Remove any NaN or Inf values that might have been created
      canton_avg <- canton_avg %>%
        filter(!is.nan(avg_value) & !is.infinite(avg_value))
      
      # Check if there are any rows left after filtering
      if (nrow(canton_avg) == 0) {
        return(plot_ly() %>% 
                 layout(title = "Keine Daten verfügbar für diese Kombination von Filtern",
                        xaxis = list(title = ""),
                        yaxis = list(title = "")))
      }
      
      # Create the plot
      p <- ggplot(canton_avg, aes(x = reorder(kanton, avg_value), y = avg_value)) +
        geom_bar(stat = "identity", fill = "#1b6d80") +  # Use consistent color #1b6d80 for all bars
        coord_flip() +
        labs(
          title = paste("Kantonsvergleich des", selected_var_label, "im Jahr", input$selected_year),
          x = "Kanton",
          y = selected_var_label
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)
        )
      
      # Convert to plotly
      p <- ggplotly(p) %>%
        layout(
          margin = list(l = 100),  # Add more margin on the left for canton names
          yaxis = list(
            title = selected_var_label,
            tickformat = ".2f"
          )
        )
      
      return(p)
    })
    
    # Data table
    output$data_table <- renderDT({
      req(filtered_data())
      req(input$variables)
      data <- filtered_data()
      
      # Select relevant columns
      selected_cols <- c("kanton", "jahr", input$variables)
      table_data <- data %>% select(all_of(selected_cols))
      
      # Rename columns for display
      col_names <- c(
        "Kanton",
        "Jahr",
        get_variable_label(input$variables)
      )
      names(table_data) <- col_names
      
      # Format values for selected variable
      col_index <- which(names(table_data) == get_variable_label(input$variables))
      decimal_places <- 2
      
      table_data[[col_index]] <- sapply(table_data[[col_index]], function(x) {
        if (is.na(x)) return(NA)
        sprintf(paste0("%.", decimal_places, "f"), as.numeric(x))
      })
      
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
    
    # Source information lookup
    output$source_info <- renderUI({
      req(filtered_data())
      req(input$variables)
      
      # Get the selected variable
      selected_var <- input$variables
      
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
        
        # Special handling for municipality variables
        if (variable_name == "ddr_gde_obl") {
          return(list(
            source = paste("Flick Witzig und Vatter (2023: 25)", sep = "\n"),
            remarks = paste("Ausmass kantonaler Vorgaben zu obligatorischen Referenden",
                            "\n\nKategorien:",
                            "\n\nHinweise:",
                            "Die Daten stammen aus dem Jahr 2019.", sep = "\n")
          ))
        }
        
        if (variable_name == "ddr_gde_fak") {
          return(list(
            source = paste("Flick Witzig und Vatter (2023: 27)", sep = "\n"),
            remarks = paste("Ausmass kantonaler Vorgaben zu fakultativen Referenden",
                            "\n\nKategorien:",
                            "\n\nHinweise:",
                            "Die Daten stammen aus dem Jahr 2019.", sep = "\n")
          ))
        }
        
        if (variable_name == "ddr_gde_init") {
          return(list(
            source = paste("Flick Witzig und Vatter (2023: 29)", sep = "\n"),
            remarks = paste("Ausmass kantonaler Vorgaben zu kommunalen Initiativen",
                            "\n\nKategorien:",
                            "\n\nHinweise:",
                            "Die Daten stammen aus dem Jahr 2019.", sep = "\n")
          ))
        }
        
        if (variable_name == "dez_policy") {
          return(list(
            source = paste("Mueller (2015: 219)", sep = "\n"),
            remarks = paste("Policy-Dezentralisierung",
                            "\n\nKategorien:",
                            "\n\nHinweise:",
                            "Die Daten stammen aus dem Jahr 2015, wobei diese für die Jahre 2015 bis 2023 verwendet wurden. Es handelt sich um gerundete und standardisierte Werte.", sep = "\n")
          ))
        }
        
        if (variable_name == "dez_polity") {
          return(list(
            source = paste("Mueller (2015: 219)", sep = "\n"),
            remarks = paste("Polity-Dezentralisierung",
                            "\n\nKategorien:",
                            "\n\nHinweise:",
                            "Die Daten stammen aus dem Jahr 2015, wobei diese für die Jahre 2015 bis 2023 verwendet wurden. Es handelt sich um gerundete und standardisierte Werte.", sep = "\n")
          ))
        }
        
        if (variable_name == "dez_politics") {
          return(list(
            source = paste("Mueller (2015: 219)", sep = "\n"),
            remarks = paste("Politics-Dezentralisierung",
                            "\n\nKategorien:",
                            "\n\nHinweise:",
                            "Die Daten stammen aus dem Jahr 2015, wobei diese für die Jahre 2015 bis 2023 verwendet wurden. Es handelt sich um gerundete und standardisierte Werte.", sep = "\n")
          ))
        }
        
        # Try to gather information from multiple sources for completeness
        sources_info <- c()
        remarks_info <- c()
        
        # Try to read variable documentation from CSV
        var_doc_path <- "data/cleaned_variable_documentation.csv"
        if (file.exists(var_doc_path)) {
          tryCatch({
            var_doc <- read.csv(var_doc_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
            
            # Look for the variable name (exact match)
            var_index <- which(var_doc[,1] == variable_name)
            
            if (length(var_index) > 0) {
              # Store the source and remarks
              sources_info <- c(sources_info, paste("Quelle:", var_doc[var_index[1], 2]))
              remarks_info <- c(remarks_info, paste("Beschreibung:", var_doc[var_index[1], 3]))
              
              # Look for additional lines with the variable's calculation or notes
              if (var_index[1] < nrow(var_doc)) {
                additional_lines <- var_index[1] + 1
                while(additional_lines <= nrow(var_doc) && 
                      (var_doc[additional_lines, 1] == "" || 
                       grepl(variable_name, var_doc[additional_lines, 1]))) {
                  
                  if (var_doc[additional_lines, 2] != "") {
                    sources_info <- c(sources_info, var_doc[additional_lines, 2])
                  }
                  if (var_doc[additional_lines, 3] != "") {
                    remarks_info <- c(remarks_info, var_doc[additional_lines, 3])
                  }
                  additional_lines <- additional_lines + 1
                }
              }
            }
            
            # If not found by exact match, try partial match
            if (length(var_index) == 0) {
              var_index <- which(grepl(variable_name, var_doc[,1], fixed = TRUE))
              
              if (length(var_index) > 0) {
                # Store the source and remarks
                sources_info <- c(sources_info, paste("Quelle (partielle Übereinstimmung):", var_doc[var_index[1], 2]))
                remarks_info <- c(remarks_info, paste("Beschreibung (partielle Übereinstimmung):", var_doc[var_index[1], 3]))
              }
            }
          }, error = function(e) {
            print(paste("Error reading variable documentation:", e$message))
          })
        }
        
        # If we've gathered information from any source, use it
        if (length(sources_info) > 0 || length(remarks_info) > 0) {
          source_text <- if (length(sources_info) > 0) paste(sources_info, collapse = "\n\n") else "Keine Quelleninformation gefunden"
          remarks_text <- if (length(remarks_info) > 0) paste(remarks_info, collapse = "\n\n") else "Keine Bemerkungen verfügbar"
          
          return(list(
            source = source_text,
            remarks = remarks_text
          ))
        }
        
        # Generic fallback for variables without specific info
        return(list(
          source = "Keine Quelleninformation gefunden",
          remarks = "Keine Bemerkungen verfügbar"
        ))
      }
      
      # Find information for the selected variable
      info <- find_variable_info(selected_var)
      
      # Variable name mapping for display
      var_label <- get_variable_label(selected_var)
      
      # Create UI elements with better styling
      tagList(
        div(style = "margin-top: 20px;"), # Extra space above
        h4(paste0("Variable: ", var_label, " (", selected_var, ")"), 
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
  })
} 