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
          card_header("Analyse"),
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
      
      # Update the canton selection checkbox group
      updateCheckboxGroupInput(
        session,
        "canton_select",
        choices = cantons,
        selected = cantons[1:3]  # Select first three by default
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
          line = list(color = '#2b6cb0', width = 2),
          marker = list(color = '#2b6cb0', size = 8),
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
        canton_colors <- c(
          "ZH" = "#003087", "BE" = "#FF0000", "LU" = "#0066CC", "UR" = "#FFCC00", 
          "SZ" = "#FF0000", "OW" = "#FFFFFF", "NW" = "#FF0000", "GL" = "#FF0000", 
          "ZG" = "#0033CC", "FR" = "#000000", "SO" = "#FF0000", "BS" = "#000000", 
          "BL" = "#FF0000", "SH" = "#000000", "AR" = "#000000", "AI" = "#000000", 
          "SG" = "#009933", "GR" = "#999999", "AG" = "#000000", "TG" = "#009933", 
          "TI" = "#FF0000", "VD" = "#006600", "VS" = "#FF0000", "NE" = "#009900", 
          "GE" = "#CC0000", "JU" = "#FF0000"
        )
        
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
          
          # Get canton abbreviation (assuming we need to derive it)
          canton_abbr <- substr(canton, 1, 2)  # Simple way to get abbreviation, adjust if needed
          
          # Get color from canton_colors or use a default
          color <- if (!is.null(canton_colors[canton_abbr])) {
            canton_colors[canton_abbr]
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
        geom_bar(stat = "identity", fill = "#2b6cb0") +
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
      req(filtered_data(), input$variables)
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
  })
} 