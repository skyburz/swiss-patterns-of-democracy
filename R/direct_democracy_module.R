# Modul für die Analyse direkter Demokratie

#' UI function for direct democracy module
#'
#' @param id The module ID
#' @return A UI definition
direct_democracy_ui <- function(id) {
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
                # 1. First filter: Direct Democracy Variables
                selectInput(
                  ns("variables"),
                  "Direkte Demokratie Variablen:",
                  choices = NULL,
                  selected = "abst_total"
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
                    value = c(1979, 2023),
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
                    min = 1979, 
                    max = 2023, 
                    value = 2023,
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
          card_header("Überblick über direkte Demokratie"),
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

#' Server function for direct democracy module
#'
#' @param id The module ID
#' @param full_dataset Reactive expression that returns the full dataset
#' @return A server function
direct_democracy_server <- function(id, full_dataset) {
  moduleServer(id, function(input, output, session) {
    
    # Variable mapping for labels and descriptions
    variable_labels <- reactiveVal(list(
      "abst_total" = "Jährliche Anzahl Abstimmungen",
      "init_total" = "Jährliche Anzahl Abstimmungen über Volksinitiativen",
      "ref_total" = "Jährliche Anzahl Abstimmungen über Referenden",
      "turnout_v" = "Stimmbeteiligung bei kantonalen Volksabstimmungen in Prozent",
      "ddr_snddi" = "Sub-National Direct Democracy Index",
      "obl_finref" = "Jährliche Anzahl Abstimmungen über obligatorische Finanzreferenden (Ausgaben)",
      "fak_finref" = "Jährliche Anzahl Abstimmungen über fakultative Finanzreferenden (Ausgaben)",
      # Add new variables with their labels
      "ddr_stutz" = "Direktdemokratische Rechte (Index)",
      "gir" = "Gesetzesinitiativrecht (Index)",
      "vir" = "Verfassungsinitiativrecht (Index)",
      "grr" = "Gesetzesreferendumsrecht (Index)",
      "frr" = "Finanzreferendumsrecht (Index)"
    ))
    
    # List of variables that need decimal formatting (continuous variables)
    continuous_vars <- c("turnout_v", "ddr_snddi", "ddr_stutz", "gir", "vir", "grr", "frr")
    decimal_places <- list(
      "turnout_v" = 1, 
      "ddr_snddi" = 2,
      "ddr_stutz" = 2,
      "gir" = 2,
      "vir" = 2,
      "grr" = 2,
      "frr" = 2
    )
    
    # List of index variables that have limited availability
    index_vars <- c("ddr_snddi", "ddr_stutz", "gir", "vir", "grr", "frr")
    
    # List of index variables that should default to 2018 for Kantonsvergleich
    index_vars_2018 <- c("ddr_stutz", "gir", "vir", "grr", "frr")
    
    # Function to get variable label
    get_var_label <- function(var_name) {
      labels <- variable_labels()
      if (var_name %in% names(labels)) {
        return(labels[[var_name]])
      }
      return(var_name)
    }
    
    # Helper function to get decimal places for a variable
    get_decimal_places <- function(var_name) {
      if (var_name %in% names(decimal_places)) {
        return(decimal_places[[var_name]])
      }
      return(2)  # Default to 2 decimal places
    }
    
    # Create a cleaned dataset function
    cleaned_data <- reactive({
      data <- full_dataset()
      
      # Variables that need numeric conversion and NA handling
      numeric_vars <- c("turnout_v", "ddr_snddi", "obl_finref", "fak_finref", 
                       "ddr_stutz", "gir", "vir", "grr", "frr")
      
      # Clean all variables in one pass
      for (var in numeric_vars) {
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
      req(input$variables)
      
      # Apply time filters based on active tab
      if (input$main_tabs == "Zeittrend" && !is.null(input$time_range)) {
        data <- data %>% filter(jahr >= input$time_range[1] & jahr <= input$time_range[2])
      } else if (input$main_tabs == "Kantonsvergleich" && !is.null(input$selected_year)) {
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
      
      # Define potential direct democracy columns
      key_vars <- c("abst_total", "init_total", "ref_total", "turnout_v", 
                   "ddr_snddi", "obl_finref", "fak_finref",
                   "ddr_stutz", "gir", "vir", "grr", "frr")
      
      # Filter to variables that exist in the dataset
      existing_vars <- key_vars[key_vars %in% names(data)]
      
      # Add any other variables that might be relevant
      pattern_vars <- names(data)[grep("abstimmung|referendum|initiative|volksabstimmung|volksinitiative",
                                     names(data), ignore.case = TRUE)]
      
      # Combine and remove duplicates
      direct_democracy_cols <- unique(c(existing_vars, pattern_vars))
      
      # Create named choices using the variable labels
      named_choices <- setNames(
        direct_democracy_cols,
        sapply(direct_democracy_cols, get_var_label)
      )
      
      # Update the variable labels with any new variables found
      current_labels <- variable_labels()
      for (var in direct_democracy_cols) {
        if (!var %in% names(current_labels)) {
          current_labels[[var]] <- var  # Default to the variable name itself
        }
      }
      variable_labels(current_labels)
      
      # Set the choices
      updateSelectInput(
        session,
        "variables",
        choices = named_choices,
        selected = if ("abst_total" %in% direct_democracy_cols) "abst_total" else direct_democracy_cols[1]
      )
      
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
      
      # Get min and max years to set slider ranges
      years <- sort(unique(data$jahr))
      min_year <- min(years)
      max_year <- max(years)
      
      # Set the time range sliders
      updateSliderInput(
        session, 
        "time_range", 
        min = min_year,
        max = max_year,
        value = c(min_year, max_year)
      )
      
      # Default to maximum year for most variables
      updateSliderInput(
        session,
        "selected_year",
        min = min_year,
        max = max_year,
        value = max_year
      )
    })
    
    # Observer for updating year slider when variable changes
    observeEvent(input$variables, {
      req(input$main_tabs == "Kantonsvergleich")
      
      selected_var <- input$variables
      data <- cleaned_data()
      years <- sort(unique(data$jahr))
      min_year <- min(years)
      max_year <- max(years)
      
      # Set default year to 2018 for specific index variables, otherwise use max year
      if (selected_var %in% index_vars_2018) {
        if (2018 %in% years) {
          updateSliderInput(
            session,
            "selected_year",
            value = 2018
          )
        }
      } else {
        updateSliderInput(
          session,
          "selected_year",
          value = max_year
        )
      }
    })
    
    # Observer for when the tab changes to Kantonsvergleich
    observeEvent(input$main_tabs, {
      if (input$main_tabs == "Kantonsvergleich") {
        req(input$variables)
        selected_var <- input$variables
        data <- cleaned_data()
        years <- sort(unique(data$jahr))
        min_year <- min(years)
        max_year <- max(years)
        
        # Set default year to 2018 for specific index variables, otherwise use max year
        if (selected_var %in% index_vars_2018) {
          if (2018 %in% years) {
            updateSliderInput(
              session,
              "selected_year",
              value = 2018
            )
          }
        } else {
          updateSliderInput(
            session,
            "selected_year",
            value = max_year
          )
        }
      }
    })
    
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
      var_label <- get_var_label(selected_var)
      is_continuous <- selected_var %in% continuous_vars
      is_index <- selected_var %in% index_vars
      
      # Check if there are valid data points to plot
      if (all(is.na(data[[selected_var]]))) {
        return(plot_ly() %>% 
                 layout(title = "Keine Daten verfügbar für diese Variable",
                        xaxis = list(title = ""),
                        yaxis = list(title = "")))
      }
      
      # Create the plot based on canton selection mode
      if (input$canton_mode == "all") {
        # For "Alle Kantone", calculate sum or average by year depending on the variable
        yearly_data <- data %>%
          group_by(jahr) %>%
          summarise(
            value = if (is_continuous) {
              # For continuous variables, calculate average and track how many values contributed
              mean(!!sym(selected_var), na.rm = TRUE)
            } else {
              sum(!!sym(selected_var), na.rm = TRUE)
            },
            n_vals = sum(!is.na(!!sym(selected_var))),
            n_total = n()
          )
        
        # Create hover text
        if (is_continuous) {
          yearly_data <- yearly_data %>%
            mutate(
              value = as.numeric(value),
              hover_text = paste0(
                "Jahr: ", jahr, "<br>",
                "Durchschnittlicher Wert: ", round(value, get_decimal_places(selected_var)), 
                if(selected_var == "turnout_v") "%" else "", "<br>",
                "Datengrundlage: ", n_vals, " von ", n_total, " Kantonen"
              )
            )
        } else {
          yearly_data <- yearly_data %>%
            mutate(
              hover_text = paste0(
                "Jahr: ", jahr, "<br>",
                "Gesamtwert: ", value, "<br>",
                "Datengrundlage: ", n_vals, " von ", n_total, " Kantonen"
              )
            )
        }
        
        # Check if there are any rows left after summarizing
        if (nrow(yearly_data) == 0) {
          return(plot_ly() %>% 
                   layout(title = "Keine Daten verfügbar für diese Kombination von Filtern",
                          xaxis = list(title = ""),
                          yaxis = list(title = "")))
        }
        
        # Create plotly plot
        p <- plot_ly(
          data = yearly_data,
          x = ~jahr,
          y = ~value,
          type = 'scatter',
          mode = 'lines+markers',
          line = list(color = '#1b6d80', width = 2),
          marker = list(color = '#1b6d80', size = 8),
          hoverinfo = 'text',
          text = ~hover_text
        ) %>%
        layout(
          title = paste0("Zeittrend ", if(is_continuous) "des durchschnittlichen " else "der ", var_label),
          xaxis = list(
            title = "Jahr",
            dtick = 1,
            tickmode = "linear",
            tickformat = "d"
          ),
          yaxis = list(
            title = var_label,
            tickformat = if(is_continuous) paste0(".", get_decimal_places(selected_var), "f") else ""
          )
        )
        
        # Add information about data availability for index variables
        if (is_index) {
          # Add a note about limited data availability
          p <- p %>% layout(
            annotations = list(
              x = 0.5,
              y = 1.05,
              text = "Hinweis: Für einige Jahre könnten Daten fehlen",
              showarrow = FALSE,
              xref = "paper",
              yref = "paper",
              font = list(size = 12)
            )
          )
        }
        
        return(p)
      } else {
        # For "Auswahl von Kantonen"
        # Filter out NA values before plotting
        plot_data <- data %>% 
          filter(!is.na(!!sym(selected_var))) %>%
          mutate(!!sym(selected_var) := as.numeric(!!sym(selected_var)))
        
        # Check if we have data to plot
        if(nrow(plot_data) == 0) {
          return(plot_ly() %>% 
                   layout(title = "Keine Daten verfügbar für diese Kombination von Filtern",
                          xaxis = list(title = ""),
                          yaxis = list(title = "")))
        }
        
        # Data availability stats
        total_cantons <- length(unique(data$kanton))
        total_years <- length(unique(data$jahr))
        total_possible <- total_cantons * total_years
        total_available <- sum(!is.na(data[[selected_var]]))
        data_completeness <- round(100 * total_available / total_possible, 1)
        
        # Get canton colors
        canton_colors <- get_canton_colors()
        
        # Create plotly plot
        p <- plot_ly() %>%
          layout(
            title = paste0(
              "Zeittrend ", var_label, "<br>",
              "<sup>Datenverfügbarkeit: ", data_completeness, "% aller möglichen Datenpunkte</sup>"
            ),
            xaxis = list(
              title = "Jahr",
              dtick = 1,
              tickmode = "linear",
              tickformat = "d"
            ),
            yaxis = list(
              title = var_label,
              tickformat = if(is_continuous) paste0(".", get_decimal_places(selected_var), "f") else ""
            ),
            legend = list(title = list(text = "Kanton"))
          )
        
        # Add a trace for each canton
        for (canton in unique(plot_data$kanton)) {
          canton_data <- plot_data %>% filter(kanton == canton)
          
          # Get color for this canton
          canton_color <- canton_colors[canton]
          if (is.na(canton_color)) canton_color = "#808080"  # Default gray if no color found
          
          # Add trace for this canton
          p <- p %>% add_trace(
            data = canton_data,
            x = ~jahr,
            y = ~get(selected_var),
            name = canton,
            type = "scatter",
            mode = "lines+markers",
            line = list(color = canton_color, width = 2),
            marker = list(color = canton_color, size = 8),
            hoverinfo = "text",
            text = ~paste(
              canton, "<br>Jahr:", jahr, 
              "<br>", var_label, ":", 
              if(is_continuous) round(get(selected_var), get_decimal_places(selected_var)) else get(selected_var),
              if(selected_var == "turnout_v") "%" else ""
            )
          )
        }
        
        # Add information about data availability for index variables
        if (is_index) {
          # Add a note about limited data availability
          p <- p %>% layout(
            annotations = list(
              x = 0.5,
              y = 1.05,
              text = "Hinweis: Für einige Jahre könnten Daten fehlen",
              showarrow = FALSE,
              xref = "paper",
              yref = "paper",
              font = list(size = 12)
            )
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
      var_label <- get_var_label(selected_var)
      is_continuous <- selected_var %in% continuous_vars
      is_index <- selected_var %in% index_vars
      
      # Check for valid data points
      if (all(is.na(data[[selected_var]]))) {
        return(plot_ly() %>% 
                 layout(title = "Keine Daten verfügbar für diese Variable",
                        xaxis = list(title = ""),
                        yaxis = list(title = "")))
      }
      
      # Filter out cantons that have all NA values
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
      
      # Remove any NaN or Inf values
      canton_avg <- canton_avg %>%
        filter(!is.nan(avg_value) & !is.infinite(avg_value))
      
      # Check if there are any rows left
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
          title = paste("Kantonsvergleich", var_label),
          x = "Kanton",
          y = var_label
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)
        )
      
      # Add appropriate scale based on variable type
      if (!is_continuous) {
        # Integer scales for count variables
        p <- p + scale_y_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1))
      } else {
        # Decimal scales for continuous variables
        decimal_format <- get_decimal_places(selected_var)
        p <- p + scale_y_continuous(
          labels = function(x) sprintf(paste0("%.", decimal_format, "f"), x)
        )
      }
      
      # Convert to plotly
      p <- ggplotly(p) %>%
        layout(
          margin = list(l = 100),  # Add more margin on the left for canton names
          yaxis = list(
            title = var_label,
            tickformat = if(is_continuous) paste0(".", get_decimal_places(selected_var), "f") else ""
          )
        )
      
      # Add note about data availability for index variables
      if (is_index) {
        p <- p %>% layout(
          annotations = list(
            x = 0.5,
            y = 1.05,
            text = "Hinweis: Für einige Jahre könnten Daten fehlen",
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            font = list(size = 12)
          )
        )
      }
      
      return(p)
    })
    
    # Data table
    output$data_table <- renderDT({
      req(filtered_data(), input$variables)
      data <- filtered_data()
      selected_var <- input$variables
      var_label <- get_var_label(selected_var)
      is_continuous <- selected_var %in% continuous_vars
      
      # Get the full dataset (not filtered by year)
      data <- cleaned_data()
      
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
      
      # Make sure we're using the canton column with abbreviations
      # Check if we have a column that explicitly contains abbreviations like "ZH", "BE", etc.
      abbr_col <- "kanton"  # Default to the current canton column
      potential_cols <- names(data)[grep("kanton", names(data), ignore.case = TRUE)]
      for (col in potential_cols) {
        if (is.character(data[[col]]) && nchar(data[[col]][1]) == 2) {
          abbr_col <- col
          print(paste("Using column for abbreviations:", abbr_col))
          break
        }
      }
      
      # Select relevant columns
      year_col <- "jahr"
      
      # Filter data to include only relevant columns and order it
      table_data <- data %>%
        select(!!sym(abbr_col), !!sym(year_col), !!sym(selected_var)) %>%
        arrange(!!sym(abbr_col), !!sym(year_col))
      
      # Add canton names and BFS numbers
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
            if(is.na(name)) return(abbr)
            return(name)
          }),
          canton_abbr = !!sym(abbr_col)  # Store the original abbreviation
        )
      
      # Rename the value column to the display name
      names(table_data)[names(table_data) == selected_var] <- var_label
      
      # Select and reorder columns
      table_data <- table_data %>%
        select(canton_name, canton_abbr, !!sym(year_col), !!var_label)
      
      # Rename columns for better display
      names(table_data)[names(table_data) == "canton_name"] <- "Kanton"
      names(table_data)[names(table_data) == "canton_abbr"] <- "Kanton-Kürzel"
      names(table_data)[names(table_data) == year_col] <- "Jahr"
      
      # Format values for continuous variables
      if (is_continuous) {
        col_index <- which(names(table_data) == var_label)
        dp <- get_decimal_places(selected_var)
        
        table_data[[col_index]] <- sapply(table_data[[col_index]], function(x) {
          if (is.na(x)) return(NA)
          sprintf(paste0("%.", dp, "f"), as.numeric(x))
        })
      }
      
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
            list(width = "140px", targets = 0),  # Kanton
            list(width = "100px", targets = 1),  # Kanton-Kürzel
            list(width = "80px", targets = 2),   # Jahr
            list(width = "140px", targets = 3)   # Value column
          )
        ),
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center; font-size: 16px; font-weight: bold;',
          ''  # Empty caption text
        ),
        rownames = FALSE,
        class = "display compact"  # No filter class
      )
      
      # Return the datatable
      dt
    })
    
    # Source information lookup
    output$source_info <- renderUI({
      req(filtered_data(), input$variables)
      
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
        
        # Special handling for specific variables
        if (variable_name == "abst_total") {
          return(list(
            source = paste("Eigene Berechnungen basierend auf angestellter Erhebung.",
                           "\n\nBerechnung:", 
                           "abst_total = init_total + ref_total + gegenvor_init", sep = "\n"),
            remarks = paste("Jährliche Anzahl Abstimmungen", sep = "\n")
          ))
        }
        
        if (variable_name == "init_total") {
          return(list(
            source = paste("Eigene Berechnungen basierend auf angestellter Erhebung.", sep = ""),
            remarks = paste("Jährliche Anzahl Abstimmungen über Volksinitiativen: Verfassungs- und Gesetzesinitiativen, konstruktive Referenden, verfahrensbezogene Anträge, Behörden- und Gemeindeinitiativen.",
                            "\n\nBerechnung:",
                            "init_total = volksinit + vi_verfahr + sonstige_init", sep = "\n")
          ))
        }
        
        if (variable_name == "ref_total") {
          return(list(
            source = paste("Eigene Berechnungen basierend auf angestellter Erhebung.", sep = ""),
            remarks = paste("Jährliche Anzahl Abstimmungen über Referenden: Obligatorische und fakultative Referenden.",
                            "\n\nBerechnung:",
                            "ref_total = ref_obl + ref_fak", sep = "\n")
          ))
        }
        
        if (variable_name == "turnout_v") {
          return(list(
            source = paste("c2d, Centre of Direct",
                           "Democracy", sep = "\n"),
            remarks = paste("Stimmbeteiligung bei kantonalen Volksabstimmungen in Prozent.",
                            "\n\nHinweise:",
                            "Durchschnitt aller kantonalen Volksabstimmungen im betreffenden Jahr (mehrere Abstimmungen am selben Abstimmungsdatum wurden je separat gezählt; jede Abstimmungsvorlage [nicht etwa jeder Abstimmungstermin] fliesst mit gleichem Gewicht ein; Schätzungen der Stimmbeteiligung bei Landsgemeinden sind hier nicht ausgewiesen).", sep = "\n")
          ))
        }
        
        if (variable_name == "obl_finref") {
          return(list(
            source = paste("c2d, Centre of Direct",
                           "Democracy", sep = "\n"),
            remarks = paste("Jährliche Anzahl Abstimmungen über obligatorische Finanzreferenden (Ausgaben)",
                            "\n\nHinweise:",
                            "Bestandteil von ref_obl", sep = "\n")
          ))
        }
        
        if (variable_name == "fak_finref") {
          return(list(
            source = paste("c2d, Centre of Direct",
                           "Democracy", sep = "\n"),
            remarks = paste("Jährliche Anzahl Abstimmungen über fakultative Finanzreferenden (Ausgaben) (inkl. entsprechenden Behördenreferenden)",
                            "\n\nHinweise:",
                            "Bestandteil von ref_fak", sep = "\n")
          ))
        }
        
        if (variable_name == "ddr_snddi") {
          return(list(
            source = paste("2015–2023:",
                           "Leemann/Stadelmann-",
                           "Steffen (2022)", sep = "\n"),
            remarks = paste("Sub-National Direct Democracy Index",
                            "\n\nHinweise:",
                            "Index aus Leemann/Stadelmann-Steffen (2022) übernommen. Der Index wurde bis anhin pro Kanton nur zu einem Zeitpunkt gemessen, dessen Wert in diesem Datensatz für die Jahre 2015 bis 2023 verwendet wurde.",
                            "\n\nTheoretischer Wertebereich: 0 (wenig ausgebaute direktdemokratische Rechte) bis 2 (stark ausgebaute direktdemokratische Rechte).",
                            "\n\nFür Detailbemerkungen zur Berechnung und zum Vorgehen: Vgl. Kapitel «Measuring Sub-National Direct Democracy» in Leemann/Stadelmann-Steffen (2022).", sep = "\n")
          ))
        }
        
        if (variable_name == "ddr_stutz") {
          return(list(
            source = paste("1997–2003: ",
                           "Fischer (2009), Notengebung JU korrigiert.",
                           "\n\n1980 und 2008: ",
                           "Eigene Erhebungen auf Basis von Trechsel/Serdült (1999) bzw. Kantonsverfassungen.",
                           "\n\n1970 und 1996: ",
                           "Stutzer (1999), eigene Erhebungen auf Basis von Trechsel/Serdült (1999) bzw. Kantonsverfassungen.",
                           "\n\n1992: ",
                           "Stutzer/Frey (2000), eigene Erhebungen auf Basis von Trechsel/Serdült (1999) bzw. Kantonsverfassungen.",
                           "\n\nEigene Erhebungen für die Jahre 2008–2018 unter Einbezug von Daten von Leeman/Stadelmann-Steffen (2019) und Kuster/Leuzinger (2019)", sep = ""),
            remarks = paste("Direktdemokratische Rechte (Index)",
                            "\n\nHinweise:",
                            "Index konstruiert nach Stutzer (1999).",
                            "\n\nTheoretischer Wertebereich: 1 (wenig ausgebaute direktdemokratische Rechte) bis 6 (stark ausgebaute direktdemokratische Rechte).",
                            "\n\nBerechnung: Mittelwert aus gir, vir, grr, frr.",
                            "\n\nStichtag: Für die Jahre 1997–2003 ist der 1. April (Fischer 2009: 65), für alle anderen Jahre jeweils der 31. Dezember des jeweiligen Jahres der Stichtag der Erhebung.",
                            "\n\nFür Detailbemerkungen zum Vorgehen: Vgl. Dokumente «Notizen zu Teilindizes Stutzer-Index» und «Gewichtung Fak-Obl GesRef SH BL SO AG» (kann auf Anfrage zugestellt werden).",
                            "\n\nDurch Quellen gesicherte Zeitpunkte: 1980, 1992, 1996–2003, 2008–2018. Lineare Interpolation: Werte von 1980 übernommen für 1979.", sep = "")
          ))
        }
        
        if (variable_name == "gir" || variable_name == "vir" || 
            variable_name == "grr" || variable_name == "frr") {
          
          var_titles = list(
            "gir" = "Gesetzesinitiativrecht (Index)",
            "vir" = "Verfassungsinitiativrecht (Index)",
            "grr" = "Gesetzesreferendumsrecht (Index)",
            "frr" = "Finanzreferendumsrecht (Index)"
          )
          
          return(list(
            source = paste("siehe ddr_stutz", sep = "\n"),
            remarks = paste(var_titles[[variable_name]],
                            "\n\nHinweise:",
                            "Index konstruiert nach Stutzer (1999); siehe ddr_stutz für detailliertere Erläuterungen.", sep = "\n")
          ))
        }
        
        # Try to gather information from multiple sources for completeness like in democratic_institutions_module
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
      var_label <- get_var_label(selected_var)
      
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