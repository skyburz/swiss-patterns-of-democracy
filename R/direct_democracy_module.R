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
      
      # Update the canton selection checkbox group
      updateCheckboxGroupInput(
        session,
        "canton_select",
        choices = cantons,
        selected = cantons[1:3]  # Select first three by default
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
          line = list(color = '#2b6cb0', width = 2),
          marker = list(color = '#2b6cb0', size = 8),
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
          
          # Add trace for this canton
          p <- p %>% add_trace(
            data = canton_data,
            x = ~jahr,
            y = ~get(selected_var),
            name = canton,
            type = "scatter",
            mode = "lines+markers",
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
        geom_bar(stat = "identity", fill = "#2b6cb0") +
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
      
      # Select relevant columns
      selected_cols <- c("kanton", "jahr", selected_var)
      table_data <- data %>% select(all_of(selected_cols))
      
      # Rename columns for display
      names(table_data) <- c("Kanton", "Jahr", var_label)
      
      # Format values for continuous variables
      if (is_continuous) {
        col_index <- which(names(table_data) == var_label)
        dp <- get_decimal_places(selected_var)
        
        table_data[[col_index]] <- sapply(table_data[[col_index]], function(x) {
          if (is.na(x)) return(NA)
          sprintf(paste0("%.", dp, "f"), as.numeric(x))
        })
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
  })
} 