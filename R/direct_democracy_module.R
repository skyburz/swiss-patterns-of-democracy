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
                  condition = paste0("input['", ns("main_tabs"), "'] == 'Kantonsvergleich' || input['", ns("main_tabs"), "'] == 'Institutionsdetails'"),
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
    
    # Create a cleaned dataset function
    cleaned_data <- reactive({
      data <- full_dataset()
      
      # Clean turnout_v - replace dots and empty strings with NA
      if ("turnout_v" %in% names(data)) {
        data <- data %>%
          mutate(turnout_v = ifelse(turnout_v == "." | turnout_v == "" | is.na(turnout_v), 
                                   NA_real_, 
                                   as.numeric(as.character(turnout_v))))
      }
      
      # Clean ddr_snddi - replace dots and empty strings with NA
      if ("ddr_snddi" %in% names(data)) {
        data <- data %>%
          mutate(ddr_snddi = ifelse(ddr_snddi == "." | ddr_snddi == "" | is.na(ddr_snddi), 
                                  NA_real_, 
                                  as.numeric(as.character(ddr_snddi))))
      }
      
      # Clean obl_finref - replace dots and empty strings with NA
      if ("obl_finref" %in% names(data)) {
        data <- data %>%
          mutate(obl_finref = ifelse(obl_finref == "." | obl_finref == "" | is.na(obl_finref), 
                                    NA_real_, 
                                    as.numeric(as.character(obl_finref))))
      }
      
      # Clean fak_finref - replace dots and empty strings with NA
      if ("fak_finref" %in% names(data)) {
        data <- data %>%
          mutate(fak_finref = ifelse(fak_finref == "." | fak_finref == "" | is.na(fak_finref), 
                                    NA_real_, 
                                    as.numeric(as.character(fak_finref))))
      }
      
      return(data)
    })
    
    # Create a reactive function to filter the data
    filtered_data <- reactive({
      # Get the cleaned dataset
      data <- cleaned_data()
      
      # Special handling for ddr_snddi: restrict time period to 2015-2023
      if (input$variables == "ddr_snddi") {
        # First check if we need to set the initial slider values (already handled in the observers)
        
        if (input$main_tabs == "Zeittrend") {
          # For Zeittrend tab, use the time range slider values but ensure within 2015-2023
          min_year <- max(2015, input$time_range[1])
          max_year <- min(2023, input$time_range[2])
          data <- data %>% filter(jahr >= min_year & jahr <= max_year)
        } else if (input$main_tabs == "Kantonsvergleich" || input$main_tabs == "Institutionsdetails") {
          # For Kantonsvergleich tab, restrict selected_year to 2015-2023
          if (input$selected_year < 2015 || input$selected_year > 2023) {
            # If outside valid range, default to 2023
            selected_year <- 2023
          } else {
            selected_year <- input$selected_year
          }
          data <- data %>% filter(jahr == selected_year)
        }
      } else {
        # For non-ddr_snddi variables, use normal filtering
        
        # Apply time filters based on active tab
        if (input$main_tabs == "Zeittrend" && !is.null(input$time_range)) {
          # For Zeittrend tab, use the time range slider
          data <- data %>% filter(jahr >= input$time_range[1] & jahr <= input$time_range[2])
        } else if (!is.null(input$selected_year) && 
                  (input$main_tabs == "Kantonsvergleich" || input$main_tabs == "Institutionsdetails")) {
          # For Kantonsvergleich and Institutionsdetails tabs, use the single year slider
          data <- data %>% filter(jahr == input$selected_year)
        }
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
      
      # Find potential direct democracy columns
      direct_democracy_cols <- c(
        # First check if abst_total, init_total, ref_total, turnout_v, and ddr_snddi exist in the dataset
        if("abst_total" %in% names(data)) "abst_total" else NULL,
        if("init_total" %in% names(data)) "init_total" else NULL,
        if("ref_total" %in% names(data)) "ref_total" else NULL,
        if("turnout_v" %in% names(data)) "turnout_v" else NULL,
        if("ddr_snddi" %in% names(data)) "ddr_snddi" else NULL,
        if("obl_finref" %in% names(data)) "obl_finref" else NULL,
        if("fak_finref" %in% names(data)) "fak_finref" else NULL,
        # Then get other direct democracy variables
        names(data)[grep("abstimmung|referendum|initiative|volksabstimmung|volksinitiative",
                         names(data), ignore.case = TRUE)]
      )
      
      # Remove any NULL values and make sure the list is unique
      direct_democracy_cols <- unique(direct_democracy_cols[!is.null(direct_democracy_cols)])
      
      # Create named choices for better display
      named_choices <- setNames(
        direct_democracy_cols,
        ifelse(
          direct_democracy_cols == "abst_total",
          "Jährliche Anzahl Abstimmungen",
          ifelse(
            direct_democracy_cols == "init_total",
            "Jährliche Anzahl Abstimmungen über Volksinitiativen",
            ifelse(
              direct_democracy_cols == "ref_total",
              "Jährliche Anzahl Abstimmungen über Referenden",
              ifelse(
                direct_democracy_cols == "turnout_v",
                "Stimmbeteiligung bei kantonalen Volksabstimmungen in Prozent",
                ifelse(
                  direct_democracy_cols == "ddr_snddi",
                  "Sub-National Direct Democracy Index",
                  ifelse(
                    direct_democracy_cols == "obl_finref",
                    "Jährliche Anzahl Abstimmungen über obligatorische Finanzreferenden (Ausgaben)",
                    ifelse(
                      direct_democracy_cols == "fak_finref",
                      "Jährliche Anzahl Abstimmungen über fakultative Finanzreferenden (Ausgaben)",
                  direct_democracy_cols
                    )
                  )
                )
              )
            )
          )
        )
      )
      
      # Set the choices
      updateSelectInput(
        session,
        "variables",
        choices = named_choices,
        selected = ifelse("abst_total" %in% direct_democracy_cols, "abst_total", direct_democracy_cols[1])
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
    })
    
    # Update time range sliders based on data
    observe({
      data <- cleaned_data()
      
      # Get min and max years
      years <- sort(unique(data$jahr))
      
      # Different behavior based on the selected variable
      if (input$variables == "ddr_snddi") {
        # For ddr_snddi, restrict time range slider to 2015-2023
        if (!isTRUE(session$userData$.ddr_snddi_time_restricted)) {
          # Update the time range slider with restricted range
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
            value = 2023
          )
          
          # Set a flag to prevent infinite loop
          session$userData$.ddr_snddi_time_restricted <- TRUE
        }
      } else {
        # If we just switched away from ddr_snddi to another variable,
        # Reset the time sliders to full dataset range
        if (isTRUE(session$userData$.ddr_snddi_time_restricted)) {
          # Reset the time range slider to full range
          updateSliderInput(
            session, 
            "time_range", 
            min = min(years),
            max = max(years),
            value = c(min(years), max(years))
          )
          
          # Reset the single year slider for Kantonsvergleich
          updateSliderInput(
            session,
            "selected_year",
            min = min(years),
            max = max(years),
            value = max(years)
          )
          
          # Reset the flag
          session$userData$.ddr_snddi_time_restricted <- FALSE
        }
      }
    })
    
    # Force re-rendering of plots when time range or selected_year changes for ddr_snddi
    observeEvent(input$time_range, {
      if (input$variables == "ddr_snddi") {
        # Invalidate the filtered data to force a redraw
        invalidateLater(100)
      }
    })
    
    observeEvent(input$selected_year, {
      if (input$variables == "ddr_snddi") {
        # Invalidate the filtered data to force a redraw
        invalidateLater(100)
      }
    })
    
    # Observe changes to the variables input to update the time sliders accordingly
    observeEvent(input$variables, {
      # If ddr_snddi is selected, restrict time sliders
      if (input$variables == "ddr_snddi") {
        # Update the time range slider for Zeittrend
        updateSliderInput(
          session, 
          "time_range", 
          min = 2015,
          max = 2023,
          value = c(2015, 2023)
        )
        
        # Update the selected_year slider for Kantonsvergleich
        updateSliderInput(
          session,
          "selected_year",
          min = 2015,
          max = 2023,
          value = 2023
        )
        
        # Set the flag
        session$userData$.ddr_snddi_time_restricted <- TRUE
      } else {
        # For other variables, use the full range of years
        if (isTRUE(session$userData$.ddr_snddi_time_restricted)) {
          # Get full date range
          years <- sort(unique(cleaned_data()$jahr))
          
          # Reset the time sliders to full range
          updateSliderInput(
            session, 
            "time_range", 
            min = min(years),
            max = max(years),
            value = c(min(years), max(years))
          )
          
          updateSliderInput(
            session,
            "selected_year",
            min = min(years),
            max = max(years),
            value = max(years)
          )
          
          # Reset the flag
          session$userData$.ddr_snddi_time_restricted <- FALSE
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
        # For "Alle Kantone", calculate sum or average by year depending on the variable
        yearly_data <- data %>%
          group_by(jahr) %>%
          summarise(
            value = if(selected_var %in% c("turnout_v", "ddr_snddi")) {
              # For continuous variables, calculate average and also track how many values contributed
              avg_val <- mean(!!sym(selected_var), na.rm = TRUE)
              n_vals <- sum(!is.na(!!sym(selected_var)))
              n_total <- n()
              avg_val  # Return the average as the value
            } else {
              sum(!!sym(selected_var), na.rm = TRUE)
            },
            n_vals = sum(!is.na(!!sym(selected_var))),
            n_total = n()
          )
        
        # For continuous variables, add information about how many cantons contributed to each year's average
        if(selected_var %in% c("turnout_v", "ddr_snddi")) {
          yearly_data <- yearly_data %>%
            mutate(
              # Ensure value is numeric
              value = as.numeric(value),
              hover_text = paste0(
                "Jahr: ", jahr, "<br>",
                "Durchschnittlicher Wert: ", round(as.numeric(value), 2), 
                if(selected_var == "turnout_v") "%" else "", "<br>",
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
        
        # Create the plot as plotly directly for continuous variables
        if(selected_var %in% c("turnout_v", "ddr_snddi")) {
          title_text <- if(selected_var == "turnout_v") {
            "Zeittrend der durchschnittlichen Stimmbeteiligung bei kantonalen Volksabstimmungen"
          } else if(selected_var == "ddr_snddi") {
            "Zeittrend des durchschnittlichen Sub-National Direct Democracy Index"
          }
          
          y_title <- if(selected_var == "turnout_v") {
            "Stimmbeteiligung in Prozent"
          } else if(selected_var == "ddr_snddi") {
            "Direct Democracy Index Wert"
          }
          
          decimal_places <- if(selected_var == "turnout_v") ".1f" else ".2f"
          
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
            title = title_text,
            xaxis = list(title = "Jahr"),
            yaxis = list(
              title = y_title,
              tickformat = decimal_places
            )
          )
          
          # Add a subtitle with time period information for ddr_snddi
          if(selected_var == "ddr_snddi") {
            p <- p %>% layout(
              annotations = list(
                x = 0.5,
                y = 1.05,
                text = "Verfügbar nur für 2015-2023",
                showarrow = FALSE,
                xref = "paper",
                yref = "paper",
                font = list(size = 12)
              )
            )
          }
          
          return(p)
        } else {
          # Create the standard ggplot for non-continuous variables
          p <- ggplot(yearly_data, aes(x = jahr, y = value)) +
            geom_line(color = "#2b6cb0", size = 1) +
            geom_point(color = "#2b6cb0", size = 3) +
            labs(
              title = paste("Zeittrend", 
                          ifelse(selected_var == "abst_total", 
                                "der Gesamtzahl Abstimmungen",
                                ifelse(selected_var == "init_total",
                                      "der Gesamtzahl Abstimmungen über Volksinitiativen",
                                      ifelse(selected_var == "ref_total",
                                            "der Gesamtzahl Abstimmungen über Referenden",
                                            ifelse(selected_var == "obl_finref",
                                                  "der Gesamtzahl Abstimmungen über obligatorische Finanzreferenden",
                                                  ifelse(selected_var == "fak_finref",
                                                        "der Gesamtzahl Abstimmungen über fakultative Finanzreferenden",
                                                        selected_var)))))),
              x = "Jahr",
              y = ifelse(selected_var == "abst_total", 
                        "Gesamtzahl Abstimmungen",
                        ifelse(selected_var == "init_total",
                              "Jährliche Anzahl Abstimmungen über Volksinitiativen",
                              ifelse(selected_var == "ref_total",
                                    "Jährliche Anzahl Abstimmungen über Referenden",
                                    ifelse(selected_var == "obl_finref",
                                          "Jährliche Anzahl Abstimmungen über obligatorische Finanzreferenden",
                                          ifelse(selected_var == "fak_finref",
                                                "Jährliche Anzahl Abstimmungen über fakultative Finanzreferenden",
                                                selected_var)))))
            )
        }
      } else {
        # For "Auswahl von Kantonen", first check if specific cantons have all NA values
        # and remove those cantons from the dataset
        if (anyNA(data[[selected_var]])) {
          # Identify which cantons have only NA values for the selected variable
          na_cantons <- data %>% 
            group_by(kanton) %>% 
            summarize(all_na = all(is.na(!!sym(selected_var)))) %>%
            filter(all_na) %>% 
            pull(kanton)
          
          # Print message about which cantons are being excluded
          if (length(na_cantons) > 0) {
            message(paste("Excluding cantons with all NA values:", paste(na_cantons, collapse=", ")))
          }
          
          # Filter out cantons with all NA values and remove remaining NA values
          data <- data %>% 
            filter(!kanton %in% na_cantons) %>%
            filter(!is.na(!!sym(selected_var)))
        }
        
        # Check if there are any rows left after filtering
        if (nrow(data) == 0) {
          return(plot_ly() %>% 
                   layout(title = "Keine Daten verfügbar für diese Kombination von Filtern",
                          xaxis = list(title = ""),
                          yaxis = list(title = "")))
        }
        
        # For turnout_v and ddr_snddi, create a plotly plot with hover info for missing data context
        if(selected_var %in% c("turnout_v", "ddr_snddi")) {
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
          
          # Set appropriate titles and formatting based on variable
          var_title <- if(selected_var == "turnout_v") {
            "Stimmbeteiligung bei kantonalen Volksabstimmungen"
          } else if(selected_var == "ddr_snddi") {
            "Sub-National Direct Democracy Index"
          }
          
          y_title <- if(selected_var == "turnout_v") {
            "Stimmbeteiligung in Prozent"
          } else if(selected_var == "ddr_snddi") {
            "Direct Democracy Index Wert"
          }
          
          decimal_places <- if(selected_var == "turnout_v") ".1f" else ".2f"
          value_suffix <- if(selected_var == "turnout_v") "%" else ""
          
          p <- plot_ly(
            data = plot_data,
            x = ~jahr,
            y = ~as.numeric(get(selected_var)),
            color = ~kanton,
            type = 'scatter',
            mode = 'lines+markers',
            hoverinfo = 'text',
            text = ~paste0(
              "Kanton: ", kanton, "<br>",
              "Jahr: ", jahr, "<br>",
              var_title, ": ", round(as.numeric(get(selected_var)), if(selected_var == "ddr_snddi") 2 else 1), 
              value_suffix
            )
          ) %>%
          layout(
            title = paste0(
              "Zeittrend ", var_title, "<br>",
              "<sup>Datenverfügbarkeit: ", data_completeness, "% aller möglichen Datenpunkte</sup>"
            ),
            xaxis = list(title = "Jahr"),
            yaxis = list(
              title = y_title,
              tickformat = decimal_places
            ),
            legend = list(title = list(text = "Kanton"))
          )
          
          # Add time period subtitle for ddr_snddi
          if(selected_var == "ddr_snddi") {
            p <- p %>% layout(
              annotations = list(
                x = 0.5,
                y = 1.05,
                text = "Verfügbar nur für 2015-2023",
                showarrow = FALSE,
                xref = "paper",
                yref = "paper",
                font = list(size = 12)
              )
            )
          }
          
          return(p)
        } else {
          # For non-continuous variables, use the standard ggplot
          p <- ggplot(data, aes(x = jahr, y = !!sym(selected_var), color = kanton)) +
            geom_line(size = 1) +
            geom_point(size = 2) +
            labs(
              title = paste("Zeittrend", 
                          ifelse(selected_var == "abst_total", 
                                "der Gesamtzahl Abstimmungen",
                                ifelse(selected_var == "init_total",
                                      "der Gesamtzahl Abstimmungen über Volksinitiativen",
                                      ifelse(selected_var == "ref_total",
                                            "der Gesamtzahl Abstimmungen über Referenden",
                                            ifelse(selected_var == "obl_finref",
                                                  "der Gesamtzahl Abstimmungen über obligatorische Finanzreferenden",
                                                  ifelse(selected_var == "fak_finref",
                                                        "der Gesamtzahl Abstimmungen über fakultative Finanzreferenden",
                                                        selected_var)))))),
              x = "Jahr",
              y = ifelse(selected_var == "abst_total", 
                        "Gesamtzahl Abstimmungen",
                        ifelse(selected_var == "init_total",
                              "Jährliche Anzahl Abstimmungen über Volksinitiativen",
                              ifelse(selected_var == "ref_total",
                                    "Jährliche Anzahl Abstimmungen über Referenden",
                                    ifelse(selected_var == "obl_finref",
                                          "Jährliche Anzahl Abstimmungen über obligatorische Finanzreferenden",
                                          ifelse(selected_var == "fak_finref",
                                                "Jährliche Anzahl Abstimmungen über fakultative Finanzreferenden",
                                                selected_var)))))
            )
        }
      }
      
      # Apply common ggplot theme for non-plotly plots
      if (!exists("p") || !inherits(p, "plotly")) {
        p <- p +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10)
          )
      }
      
      # Convert ggplot to plotly if it's not already a plotly object
      if (!inherits(p, "plotly")) {
        p <- ggplotly(p, tooltip = "text")
      }
      
      return(p)
    })
    
    # Canton comparison plot
    output$canton_comparison_plot <- renderPlotly({
      req(filtered_data(), input$variables)
      data <- filtered_data()
      
      # Get the selected variable
      selected_var <- input$variables
      
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
      
      # Get title and y-axis label based on variable
      title_text <- paste("Kantonsvergleich der", 
                        ifelse(selected_var == "abst_total", 
                              "Anzahl Abstimmungen",
                              ifelse(selected_var == "init_total",
                                    "Anzahl Abstimmungen über Volksinitiativen",
                                    ifelse(selected_var == "ref_total",
                                          "Anzahl Abstimmungen über Referenden",
                                          ifelse(selected_var == "turnout_v",
                                                "Stimmbeteiligung bei kantonalen Volksabstimmungen",
                                                ifelse(selected_var == "ddr_snddi",
                                                      "Sub-National Direct Democracy Index Werte",
                                                      ifelse(selected_var == "obl_finref",
                                                            "Anzahl Abstimmungen über obligatorische Finanzreferenden",
                                                            ifelse(selected_var == "fak_finref",
                                                                  "Anzahl Abstimmungen über fakultative Finanzreferenden",
                                                                  selected_var))))))))
      
      y_title <- ifelse(selected_var == "abst_total", 
                      "Anzahl Abstimmungen",
                      ifelse(selected_var == "init_total",
                            "Anzahl Abstimmungen über Volksinitiativen",
                            ifelse(selected_var == "ref_total",
                                  "Anzahl Abstimmungen über Referenden",
                                  ifelse(selected_var == "turnout_v",
                                        "Stimmbeteiligung in Prozent",
                                        ifelse(selected_var == "ddr_snddi",
                                              "Direct Democracy Index Wert",
                                              ifelse(selected_var == "obl_finref",
                                                    "Anzahl Abstimmungen über obligatorische Finanzreferenden",
                                                    ifelse(selected_var == "fak_finref",
                                                          "Anzahl Abstimmungen über fakultative Finanzreferenden",
                                                          selected_var)))))))
      
      # Create the plot
      p <- ggplot(canton_avg, aes(x = reorder(kanton, avg_value), y = avg_value)) +
        geom_bar(stat = "identity", fill = "#2b6cb0") +
        coord_flip() +
        labs(
          title = title_text,
          x = "Kanton",
          y = y_title
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)
        )
      
      # Add scale_y_continuous with breaks for full integers if abst_total, init_total, or ref_total is selected
      if (selected_var %in% c("abst_total", "init_total", "ref_total", "obl_finref", "fak_finref")) {
        p <- p + scale_y_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1))
      }
      
      # Add scale_y_continuous with breaks for decimal values if turnout_v or ddr_snddi is selected
      if (selected_var %in% c("turnout_v", "ddr_snddi")) {
        decimal_places <- if(selected_var == "ddr_snddi") 2 else 1
        p <- p + scale_y_continuous(labels = function(x) sprintf(paste0("%.", decimal_places, "f"), x))
      }
      
      # Convert to plotly
      p <- ggplotly(p) %>%
        layout(
          margin = list(l = 100),  # Add more margin on the left for canton names
          yaxis = list(
            title = y_title,
            tickformat = if(selected_var %in% c("turnout_v", "ddr_snddi")) 
                         ifelse(selected_var == "ddr_snddi", ".2f", ".1f") else NULL
          )
        )
      
      # Add special annotation for ddr_snddi
      if(selected_var == "ddr_snddi") {
        p <- p %>% layout(
          annotations = list(
            list(
              x = 0.5,
              y = 1.05,
              text = "Verfügbar nur für 2015-2023",
              showarrow = FALSE,
              xref = "paper",
              yref = "paper",
              font = list(size = 12)
            )
          )
        )
      }
      
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
      names(table_data) <- c(
        "Kanton",
        "Jahr",
        ifelse(input$variables == "abst_total", 
               "Anzahl Abstimmungen",
               ifelse(input$variables == "init_total",
                     "Anzahl Abstimmungen über Volksinitiativen",
                     ifelse(input$variables == "ref_total",
                           "Anzahl Abstimmungen über Referenden",
                           ifelse(input$variables == "turnout_v",
                                 "Stimmbeteiligung in Prozent",
                                 ifelse(input$variables == "ddr_snddi",
                                       "Sub-National Direct Democracy Index",
                                       ifelse(input$variables == "obl_finref",
                                             "Anzahl Abstimmungen über obligatorische Finanzreferenden (Ausgaben)",
                                             ifelse(input$variables == "fak_finref",
                                                   "Anzahl Abstimmungen über fakultative Finanzreferenden (Ausgaben)",
                                                   input$variables)))))))
      )
      
      # Format values for specific variables
      if (input$variables %in% c("turnout_v", "ddr_snddi")) {
        col_index <- which(names(table_data) != "Kanton" & names(table_data) != "Jahr")
        decimal_places <- if(input$variables == "ddr_snddi") 2 else 1
        
        table_data[[col_index]] <- sapply(table_data[[col_index]], function(x) {
          if (is.na(x)) return(NA)
          sprintf(paste0("%.", decimal_places, "f"), as.numeric(x))
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
    
    # Correlation plot
    output$correlation_plot <- renderPlotly({
      req(filtered_data(), input$variables)
      data <- filtered_data()
      
      # Get the selected variable
      selected_var <- input$variables
      
      # Check if there are valid data points to correlate
      if (all(is.na(data[[selected_var]]))) {
        # Return an empty plot with a message if all values are NA
        return(plot_ly() %>% 
                 layout(title = "Keine Daten verfügbar für diese Variable",
                        xaxis = list(title = ""),
                        yaxis = list(title = "")))
      }
      
      # Find potential correlates
      potential_correlates <- names(data)[grep("institution|democracy|demokratie|vote|abstimmung|referendum|initiative",
                                              names(data), ignore.case = TRUE)]
      
      # Remove the selected variable from potential correlates
      potential_correlates <- potential_correlates[potential_correlates != selected_var]
      
      # Filter to only include variables that have at least some non-NA values
      potential_correlates <- potential_correlates[sapply(potential_correlates, function(var) {
        !all(is.na(data[[var]]))
      })]
      
      # Check if there are any correlates left
      if (length(potential_correlates) == 0) {
        return(plot_ly() %>% 
                 layout(title = "Keine korrelierten Variablen gefunden",
                        xaxis = list(title = ""),
                        yaxis = list(title = "")))
      }
      
      # Calculate correlations with error handling
      correlations <- sapply(potential_correlates, function(var) {
        tryCatch({
          # Check if there are enough paired non-NA values to calculate correlation
          if (sum(!is.na(data[[selected_var]]) & !is.na(data[[var]])) < 2) {
            return(NA)
          }
          cor(data[[selected_var]], data[[var]], use = "complete.obs")
        }, error = function(e) {
          return(NA)
        })
      })
      
      # Remove NA correlations
      valid_correlations <- correlations[!is.na(correlations)]
      
      # Check if there are any valid correlations
      if (length(valid_correlations) == 0) {
        return(plot_ly() %>% 
                 layout(title = "Keine validen Korrelationen gefunden",
                        xaxis = list(title = ""),
                        yaxis = list(title = "")))
      }
      
      # Create correlation data frame
      cor_data <- data.frame(
        variable = names(valid_correlations),
        correlation = valid_correlations
      )
      
      # Create the plot
      p <- ggplot(cor_data, aes(x = reorder(variable, correlation), y = correlation)) +
        geom_bar(stat = "identity", fill = "#2b6cb0") +
        coord_flip() +
        labs(
          title = paste("Korrelationen mit", 
                       ifelse(selected_var == "abst_total", 
                              "Anzahl Abstimmungen",
                              ifelse(selected_var == "init_total",
                                    "Anzahl Abstimmungen über Volksinitiativen",
                                    ifelse(selected_var == "ref_total",
                                          "Anzahl Abstimmungen über Referenden",
                                          ifelse(selected_var == "turnout_v",
                                                "Stimmbeteiligung bei kantonalen Volksabstimmungen",
                                                ifelse(selected_var == "ddr_snddi",
                                                      "Sub-National Direct Democracy Index",
                                                      ifelse(selected_var == "obl_finref",
                                                            "Anzahl Abstimmungen über obligatorische Finanzreferenden (Ausgaben)",
                                                            ifelse(selected_var == "fak_finref",
                                                                  "Anzahl Abstimmungen über fakultative Finanzreferenden (Ausgaben)",
                                                                  selected_var)))))))),
          x = "Variable",
          y = "Korrelation"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)
        )
      
      ggplotly(p) %>%
        layout(
          hovermode = "y unified",
          showlegend = FALSE
        )
    })
    
    # Statistical summary
    output$statistical_summary <- renderPrint({
      req(filtered_data(), input$variables)
      data <- filtered_data()
      
      # Get the selected variable
      selected_var <- input$variables
      
      # Calculate summary statistics
      summary_stats <- data %>%
        summarise(
          Mean = mean(!!sym(selected_var), na.rm = TRUE),
          Median = median(!!sym(selected_var), na.rm = TRUE),
          SD = sd(!!sym(selected_var), na.rm = TRUE),
          Min = min(!!sym(selected_var), na.rm = TRUE),
          Max = max(!!sym(selected_var), na.rm = TRUE),
          N = n(),
          Missing = sum(is.na(!!sym(selected_var)))
        )
      
      # Format summary statistics for continuous variables (more decimal places)
      if (selected_var %in% c("turnout_v", "ddr_snddi")) {
        decimal_places <- if(selected_var == "ddr_snddi") 2 else 1
        
        summary_stats <- summary_stats %>%
          mutate(across(c(Mean, Median, SD, Min, Max), ~round(., decimal_places)))
      }
      
      # Identify cantons with all NA values
      na_cantons <- data %>% 
        group_by(kanton) %>% 
        summarize(all_na = all(is.na(!!sym(selected_var)))) %>%
        filter(all_na) %>% 
        pull(kanton)
      
      # Calculate the completeness of data if selected_var is turnout_v or ddr_snddi
      data_completeness <- NULL
      years_with_data <- NULL
      if(selected_var %in% c("turnout_v", "ddr_snddi")) {
        # Calculate percentage of data available
        total_possible <- nrow(data)
        total_available <- total_possible - summary_stats$Missing
        data_completeness <- round(100 * total_available / total_possible, 1)
        
        # Calculate how many years have at least some data
        years_with_data <- data %>%
          group_by(jahr) %>%
          summarize(has_data = any(!is.na(!!sym(selected_var)))) %>%
          summarize(count = sum(has_data)) %>%
          pull(count)
      }
      
      # Print the summary
      cat("Statistische Zusammenfassung:\n\n")
      cat(paste("Variable:", ifelse(selected_var == "abst_total", 
                                   "Anzahl Abstimmungen",
                                   ifelse(selected_var == "init_total",
                                         "Anzahl Abstimmungen über Volksinitiativen",
                                         ifelse(selected_var == "ref_total",
                                               "Anzahl Abstimmungen über Referenden",
                                               ifelse(selected_var == "turnout_v",
                                                     "Stimmbeteiligung bei kantonalen Volksabstimmungen in Prozent",
                                                     ifelse(selected_var == "ddr_snddi",
                                                           "Sub-National Direct Democracy Index",
                                                           ifelse(selected_var == "obl_finref",
                                                                 "Jährliche Anzahl Abstimmungen über obligatorische Finanzreferenden (Ausgaben)",
                                                                 ifelse(selected_var == "fak_finref",
                                                                       "Jährliche Anzahl Abstimmungen über fakultative Finanzreferenden (Ausgaben)",
                                                                       selected_var))))))), "\n\n"))
      
      # Add a special note for ddr_snddi
      if(selected_var == "ddr_snddi") {
        cat("Hinweis: Diese Variable ist nur für die Jahre 2015-2023 verfügbar.\n\n")
      }
      
      # Add a note about missing values if the selected variable has any
      if (summary_stats$Missing > 0) {
        cat(paste("Hinweis: Diese Variable enthält", summary_stats$Missing, "fehlende Werte (NA). Alle Berechnungen wurden ohne Berücksichtigung dieser Werte durchgeführt.\n\n"))
        
        # If it's turnout_v or ddr_snddi, add more detailed information about data completeness
        if(selected_var %in% c("turnout_v", "ddr_snddi")) {
          cat(paste("Datenverfügbarkeit:", data_completeness, "% aller möglichen Datenpunkte\n"))
          cat(paste("Jahre mit Daten:", years_with_data, "von", length(unique(data$jahr)), "\n\n"))
        }
        
        # If there are cantons with all NA values, list them
        if (length(na_cantons) > 0) {
          cat(paste("Folgende Kantone haben nur NA-Werte für diese Variable und wurden aus den Visualisierungen ausgeschlossen:\n", 
                   paste(na_cantons, collapse=", "), "\n\n"))
        }
      }
      
      print(summary_stats)
    })
  })
} 