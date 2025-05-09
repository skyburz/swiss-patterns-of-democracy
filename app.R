# Swiss Patterns of Democracy - Main Application
# Dies ist die Haupt-Shiny-App-Datei, die die Benutzeroberfläche und Serverlogik definiert

# Source global file which loads packages and helper functions
source("global.R")
source("R/helpers.R")
source("R/democratic_institutions_module.R")
source("R/direct_democracy_module.R")
source("R/canton_map_module.R")
source("R/elections_module.R")
source("R/municipalities_module.R")

# User Interface ---------------------------------------------------------
ui <- page_navbar(
  # App title
  title = "Schweizer Demokratiemuster",
  
  # Use the custom theme from global.R
  theme = custom_theme,
  
  # Add a custom CSS file - moved it here before the nav_panel items
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  # Navigation pages
  nav_panel(
    title = "Dashboard",
    icon = bsicons::bs_icon("speedometer2"),
    
    layout_sidebar(
      # Sidebar with controls
      sidebar = sidebar(
        title = "Steuerung",
        
        # File selector
        selectInput(
          "dataset", 
          "Datensatz auswählen:",
          choices = list_data_files(),
          selected = list_data_files()[1]
        ),
        
        # We'll add more controls here as we understand the data
        hr(),
        
        # Add info text
        p("Wählen Sie einen Datensatz aus, um mit der Erkundung der Schweizer Demokratiemuster zu beginnen."),
        tags$small("Datenvisualisierungen werden automatisch aktualisiert.")
      ),
      
      # Main content area - removing the value boxes
      
      # Card with data overview
      card(
        card_header("Datensatzübersicht"),
        card_body(
          DTOutput("data_preview")
        )
      ),
      
      # Placeholder for visualizations
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Visualisierung"),
          card_body(
            "Wählen Sie Daten und Variablen aus, um dynamische Diagramme zu visualisieren."
          )
        ),
        card(
          card_header("Zusätzliche Informationen"),
          card_body(
            "Analysen und Erkenntnisse werden hier basierend auf den ausgewählten Daten angezeigt."
          )
        )
      )
    )
  ),
  
  # Democratic Institutions page
  nav_panel(
    title = "Demokratische Institutionen",
    icon = bsicons::bs_icon("building"),
    
    # Add our democratic institutions module UI here
    democratic_institutions_ui("institutions")
  ),
  
  # Direct Democracy page
  nav_panel(
    title = "Direkte Demokratie",
    icon = bsicons::bs_icon("bar-chart"),
    
    # Add our direct democracy module UI here
    direct_democracy_ui("direct_democracy")
  ),
  
  # Elections page
  nav_panel(
    title = "Wahlen",
    icon = bsicons::bs_icon("check-square"),
    
    # Add our elections module UI here
    elections_ui("elections")
  ),
  
  # Municipalities page
  nav_panel(
    title = "Gemeinden",
    icon = bsicons::bs_icon("houses"),
    
    # Add our municipalities module UI here
    municipalities_ui("municipalities")
  ),
  
  # Map Visualization - moved here between Gemeinden and Über
  nav_panel(
    title = "Kantonskarte",
    icon = bsicons::bs_icon("map"),
    
    # Add our canton map module UI here
    canton_map_ui("canton_map")
  ),
  
  # About page
  nav_panel(
    title = "Über",
    icon = bsicons::bs_icon("info-circle"),
    
    card(
      card_header("Über diese Anwendung"),
      card_body(
        h3("Schweizer Demokratiemuster"),
        p("Diese Anwendung untersucht demokratische Muster und Trends in der Schweiz."),
        p("Verwenden Sie das Dashboard, um mit verschiedenen Datensätzen zu interagieren und verschiedene Aspekte der Schweizer Demokratie zu visualisieren."),
        hr(),
        h4("Datenquellen"),
        p("Die in dieser Anwendung verwendeten Daten stammen aus verschiedenen Quellen zu Schweizer demokratischen Institutionen und Prozessen."),
        hr(),
        h4("Kontakt"),
        p("Für Fragen oder Feedback wenden Sie sich bitte an das Entwicklungsteam.")
      )
    )
  ),
  
  # Simple footer
  footer = div(
    class = "footer",
    "Schweizer Demokratiemuster App © ", format(Sys.Date(), "%Y")
  )
)

# Server Logic ----------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive expression to load the selected dataset
  selected_data <- reactive({
    req(input$dataset)
    tryCatch({
      load_dataset(input$dataset)
    }, error = function(e) {
      showNotification(
        paste("Fehler beim Laden des Datensatzes:", e$message),
        type = "error",
        duration = 10
      )
      return(NULL)
    })
  })
  
  # Display data preview
  output$data_preview <- renderDT({
    req(selected_data())
    
    # Get the data and column names in separate steps to better debug issues
    data <- selected_data()
    col_names <- names(data)
    
    # Display the data with a safer approach to column names
    datatable(
      data,
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        dom = 'ftip'
      ),
      rownames = FALSE,
      # Don't use clean_names directly - handle column names more safely
      # colnames = clean_names(names(selected_data())),
      class = "compact stripe"
    )
  })
  
  # Call democratic institutions module server and pass the selected_data reactive
  democratic_institutions_server("institutions", selected_data)
  
  # Call direct democracy module server and pass the selected_data reactive
  direct_democracy_server("direct_democracy", selected_data)
  
  # Call canton map module server and pass the selected_data reactive
  canton_map_server("canton_map", selected_data)
  
  # Call elections module server and pass the selected_data reactive
  elections_server("elections", selected_data)
  
  # Call municipalities module server and pass the selected_data reactive
  municipalities_server("municipalities", selected_data)
}

# Run the application
shinyApp(ui = ui, server = server) 