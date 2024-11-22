## Code to make the dashboard for the climate modelling tutorial 
## taking place in Kigali in December 2024
## Monica Golumbeanu monica.golumbeanu@swisstph.ch
## Last update 18/11/2024

library(shiny)
library(shinyWidgets)
library(bslib)
library(shinyhelper)
library(plotly)
library(shinyjs)
library(shinycssloaders)

# page_navbar from bslib provides a tabbed page layout. The Swiss TPH logo is
# integrated into the title bar and the minty theme used by default.
page_navbar(
  title = span(img(src = "logosmall.png"), "Climate-Driven Malaria Model Predictions"),
  theme = bs_theme(bootswatch = "minty"),
  useShinyjs(),
  
  # Theme can be adjust here
  # theme = bs_theme(bootswatch = "minty"),
  
  # Include custom CSS for styling
  tags$head(
    tags$style(HTML("
      .sidebar-section {
        border: 1px solid black; /* Changed color to black */
        border-radius: 5px;
        padding: 10px;
        margin-bottom: 15px;
      }
    "))
  ),
  
  
  ### Panel 1 ####
  # The first panel corresponds to the homepage and contains all the analysis.
  nav_panel(
    "Home",
    layout_sidebar(
      # User input is handled in the side bar
      sidebar = sidebar(
        
        # Contour wrapper for location
        tags$div(
          class = "sidebar-section",
          tags$h5("Location", style = "margin-top: 0;"),
          # Districts to select (grouped by region)
          selectInput(
            inputId = "province_choice",              # Unique ID for the dropdown
            label = "Select a province:",      # Label displayed to users
            choices = c("Kigali City", "East", "West", 
                        "North", "South"),
            selected = "Kigali City"                 # Default selected value
          ),
          textOutput("selected_site_name")
        ),
        
        # Contour wrapper for climate
        tags$div(
          class = "sidebar-section",
          tags$h5("Climate parameters", style = "margin-top: 0;"),
          # Climate scenarios to select
          selectInput(
            inputId = "temperature_scenario",              # Unique ID for the dropdown
            label = "Select a temperature scenario:",      # Label displayed to users
            choices = c("normal", "warmer", "extreme warm", "colder", "extreme cold"),
            selected = "normal"                 # Default selected value
          ),
          selectInput(
            inputId = "rainfall_scenario",              # Unique ID for the dropdown
            label = "Select a rainfall scenario:",      # Label displayed to users
            choices = c("normal", "drier", "extreme dry", "wetter", "extreme wet"),
            selected = "normal"                 # Default selected value
          ),
          conditionalPanel(
            condition = "input.rainfall_scenario == 'normal'", # Show only if seasonal scenarios are selected
            selectInput(
              inputId = "seasonality_scenario",              # Unique ID for the dropdown
              label = "Select a seasonality scenario:",      # Label displayed to users
              choices = c("no change", "none", "amplified", "longer", "shorter"),
              selected = "no change"                 # Default selected value
            )
          )
        ),
        
        # Seasonal malaria chemoprevention
        # Contour wrapper for interventions
        conditionalPanel(
          condition = "input.seasonality_scenario != 'none'", # Show only if seasonal scenarios are selected
          tags$div(
            class = "sidebar-section",
            tags$h5("Seasonal malaria chemoprevention", style = "margin-top: 0;"),
            # Year range
            sliderInput("SMC_cov", "Select coverage:", 
                        min = 0, max = 100, 
                        value = c(0), step = 10,
                        sep = ""),
            
          )
        ),
        
        # Contour wrapper for display
        tags$div(
          class = "sidebar-section",
          tags$h5("Display options", style = "margin-top: 0;"),
          # Year range
          sliderInput("year_forecast", "Select Year Range:", 
                      min = 2024, max = 2039, 
                      value = c(2030), step = 1,
                      sep = ""),
          tags$hr(),
          # Output
          radioButtons(
            inputId = "output_type",   # Unique ID for this input
            label = "Choose displayed output:",    # Label displayed above the buttons
            choices = c("Incidence" = "inc", 
                        "Entomological inoculation rate" = "EIR"),  # Define choices
            selected = "inc"               # Default selected option
          ),
          tags$hr(),
          # Conditional Panel: Sub-options for age groups if selected incidence
          conditionalPanel(
            condition = "input.output_type == 'inc'", # Show only if incidence is selected
            radioButtons(
              inputId = "age_groups",
              label = "Choose an age group:",
              choices = c("under 5 years", "all ages"),
              selected = "all ages"
            )
          ),
        ),
        
        # Reset button
        #actionButton("reset_button", "Reset")
      ),
      
      # Main panel for displaying outputs ----
      # Climate variables
      div(
        class = "card",
        full_screen = TRUE,
        style = "border: 1px solid #ccc; padding: 10px; margin-top: 10px; position: relative;",
        # Superposed title at the top of the border
        div(
          id = "climate_title",
          style = "display: flex; align-items: center;",
          h3("Climate specifications", style = "margin-right: 10px;"),  # Title with right margin
          actionButton("hide_climate_btn", label = "\u25B2", style = "padding: 2px 5px; font-size: 14px;")  # Small button
        ),
        
        div(
          id = "climate_div",
          style = "margin-top: 3px;",
          layout_column_wrap(
            width = 1 / 2,
            card(
              card_header("Temperature trends"),
              full_screen = TRUE,
              withSpinner(plotlyOutput("temp_plot"), type = 8, color = "grey")  # Add spinner
            ),
            card(
              card_header("Rainfall trends"),
              full_screen = TRUE,
              withSpinner(plotlyOutput("rain_plot"), type = 8, color = "grey")  # Add spinner
            )
          )
        )
      ),
      
      # Model outputs
      div(class = "card",
          full_screen = TRUE,
          div(class = "card-body",
              full_screen = TRUE,
              h4("Predicted malaria transmission dynamics"),
              # p("This plot shows the predicted incidence based on the selected parameters on the model output."),
              plotlyOutput("model_plot")
          )
      )
      
    ) 
  ),
  
  nav_panel(
    "Assumptions & Calibration"
  )
  
  
  
)