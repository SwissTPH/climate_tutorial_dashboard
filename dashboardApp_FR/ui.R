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
  title = span(img(src = "logosmall.png"), HTML("Mod&egrave;le Pr&eacute;dictif du Paludisme Bas&eacute; sur le Climat")),
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
    HTML("Pr&eacute;dictions"),
    layout_sidebar(
      # User input is handled in the side bar
      sidebar = sidebar(
        p(em(HTML("Modifiez les param&egrave;tres ci-dessous pour explorer les r&eacute;sultats de diff&eacute;rents sc&eacute;narios."))),
        # Contour wrapper for location
        tags$div(
          class = "sidebar-section",
          tags$h5(HTML("R&eacute;gion"), style = "margin-top: 0;"),
          # Districts to select (grouped by region)
          selectInput(
            inputId = "province_choice",              # Unique ID for the dropdown
            label = HTML("S&egrave;lectionnez une province:"),      # Label displayed to users
            choices = c("Kigali City", "East", "West", 
                        "North", "South"),
            selected = "Kigali City"                 # Default selected value
          ),
          textOutput("selected_site_name")
        ),
        
        # Contour wrapper for climate
        tags$div(
          class = "sidebar-section",
          tags$h5(HTML("Param&egrave;tres climatiques"), style = "margin-top: 0;"),
          # Climate scenarios to select
          selectInput(
            inputId = "temperature_type",              # Unique ID for the dropdown
            label = HTML("S&eacute;lectionnez le type d'hypoth&egrave;se:"),      # Label displayed to users
            choices = c("baseline constant", "temporal evolution"),
            selected = "temporal evolution"                 # Default selected value
          ),
          conditionalPanel(
            condition = "input.temperature_type != 'baseline constant'", # Show only if continuous temperatures are selected
            selectInput(
              inputId = "temperature_scenario",              # Unique ID for the dropdown
              label = HTML("S&eacute;lectionnez un sc&eacute;nario de temp&eacute;rature:"),      # Label displayed to users
              choices = c("normal", "warmer", "extreme warm", "colder", "extreme cold"),
              selected = "normal"                 # Default selected value
            )
          ),
          conditionalPanel(
            condition = "input.temperature_type == 'baseline constant'", # Show only if discrete temperatures are selected
            sliderInput("discrete_temperature", HTML("S&eacute;lectionnez une temp&eacute;rature:"), 
                        min = 20, max = 28, 
                        value = c(20), step = 2,
                        sep = "")
          ),
          selectInput(
            inputId = "rainfall_scenario",              # Unique ID for the dropdown
            label = HTML("S&eacute;lectionnez un sc&eacute;nario de pr&eacute;cipitations:"),      # Label displayed to users
            choices = c("normal", "drier", "extreme dry", "more wet", "extreme wet"),
            selected = "normal"                 # Default selected value
          ),
          selectInput(
            inputId = "seasonality_scenario",              # Unique ID for the dropdown
            label = HTML("S&eacute;lectionnez un sc&eacute;nario de saisonnalit&eacute;:"),      # Label displayed to users
            choices = c("no change", "amplified", "longer", "shorter"), #"none"
            selected = "no change"                 # Default selected value
          )
        ),
        
        # Contour wrapper for seasonal malaria chemoprevention
        conditionalPanel(
          condition = "input.seasonality_scenario != 'none'", # Show only if seasonal scenarios are selected
          tags$div(
            class = "sidebar-section",
            tags$h5(HTML("Chimiopr&eacute;vention du paludisme saisonnier (CPS)"), style = "margin-top: 0;"),
            # Year range
            sliderTextInput(
              inputId = "SMC_cov",
              label = HTML("S&eacute;lectionnez une valeur pour la couverture CPS:"),
              choices = c(0, 20, 40, 60, 80, 100),   # Custom values
              selected = 0,                 # Default selected value
              grid = TRUE                    # Display tick marks
            ),
          )
        ),
        
        # Contour wrapper for display
        tags$div(
          class = "sidebar-section",
          tags$h5("Display options", style = "margin-top: 0;"),
          # Year range
          sliderInput("year_forecast", HTML("S&eacute;lectionnez l'ann&eacute;e de fin de la pr&eacute;vision:"), 
                      min = 2025, max = 2039, 
                      value = c(2030), step = 1,
                      sep = ""),
          tags$hr(),
          # Output
          radioButtons(
            inputId = "output_type",   # Unique ID for this input
            label = HTML("Choisissez le r&eacute;sultat affich&eacute; :"),    # Label displayed above the buttons
            choices = c("Incidence" = "inc", 
                        "Taux d'inoculation entomologique" = "EIR"),  # Define choices
            selected = "inc"               # Default selected option
          ),
          tags$hr(),
          # Conditional Panel: Sub-options for age groups if selected incidence
          conditionalPanel(
            condition = "input.output_type == 'inc'", # Show only if incidence is selected
            radioButtons(
              inputId = "age_groups",
              label = HTML("Choisissez un groupe d&apos;âge:"),
              choices = c("moins de 5 ans", "tous"),
              selected = "tous"
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
          h3(HTML("Sp&eacute;cifications climatiques"), style = "margin-right: 10px;"),  # Title with right margin
          actionButton("hide_climate_btn", label = "\u25B2", style = "padding: 2px 5px; font-size: 14px;")  # Small button
        ),
        
        div(
          id = "climate_div",
          style = "margin-top: 3px;",
          layout_column_wrap(
            width = 1 / 2,
            card(
              card_header(HTML("Les profils de temp&eacute;rature")),
              full_screen = TRUE,
              # Tabset panel with two tabs
              tabsetPanel(
                tabPanel("Quotidien", 
                         withSpinner(plotlyOutput("temp_plot"), type = 8, color = "grey")  # Add spinner
                ),
                tabPanel("Moyenne mobile", 
                         withSpinner(plotlyOutput("temp_plot_rollmean"), type = 8, color = "grey")  # Add spinner
                )
              )
            ),
            card(
              card_header(HTML("&Eacute;volutions des pr&eacute;cipitations")),
              full_screen = TRUE,
              # Tabset panel with two tabs
              tabsetPanel(
                tabPanel("Quotidien", 
                         withSpinner(plotlyOutput("rain_plot"), type = 8, color = "grey")  # Add spinner
                ),
                tabPanel("Moyenne mobile", 
                         withSpinner(plotlyOutput("rain_plot_rollmean"), type = 8, color = "grey")  # Add spinner
                ),
                tabPanel("Anomalie", 
                         withSpinner(plotlyOutput("rain_plot_anomaly"), type = 8, color = "grey")  # Add spinner
                )
              )
            )
          )
        )
      ),
      
      # Model outputs
      div(class = "card",
          full_screen = TRUE,
          div(class = "card-body",
              full_screen = TRUE,
              h4(HTML("Pr&eacute;dictions de la dynamique de transmission du paludisme")),
              # Tabset panel with two tabs
              tabsetPanel(
                tabPanel("Mensuel", 
                         withSpinner(plotlyOutput("model_plot_monthly"), type = 8, color = "grey")  # Add spinner
                ),
                tabPanel("Annuel",
                         withSpinner(plotlyOutput("model_plot_yearly"), type = 8, color = "grey") 
                )
              )
          )
      )
      
    ) 
  ),
  
  nav_panel(
    HTML("M&eacute;thodes et Hypoth&egrave;ses"),
    tabPanel("Table of Contents",
             fluidRow(
               
               column(3,
                      wellPanel(
                        h4(HTML("Table des mati&egrave;res")),
                        tags$ul(
                          tags$li(a(HTML("Description des donn&eacute;es climatiques"), href = "#section1")),
                          tags$li(a("Profils climatiques", href = "#section2")),
                          tags$li(a(HTML("Mod&egrave;le climatique"), href = "#section3")),
                          tags$li(a(HTML("Chimiopr&eacute;vention du paludisme saisonnier"), href = "#section4"))
                        )
                      )
               ),
               column(9,
                      tags$div(id = "section1",
                               h3(HTML("Description des donn&eacute;es climatiques")),
                               p(HTML("Les donn&eacute;es climatiques utilis&eacute;es dans le 
                        tableau de bord comprennent les pr&eacute;cipitations 
                        quotidiennes (1995&ndash;2023) et les temp&eacute;ratures 
                        quotidiennes (1995&ndash;2016) obtenues &agrave; partir 
                        des produits du groupe Climate Hazards Group Infrared 
                        Precipitation and Temperature with Stations (CHIRPS et CHIRTS). 
                        En raison d'un manque de donn&eacute;es de temp&eacute;rature r&eacute;centes, 
                        des moyennes quotidiennes de temp&eacute;rature, 
                        d&eacute;riv&eacute;es des observations de 1995 &agrave; 2016, 
                        ont &eacute;t&eacute; utilis&eacute;es pour la p&eacute;riode 
                        allant de 2017 &agrave; 2023. Les valeurs de 
                        temp&eacute;rature et de pr&eacute;cipitations de 2024 
                        &agrave; 2039 sont bas&eacute;es sur des projections 
                        r&eacute;gionales r&eacute;duites provenant du 
                        sixi&egrave;me rapport d&rsquo;&eacute;valuation (AR 6) 
                        du Groupe d'experts intergouvernemental sur l'&eacute;volution 
                        du climat (GIEC). Ces projections supposent un sc&eacute;nario 
                        de changement climatique interm&eacute;diaire, suivant les 
                        trajectoires repr&eacute;sentatives de concentration (RCP) 4.5.")),
                      ),
                      br(),
                      tags$div(id = "section2",
                               h3("Profils climatiques"),
                               p(HTML("Plusieurs sc&eacute;narios de temp&eacute;rature 
                      et de pr&eacute;cipitations ont &eacute;t&eacute; fournis 
                      dans le tableau de bord climatique. Ces profils de 
                      temp&eacute;rature et de pr&eacute;cipitations ont 
                      &eacute;t&eacute; d&eacute;riv&eacute;s &agrave; partir 
                      des valeurs observ&eacute;es historiquement, ainsi que de 
                      projections probables et th&eacute;oriques en supposant des 
                      conditions de changement climatique. Les profils climatiques 
                      suivants ont &eacute;t&eacute; sp&eacute;cialement con&ccedil;us 
                      en tenant compte des relations existantes entre le climat 
                      et le paludisme au Rwanda, afin de permettre l'exploration 
                      de l'influence dynamique de la saisonnalit&eacute; et de 
                      la variabilit&eacute; climatiques sur la transmission du 
                      paludisme. Dans la plupart des cas, une description 
                      suppl&eacute;mentaire est fournie ci-dessous.")),
                               br(),
                               
                               p(strong(em("Normal temperature (temporal evolution)"))),
                               p(HTML("Calcul&eacute;e comme la moyenne journali&egrave;re 
                             des valeurs de temp&eacute;rature observ&eacute;es 
                             entre 1995 et 2016. Les valeurs futures &agrave; 
                             partir de 2024 sont suppos&eacute;es maintenir 
                             cette normale climatique comme r&eacute;f&eacute;rence. 
                             En raison de l'absence de donn&eacute;es CHIRTS pendant 
                             la p&eacute;riode 2017-2023, les valeurs sont 
                             d&eacute;riv&eacute;es comme la moyenne 
                             journali&egrave;re des temp&eacute;ratures de 1995&ndash;2016*.")),
                               
                               p(strong(em("Colder temperature (temporal evolution)"))),
                               p(HTML("Calcul&eacute;e comme le 10e centile des valeurs 
                              quotidiennes de temp&eacute;rature observ&eacute;es entre 1995 
                              et 2016. Les valeurs &agrave; partir de 2024 sont suppos&eacute;es 
                              suivre l'augmentation projet&eacute;e des temp&eacute;ratures moyennes selon le RCP 4.5.")),
                               
                               p(strong(em("Extreme cold temperature (temporal evolution)"))),
                               p(HTML("Calcul&eacute;e comme le 35e centile des valeurs 
                              quotidiennes de temp&eacute;rature observ&eacute;es entre 1995 
                              et 2016. Les valeurs &agrave; partir de 2024 sont suppos&eacute;es 
                              suivre l'augmentation projet&eacute;e des temp&eacute;ratures moyennes selon le RCP 4.5.")),
                               
                               p(strong(em("Warmer temperature (temporal evolution)"))),
                               p(HTML("Calcul&eacute;e comme la moyenne quotidienne des valeurs 
                              de temp&eacute;rature observ&eacute;es entre 1995 et 2016, 
                              similaire &agrave; la temp&eacute;rature normale. Cependant, 
                              les valeurs futures &agrave; partir de 2024 sont suppos&eacute;es 
                              suivre l'augmentation projet&eacute;e des temp&eacute;ratures 
                              moyennes selon le RCP 4.5.")),
                               
                               p(strong(em("Extreme warm temperature (temporal evolution)"))),
                               p(HTML("Calcul&eacute;e comme le 90e centile des valeurs 
                              quotidiennes de temp&eacute;rature observ&eacute;es entre 1995 
                              et 2016. Les valeurs &agrave; partir de 2024 sont suppos&eacute;es 
                              suivre l'augmentation projet&eacute;e des temp&eacute;ratures 
                              moyennes selon le RCP 4.5.")),
                               
                               p(strong(em("Baseline constant temperature"))),
                               p(HTML("Calcul&eacute;e comme la moyenne quotidienne des valeurs 
                              de temp&eacute;rature observ&eacute;es entre 1995 et 2016. 
                              Contrairement au profil de temp&eacute;rature normal, une 
                              moyenne stationnaire de long terme de temp&eacute;rature 
                              choisie par l'utilisateur est impos&eacute;e 
                              au jeu de donn&eacute;es. Les valeurs futures &agrave; partir 
                              de 2024 sont suppos&eacute;es maintenir cette moyenne de long 
                              terme et la saisonnalit&eacute; quotidienne.")),
                               
                               p(strong(em("Normal rainfall (temporal evolution)"))),
                               p(HTML("Calcul&eacute;e comme la moyenne quotidienne des valeurs 
                              quotidiennes de pr&eacute;cipitations observ&eacute;es entre 
                              1995 et 2023. Les valeurs &agrave; partir de 2024 sont 
                              suppos&eacute;es maintenir cette norme climatique comme r&eacute;f&eacute;rence.")),
                               
                               p(strong(em("Drier rainfall"))),
                               p(HTML("Calcul&eacute;e comme le 20e centile des valeurs quotidiennes 
                              de pr&eacute;cipitations observ&eacute;es entre 1995 et 2023. 
                              &Agrave; partir de 2024, les valeurs de pr&eacute;cipitations 
                              suivent le changement projet&eacute; du niveau de 
                              pr&eacute;cipitations et de la saisonnalit&eacute; selon le RCP 4.5.")),
                               
                               p(strong(em("Extreme dry rainfall"))),
                               p(HTML("Calcul&eacute;e comme le 45e centile des valeurs quotidiennes 
                              de pr&eacute;cipitations observ&eacute;es entre 1995 et 2023. 
                              &agrave; partir de 2024, les valeurs de pr&eacute;cipitations suivent 
                              le changement projet&eacute; du niveau de pr&eacute;cipitations et de 
                              la saisonnalit&eacute; selon le RCP 4.5.")),
                               
                               p(strong(em("More wet rainfall"))),
                               p(HTML("Calcul&eacute;e comme la moyenne quotidienne des valeurs 
                              quotidiennes de pr&eacute;cipitations observ&eacute;es entre 
                              1995 et 2023, similaire aux valeurs de pr&eacute;cipitations normales. 
                              Cependant, les valeurs futures &agrave; partir de 2024 sont suppos&eacute;es 
                              suivre le changement projet&eacute; des niveaux moyens de 
                              pr&eacute;cipitations et de la saisonnalit&eacute; selon le RCP 4.5.")),
                               
                               p(strong(em("Extreme wet rainfall"))),
                               p(HTML("Calcul&eacute;e comme le 90e centile des valeurs 
                              quotidiennes de pr&eacute;cipitations observ&eacute;es entre 
                              1995 et 2023. &Agrave; partir de 2024, les valeurs de 
                              pr&eacute;cipitations suivent le changement projet&eacute; du 
                              niveau de pr&eacute;cipitations et de la saisonnalit&eacute; 
                              selon le RCP 4.5.")),
                      ),
                      
                      tags$div(id = "section3",
                               h3(HTML("Mod&egrave;le climatique")),
                               p(HTML("Les projections utilisent un mod&egrave;le entomologique 
                      de transmission du paludisme bas&eacute; sur le climat, 
                      initialement d&eacute;velopp&eacute; par <a href=\"https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1010161\">Ukawuba et al.</a> 
                      Ce mod&egrave;le int&egrave;gre directement une modulation 
                      de l'intensit&eacute; de la transmission m&eacute;di&eacute;e 
                      par le climat via le taux d'inoculation entomologique (TIE). 
                      Sp&eacute;cifiquement adapt&eacute; &agrave; diverses r&eacute;gions 
                      du Rwanda, le mod&egrave;le inclut des am&eacute;liorations 
                      telles que la stratification par &acirc;ge et la mise en 
                      oeuvre de la chimioprophylaxie saisonni&egrave;re du paludisme (CPS).")),
                      ),
                      br(),
                      tags$div(id = "section4",
                               h3(HTML("Chimiopr&eacute;vention du paludisme saisonnier (CPS)")),
                               p(HTML("La CPS est toujours mis en oeuvre en 4 vagues 
                             commen&ccedil;ant en octobre et est 
                             administr&eacute; aux enfants de moins de 5 ans."))
                      )
               )
             )
    )
  )
  
  
  
)