################################################################################
# Set-up
################################################################################

# Load shiny related packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(dashboardthemes)
library(htmltools)
library(htmlwidgets)
library(cicerone)

# Load general R packages
library(rmarkdown)
library(drake)
library(tidyverse)
library(zoo)
library(DT)

# Load specific visualisation packages
library(networkD3)
library(plotly)
library(scales)
library(gt)
library(xtable)

library(MKHthemes)
library(colorspace)

# Load Energy-Economy Decoupling related packages
library(Recca)
library(SEAPSUTWorkflow)
library(PFUSetup)
library(matsbyname)
library(matsindf)
library(micEconCES)
library(MacroGrowth)
library(ReboundTools)

# Loads shiny modules to get and wrangle data
source("App-Modules/load_drake_data.R", local = TRUE) # Must source 1st
source("App-Modules/load_other_data.R", local = TRUE) # Must source 2nd
source("App-Modules/apf_functions.R", local = TRUE) ### Move to SEAPSUTWorkflow ###

# Load main shiny modules

source("App-Modules/outline.R", local = TRUE)
source("App-Modules/relresources.R", local = TRUE)

# Load PFU modules
source("App-Modules/intro_pfu.R", local = TRUE)
source("App-Modules/sum_dash_pfu.R", local = TRUE)
source("App-Modules/allocations.R", local = TRUE)
source("App-Modules/eta.R", local = TRUE)
source("App-Modules/consumption.R", local = TRUE)
source("App-Modules/ecc.R", local = TRUE)
source("App-Modules/documentation.R", local = TRUE)

# Load Energy-Economy modules
source("App-Modules/fd_gdp.R", local = TRUE)
source("App-Modules/decoupling_space.R", local = TRUE)
source("App-Modules/rebound_space.R", local = TRUE)
source("App-Modules/sum_dash_ee.R", local = TRUE)

# Load Rebound modules
source("App-Modules/intro_rebound.R", local = TRUE)
source("App-Modules/sum_dash_rebound.R", local = TRUE)
source("App-Modules/reboundtools_rebound.R", local = TRUE)
source("App-Modules/citation_rebound.R", local = TRUE)

# Loads bespoke functions for use in the app
source("App-Modules/functions.R", local = TRUE)

# Loads custom theme
source("App-Modules/customTheme.R", local = TRUE)

# Loads cicerone guides
source("App-Modules/guides_main.R", local = TRUE)
source("App-Modules/guides_pfu.R", local = TRUE)
source("App-Modules/guides_ee.R", local = TRUE)


################################################################################
# UI
################################################################################

ui = dashboardPage(

################################################################################
# Header UI
################################################################################

  # Custom css for app appearance
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
  # tags$head(includeCSS("www/custom.css")),

  header = dashboardHeader(

    title = tags$h4(HTML("Energy-Economy <br/> Decoupling"),
                    style =
                    "
                    font-weight: bold;
                    font-family: Georgia, Times, Times New Roman, serif;
                    font-size: 20px;
                    padding-bottom:0px;
                    padding-top:0px;
                    margin-top:3px;
                    margin-bottom:0px;
                    background-color: #000000;
                    color: #FFFFFF;
                    "
                    ),

                  # title = span("Energy-Economy Decoupling",
                  #              style = "color: white; font-size: 18px"),
                  # titleWidth = 300,

                  tags$li(a(href = 'http://www.leeds.ac.uk',
                            img(src = 'Leeds Logo White Text 1.png',
                                title = "Leeds", height = "40px"),
                            style = "padding-top:5px;
                            padding-bottom:5px;
                            padding-right:0px;
                            padding-left:0px;
                            margin-right:20px;"),
                          class = "dropdown"),
                  tags$li(a(href = 'https://calvin.edu/',
                            img(src = 'Calvin Logo White Text 1.png',
                                title = "Calvin", height = "40px"),
                            style = "padding-top:5px;
                            padding-bottom:5px;
                            padding-right:0px;
                            padding-left:0px;
                            margin-right:10px;"),
                          class = "dropdown")
                  ),

################################################################################
# Left Sidebar UI
################################################################################

  sidebar = dashboardSidebar(

    width = 272.25, # Header is 230px button is 42.5px

    minified = FALSE,

    sidebarMenu(id = "sidebarmenu",

                menuItem("Outline", tabName = "outline", icon = icon("chalkboard-teacher")),

                menuItem("PFU Database", expandedName = "pfu_menu", icon = icon("battery-quarter"),

                         menuItem("Introduction", tabName = "intro_pfu", icon = icon("book-reader")),

                         menuItem("PFU Dashboard", tabName = "dashboard_pfu", icon = icon("dashboard")),

                         menuItem("Final-to-useful Allocations", tabName = "allocations", icon = icon("chart-pie")),

                         menuItem("Final-to-useful Efficiencies", tabName = "eta", icon = icon("chart-line")),

                         menuItem("PFU Consumption", tabName = "consumption", icon = icon("lightbulb")),

                         menuItem("Energy Conversion Chain", tabName = "sankey", icon = icon("project-diagram")),

                         menuItem("Database documentation", tabName = "documentation", icon = icon("book")),

                         menuItem("Citation", tabName = "citation_pfu", icon = icon("user-graduate"))

                         ),
                menuItem("Energy-Economy", expandedName = "ee_menu", icon = icon("industry"),

                         menuItem("Energy-Economy Dashboard", tabName = "dashboard_ee", icon = icon("dashboard")),

                         menuItem("Indexed Data", tabName = "index_data", icon = icon("chart-line")),

                         menuItem("Energy-GDP Coupling Space", tabName = "energy_gdp_space", icon = icon("link")),

                         menuItem("Energy-Efficiency Rebound Space", tabName = "rebound_space", icon = icon("compress-alt")),

                         menuItem("Causality Testing", tabName = "cause_test", icon = icon("file-invoice-dollar")),

                         menuItem("APF Fitting", tabName = "apf_fit", icon = icon("hand-holding-usd")),

                         menuItem("Citation", tabName = "citation_ee", icon = icon("user-graduate"))

                         ),

                menuItem("Rebound", expandedName = "rebound", icon = icon("compress-alt fa-fw"),

                         menuItem("Introduction", tabName = "intro_rebound", icon = icon("book-reader fa-fw")),

                         menuItem("Rebound Dashboard", tabName = "dashboard_rebound", icon = icon("dashboard fa-fw")),

                         menuItem("ReboundTools", tabName = "rebound_tools", icon = icon("r-project")),

                         menuItem("Citation", tabName = "citation_rebound", icon = icon("user-graduate fa-fw"))

                         ),

                menuItem("Net Energy Analysis", tabName = "netenergy", icon = icon("balance-scale")),

                menuItem("Related Resources", tabName = "relatedresources", icon = icon("book")),

                menuItem("Contact", tabName = "contact", icon = icon("phone-alt"))

                )
    ),

################################################################################
# Body UI
################################################################################

  body = dashboardBody(

    # Reduce size of UI to 80% by default
    # Loading with whitespace below until new tab is opened
    # tags$style("body {
    #            -moz-transform: scale(0.9, 0.9); /* Moz-browsers */
    #            zoom: 0.9; /* Other non-webkit browsers */
    #            zoom: 90%; /* Webkit browsers */
    #            }"),

    # Use katex
    tags$head(
      tags$link(rel="stylesheet", href="https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.css", integrity="sha384-9tPv11A+glH/on/wEu99NVwDPwkMQESOocs/ZGXPoIiLE8MU/qkqUcZ3zzL+6DuH", crossorigin="anonymous"),
      tags$script(src="https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.js", integrity="sha384-U8Vrjwb8fuHMt6ewaCy8uqeUXv4oitYACKdB0VziCerzt011iQ/0TqlSlv8MReCm", crossorigin="anonymous"),
      tags$script(HTML(render_katex))
    ),

    # Load custom theme from customTheme.R script
    customTheme,

    tabItems(

      # General
      tabItem(tabName = "outline",
              outlineUI(id = "outline1")),


      # PFU-Database items
      tabItem(tabName = "intro_pfu",
              intro_pfuUI(id = "intro2")),

      tabItem(tabName = "dashboard_pfu",
              sumdashplotsUI(id = "dash1")),

      tabItem(tabName = "sankey",
              eccUI(id = "ecc1")),

      tabItem(tabName = "allocations",
              allocplotsUI(id = "allocations1")),

      tabItem(tabName = "eta",
              etaplotsUI(id = "eta1")),

      tabItem(tabName = "consumption",
              consumptionUI(id = "consumption1")),

      tabItem(tabName = "documentation",
              documentationUI(id = "doc1")),

      # Energy-Economy items
      tabItem(tabName = "index_data",
              fd_gdp_plotUI(id = "fd_gdp1")),

      tabItem(tabName = "energy_gdp_space",
              decoupling_spaceUI(id = "ds1")),

      tabItem(tabName = "rebound_space",
              rebound_spaceUI(id = "rs1")),

      tabItem(tabName = "lmdi",
              decoupling_spaceUI(id = "lmdi1")),

      tabItem(tabName = "dashboard_ee",
              sumdashplots_eeUI(id = "dash2")),



      ## Rebound modules - UI
      tabItem(tabName = "intro_rebound",
              intro_reboundUI(id = "intro3")),

      tabItem(tabName = "dashboard_rebound",
              rebound_dashUI(id = "dash3")),

      tabItem(tabName = "rebound_tools",
              rebound_docUI(id = "doc2")),

      tabItem(tabName = "citation_rebound",
              citation_reboundUI(id = "cit2")),




      ## Other modules - UI

      # Related resources UI
      tabItem(tabName = "relatedresources",
              relresourcesUI(id = "rr1"))

      # Contact UI

      ),
  )
)


################################################################################
# Server
################################################################################

server <- function(input, output, session) {

################################################################################
# Call guides
################################################################################

  # # Initialise guide, but do not start automatically
  # guide_main$
  #   init()
  #
  # # Observes buttons which launch tours of app
  # observeEvent(input$guide_main_ID, {
  #   guide_main$start()
  # })

  # Initialise guide_pfu, but do not start automatically
  guide_pfu$
    init()

  # Triggers PFU Database guide when childless menuItem is opened
  observeEvent(input$sidebarItemExpanded, {
    req(input$sidebarItemExpanded)
    if (input$sidebarItemExpanded == "pfu_menu") {
    guide_pfu$start()
    }
  })

  # Initialise guide_pfu_dash, but do not start automatically
  guide_pfu_dash$
    init()

  # Initialise guide_pfu, but do not start automatically
  guide_ee_dash$
    init()

  # Triggers Energy-Economy dashboard guide when menuItem dashboard_ee is selected
  observeEvent(input$sidebarmenu, {
    req(input$sidebarmenu)
    if (input$sidebarmenu == "dashboard_ee") {
      guide_ee_dash$start()
    }
  })

################################################################################
# Call server-side module components
################################################################################

  ## PFU-Database modules

  # Calls sum_dash_pfu.R module
  callModule(module = sumdashplots,
             id = "dash1",
             PSUT_etas,
             PSUT_metrics_total,
             GDP_metrics)

  # Calls allocations.R module
  callModule(module = allocplots,
             id = "allocations1",
             allocations_data)

  # Calls eta.R module
  callModule(module = etaplots,
             id = "eta1",
             eta_data)

  # Calls ecc.R module
  callModule(module = ecc,
             id = "ecc1",
             PSUT_useful_data)

  # Calls consumption.R module
  callModule(module = consumption,
             id = "consumption1",
             PSUT_metrics_total)



  ## Energy-Economy modules

  # Calls fd_gdp.R module
  callModule(module = fd_gdp_plot,
             id = "fd_gdp1",
             PSUT_metrics_total,
             GDP_metrics)

  # Calls decoupling_space.R module
  callModule(module = decoupling_space,
             id = "ds1",
             PSUT_metrics_total,
             GDP_metrics)

  # Calls rebound_space.R module
  callModule(module = rebound_space,
             id = "rs1",
             PSUT_metrics_total,
             PSUT_etas)

  # Calls energy-economy dashboard module
  callModule(module = sumdashplots_ee,
             id = "dash2",
             PSUT_etas,
             PSUT_metrics_total,
             EX_Econ_Data,
             EA_data_final, #?
             APF_Data)

  # Calls causality module


  ## Rebound modules

  # Calls rebound introduction module

  # Calls rebound dashboard module

  callModule(module = rebound_dash,
             id = "dash3")

  # Calls rebound citation module


}

shinyApp(ui, server)
