# Load required packages
library(Recca)
library(magrittr)
library(networkD3)

# Establishes UI module function
eccUI <- function(id) {
  ns <- NS(id)
  fluidRow(

    box(
      title = "Variables",
      id = "Variables_pfu",
      # status = "primary",
      solidHeader = FALSE,
      closable = FALSE,
      collapsible = TRUE,
      width = 12,
      # height = 100,
      splitLayout(

        sliderInput(
          inputId = ns("Year"),
          label = "Year",
          min = 1960,
          max = max_year,
          value = max_year,
          step = 1,
          sep = "",
          width = "100%",
          animate = FALSE),

        selectizeInput(
          inputId = ns("Country"),
          label = "Country:",
          choices = countries,
          width = "100%",
          options = list(dropdownParent = 'body')),

        # style = "z-index:1002;",

        cellWidths = c("90%", "10%"),

        cellArgs = list(style = "vertical-align: center;
                                  padding-right: 6px;
                                  padding-left: 6px;
                                  padding-bottom: 0px;
                                  padding-top: 0px;"
        )

      )),

    box(
      title = "Energy Conversion Chain",
      id = "ccc",
      width = 12,
      sankeyNetworkOutput(outputId = ns("sankey"),
                          height = 860)),

    tags$head(tags$style('#ccc .box-header{ display: none}'))

  ) # Closes fluidRow

}

# Establishes server module function
ecc <- function(input, output, session,
                Year,
                Country,
                sankey) {


  # Creates reactive data frame for energy conversion chain from PSUT_useful target
  selected_data <- reactive({
    dplyr::filter(PSUT_useful_data,
                  Country == input$Country,
                  Year == input$Year)

  })


  # Energy conversion chain sankey diagrams
  output$sankey <- renderSankeyNetwork({

    ecc_sankey <- selected_data() %>%
      Recca::make_sankey(nodeWidth = 20,
                         units = "ktoe",
                         sinksRight = TRUE,
                         margin = list(left = 200)
                         ) %>%
      magrittr::extract2("Sankey") %>%
      magrittr::extract2(2)

    # Add onRender JavaScript code
    ecc_sankey <- htmlwidgets::onRender(ecc_sankey,
      'function(el, x) {

      // Select all node text
      d3.selectAll(".node text")
        .style("fill", "black") // Set text colour to black
        .style("font-size", "12px") // Change font size to 12
        .attr("text-anchor", "begin") // Set text node side to right
        .attr("x", 20);

      }'
      )

    ecc_sankey


  })

}
