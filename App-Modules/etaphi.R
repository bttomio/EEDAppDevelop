# Loads required packages
library(tidyverse)

# Establishes UI module function
etaphiplotsUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    tabBox(title = "Final-to-useful efficiencies and exergy-to-energy ratios",
           id = "etaphi",
           width = 10,
           height = 920,
           tabPanel(title = "Plots",
                    plotlyOutput(outputId = ns("etaphi_plot"))
                    ),
           tabPanel(title = "Data",
                    dataTableOutput(outputId = ns("etaphi_data")),
                    style = "font-size:78%"
                    )
           ),

    box(title = "Variables",
        solidHeader = TRUE,
        width = 2,

        selectInput(inputId = ns("EorX"),
                    label = "Energy Quantification:",
                    choices = c(Energy = "eta.fu", `Exergy-to-energy ratio` = "phi.u")
                    ),
        selectizeInput(inputId = ns("country"),
                       label = "Country:",
                       choices = countries,
                       multiple = TRUE
                       %>% sort()
                       ),
        selectInput(inputId = ns("machine"), # Need to change to FUMachine throughout
                    label = "Final-to-useful machine:",
                    choices = unique(etaphi_data$Machine)
                    %>% sort()
                    ),
        selectInput(inputId = ns("euproduct"),
                    label = "Useful product:",
                    choices = unique(etaphi_data$Eu.product)
                    %>% sort()
                    ),
        downloadButton(outputId = ns("download_data"),
                       label = "Download Data",
                       class = NULL,
                       icon = shiny::icon("download")
                       )
        ) # Closes variables box

  ) # Closes fluidrow

} # Closes UI

# Establishes the server module function
etaphiplots <- function(input, output, session,
                        country,
                        EorX,
                        machine,
                        euproduct) {

  # Creates a dataframe with the selected country, machine, and Eu.product
  selected_data <- reactive({
    validate(
      need(input$EorX != "", "Please select atleast one Energy quantification"),
      need(input$country != "", "Please select atleast one Country"),
      need(input$machine != "", "Please select one Machine"),
      need(input$euproduct != "", "Please select one Useful work product")
    )
    dplyr::filter(etaphi_data,
                  Quantity == input$EorX,
                  Country %in% input$country,
                  Machine == input$machine,
                  Eu.product == input$euproduct)
    })

  # These observe events update the variables for selection
  observeEvent(input$country,  {
    req(input$country)
    post_country_data <- etaphi_data %>%
      dplyr::filter(Country %in% input$country)

    updateSelectInput(session,
                      inputId = "machine",
                      choices = sort(unique(post_country_data$machine)))
    })

  observeEvent(input$machine,  {
    req(input$country)
    req(input$machine)
    post_machine_data <- etaphi_data %>%
      dplyr::filter(Country %in% input$country) %>%
      dplyr::filter(Machine %in% input$machine)

    updateSelectInput(session,
                      inputId = "euproduct",
                      choices = sort(unique(post_machine_data$Eu.product)))
    })

  # Machine - Useful work combination eta.fu plots
  output$etaphi_plot <- renderPlotly({
      p <- ggplot2::ggplot(data = selected_data()) +
        ggplot2::geom_line(mapping = aes(x = Year, y = .values, colour = Country)) +
        ggplot2::scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
        ggplot2::scale_x_continuous(limits = c(1960, 2020), breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
        ggplot2::xlab("") +
        ggplot2::ylab("Final-to-useful Efficiency [-]") +
        MKHthemes::xy_theme()

      plotly::ggplotly(p, height = 850) %>%
        move_legend_annotation_no_facet(y = 0.97, mar = 80)

    })

  output$etaphi_data <- renderDataTable({

    data <- selected_data() %>%
      as.data.frame()

    #div(
    DT::datatable(data = data,
                  rownames = TRUE,
                  fillContainer = TRUE,
                  # height = 880,
                  options = list(paging = FALSE,    ## paginate the output
                                 # pageLength = 20,  ## number of rows to output for each page
                                 scrollX = TRUE,   ## enable scrolling on X axis
                                 scrollY = "800px",   ## enable scrolling on Y axis
                                 autoWidth = FALSE, ## use smart column width handling
                                 server = FALSE,   ## use client-side processing
                                 dom = 'Bfrtip',
                                 columnDefs = list(

                                   # Centers columns
                                   list(targets = '_all',
                                        className = 'dt-center'),

                                   # Removes columns
                                   list(targets = c(0),
                                        visible = FALSE)

                                   ))) %>%

      DT::formatRound(columns=c(".values"), digits=3)

  })

  output$download_data <- downloadHandler(

    filename = function() {

      paste("PFU_",
            as.character(unique(selected_data()$Machine)),
            "_",
            as.character(unique(selected_data()$Eu.product)),
            "_",
            Sys.Date(),
            ".csv",
            sep="")
    },

    content = function(file) {

      data <- selected_data()

      write.csv(data, file)
    }

  )

}


