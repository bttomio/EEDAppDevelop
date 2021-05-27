# Establishes UI module function
consumptionUI <- function(id) {
  ns <- NS(id)

  fluidRow(

    tabBox(title = "Consumption of PFU Energy and Exergy",
           id = "consumption",
           width = 10,
           height = 920,

           tabPanel(title = "Plots - Indexed",
                    plotlyOutput(outputId = ns("consumption_indexed_plot"))),

           tabPanel(title = "Plots - Total",
                    plotlyOutput(outputId = ns("consumption_total_plot"))),

           tabPanel(title = "Plots - Product",
                    plotlyOutput(outputId = ns("consumption_product_plot"))),

           tabPanel(title = "Plots - Flow or Sector",
                    plotlyOutput(outputId = ns("consumption_flowsec_plot"))),

           tabPanel(title = "Data",
                    dataTableOutput(outputId = ns("consumption_data")),
                    style = "font-size:78%")
           ),

    box(title = "Variables",
        solidHeader = TRUE,
        width = 2,

        selectizeInput(inputId = ns("country"),
                       label = "Country:",
                       choices = countries,
                       multiple = TRUE %>%
                         sort()),

        selectizeInput(inputId = ns("EorX"),
                       label = "Energy Type:",
                       choices = c(Energy = "E", `Exergy` = "X"),
                       width = "150px",
                       options = list(dropdownParent = 'body')),

        selectizeInput(inputId = ns("stage"),
                       label = "ECC Stage:",
                       choices = c(Primary = "Primary",
                                   Final = "Final",
                                   Useful = "Useful"),
                       width = "150px",
                       options = list(dropdownParent = 'body')),

        selectizeInput(inputId = ns("gross_net"),
                       label = "Gross or Net:",
                       choices = c(Neither = "Neither",
                                   Gross = "Gross",
                                   Net = "Net")
                       %>% sort(),
                       width = "150px",
                       options = list(dropdownParent = 'body')),

        selectizeInput(inputId = ns("stackfill"),
                       label = "Stack or Fill Area Plots:",
                       choices = c(Stack = "Stack", Fill = "Fill"),
                       width = "150px",
                       options = list(dropdownParent = 'body')),

        selectInput(inputId = ns("dataformat"),
                    label = "Data Format:",
                    choices = c("Long", "Wide")),


        tags$h5(tags$b("Download Selected Data")),

        downloadButton(outputId = ns("download_data"),
                       label = "Download",
                       class = NULL,
                       icon = shiny::icon("download")),

        tags$h5(tags$b("Download All Data")),

        downloadButton(outputId = ns("download_alldata"),
                       label = "Download",
                       class = NULL,
                       icon = shiny::icon("download"))
    )
  )
}

################################################################################
# Server
################################################################################

# Establishes the server module function
consumption <- function(input, output, session,
                        country,
                        EorX,
                        stage,
                        gross_net,
                        stackfill,
                        dataformat) {

################################################################################
# Update events
################################################################################


  # These observe events update the variables for selection
  observeEvent(input$stage,  {
    req(input$stage)

    if (input$stage == "Primary") {
      gross_net <- c("Neither")
    }

    if (input$stage == "Final") {
      gross_net <- c("Gross", "Net")
    }

    if (input$stage == "Useful") {
      gross_net <- c("Gross", "Net")
    }

    updateSelectInput(session,
                      inputId = "gross_net",
                      choices = gross_net)
  })

################################################################################
# Select data
################################################################################

  # Creates reactive data frame for indexed EX
  selected_data_i <- reactive({
    validate(
      # need(input$stage != "", "Please select atleast one Energy Conversion Chain (ECC) stage")
    )

    data <- PSUT_metrics_total_i %>%
      dplyr::filter(Aggregation.by == "Total") %>%
      dplyr::filter(Energy.type == input$EorX) %>%
      # dplyr::filter(Stage == input$stage) %>%
      dplyr::filter(Gross.Net %in% c("Neither", "Net")) %>%
      dplyr::filter(Country == input$country)

    data$Stage <- factor(data$Stage, levels = c("Primary", "Final", "Useful"))

    data


  })


  # Creates reactive data frame for EX by product plot
  selected_data_prod <- reactive({
    validate(
      # need(input$stage != "", "Please select one Energy Conversion Chain (ECC) stage")
    )

    Agg_all_data %>%
      dplyr::filter(Aggregation.by == "Product") %>%
      dplyr::filter(Energy.type == input$EorX,
                    Gross.Net == input$gross_net,
                    Stage == input$stage,
                    Country == input$country)
  })


  # Creates reactive data frame for EX by product plot
  selected_data_flowsec <- reactive({
    validate(
      # need(input$stage != "", "Please select one Energy Conversion Chain (ECC) stage")
    )

    Agg_all_data %>%
      dplyr::mutate(Gross.Net = ifelse(is.na(Gross.Net), "Neither", Gross.Net)) %>%
      dplyr::filter(Aggregation.by == c("Flow", "Sector")) %>%
      dplyr::filter(Energy.type == input$EorX,
                    Gross.Net == input$gross_net,
                    Stage == input$stage,
                    Country == input$country)
  })


################################################################################
# Outputs - plots
################################################################################


  # Creates colour scheme
  cols <- c("Primary" = "red4", "Final" = "red", "Useful" = "orange", "GDP.i" = "black")

  # Plots indexed data
  output$consumption_indexed_plot <- renderPlotly({
    p_i <- ggplot2::ggplot(data = selected_data_i()) +

      ggplot2::geom_line(mapping = aes(x = Year, y = EX.i, color = Stage)) +

      ggplot2::scale_colour_manual(values = cols) +

      # ggplot2::scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 10)) +

      ggplot2::xlab("") +
      ggplot2::ylab("Indexed Value [-]") +
      MKHthemes::xy_theme()

    p_i_plotly <- plotly::ggplotly(p_i, height = 850, tooltip = c("Year","Stage", "EX.i", "GDP.i")) %>%

      plotly::layout(showlegend = as.logical(input$legend),
                     legend = list(itemclick = TRUE,
                                   itemdoubleclick = TRUE)) %>%
      move_annotations(x = 1.02, y = 0.97, mar = 80)

  })

  # EX by product stacked area plots
  output$consumption_product_plot <- renderPlotly({

    data_prod <- highlight_key(selected_data_prod(), key=~E.product)

    year_min <- min(selected_data_prod()$Year)

    p_prod <- ggplot2::ggplot(data = data_prod) +

      ggplot2::geom_area(mapping = aes(x = Year,
                                       y = EX,
                                       fill = E.product),
                         position = input$stackfill) +

      # ggplot2::scale_x_continuous(limits = c(year_min, max_year), breaks = seq(year_min, max_year, by = 10)) +

      ggplot2::xlab("") +
      ggplot2::ylab("EX Consumption [ktoe]") +
      MKHthemes::xy_theme()


    p_prod_plotly <- plotly::ggplotly(p_prod,
                                      height = 850,
                                      tooltip = c("Year", "EX", "E.product")) %>%

      move_annotations(x = 1.02, y = 0.97, mar = 80) %>%

      plotly::layout(showlegend = as.logical(input$legend),
                     legend = list(font = list(size = 9),
                                   tracegroupgap = 0.3,
                                   itemwidth = 15 #,
                                   # itemclick = TRUE,
                                   # itemdoubleclick = TRUE
                     ))

  })

  # EX by flow or sector stacked area plots
  output$consumption_flowsec_plot <- renderPlotly({

    data_flowsec <- highlight_key(selected_data_flowsec(), key=~Flow.Sector)

    year_min <- min(selected_data_prod()$Year)

    p_flowsec <- ggplot2::ggplot(data = data_flowsec) +

      ggplot2::geom_area(mapping = aes(x = Year,
                                       y = EX,
                                       fill = Flow.Sector),
                         position = input$stackfill) +

      # ggplot2::scale_x_continuous(limits = c(year_min, max_year), breaks = seq(year_min, max_year, by = 10)) +

      ggplot2::xlab("") +
      ggplot2::ylab("EX Consumption [ktoe]") +
      MKHthemes::xy_theme()


    p_flowsec_plotly <- plotly::ggplotly(p_flowsec,
                                         height = 850,
                                         tooltip = c("Year", "EX", "Flow.Sector")) %>%

      move_annotations(x = 1.02, y = 0.97, mar = 80) %>%

      plotly::layout(showlegend = as.logical(input$legend),
                     legend = list(font = list(size = 9),
                                   tracegroupgap = 0.3,
                                   itemwidth = 15#,
                                   # itemclick = TRUE,
                                   # itemdoubleclick = TRUE
                     ))


  })


################################################################################
# Outputs - tables
################################################################################

  output$consumption_data <- renderDataTable({

    req(input$dataformat)

    if(input$dataformat == "Long"){

      data_long <- selected_data() %>%
        as.data.frame()

      allocations_table <- DT::datatable(data = data_long,
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
                                                          list(targets = c(0, 15),
                                                              visible = FALSE)

                                                        ))) %>%
        DT::formatRound(columns=c(".values"), digits=3)

    } else if (input$dataformat == "Wide") {

      data_wide <- selected_data() %>%
        as.data.frame() %>%
        tidyr::pivot_wider(names_from = "Year",
                           values_from = ".values")

      allocations_table <- DT::datatable(data = data_wide,
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
                                                          list(targets = c(0, 13),
                                                               visible = FALSE)

                                                        ))) %>%
        DT::formatRound(columns = IEATools::year_cols(data_wide), digits=3)

    } else {

      print("Error")

    }

    # DT::datatable(data = data,
    #               rownames = TRUE,
    #               fillContainer = TRUE,
    #               # height = 880,
    #               options = list(paging = FALSE,    ## paginate the output
    #                              # pageLength = 20,  ## number of rows to output for each page
    #                              scrollX = TRUE,   ## enable scrolling on X axis
    #                              scrollY = "800px",   ## enable scrolling on Y axis
    #                              autoWidth = TRUE, ## use smart column width handling
    #                              server = FALSE,   ## use client-side processing
    #                              dom = 'Bfrtip',
    #                              columnDefs = list(
    #
    #                                # Centers columns
    #                                list(targets = '_all',
    #                                     className = 'dt-center'),
    #
    #                                # Removes columns
    #                                list(targets = c(0, 15),
    #                                     visible = FALSE)
    #
    #                                ))) %>%
    #
    #   DT::formatRound(columns=c(".values"), digits=3)

    return(allocations_table)

  })


  output$download_data <- downloadHandler(

    filename = function() {

      paste("PFU_",
            as.character(unique(selected_data()$Ef.product)),
            "_",
            as.character(unique(selected_data()$Destination)),
            "_",
            Sys.Date(),
            ".csv",
            sep="")
    },

    content = function(file) {

      req(input$dataformat)

      if(input$dataformat == "Long"){

        data <- selected_data() %>%
          as.data.frame()


      } else if (input$dataformat == "Wide") {

        data <- selected_data() %>%
          as.data.frame() %>%
          tidyr::pivot_wider(names_from = "Year",
                             values_from = ".values")

      } else {

        print("Error")

      }

      write.csv(data, file)
    }

  )

################################################################################
# Outputs - downloads
################################################################################


  output$download_alldata <- downloadHandler(

    filename = function() {

      paste("PFU_",
            Sys.Date(),
            ".csv",
            sep="")
    },

    content = function(file) {

      req(input$dataformat)

      if(input$dataformat == "Long"){

        data <- allocations_data %>%
          as.data.frame()


      } else if (input$dataformat == "Wide") {

        data <- allocations_data %>%
          as.data.frame() %>%
          tidyr::pivot_wider(names_from = "Year",
                             values_from = ".values")

      } else {

        print("Error")

      }

      write.csv(data, file)
    }

  )

}


