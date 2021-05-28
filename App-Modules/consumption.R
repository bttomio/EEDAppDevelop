# Establishes UI module function
consumptionUI <- function(id) {
  ns <- NS(id)

  fluidRow(

    tabBox(title = "Consumption of PFU Energy and Exergy",
           id = "consumption",
           width = 10,
           height = 920,

           # tabPanel(title = "Plots - Indexed",
           #          plotlyOutput(outputId = ns("consumption_indexed_plot"))),
           #
           # tabPanel(title = "Data - Indexed",
           #          dataTableOutput(outputId = ns("consumption_indexed_data")),
           #          style = "font-size:78%"),

           tabPanel(title = "Plots - Consumption",
                    plotlyOutput(outputId = ns("consumption_plot"))),

           tabPanel(title = "Data - Consumption",
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

        selectizeInput(inputId = ns("aggby"),
                       label = "Aggregation:",
                       choices = c(Total = "Total",
                                   Product = "Product",
                                   `Flow or Sector` = "flowsec"),
                       width = "150px",
                       options = list(dropdownParent = 'body')),

        selectizeInput(inputId = ns("stackfill"),
                       label = "Stack or Fill Area Plots:",
                       choices = c(Stack = "Stack", Fill = "Fill"),
                       width = "150px",
                       options = list(dropdownParent = 'body')),

        selectInput(inputId = ns("dataformat"),
                    label = "Data Format:",
                    choices = c("Wide", "Long")),


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
                        aggby,
                        stackfill,
                        dataformat) {

################################################################################
# Update events
################################################################################


  # Update value of Gross.Net so combinations of Stage and Gross.Net that don't
  # exist e.g. Primary and Net, cannot be selected
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

  # # Creates reactive data frame for indexed EX
  # selected_data_i <- reactive({
  #   validate(
  #     need(input$country != "", "Please select atleast one country")
  #   )
  #
  #   data <- PSUT_metrics_total_i %>%
  #     dplyr::filter(Aggregation.by == "Total") %>%
  #     dplyr::filter(Energy.type == input$EorX) %>%
  #     dplyr::filter(Gross.Net %in% c("Neither", "Net")) %>%
  #     dplyr::filter(Country == input$country)
  #
  #   data$Stage <- factor(data$Stage, levels = c("Primary", "Final", "Useful"))
  #
  #   data
  #
  #
  # })

  # Creates reactive data frame for EX data aggregated by selected value
  selected_data_consumption <- reactive({
    validate(
      need(input$country != "", "Please select atleast one country")
    )

    if(input$aggby == "flowsec"){
      aggby_value = c("Flow", "Sector")
    } else {
      aggby_value = input$aggby
    }

    Agg_all_data %>%
      dplyr::filter(Aggregation.by %in% aggby_value) %>%
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


  # # Plots indexed data
  # output$consumption_indexed_plot <- renderPlotly({
  #   p_i <- ggplot2::ggplot(data = selected_data_i()) +
  #
  #     ggplot2::geom_line(mapping = aes(x = Year, y = EX.i, color = Stage)) +
  #
  #     ggplot2::scale_colour_manual(values = cols) +
  #
  #     # ggplot2::scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 10)) +
  #
  #     ggplot2::xlab("") +
  #     ggplot2::ylab("Indexed Value [-]") +
  #     MKHthemes::xy_theme()
  #
  #   p_i_plotly <- plotly::ggplotly(p_i, height = 850, tooltip = c("Year","Stage", "EX.i", "GDP.i")) %>%
  #
  #     plotly::layout(showlegend = as.logical(input$legend),
  #                    legend = list(itemclick = TRUE,
  #                                  itemdoubleclick = TRUE)) %>%
  #     move_annotations(x = 1.02, y = 0.97, mar = 80)
  #
  # })


  # EX plot - aggreated by selected value
  output$consumption_plot <- renderPlotly({

    # Establishes highlight keys
    if(input$aggby == "Total"){
      key_consumption <- "E.product"
    } else if (input$aggby == "Product"){
      key_consumption <- "E.product"
    } else if (input$aggby == "flowsec"){
      key_consumption <- "Flow.Sector"
    }

    # Establishes fill values
    if(input$aggby == "Total"){
      fill_consumption <- "Aggregation.by"
    } else if (input$aggby == "Product"){
      fill_consumption <- "E.product"
    } else if (input$aggby == "flowsec"){
      fill_consumption <- "Flow.Sector"
    }

    # Attaches highlight key to data
    consumption_data <- highlight_key(selected_data_consumption(), key = key_consumption)

    # Finds minimum year
    # year_min <- min(selected_data_consumption()$Year)

    # Builds ggplot object
    consumption_plot <- ggplot2::ggplot(data = consumption_data) +

      ggplot2::geom_area(mapping = aes(x = Year,
                                       y = EX,
                                       fill = !!sym(fill_consumption)),
                         position = input$stackfill) +

      # ggplot2::scale_x_continuous(limits = c(year_min, max_year), breaks = seq(year_min, max_year, by = 10)) +

      ggplot2::xlab("") +
      ggplot2::ylab("EX Consumption [ktoe]") +
      MKHthemes::xy_theme()


    # Converts ggplot object into plotly
    consumption_plotly <- plotly::ggplotly(consumption_plot,
                                           height = 850,
                                           tooltip = c("Year", "EX", as.character(key_consumption))) %>%

      move_annotations(x = 1.02, y = 0.97, mar = 80) %>%

      plotly::layout(showlegend = as.logical(input$legend),
                     legend = list(font = list(size = 9),
                                   tracegroupgap = 0.3,
                                   itemwidth = 15
                     ))

  })


################################################################################
# Outputs - tables
################################################################################


  # Indexed data
  output$consumption_indexed_data <- renderDataTable({

    req(input$dataformat)

    if(input$dataformat == "Long"){

      data_long <- selected_data_i() %>%
        as.data.frame()

      consumption_indexed_table <- DT::datatable(data = data_long,
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
        DT::formatRound(columns=c("EX", "EX.i"), digits=3)

    } else if (input$dataformat == "Wide") {

      data_wide <- selected_data_i() %>%
        as.data.frame() %>%
        tidyr::pivot_longer(cols = c("EX", "EX.i"),
                            names_to = "Metric",
                            values_to = ".values") %>%
        tidyr::pivot_wider(names_from = "Year",
                           values_from = ".values")

      consumption_indexed_table <- DT::datatable(data = data_wide,
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
        DT::formatRound(columns = IEATools::year_cols(data_wide), digits=3)

    } else {

      print("Error")

    }


    return(consumption_indexed_table)

  })

  # Consumption data
  output$consumption_data <- renderDataTable({

    req(input$dataformat)

    if(input$dataformat == "Long"){

      data_long <- selected_data_consumption() %>%
        as.data.frame()

      consumption_table <- DT::datatable(data = data_long,
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
        DT::formatRound(columns=c("EX"), digits=3)

    } else if (input$dataformat == "Wide") {

      data_wide <- selected_data_consumption() %>%
        as.data.frame() %>%
        tidyr::pivot_wider(names_from = "Year",
                           values_from = "EX")

      consumption_table <- DT::datatable(data = data_wide,
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
        DT::formatRound(columns = IEATools::year_cols(data_wide), digits=3)

    } else {

      print("Error")

    }


    return(consumption_table)

  })


################################################################################
# Outputs - downloads
################################################################################

  output$download_data <- downloadHandler(

    filename = function() {

      paste("PFU_",
            as.character(unique(selected_data_consumption()$Aggregation.by)),
            ".Consumption.Data_",
            Sys.Date(),
            ".csv",
            sep="")
    },

    content = function(file) {

      req(input$dataformat)

      if(input$dataformat == "Long"){

        data <- selected_data_consumption() %>%
          as.data.frame()


      } else if (input$dataformat == "Wide") {

        data <- selected_data_consumption() %>%
          as.data.frame() %>%
          tidyr::pivot_wider(names_from = "Year",
                             values_from = "EX")

      } else {

        print("Error")

      }

      write.csv(data, file)
    }

  )


  output$download_alldata <- downloadHandler(

    filename = function() {

      paste("PFU_All.Consumption.Data_",
            Sys.Date(),
            ".csv",
            sep="")
    },

    content = function(file) {

      req(input$dataformat)

      if(input$dataformat == "Long"){

        data <- Agg_all_data %>%
          as.data.frame()


      } else if (input$dataformat == "Wide") {

        data <- Agg_all_data %>%
          as.data.frame() %>%
          tidyr::pivot_wider(names_from = "Year",
                             values_from = "EX")

      } else {

        print("Error")

      }

      write.csv(data, file)
    }

  )

}


