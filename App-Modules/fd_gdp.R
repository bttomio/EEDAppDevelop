# Loads required packages
library(tidyverse)

# Establishes UI module function
fd_gdp_plotUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    tabBox(title = "Energy vs GDP",
           # id = "tabset_fd_gdp",
           width = 9,
           height = 800,
           tabPanel(
             title = "Plots",
             plotlyOutput(outputId = ns("fd_gdp_plot"))
           ),
           tabPanel(
             title = "Indexed plots",
             plotlyOutput(outputId = ns("fd_gdp_plot_i"))
           )
           # tabPanel( # Add data tab here!
           #   
           # )
    ),
    box(title = "Variables", 
        # status = "warning", # sets color
        selectInput(inputId = ns("EorX"),
                    label = "Energy Quantification:",
                    choices = c(Energy = "E", Exergy = "X")
        ),
        selectizeInput(inputId = ns("stage"), # Need to change to FUMachine throughout
                       label = "ECC Stage:",
                       choices = c(Primary = "Primary", Final = "Final", Useful = "Useful"),
                       multiple = TRUE
        ),
        # selectInput(inputId = ns("gross_net"),
        #             label = "Gross or Net:",
        #             choices = c(Gross = "Gross", Net = "Net")
        #             %>% sort()
        # ),
        selectizeInput(inputId = ns("country"),
                       label = "Country:",
                       choices = countries,
                       multiple = TRUE
        ),
        solidHeader = TRUE,
        width = 3
    )
  )
}

# Establishes the server module function
fd_gdp_plot <- function(input, output, session,
                        EorX,
                        stage,
                        # gross_net,
                        country,
                        fd_gdp_plot_i) {
  
  # Creates a dataframe with the selected country, efproduct, and destination 
  selected_data <- reactive({
    validate(
      need(input$EorX != "", "Please select one Energy quantification"),
      need(input$stage != "", "Please select atleast one Energy Conversion Chain (ECC) stage"),
      # need(input$gross_net != "", "Please select one Final demand quantification"),
      need(input$country != "", "Please select atleast one Country")
    )
    
    data <- PSUT_metrics_total %>%
      dplyr::left_join(GDP_metrics, 
                       by = c("Country", "Year")) %>%
      dplyr::filter(Energy.type == input$EorX) %>%
      dplyr::filter(Stage %in% input$stage) %>%
      dplyr::filter(Gross.Net %in% c("Net", "Neither")) %>%
      dplyr::filter(Country %in% input$country)
    
    data$Stage <- factor(data$Stage, levels = c("Primary", "Final", "Useful"))
    
    data
      
  
    })
  
  
  # Final Energy carrier-Destination to Machine-useful work allocation plots
  # output$fd_gdp_plot <- renderPlot(
  #   height = 600, {
  #     selected_data = selected_data()
  #     ggplot2::ggplot(data = selected_data) +
  #       geom_line(mapping = aes(x = Year, y = Final.demand.i, color = Last.stage)) +
  #       geom_line(mapping = aes(x = Year, y = GDP.i, color = "GDP (Constant 2010 US$)")) +
  #       theme(legend.title = "Metric") +
  #       scale_y_continuous(limits = c(0, 24), breaks = c(1:24)) +
  #       scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960,2020,5)) +
  #       MKHthemes::xy_theme() +
  #       xlab("Year [-]") +
  #       ylab("Indexed value (1960) [-]") +
  #       ggplot2::facet_wrap(vars(Country))
  #   })
  
  # output$fd_gdp_plot_i <- plotly::renderPlotly(
  #     plotly::plot_ly(height = 700) %>% # height = 700
  #       add_lines(data = selected_data(), 
  #                 x = ~Year, 
  #                 y = ~Final.demand.i,
  #                 split = ~Last.stage,
  #                 type = 'scatter', 
  #                 mode = 'lines', 
  #                 name = paste("Final", "demand", input$EorX)) %>%
  #       add_lines(data = selected_data(), 
  #                 x = ~Year, 
  #                 y = ~GDP.i, 
  #                 type = 'scatter', 
  #                 mode = 'lines', 
  #                 name = "GDP (constant 2010 US$)") %>%
  #       layout(xaxis = list(range = c(1960, 2020), title = "Year"), 
  #              yaxis = list(range = c(0, 25), title = "Indexed Value (1960i) [-]"))
  # )
  
  # Creates color scheme
  cols <- c("Primary" = "red4", "Final" = "red", "Useful" = "orange", "GDP.i" = "black") # , "GDP.i" = "black"
  
  output$fd_gdp_plot <- renderPlotly({
    p <- ggplot2::ggplot(data = selected_data()) + 
      ggplot2::geom_line(mapping = aes(x = Year, y = EX, color = Stage)) +
      ggplot2::scale_colour_manual(values = cols) +
      ggplot2::facet_wrap(facets = "Country", ncol = 2, scales = "free_y") +
      ggplot2::xlab("") +
      ggplot2::ylab("Energy [ktoe]") +
      MKHthemes::xy_theme()
    
    p_plotly <- plotly::ggplotly(p, height = 700) %>%
      move_annotations(x = -0.05, y = 0.97, mar = 80)
    
  })
  
  output$fd_gdp_plot_i <- renderPlotly({
    p_i <- ggplot2::ggplot(data = selected_data()) + 
      ggplot2::geom_line(mapping = aes(x = Year, y = EX.i, color = Stage)) +
      ggplot2::geom_line(data = GDP_metrics, mapping = aes(x = Year, y = GDP.i, color = "GDP.i")) +
      ggplot2::scale_colour_manual(values = cols) +
      # ggplot2::theme(legend.title = "Metric") +
      ggplot2::facet_wrap(facets = "Country", ncol = 2) +
      ggplot2::xlab("") +
      ggplot2::ylab("Indexed Value [-]") +
      MKHthemes::xy_theme()
    
    p_i_plotly <- plotly::ggplotly(p_i, height = 700, tooltip = c("Year","Stage", "EX.i", "GDP.i")) %>%
      move_annotations(x = -0.05, y = 0.97, mar = 80)
    
  })
  
  
}


