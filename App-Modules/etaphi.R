# Loads required packages
library(tidyverse)

# Establishes UI module function
etaphiplotsUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    tabBox(title = "Final-to-useful efficiencies and exergy-to-energy ratios",
           # id = "tabset_etaphi",
           width = 9,
           height = 800,
           tabPanel(
             title = "Plots",
             plotlyOutput(outputId = ns("etaphi_plot"))
           )
           # tabPanel( # Add data tab here!
           #   
           # )
    ),
    box(title = "Variables", 
        # status = "warning", # sets color
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
        solidHeader = TRUE,
        width = 3
    )
  )
}

# Establishes the server module function
etaphiplots <- function(input, output, session,
                        country,
                        EorX,
                        machine,
                        euproduct,
                        etaphi_plot) {
  
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
  
  # Final Energy carrier-Destination to Machine-useful work allocation plots
  # output$etaphi_plot <- renderPlot(
  #   height = 600, {
  #     selected_data = selected_data()
  #     ggplot2::ggplot(data = selected_data) +
  #       geom_line(mapping = aes(x = Year, y = .values, colour = Country)) +
  #       scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  #       scale_x_continuous(limits = c(1960, 2020), breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
  #       MKHthemes::xy_theme() +
  #       xlab("Year [-]") +
  #       ylab("Final-to-useful Efficiency [-]")
  #   })
  
  # output$etaphi_plot <- plotly::renderPlotly(
  #   plotly::plot_ly(height = 700) %>%
  #     add_lines(data = selected_data(),
  #               x = ~Year,
  #               y = ~.values,
  #               split = ~Country,
  #               type = 'scatter',
  #               mode = 'lines',
  #               name = paste("Test")) %>%
  #     layout(xaxis = list(range = c(1960, 2020), title = "Year"),
  #            yaxis = list(range = c(0, 1), title = "Final-to-Useful Efficiency [-]"))
  # )
  
  # Machine - Useful work combination eta.fu plots
  output$etaphi_plot <- renderPlotly({
      p <- ggplot2::ggplot(data = selected_data()) +
        ggplot2::geom_line(mapping = aes(x = Year, y = .values, colour = Country)) +
        ggplot2::scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
        ggplot2::scale_x_continuous(limits = c(1960, 2020), breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
        ggplot2::xlab("") +
        ggplot2::ylab("Final-to-useful Efficiency [-]") + 
        MKHthemes::xy_theme()
      
      plotly::ggplotly(p, height = 700) %>%
        move_legend_annotation_no_facet(y = 0.97, mar = 80)
      
    })
  
}


