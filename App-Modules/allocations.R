# Loads required packages
library(tidyverse)

# Establishes UI module function
allocplotsUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    tabBox(title = "Final-to-useful Allocations",
           # id = "tabset_allocations",
           width = 12,
           height = 1100,
           tabPanel(
             
             shinyWidgets::dropdownButton(
               
               selectizeInput(inputId = ns("country"),
                              label = "Country:",
                              choices = countries,
                              multiple = TRUE %>%
                                sort()
                              ),
               selectInput(inputId = ns("efproduct"),
                           label = "Final energy carrier:",
                           choices = unique(allocations$Ef.product) %>%
                             sort()
                           ),
               selectInput(inputId = ns("destination"),
                           label = "Destination:",
                           choices = unique(allocations$Destination) %>%
                             sort()
                           ),
               
               # Button attributes
               circle = TRUE,
               size = "sm",
               status = "default",
               icon = icon("gear"), 
               width = "300px",
               
               tooltip = tooltipOptions(title = "Click to see inputs !")
             ),
             
             title = "Plots",
             plotlyOutput(outputId = ns("allocations_plot"))
           )
           # tabPanel( # Add data tab here!
           #   
           # )
    )#,
    # box(title = "Variables", 
    #     # status = "warning", # sets color
    #     selectizeInput(inputId = ns("country"),
    #                    label = "Country:",
    #                    choices = countries,
    #                    multiple = TRUE %>%
    #                      sort()
    #     ),
    #     selectInput(inputId = ns("efproduct"),
    #                 label = "Final energy carrier:",
    #                 choices = unique(allocations$Ef.product) %>%
    #                   sort()
    #     ),
    #     selectInput(inputId = ns("destination"),
    #                 label = "Destination:",
    #                 choices = unique(allocations$Destination) %>%
    #                   sort()
    #     ),
    #     solidHeader = TRUE,
    #     width = 3
    # )
  )
}

# Establishes the server module function
allocplots <- function(input, output, session, 
                       country,
                       efproduct,
                       destination,
                       allocations_plot) {
  
  # Creates a dataframe with the selected country, efproduct, and destination 
  selected_data <- reactive({
    validate(
      need(input$country != "", "Please select atleast one Country"),
      need(input$efproduct != "", "Please select one Final energy carrier"),
      need(input$destination != "", "Please select one Destination")
    )
    dplyr::filter(allocations_data,
                  Country %in% input$country,  
                  Ef.product == input$efproduct,
                  Destination == input$destination)
  })
  
  
  # These observe events update the allocations tab
  observeEvent(input$country,  {
    req(input$country)
    post_country_data <- allocations_data %>%
      dplyr::filter(Country %in% input$country)
    
    updateSelectInput(session,
                      inputId = "efproduct", 
                      choices = sort(unique(post_country_data$Ef.product)))
    })
  
  observeEvent(input$efproduct,  {
    req(input$country)
    req(input$efproduct)
    post_efproduct_data <- allocations_data %>%
      dplyr::filter(Country %in% input$country) %>%
      dplyr::filter(Ef.product %in% input$efproduct) 
    
    updateSelectInput(session,
                      inputId = "destination", 
                      choices = sort(unique(post_efproduct_data$Destination)))
    })
  
  # Final Energy carrier-Destination to Machine-useful work allocation plots
  # output$allocations_plot <- renderPlot(
  #   height = 900, {
  #     selected_data = selected_data()
  #     ggplot2::ggplot(data = selected_data) +
  #       geom_area(mapping = aes(x = Year, y = .values, 
  #                               group = Machine_Eu.product,
  #                               fill = Machine_Eu.product 
  #       ),
  #       position = "fill") +
  #       scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  #       scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
  #       theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #       MKHthemes::xy_theme() + 
  #       ggplot2::facet_wrap(vars(Country)
  #       ) +
  #       xlab("Year [-]") +
  #       ylab("Proportion of energy consumption [-]")
  #   })
  
  
  # output$allocations_plot <- plotly::renderPlotly(
  #   plotly::plot_ly(height = 700) %>% # height = 700
  #     add_lines(data = selected_data(),
  #               x = ~Year,
  #               y = ~.values,
  #               split = ~Machine_Eu.product,
  #               type = 'scatter',
  #               mode = 'none',
  #               stackgroup = "one",
  #               split = ~type) %>%
  #     layout(xaxis = list(range = c(1960, 2020), title = "Year"),
  #            yaxis = list(range = c(0, 1), title = "Proportion of final energy consumption [-]"))
  # )
  
  # Final Energy carrier-Destination to Machine-useful work allocation plots
  output$allocations_plot <- renderPlotly({
      p <- ggplot2::ggplot(data = selected_data()) +
        ggplot2::geom_area(mapping = aes(x = Year,
                                         y = .values,
                                         group = Machine_Eu.product,
                                         fill = Machine_Eu.product),
                           position = "fill") +
        ggplot2::scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
        ggplot2::scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
        # ggplot2::theme(axis.title.y = element_text(margin = margin(r=10))) +
        ggplot2::facet_wrap(facets = "Country", ncol = 2) +
        ggplot2::labs(fill = "Machine - Useful Work") +
        ggplot2::xlab("") +
        ggplot2::ylab("Proportion of energy consumption [-]") +
        MKHthemes::xy_theme()


      p_plotly <- plotly::ggplotly(p, height = 1000) %>%
        plotly::layout(showlegend = TRUE,
                       legend = list(font = list(size = 12))) %>%
        move_annotations(x = -0.05, y = 0.97, mar = 80)

      # p_plotly[['x']][['layout']][['annotations']][[1]][['x']] <- -0.06
      #
      # p_plotly %>% plotly::layout(margin = list(l = 75))

  })
      
      
  # output$allocations_plot <- renderHighchart({
  #   
  #   highcharter::hchart(object = selected_data(), 
  #                       type = "area", 
  #                       hcaes(x = Year, y = .values, group = Machine_Eu.product)) %>%
  #     hc_size(height = "700px") %>%
  #     hc_plotOptions(area = list(
  #       dataLabels = list(enabled = FALSE),
  #       marker = list(enabled = FALSE),
  #       stacking = "normal",
  #       enableMouseTracking = TRUE)) #%>%
  #     # hc_legend(
  #     #   align = "bottom",
  #     #   verticalAlign = "top",
  #     #   layout = "vertical",
  #     #   x = 0,
  #     #   y = 100) 
  #   
  #   
  #   map(~get(input$country), function(){
  #     hchart(object = selected_data(), 
  #                         type = "area", 
  #                         hcaes(x = Year, y = .values, group = Machine_Eu.product)) %>%
  #       hc_size(height = "700px") %>%
  #       hc_plotOptions(area = list(
  #         dataLabels = list(enabled = FALSE),
  #         marker = list(enabled = FALSE),
  #         stacking = "normal",
  #         enableMouseTracking = TRUE))
  #   }) %>% 
  #     hw_grid(rowheight = 225, ncol = 3)  %>% browsable()
  #   
  #   
  # })
  #     
  
  
  
}


