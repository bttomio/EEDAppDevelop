# Loads required packages
library(tidyverse)

# Establishes UI module function
ieadata_plotsUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    tabBox(title = "IEA Data",
           # id = "tabset_allocations",
           width = 9,
           height = 800,
           tabPanel(
             title = "Plots",
             plotlyOutput(outputId = ns("ieadata_plot"))
           )
           # tabPanel( # Add data tab here!
           #   
           # )
    ),
    box(title = "Variables", 
        
        selectInput(inputId = ns("facet_var"),
                    label = "Faceting variable:",
                    choices = c(
                      Country = "Country",
                      `Ledger side` = "Ledger.side",
                      `Flow aggregation point` = "Flow.aggregation.point",
                      Flow = "Flow",
                      Product = "Product"
                    )
        ),
        
        selectInput(inputId = ns("stack_var"),
                    label = "Stacking variable:",
                    choices = c(
                      Country = "Country",
                      `Flow aggregation point` = "Flow.aggregation.point",
                      Flow = "Flow",
                      Product = "Product"
                    )
        ),
        
        selectizeInput(inputId = ns("country"),
                    label = "Country:",
                    choices = countries,
                    multiple = TRUE %>%
                      sort()
        ),
        
        selectizeInput(inputId = ns("ledgerside"),
                    label = "Ledger side:",
                    choices = unique(balanced_iea_data$Ledger.side),
                    multiple = TRUE %>%
                      sort()
        ),
        
        selectizeInput(inputId = ns("flowaggpoint"),
                       label = "Flow aggregation point:",
                       choices = unique(balanced_iea_data$Flow.aggregation.point),
                       multiple = TRUE %>%
                         sort()
        ),
        
        selectizeInput(inputId = ns("flow"),
                       label = "Flow:",
                       choices = unique(balanced_iea_data$Flow),
                       multiple = TRUE %>%
                         sort()
        ),
        
        selectizeInput(inputId = ns("product"),
                    label = "Energy product:",
                    choices = unique(balanced_iea_data$Product),
                    multiple = TRUE %>%
                      sort()
        ),
        
        solidHeader = TRUE,
        width = 3
    )
  )
}

# Establishes the server module function
ieadata_plots <- function(input, output, session, 
                       country,
                       legderside,
                       flowaggpoint,
                       flow,
                       product,
                       ieadata_plot) {
  
  # Creates a dataframe with the selected country, flowaggpoint, and product 
  selected_data <- reactive({
    validate(
      need(input$country != "", "Please select atleast one Country"),
      need(input$ledgerside != "", "Please select one Ledger side (supply or consumption)"),
      need(input$flowaggpoint != "", "Please select one Flow aggregation point"),
      need(input$flow != "", "Please select one Flow"),
      need(input$product != "", "Please select one Product")
    )
    dplyr::filter(balanced_iea_data,
                  Country %in% input$country,
                  Ledger.side %in% input$ledgerside,
                  Flow.aggregation.point %in% input$flowaggpoint,
                  # Flow %in% input$flow,
                  Product %in% input$product)
  })
  
  
  # These observe events update the ieadata tab
  
  # Update Ledger.side selection
  observeEvent(input$country,  {
    req(input$country)
    post_country_data <- balanced_iea_data %>%
      dplyr::filter(Country %in% input$country)
    
    updateSelectizeInput(session,
                      inputId = "ledgerside", 
                      choices = sort(unique(balanced_iea_data$Ledger.side)))
  })
  
  # Update Flow.Aggregation.point selection
  observeEvent(input$ledgerside,  {
    req(input$country)
    req(input$ledgerside)
    post_ledgerside_data <- balanced_iea_data %>%
      dplyr::filter(Country %in% input$country) %>%
      dplyr::filter(Ledger.side %in% input$ledgerside) 
    
    updateSelectizeInput(session,
                      inputId = "flowaggpoint", 
                      choices = sort(unique(post_ledgerside_data$Flow.aggregation.point)))
  })
  
  # # Update Product selection
  # observeEvent(input$flowaggpoint,  {
  #   req(input$country)
  #   req(input$ledgerside)
  #   req(input$flowaggpoint)
  #   post_flowaggpoint_data <- balanced_iea_data %>%
  #     dplyr::filter(Country %in% input$country) %>%
  #     dplyr::filter(Ledger.side %in% input$ledgerside) %>%
  #     dplyr::filter(Flow.aggregation.point %in% input$flowaggpoint)
  #   
  #   updateSelectizeInput(session,
  #                        inputId = "product", 
  #                        choices = sort(unique(post_flowaggpoint_data$Product)))
  # })
  
  # Update Flow selection
  observeEvent(input$flowaggpoint,  {
    req(input$country)
    req(input$ledgerside)
    req(input$flowaggpoint)
    post_flowaggpoint_data <- balanced_iea_data %>%
      dplyr::filter(Country %in% input$country) %>%
      dplyr::filter(Ledger.side %in% input$ledgerside) %>%
      dplyr::filter(Flow.aggregation.point %in% input$flowaggpoint)

    updateSelectizeInput(session,
                      inputId = "flow",
                      choices = sort(unique(post_flowaggpoint_data$Flow)))
  })

  # Update Product selection
  observeEvent(input$flow,  {
    req(input$country)
    req(input$ledgerside)
    req(input$flowaggpoint)
    req(input$flow)
    post_flow_data <- balanced_iea_data %>%
      dplyr::filter(Country %in% input$country) %>%
      dplyr::filter(Ledger.side %in% input$ledgerside) %>%
      dplyr::filter(Flow.aggregation.point %in% input$flowaggpoint) %>%
      dplyr::filter(Flow %in% input$flow)

    updateSelectInput(session,
                      inputId = "product",
                      choices = sort(unique(post_flow_data$Product)))
  })
  

  # Final Energy carrier-Destination to Machine-useful work allocation plots
  output$ieadata_plot <- renderPlotly({
    p <- ggplot2::ggplot(data = selected_data()) +
      ggplot2::geom_area(mapping = aes_string(x = "Year",
                                              y = "E.dot",
                                              # group = Flow,
                                              fill = input$stack_var) #,
                         # position = "fill"
      ) +
      # ggplot2::scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
      ggplot2::scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
      # ggplot2::theme(axis.title.y = element_text(margin = margin(r=10))) +
      ggplot2::facet_wrap(facets = ~get(input$facet_var), ncol = 2, scales = "free_y") + # Add more facets? 
      ggplot2::labs(fill = "Product") +
      ggplot2::xlab("") +
      ggplot2::ylab("Energy [ktoe]") +
      MKHthemes::xy_theme()
    
    
    p_plotly <- plotly::ggplotly(p, height = 700) %>%
      plotly::layout(showlegend = TRUE, 
                     legend = list(font = list(size = 12))) %>%
      move_annotations(x = -0.05, y = 0.97, mar = 80)
    
    # p_plotly[['x']][['layout']][['annotations']][[1]][['x']] <- -0.06
    # 
    # p_plotly %>% plotly::layout(margin = list(l = 75))
    
  })
  
  
}


