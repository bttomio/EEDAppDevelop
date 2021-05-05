# Loads required packages
library(tidyverse)
library(zoo)

# Establishes UI module function
decoupling_spaceUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    tabBox(title = "Energy-Economy Decoupling Space",
           # id = ,
           width = 9,
           height = 950,
           tabPanel(
             title = "Plots",
             plotlyOutput(outputId = ns("ds_plot"))
           )
           # tabPanel( # Add data tab here!
           #   
           # )
    ),
    box(title = "Variables", 
        # status = "warning", # sets color
        selectizeInput(inputId = ns("EorX"),
                       label = "Energy Quantification:",
                       choices = c(Energy = "E", Exergy = "X")
        ), 
        selectizeInput(inputId = ns("stage"), # Need to change to FUMachine throughout
                       label = "ECC Stage:",
                       choices = c(Primary = "Primary",
                                   Final = "Final",
                                   Useful = "Useful")
                       %>% sort()
        ), 
        selectizeInput(inputId = ns("gross_net"),
                       label = "Gross or Net:",
                       choices = c(Neither = "Neither",
                                   Gross = "Gross",
                                   Net = "Net")
                       %>% sort()
        ), 
        selectizeInput(inputId = ns("gdpmet"),
                       label = "GDP Metric:",
                       choices = c(
                         `Real` = "rgdpna",
                         `Expenditure-side` = "rgdpe",
                         `Output-side` = "rgdpo"
                       ),
                       width = "150px",
                       options = list(dropdownParent = 'body')
        ), 
        selectizeInput(inputId = ns("country"),
                       label = "Country:",
                       choices = countries,
                       multiple = TRUE
                       %>% sort()
        ), 
        selectizeInput(inputId = ns("rollavg"),
                       label = "Rolling Average Period:",
                       choices = c(
                         "1" = 1,
                         "3" = 3,
                         "5" = 5,
                         "7" = 7
                       )
        ), 
        solidHeader = TRUE,
        width = 3
    ),
  )
}

# Establishes the server module function
decoupling_space <- function(input, output, session,
                             EorX,
                             stage,
                             gross_net,
                             country,
                             rollavg,
                             gdpmet,
                             ds_plot) {
  
  # Creates a dataframe with the selected country, efproduct, and destination 
  selected_data <- reactive({
    validate(
      need(input$EorX != "", "Please select one Energy quantification"),
      need(input$stage != "", "Please select one Energy Conversion Chain (ECC) stage"),
      need(input$gross_net != "", "Please select one Final demand quantification"),
      need(input$gdpmet != "", "Please select one GDP metric type"),
      need(input$country != "", "Please select atleast one Country"),
      need(input$rollavg != "", "Please select the number of years for average")
    )
    
    
    data <- PSUT_metrics_total %>%
      dplyr::left_join(GDP_metrics, by = c("Country", "Year")) %>%
      dplyr::filter(Energy.type == input$EorX,
                    Gross.Net == input$gross_net,
                    Stage == input$stage,
                    Country %in% input$country,
                    GDP_Metric == input$gdpmet) %>%
      dplyr::group_by(Country, Method, Energy.type, Gross.Net, Stage) %>%
      dplyr::mutate(EX.CAAGR = calc_roll_caagr(metric = EX, period = as.numeric(input$rollavg), direction = "Center") * 100) %>%
      dplyr::mutate(GDP.CAAGR = calc_roll_caagr(metric = GDP, period = as.numeric(input$rollavg), direction = "Center") * 100) %>%
      accumulate_by(~Year) %>%
      dplyr::ungroup()
  
  })
  
  
  output$ds_plot <- renderPlotly({
    
        p <- ggplot2::ggplot(data = selected_data()[order(selected_data()$Year),], mapping = aes(x = GDP.CAAGR, y = EX.CAAGR)) +
        
        ggplot2::geom_point(mapping = aes(colour = Year, frame = frame)) + #, frame = Year
        
        ggplot2::geom_path(size = 0.25,
                           colour = "grey") +
          
        ggplot2::scale_y_continuous(limits = c(-5, 12), breaks = seq(-5, 12, by = 1)) +
        ggplot2::scale_x_continuous(limits = c(-5, 12), breaks = seq(-5, 12, by = 1)) +
        
        ggplot2::geom_abline(slope = 1, intercept = 0, colour = "black") +
      
        geom_hline(data = data.frame(type="A", y=0), mapping=aes(yintercept = y)) +
        geom_vline(data = data.frame(type="A", x=0), mapping=aes(xintercept = x)) +
        
        ggplot2::geom_segment(data = data.frame(x = 0, y = 0, xend = 0.1, yend = 0.1),
                              mapping = aes(x = x, y = y, xend = xend, yend = yend),
                              colour = "black") +
        
        ggplot2::geom_segment(data = data.frame(x = 0, y = 0, xend = 0.1, yend = 0),
                              mapping = aes(x = x, y = y, xend = xend, yend = yend),
                              colour = "black") +
        
        ggplot2::scale_color_gradientn(colors = c("#F0E442", "#D55E00", "#999999")) + 
        
        ggplot2::xlab("GDP CAAGR [%]") + # [1/year]
        ggplot2::ylab("Final demand CAAGR [%]") + # [1/year]
        
        ggplot2::facet_wrap(facets = "Country", ncol = 2) +
        
        MKHthemes::xy_theme()
    
      
    p_plotly <- plotly::ggplotly(p, height = 850, tooltip = c("Year", "GDP.CAAGR", "EX.CAAGR")) %>%
      move_annotations_ds(x_y = -0.05, 
                          y_x = -0.05, 
                          mar = 80) %>%
      
      add_annotations(
        x = 4,
        y = 11,
        text = "Hypercoupling",
        font = list(size = 12),
        size = 0.2,
        showarrow = F) %>%
      
      add_annotations(
        x = 10,
        y = 10,
        text = "Coupled",
        # textangle = -45,
        font = list(size = 12),
        size = 0.2,
        showarrow = T) %>%
      
      add_annotations(
        x = 9,
        y = 4,
        text = "Relative Decoupling",
        font = list(size = 12),
        size = 0.2,
        showarrow = F) %>%
      
      add_annotations(
        x = 10,
        y = 0,
        text = "Decoupled",
        font = list(size = 12),
        size = 0.2,
        showarrow = T) %>%
      
      add_annotations(
        x = 5,
        y = -4,
        text = "Absolute Decoupling",
        font = list(size = 12),
        size = 0.2,
        showarrow = F)

    
  })
  
}