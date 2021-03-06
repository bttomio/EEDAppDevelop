# Loads required packages
library(tidyverse)

# Establishes UI module function
rebound_spaceUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    tabBox(title = "Rebound Space",
           # id = ,
           width = 9,
           height = 950,
           tabPanel(
             title = "Plots",
             plotlyOutput(outputId = ns("rs_plot"))
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
        selectizeInput(inputId = ns("stage"),
                       label = "ECC Stage:",
                       choices = c(Primary = "Primary", Final = "Final", Useful = "Useful") # Primary = "Primary",            
        ),
        selectizeInput(inputId = ns("stages"),
                       label = "Efficiency Stages:",
                       choices = c(`Primary-Final` = "Primary-Final", `Final-Useful` = "Final-Useful")
        ),
        selectizeInput(inputId = ns("country"),
                       label = "Country:",
                       choices = countries,
                       multiple = TRUE
                       %>% sort()
        ),
        selectizeInput(inputId = ns("rollavg"),
                    label = "Rolling Average Period:",
                    choices = c("1" = 1, "3" = 3, "5" = 5, "7" = 7)
        ),
        solidHeader = TRUE,
        width = 3
    ),
  )
}

# Establishes the server module function
rebound_space <- function(input, output, session,
                          EorX,
                          stage,
                          stages,
                          # gross_net,
                          country,
                          rollavg,
                          rs_plot) {
  
  # Creates a dataframe with the selected country, efproduct, and destination 
  selected_data <- reactive({
    validate(
      need(input$EorX != "", "Please select one Energy quantification"),
      need(input$stage != "", "Please select one Energy Conversion Chain (ECC) stage"),
      need(input$stages != "", "Please select one efficiency stage"),
      # need(input$gross_net != "", "Please select one Final demand quantification"),
      need(input$country != "", "Please select atleast one Country"),
      need(input$rollavg != "", "Please select the number of years for average")
    )
    
    PSUT_etas <- PSUT_etas %>%
      dplyr::filter(Gross.Net == "Net") %>% 
      dplyr::select(-Gross.Net)
    
    data <- PSUT_metrics_total %>%
      dplyr::left_join(PSUT_etas, 
                       by = c("Country", "Method", "Energy.type", "Year")) %>% # "Gross.Net",
      dplyr::filter(Energy.type == input$EorX,
                    # Gross.Net == input$gross_net,
                    Gross.Net != "Gross",
                    Stage == input$stage,
                    Stages ==input$stages,
                    Country %in% input$country) %>%
      dplyr::select(-Gross.Net) %>%
      dplyr::group_by(Country, Method, Energy.type, Stage) %>%
      dplyr::mutate(EX.CAAGR = calc_roll_caagr(metric = EX, period = as.numeric(input$rollavg), direction = "Center") * 100) %>%
      dplyr::mutate(Eta.CAAGR = calc_roll_caagr(metric = Eta, period = as.numeric(input$rollavg), direction = "Center") * 100) %>%
      accumulate_by(~Year) %>%
      dplyr::ungroup()
    
    
  })

  
  
  output$rs_plot <- renderPlotly({
    
    p <- ggplot2::ggplot(data = selected_data()[order(selected_data()$Year),], mapping = aes(x = Eta.CAAGR, y = EX.CAAGR)) +
      
      ggplot2::geom_point(mapping = aes(colour = Year, frame = frame)) + #, frame = Year
      
      ggplot2::geom_path(size = 0.25,
                         colour = "grey") +
      
      ggplot2::scale_y_continuous(limits = c(-6, 12), breaks = seq(-6, 12, by = 1)) +
      ggplot2::scale_x_continuous(limits = c(-6, 12), breaks = seq(-6, 12, by = 1)) +
      
      ggplot2::geom_abline(slope = 1, intercept = 0, colour = "black") +
      ggplot2::geom_abline(slope = -1, intercept = 0, colour = "black") +
      
      ggplot2::geom_hline(data = data.frame(type="A", y=0), mapping=aes(yintercept = y), linetype="dashed", size = 0.3) +
      ggplot2::geom_vline(data = data.frame(type="A", x=0), mapping=aes(xintercept = x)) +
      
      ggplot2::geom_segment(data = data.frame(x = 0, y = 0, xend = 0.1, yend = 0.1),
                            mapping = aes(x = x, y = y, xend = xend, yend = yend),
                            colour = "black") +
      
      ggplot2::geom_segment(data = data.frame(x = 0, y = 0, xend = 0.1, yend = 0),
                            mapping = aes(x = x, y = y, xend = xend, yend = yend),
                            colour = "black") +

    
    ggplot2::scale_color_gradientn(colors = c("#F0E442", "#D55E00", "#999999")) + 
      
    ggplot2::xlab("Efficiency CAAGR [%]") + # [1/year]
    ggplot2::ylab("Final demand CAAGR [%]") + # [1/year]
      
    ggplot2::facet_wrap(facets = "Country", ncol = 2) +
      
    MKHthemes::xy_theme()
    
    
    p_plotly <- plotly::ggplotly(p, height = 850, tooltip = c("Year", "Eta.CAAGR", "EX.CAAGR")) %>%
      move_annotations_ds(x_y = -0.05, 
                          y_x = -0.05, 
                          mar = 80) %>%
    
      add_annotations(
        x = 3,
        y = -6,
        text = "No Rebound",
        font = list(size = 12),
        size = 0.2,
        showarrow = F) %>%
      
      add_annotations(
        x = 8,
        y = -1,
        text = "Partial Rebound",
        font = list(size = 12),
        size = 0.2,
        showarrow = F) %>%
      
      add_annotations(
        x = 4,
        y = 12,
        text = "Backfire",
        font = list(size = 12),
        size = 0.2,
        showarrow = F)
    
    
  })

  
}