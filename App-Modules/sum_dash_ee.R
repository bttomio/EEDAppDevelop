# Loads required packages
library(tidyverse)
library(scales)

# Establishes UI module function
sumdashplots_eeUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    
    use_cicerone(),
    
    withMathJax(),
    
    box(title = "Variables",
        id = "Variables_ee",
        # status = "primary",
        solidHeader = FALSE,
        closable = FALSE, 
        collapsible = TRUE,
        width = 12,
        # height = 100,
        splitLayout(
          
          style = "vertical-align: middle;",
          
          cellArgs = list(style = "padding: 0px"),
          
          selectizeInput(inputId = ns("country"),
                         label = "Country:",
                         choices = countries,
                         width = "150px",
                         options = list(dropdownParent = 'body')),
          
          selectizeInput(inputId = ns("EorX"),
                         label = "Energy Quantification:",
                         choices = c(Energy = "E", 
                                     Exergy = "X"),
                         width = "150px",
                         options = list(dropdownParent = 'body')),
          
          selectizeInput(inputId = ns("stage"),
                         label = "ECC Stage:",
                         choices = c(Primary = "Primary",
                                     Final = "Final",
                                     Useful = "Useful"),
                         width = "150px",
                         options = list(dropdownParent = 'body')),
          
          selectizeInput(inputId = ns("stages"),
                         label = "Efficiency Stages:",
                         choices = c(`Primary-Useful` = "Primary-Useful",
                                     `Primary-Final` = "Primary-Final",
                                     `Final-Useful` = "Final-Useful"),
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
          
          selectizeInput(inputId = ns("gdpmet"),
                         label = "GDP Metric:",
                         choices = c(`Real` = "rgdpna",
                                     `Expenditure-side` = "rgdpe", 
                                     `Output-side` = "rgdpo"),
                         width = "150px", 
                         options = list(dropdownParent = 'body')),
          
          selectizeInput(inputId = ns("rollavg"),
                         label = "CAAGR Period:",
                         choices = c(#"9" = 9,
                                     "7" = 7, 
                                     "5" = 5, 
                                     "3" = 3, 
                                     "1" = 1),
                         width = "150px", 
                         options = list(dropdownParent = 'body')),
          
          selectizeInput(inputId = ns("prodfunc"),
                         label = "Production function:",
                         choices = c(# `Single-Factor` = "sf",
                                     `Cobb-Douglas` = "cd",
                                     `CES (kl, e)` = "ces.kl.e",
                                     `CES (ke, l)` = "ces.ke.l",
                                     `CES (le, k)` = "ces.le.k",
                                     `Linex` = "linex"),
                         width = "150px", 
                         options = list(dropdownParent = 'body')),
          
          # selectizeInput(inputId = ns("percap"),
          #                label = "Per Capita:",
          #                choices = c(No = "abs", 
          #                            Yes = "pc"),
          #                width = "150px",
          #                options = list(dropdownParent = 'body')),
          
          # selectizeInput(inputId = ns("legend"),
          #                label = "Show Legend:",
          #                choices = c(Yes = "TRUE", 
          #                            No = "FALSE"),
          #                width = "150px",
          #                options = list(dropdownParent = 'body')),
          
          actionButton(inputId = ns("ee_dash_button"),
                       label = "Help",
                       style = 'margin-top:25px;
                                margin-left:30px;')
          
          )),
    
    
    # Energy-GDP Decoupling space box and sidebar
    box(
      width = 4,
      # height = 450,
      title = "Energy-GDP Coupling Space",
      id = "sumdash_ds",
      closable = FALSE, 
      # status = "warning", 
      solidHeader = FALSE, 
      collapsible = TRUE,
      enable_sidebar = TRUE,
      sidebar_width = 25,
      sidebar_start_open = FALSE,
      sidebar_background = "#FFFFFF",
      sidebar_title = "Variables",
      sidebar_content = tagList(),
      plotlyOutput(outputId = ns("sumdash_ds")) 
    ),
    
    # Energy-Efficiency Rebound Space box and sidebar
    box(
      width = 4,
      # height = 450,
      title = "Energy-Efficiency Rebound Space",
      id = "sumdash_rs",
      closable = FALSE, 
      # status = "warning", 
      solidHeader = FALSE, 
      collapsible = TRUE,
      enable_sidebar = TRUE,
      sidebar_width = 50,
      sidebar_start_open = FALSE,
      sidebar_background = "#f2f2f2",
      sidebar_title = "Variables",
      sidebar_content = tagList(),
      plotlyOutput(outputId = ns("sumdash_rs"))
    ),
    
    # Efficiency-GDP space
    box(
      width = 4,
      # height = 450,
      title = "Efficiency-GDP Coupling Space",
      id = "sumdash_etagdp",
      closable = FALSE, 
      # status = "warning", 
      solidHeader = FALSE, 
      collapsible = TRUE,
      enable_sidebar = TRUE,
      sidebar_width = 25,
      sidebar_start_open = FALSE,
      sidebar_background = "#FFFFFF",
      sidebar_title = "Variables",
      sidebar_content = tagList(),
      plotlyOutput(outputId = ns("sumdash_etagdp"))
    ),
    
    # Indexed Data box and sidebar
    box(
      width = 4,
      # height = 450,
      title = "Indexed data",
      id = "sumdash_i",
      closable = FALSE, 
      # status = "warning", 
      solidHeader = FALSE, 
      collapsible = TRUE,
      enable_sidebar = TRUE,
      sidebar_width = 25,
      sidebar_start_open = FALSE,
      sidebar_background = "#FFFFFF",
      sidebar_title = "Variables",
      sidebar_content = tagList(),
      plotlyOutput(outputId = ns("sumdash_i"))
    ),
    
    # APF Fitting box and sidebar
    box(
      width = 4,
      # height = 450,
      title = "APF Fitting", 
      id = "sumdash_apf",
      closable = FALSE, 
      # status = "warning", 
      solidHeader = FALSE, 
      collapsible = TRUE,
      enable_sidebar = TRUE,
      sidebar_width = 25,
      sidebar_start_open = FALSE,
      sidebar_background = "#FFFFFF",
      sidebar_title = "Variables",
      sidebar_content = tagList(),
      plotlyOutput(outputId = ns("sumdash_apf"))
    ),
    
    # Causality Analysis box and sidebar
    box(
      width = 4,
      # height = 450,
      title = "Causality Analysis", 
      closable = FALSE, 
      # status = "warning", 
      solidHeader = FALSE, 
      collapsible = TRUE,
      enable_sidebar = TRUE,
      sidebar_width = 25,
      sidebar_start_open = FALSE,
      sidebar_background = "#FFFFFF",
      sidebar_title = "Variables",
      sidebar_content = tagList()
    )
    
    )
}

# Establishes the server module function
sumdashplots_ee <- function(input, output, session,
                            
                            EorX,
                            
                            country,
                            
                            stage,
                            
                            stages,
                            
                            gross_net,
                            
                            gdpmet,
                            
                            rollavg,
                            
                            # legend,
                            
                            prodfunc,
                            
                            ee_dash_button
                            
                            ) {
  
################################################################################
# Add tour #
################################################################################ 

guide_ee_dash$init()
  
################################################################################
# Update Events #
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
  
  
  observeEvent(input$ee_dash_button, {
    guide_ee_dash$start()
  })

  
################################################################################
# Select Data #
################################################################################  
  

  # Creates data frame for indexed GDP
  selected_data_GDP <- reactive({
    GDP_metrics %>%
      dplyr::filter(Country == input$country,
                    GDP_Metric == input$gdpmet)
  })
  
  
  # Creates reactive data frame for decoupling space plot
  selected_data_ds <- reactive({
    validate(
      # need(input$stage != "", "Please select one Energy Conversion Chain (ECC) stage")
    )
    
    data_ds <- EX_Econ_Data %>%
      dplyr::filter(Energy.type == input$EorX) %>%
      dplyr::filter(Stage == input$stage) %>%
      dplyr::filter(Stages == input$stages) %>%
      # dplyr::filter(Gross.Net %in% c("Net", "Neither")) %>%
      dplyr::filter(GDP_Metric == input$gdpmet) %>%
      dplyr::filter(Country == input$country) %>%
      dplyr::mutate(EX.CAAGR = calc_roll_caagr(metric = EX, 
                                               period = as.numeric(input$rollavg), 
                                               direction = "Center") * 100) %>%
      dplyr::mutate(GDP.CAAGR = calc_roll_caagr(metric = GDP, 
                                                period = as.numeric(input$rollavg), 
                                                direction = "Center") * 100)
    
    data_ds
    
  })
  
  # Creates reactive data frame for rebound space plot
  selected_data_rs <- reactive({
    # validate(
    #   need(input$stage != "", "Please select one Energy Conversion Chain (ECC) stage"),
    #   need(input$stages != "", "Please select one efficiency stage")
    # )
    
    data_rs <- EX_Econ_Data %>%
      dplyr::filter(Energy.type == input$EorX) %>%
      dplyr::filter(Stage == input$stage) %>%
      dplyr::filter(Stages == input$stages) %>%
      dplyr::filter(GDP_Metric == input$gdpmet) %>%
      dplyr::filter(Country == input$country) %>%
      dplyr::mutate(EX.CAAGR = calc_roll_caagr(metric = EX, 
                                               period = as.numeric(input$rollavg), 
                                               direction = "Center") * 100) %>%
      dplyr::mutate(Eta.CAAGR = calc_roll_caagr(metric = Eta, 
                                                period = as.numeric(input$rollavg), 
                                                direction = "Center") * 100)
    
    data_rs
    
    
  })
  
  # Creates reactive data frame for decoupling space plot
  selected_data_etagdp <- reactive({
    validate(
      # need(input$stage != "", "Please select one Energy Conversion Chain (ECC) stage")
    )
    
    data_etagdp <- EX_Econ_Data %>%
      dplyr::filter(Energy.type == input$EorX) %>%
      dplyr::filter(Stage == input$stage) %>%
      dplyr::filter(Stages == input$stages) %>%
      dplyr::filter(GDP_Metric == input$gdpmet) %>%
      dplyr::filter(Country == input$country) %>%
      dplyr::mutate(Eta.CAAGR = calc_roll_caagr(metric = Eta, 
                                                period = as.numeric(input$rollavg), 
                                                direction = "Center") * 100) %>%
      dplyr::mutate(GDP.CAAGR = calc_roll_caagr(metric = GDP, 
                                                period = as.numeric(input$rollavg), 
                                                direction = "Center") * 100)
    
    data_etagdp
    
  })
  
  # Creates reactive data frame for indexed EX
  selected_data_i <- reactive({
    
    data <- EX_Econ_Data %>%
      dplyr::filter(Energy.type == input$EorX) %>%
      dplyr::filter(Stage == input$stage) %>%
      dplyr::filter(Stages == input$stages) %>%
      dplyr::filter(GDP_Metric == input$gdpmet) %>%
      dplyr::filter(Country == input$country)
    
    data$Stage <- factor(data$Stage, levels = c("Primary", "Final", "Useful"))
    data$Stages <- factor(data$Stages, levels = c("Primary-Final", "Primary-Useful", "Final-Useful"))
    
    data
    
    
  })
  
  # Creates reactive data frame for APF fitting
  selected_data_apf <- reactive({
    
    data <- APF_Data %>%
      dplyr::filter(Stage == input$stage) %>%
      dplyr::filter(GDP_Metric == input$gdpmet) %>%
      dplyr::filter(Country == input$country) %>%
      dplyr::filter(APF == input$prodfunc)
    
  })
  
  # Creates reactive data frame for socio-econ data
  selected_data_socecon <- reactive({
    
    data <- EA_data_final %>%
      dplyr::filter(Country == input$country)
    
  })
  
################################################################################
# Outputs #
################################################################################
  
  
  # Plots Energy-GDP decoupling space
  output$sumdash_ds <- renderPlotly({
    
    # Reactively set x-axis and y-axis maximum values
    max_GDP.CAAGR <- max(selected_data_ds()$GDP.CAAGR, na.rm = TRUE)
    max_EX.CAAGR <- max(selected_data_ds()$EX.CAAGR, na.rm = TRUE)
    
    if(max_GDP.CAAGR > max_EX.CAAGR) {
      
      xmax <- max_GDP.CAAGR * (1.1)
      
      ymax <- xmax
      
    } else {
      
      xmax <- max_EX.CAAGR * (1.1)
      
      ymax <- xmax
      
    }
    
    # Reactively set x-axis and y-axis minimum values
    min_GDP.CAAGR <- min(selected_data_ds()$GDP.CAAGR, na.rm = TRUE)
    min_EX.CAAGR <- min(selected_data_ds()$EX.CAAGR, na.rm = TRUE)
    
    # xmin <- min_GDP.CAAGR * (1.1)
    # 
    # ymin <- min_EX.CAAGR * (1.1)
    
    if (min_GDP.CAAGR > 0 | min_EX.CAAGR > 0) {
      
      xmin <- -xmax/4
      
      ymin <- xmin
        
    } else if(abs(min_GDP.CAAGR) > abs(min_EX.CAAGR)) {

      xmin <- min_GDP.CAAGR * (1.1)

      ymin <- xmin

    } else {

      xmin <- min_EX.CAAGR * (1.1)

      ymin <- xmin

    }
    
    # Hypercoupling position
    hc_y <- ymax
    hc_x <- xmax * 0.5
    
    # Coupled position
    c_y <- ymax * 0.75
    c_x <- c_y
    
    # Relative decoupling position
    rd_y <- ymax * 0.35
    rd_x <- xmax * 0.8
    
    # Decoupled position
    dc_y <- 0
    dc_x <- xmax * 0.75
    
    # Absolute decoupling position
    ad_y <- ymin - 0.1
    ad_x <- xmax * 0.5
    
    
    
    p_ds <- ggplot2::ggplot(data = selected_data_ds()[order(selected_data_ds()$Year),], 
                            mapping = aes(x = GDP.CAAGR, y = EX.CAAGR)) +
      
      ggplot2::geom_point(mapping = aes(colour = Year)) +
      ggplot2::geom_path(size = 0.25,
                         colour = "grey") +
      
      ggplot2::scale_y_continuous(limits = c(ymin, ymax), 
                                  breaks = seq(ceiling(ymin), ceiling(ymax), by = 1),
                                  labels = scales::number_format(accuracy = 1.0)) +
      
      ggplot2::scale_x_continuous(limits = c(xmin, xmax), 
                                  breaks = seq(ceiling(xmin), ceiling(xmax), by = 1),
                                  labels = scales::number_format(accuracy = 1.0)) +
      
      ggplot2::geom_abline(slope = 1, intercept = 0, colour = "black") +
      geom_hline(data = data.frame(type="A", y=0), mapping=aes(yintercept = y)) +
      geom_vline(data = data.frame(type="A", x=0), mapping=aes(xintercept = x)) +
      
      ggplot2::scale_color_gradientn(colors = c("#F0E442", "#D55E00", "#999999")) + 
      ggplot2::xlab("GDP CAAGR [%]") +
      ggplot2::ylab("Final demand CAAGR [%]") +
      
      MKHthemes::xy_theme()
    
    
    p_plotly_ds <- plotly::ggplotly(p_ds, height = 400, 
                                    tooltip = c("Year", "GDP.CAAGR", "EX.CAAGR")) %>%
      
      plotly::layout(# showlegend = as.logical(input$legend), # showscale
        legend = list(itemclick = TRUE, 
                      itemdoubleclick = TRUE)) %>%
      
      # toggle_colorbar(toggle = as.logical(input$legend)) %>%
      
      # plotly::colorbar(len = 1) %>%
      
      add_annotations(
        x = hc_x,
        y = hc_y,
        text = "Hypercoupling",
        font = list(size = 12),
        opacity = 0.7,
        size = 0.2,
        showarrow = F) %>%
      
      add_annotations(
        x = c_x,
        y = c_y,
        text = "Coupled",
        font = list(size = 12),
        opacity = 0.7,
        ax = 30,
        ay = 30,
        size = 0.2,
        showarrow = T) %>%
      
      add_annotations(
        x = rd_x,
        y = rd_y,
        text = "Relative Decoupling",
        font = list(size = 12),
        opacity = 0.7,
        size = 0.2,
        showarrow = F) %>%
      
      add_annotations(
        x = dc_x,
        y = dc_y,
        text = "Decoupled",
        font = list(size = 12),
        opacity = 0.7,
        size = 0.2,
        showarrow = T) %>%
      
      add_annotations(
        x = ad_x,
        y = ad_y,
        text = "Absolute Decoupling",
        font = list(size = 12),
        opacity = 0.7,
        size = 0.2,
        showarrow = F)
    
    p_plotly_ds
    
    
  })
  
  # Plots Energy-Efficiency rebound space
  output$sumdash_rs <- renderPlotly({
    
    # Reactively set x-axis and y-axis maximum values
    max_Eta.CAAGR <- max(selected_data_rs()$Eta.CAAGR, na.rm = TRUE)
    max_EX.CAAGR <- max(selected_data_rs()$EX.CAAGR, na.rm = TRUE)
    
    if(max_Eta.CAAGR > max_EX.CAAGR) {
      
      xmax <- max_Eta.CAAGR * (1.1)
      
      ymax <- xmax
      
    } else {
      
      xmax <- max_EX.CAAGR * (1.1)
      
      ymax <- xmax
      
    }
    
    # Reactively set x-axis and y-axis minimum values
    min_Eta.CAAGR <- min(selected_data_rs()$Eta.CAAGR, na.rm = TRUE)
    min_EX.CAAGR <- min(selected_data_rs()$EX.CAAGR, na.rm = TRUE)
    
    
    if (min_Eta.CAAGR > 0 | min_EX.CAAGR > 0) {
      
      xmin <- -xmax/4
      
      ymin <- xmin
      
    } else if(abs(min_Eta.CAAGR) > abs(min_EX.CAAGR)) {

      xmin <- min_Eta.CAAGR * (1.1)

      ymin <- xmin

    } else {

      xmin <- min_EX.CAAGR * (1.1)

      ymin <- xmin

    }
    
    # Hyperconservation position
    hc_y <- ymin - 0.1
    hc_x <- 0.01 # Set text start to left in annotations
    
    # 0% rebound position
    r0_y <- ymin * 0.5
    r0_x <- r0_y * -1
    
    # Partial rebound position
    rP_y <- 0.5
    rP_x <- xmax # Set text start from right in annotations
    
    # 100% rebound position
    r100_y <- ymax * 0.5
    r100_x <- r100_y
    
    # Backfire position
    b_y <- ymax
    b_x <- 0.01 # Set text start to left in annotations
    
    
    p <- ggplot2::ggplot(data = selected_data_rs()[order(selected_data_rs()$Year),], 
                         mapping = aes(x = Eta.CAAGR, y = EX.CAAGR)) +
      
      ggplot2::geom_point(mapping = aes(colour = Year)) +
      
      ggplot2::geom_path(size = 0.25, colour = "grey") +
      
      ggplot2::scale_y_continuous(limits = c(ymin, ymax), 
                                  breaks = seq(ceiling(ymin), ceiling(ymax), by = 1),
                                  labels = scales::number_format(accuracy = 1.0)) +
      
      ggplot2::scale_x_continuous(limits = c(xmin, xmax), 
                                  breaks = seq(ceiling(xmin), ceiling(xmax), by = 1),
                                  labels = scales::number_format(accuracy = 1.0)) +
      
      ggplot2::geom_abline(slope = 1, intercept = 0, colour = "black") +
      ggplot2::geom_abline(slope = -1, intercept = 0, colour = "black") +
      
      ggplot2::geom_hline(data = data.frame(type="A", y=0), 
                          mapping=aes(yintercept = y), 
                          linetype="dashed", 
                          size = 0.3) +
      
      ggplot2::geom_vline(data = data.frame(type="A", x=0), 
                          mapping=aes(xintercept = x)) +
      
      ggplot2::scale_color_gradientn(colors = c("#F0E442", "#D55E00", "#999999")) + 
      
      ggplot2::xlab("Efficiency CAAGR [%]") +
      ggplot2::ylab("Final demand CAAGR [%]") +
      
      MKHthemes::xy_theme()
    
    
    p_plotly <- plotly::ggplotly(p, height = 400, tooltip = c("Year", "Eta.CAAGR", "EX.CAAGR")) %>%
      
      plotly::layout(# showlegend = as.logical(input$legend), # showscale
        legend = list(itemclick = TRUE, 
                      itemdoubleclick = TRUE)) %>%
      
      # toggle_colorbar(toggle = as.logical(input$legend)) %>%
      
      # plotly::colorbar(len = 2) %>%
      
      add_annotations(
        x = hc_x,
        y = hc_y,
        text = "Hyperconservation",
        font = list(size = 12),
        xanchor = "left",
        opacity = 0.7,
        size = 0.2,
        showarrow = F) %>%
      
      add_annotations(
        x = r0_x,
        y = r0_y,
        text = "0% Rebound",
        font = list(size = 12),
        opacity = 0.7,
        xanchor = "left",
        ax = 20,
        ay = -20,
        size = 0.2,
        showarrow = T) %>%
      
      add_annotations(
        x = rP_x,
        y = rP_y,
        text = "Partial Rebound",
        font = list(size = 12),
        opacity = 0.7,
        xanchor = "right",
        size = 0.2,
        showarrow = F) %>%
      
      add_annotations(
        x = r100_x,
        y = r100_y,
        text = "100% Rebound",
        font = list(size = 12),
        opacity = 0.7,
        xanchor = "left",
        ax = 20,
        ay = 20,
        size = 0.2,
        showarrow = T) %>%
      
      add_annotations(
        x = b_x,
        y = b_y,
        text = "Backfire",
        font = list(size = 12),
        xanchor = "left",
        opacity = 0.7,
        size = 0.2,
        showarrow = F)
    
    p_plotly
    
    
  })
  
  # Plots Efficiency-GDP space
  output$sumdash_etagdp <- renderPlotly({
    
    # Reactively set x-axis and y-axis maximum values
    max_GDP.CAAGR <- max(selected_data_etagdp()$GDP.CAAGR, na.rm = TRUE)
    max_Eta.CAAGR <- max(selected_data_etagdp()$Eta.CAAGR, na.rm = TRUE)
    
    if(max_GDP.CAAGR > max_Eta.CAAGR) {
      
      xmax <- max_GDP.CAAGR * (1.1)
      
      ymax <- xmax
      
    } else {
      
      xmax <- max_Eta.CAAGR * (1.1)
      
      ymax <- xmax
      
    }
    
    # Reactively set x-axis and y-axis minimum values
    min_GDP.CAAGR <- min(selected_data_etagdp()$GDP.CAAGR, na.rm = TRUE)
    min_Eta.CAAGR <- min(selected_data_etagdp()$Eta.CAAGR, na.rm = TRUE)
    
    if (min_GDP.CAAGR > 0 | min_Eta.CAAGR > 0) {
      
      xmin <- -xmax/4
      
      ymin <- xmin
      
    } else if(abs(min_GDP.CAAGR) > abs(min_Eta.CAAGR)) {

      xmin <- min_GDP.CAAGR * (1.1)

      ymin <- xmin

    } else {

      xmin <- min_Eta.CAAGR * (1.1)

      ymin <- xmin

    }
    
    
    p <- ggplot2::ggplot(data = selected_data_etagdp()[order(selected_data_etagdp()$Year),], 
                         mapping = aes(x = GDP.CAAGR, y = Eta.CAAGR)) +
      
      ggplot2::geom_point(mapping = aes(colour = Year)) +
      ggplot2::geom_path(size = 0.25,
                         colour = "grey") +
      
      ggplot2::scale_y_continuous(limits = c(ymin, ymax), 
                                  breaks = seq(ceiling(ymin), ceiling(ymax), by = 1), 
                                  labels = scales::number_format(accuracy = 1.0)
                                  ) +
      
      ggplot2::scale_x_continuous(limits = c(xmin, xmax), 
                                  breaks = seq(ceiling(xmin), ceiling(xmax), by = 1), 
                                  labels = scales::number_format(accuracy = 1.0)
                                  ) +

      ggplot2::geom_hline(data = data.frame(type="A", y=0), mapping=aes(yintercept = y)) +
      ggplot2::geom_vline(data = data.frame(type="A", x=0), mapping=aes(xintercept = x)) +
      
      ggplot2::scale_color_gradientn(colors = c("#F0E442", "#D55E00", "#999999")) + 
      
      ggplot2::xlab("GDP CAAGR [%]") + 
      ggplot2::ylab("Efficiency CAAGR [%]") +
      
      MKHthemes::xy_theme()
    
    
    p_plotly <- plotly::ggplotly(p, height = 400, tooltip = c("Year", "Eta.CAAGR", "GDP.CAAGR")) %>%
      
      plotly::layout(# showlegend = as.logical(input$legend), # showscale
        legend = list(itemclick = TRUE, 
                      itemdoubleclick = TRUE))
    
  })
  
  index_colors <- c("Primary" = "red4", "Final" = "red", "Useful" = "orange", 
                    "GDP.i" = "black", "Final-Useful" = "green", "Primary-Final" = "purple",
                    "Primary-Useful" = "blue", "EX_intensity.i" = "brown")
  
  # Plots indexed data
  output$sumdash_i <- renderPlotly({
    
    p_i <- ggplot2::ggplot(data = selected_data_i()) + 
      
      ggplot2::geom_line(mapping = aes(x = Year, y = EX.i, color = Stage)) +
      ggplot2::geom_line(mapping = aes(x = Year, y = GDP.i, color = "GDP.i")) +
      ggplot2::geom_line(mapping = aes(x = Year, y = EX_intensity.i, color = "EX_intensity.i")) +
      ggplot2::geom_line(mapping = aes(x = Year, y = Eta.i, color = Stages)) +
      
      ggplot2::scale_colour_manual(values = index_colors) +
      
      # ggplot2::scale_y_continuous(limits = c(1960, 2020), breaks = seq(-3, 12, by = 1)) +
      ggplot2::scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 10)) +
      
      ggplot2::xlab("") +
      ggplot2::ylab("Indexed Value [-]") +
      
      MKHthemes::xy_theme() +
      theme(legend.title = element_blank())
    
    p_i_plotly <- plotly::ggplotly(p_i, height = 400, tooltip = c("Year", 
                                                                  "Stage",
                                                                  "Stages",
                                                                  "Eta.i",
                                                                  "EX.i", 
                                                                  "GDP.i", 
                                                                  "EX_intensity.i")) %>%
      
      plotly::layout(showlegend = as.logical(input$legend),
                     legend = list(itemclick = TRUE, 
                                   itemdoubleclick = TRUE,
                                   font = list(size = 12),
                                   y = 0.5
                                   # title = list(text = 'Metric', # '<b> Metric </b>'
                                   #              size = 10,
                                   #              x = -0.5)
                     )) #%>%
    
    # move_legend_annotation_no_facet(y = 0.925, mar = 80)
    
  })
  
  apf_colors <- c("GDP" = "black", "GDP.Fitted" = "blue", "L" = "red", "K" = "darkgreen")
  
  # Plots APF fitting space
  output$sumdash_apf <- renderPlotly({
    
    p <- ggplot2::ggplot(data = selected_data_apf()) +
      
      ggplot2::geom_point(mapping = aes(x= Year, y = GDP, color = "GDP")) +
      ggplot2::geom_line(mapping = aes(x= Year, y = GDP.Fitted, color = "GDP.Fitted")) +
      ggplot2::geom_line(data = selected_data_socecon(), mapping = aes(x= Year, y = L, color = "L")) +
      ggplot2::geom_line(data = selected_data_socecon(), mapping = aes(x= Year, y = K, color = "K")) +
      
      ggplot2::scale_colour_manual(values = apf_colors) +
      
      ggplot2::xlab("Year") +
      ggplot2::ylab("GDP - index 1960") + 
      
      ggplot2::scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 10)) +
      
      MKHthemes::xy_theme() +
      theme(legend.title = element_blank())
    
    p_plotly <- plotly::ggplotly(p, height = 400, tooltip = c("Year", "GDP", "GDP.Fitted", "L", "K")) %>%
      
      plotly::layout(showlegend = as.logical(input$legend),
                     legend = list(itemclick = TRUE, 
                                   itemdoubleclick = TRUE,
                                   font = list(size = 12),
                                   y = 0.5)
                     )
    
    
  })
  
}


