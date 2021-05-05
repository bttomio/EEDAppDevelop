# Loads required packages
library(tidyverse)

# Establishes UI module function
sumdashplotsUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    
    use_cicerone(),
    
    box(
      title = "Variables",
      id = "Variables_pfu",
      # status = "primary",
      solidHeader = FALSE,
      closable = FALSE, 
      collapsible = TRUE,
      width = 12,
      # height = 100,
      splitLayout(
        
        cellArgs = list(style = "padding: 0px"),
        
        selectizeInput(inputId = ns("country"),
                       label = "Country:",
                       choices = countries,
                       width = "150px",
                       options = list(dropdownParent = 'body')),
        
        selectizeInput(inputId = ns("EorX"),
                       label = "Energy Quantification:",
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
        
        selectizeInput(inputId = ns("percap"),
                       label = "Per Capita:",
                       choices = c(No = "abs", Yes = "pc"),
                       width = "150px",
                       options = list(dropdownParent = 'body')),
        
        selectizeInput(inputId = ns("legend"),
                       label = "Show Legend:",
                       choices = c(Yes = "TRUE", No = "FALSE"),
                       width = "150px",
                       options =  list(dropdownParent = 'body')),
        
        selectizeInput(inputId = ns("stackfill"),
                       label = "Stack or Fill Area Plots:",
                       choices = c(Stack = "Stack", Fill = "Fill"),
                       width = "150px",
                       options = list(dropdownParent = 'body')),
        
        actionButton(inputId = ns("pfu_dash_button"),
                     label = "Help",
                     style = 'margin-top:25px;
                              margin-left:30px;')
        )),
    
    # Consumption by product
    box(
      width = 6,
      # height = 450,
      title = "Consumption by Product",
      id = "con_by_prod",
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
      plotlyOutput(outputId = ns("sumdash_prod"))
      ),
    
    # Consumption by Flow or Sector
    box(
      width = 6,
      # height = 450,
      title = "Consumption by Flow or Sector",
      id = "con_by_flowsec",
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
      plotlyOutput(outputId = ns("sumdash_flowsec"))
      ),
    
    # Indexed Data box and sidebar
    box(
      width = 4,
      # height = 450,
      title = "Indexed Data",
      id = "index_data",
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
    
    # Decomposition Analysis box and sidebar
    box(
      width = 4,
      # height = 450,
      title = "Decomposition Analysis", 
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
sumdashplots <- function(input, output, session,
                         
                         EorX,
                         
                         country,
                         
                         stage,
                         
                         stages_rs,
                         
                         gross_net,
                         
                         rollavg,
                         
                         legend, 
                         
                         stackfill
                         
                         ) {

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
  
  observeEvent(input$pfu_dash_button, {
    guide_pfu_dash$start()
  })
  
  
################################################################################
# Select Data #
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
# Outputs #
################################################################################
  
  # Creates colour scheme
  cols <- c("Primary" = "red4", "Final" = "red", "Useful" = "orange", "GDP.i" = "black")
  
  # Plots indexed data
  output$sumdash_i <- renderPlotly({
    p_i <- ggplot2::ggplot(data = selected_data_i()) + 
      
      ggplot2::geom_line(mapping = aes(x = Year, y = EX.i, color = Stage)) +
      
      ggplot2::scale_colour_manual(values = cols) +
      
      # ggplot2::scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 10)) +
      
      ggplot2::xlab("") +
      ggplot2::ylab("Indexed Value [-]") +
      MKHthemes::xy_theme()
    
    p_i_plotly <- plotly::ggplotly(p_i, height = 400, tooltip = c("Year","Stage", "EX.i", "GDP.i")) %>%
      
      plotly::layout(showlegend = as.logical(input$legend),
                     legend = list(itemclick = TRUE, 
                                   itemdoubleclick = TRUE)) %>%
      
      move_legend_annotation_no_facet(y = 0.925, mar = 80)
    
  })
  
  # EX by product stacked area plots
  output$sumdash_prod <- renderPlotly({
    
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
                                      height = 400,
                                      tooltip = c("Year", "EX", "E.product")) %>%
      
      plotly::layout(showlegend = as.logical(input$legend),
                     legend = list(font = list(size = 9),
                                   tracegroupgap = 0.3,
                                   itemwidth = 15 #,
                                   # itemclick = TRUE, 
                                   # itemdoubleclick = TRUE
                                   )) %>%
      
      move_legend_annotation_no_facet(y = 0.925, mar = 80) # %>%
      
      # plotly::highlight(on = "plotly_hover",
      #                   off = c("plotly_doubleclick", "plotly_deselect", "plotly_relayout"),
      #                   persistent = getOption("persistent", FALSE),
      #                   dynamic = FALSE,
      #                   color = NULL,
      #                   selectize = FALSE,
      #                   defaultValues = NULL,
      #                   opacityDim = 1, #getOption("opacityDim", 0.2),
      #                   selected = attrs_selected(),
      #                   debounce = 0
      # )
    
  })
  
  # EX by flow or sector stacked area plots
  output$sumdash_flowsec <- renderPlotly({
  
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
                                         height = 400,
                                         tooltip = c("Year", "EX", "Flow.Sector")) %>%
      
      plotly::layout(showlegend = as.logical(input$legend),
                     legend = list(font = list(size = 9),
                                   tracegroupgap = 0.3,
                                   itemwidth = 15#,
                                   # itemclick = TRUE, 
                                   # itemdoubleclick = TRUE
                                   )) %>%
      
      move_legend_annotation_no_facet(y = 0.925, mar = 80) # %>%
    
      # plotly::highlight(on = "plotly_hover",
      #                   off = c("plotly_doubleclick", "plotly_deselect", "plotly_relayout"),
      #                   # persistent = FALSE,
      #                   persistent = getOption("persistent", FALSE),
      #                   dynamic = FALSE,
      #                   color = NULL,
      #                   selectize = FALSE,
      #                   defaultValues = NULL,
      #                   opacityDim = 1, # getOption("opacityDim", 0.2),
      #                   selected = attrs_selected(),
      #                   debounce = 0
      #                   )
    
    
  })
  
}


