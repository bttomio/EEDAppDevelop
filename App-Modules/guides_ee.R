# Establish guide for Energy-Economy summary dashboard
guide_ee_dash <- Cicerone$
  new(opacity = 0.5,
      padding = 0,
      allow_close = TRUE,
      overlay_click_next = FALSE,
      done_btn_text = "Done",
      close_btn_text = "Close",
      stage_background = "#ffffff",
      next_btn_text = "Next",
      prev_btn_text = "Previous",
      show_btns = TRUE,
      keyboard_control = TRUE,
      id = "guide_ee_dash",
      mathjax = TRUE)$
  step(
    el = "Variables_ee",
    title = "Variables",
    # To create mathjax formatting encase text in $$ $$
    # There are several issues that need solving here:
    # 1) How to increase the width of the box
    # 2) Mathjax 3 does not currently support the line break \\
  description = tags$div(
    
    tags$p("Here are a set of variables linked to each of the dashboard plots:"),
    
    # tags$p("Country - This database currently consists of X countries and 
    #                   X rest-of-world regions. Countries are denoted by their 
    #                   3-letter iso codes"),
    #                         
    # tags$p("Energy quanitifcation - Two energy quantifications are currently
    #                                       supported, energy and exergy"),
    # 
    # tags$p("ECC Stage - Three energy conversion chain stages are available:
    #                           Primary, Final, and Useful."),
    # 
    # tags$p("Efficiency Stages - Three efficiency stages are available for selection:
    #                                   Primary-Useful, Primary-Final, and Final-Useful."),
    # 
    # tags$p("Gross or Net - Gross energy quantifies energy consumed by both
    #                        the energy industry, and non-energy sectors.
    #                        Net energy quantifies the energy consumed only
    #                        by the non-energy industry"),
    
    tags$p("GDP Metric - Three GDP metrics are available:"),

    tags$ul(
            
             tags$li("Real - Real GDP at constant 2017 national prices
                    (in million 2017 USD)"),
             tags$li("Expenditure-side - Expenditure-side real GDP at
                        chained PPPs (in million 2017 USD)"),
             tags$li("Output-side - Output-side real GDP at chained PPPs
                   (in million 2017 USD)")),

    tags$p("CAAGR Period - The CAAGR period represents the number of years
            over which a rolling Compound Annual Average Growth Rate
            is taken, selecting a period of greater than one
            acts to smooth inter-annual variations,
            and therefore represent longer-term trends."),

    tags$p("Production Function - Six production functions are available to
            fit to actual GDP data.....")
  )
  )$
  step(
    el = "sumdash_int",
    title = "Energy Intensity Plots",
    description = "Displayed here is the energy intensity of the selected nations GDP"
  )$
  step(
    el = "sumdash_ds",
    title = "GDP-Energy Decoupling Space",
    description = "Displayed here is the GDP-Energy decoupling space of the selected nation"
  )$
  step(
    el = "sumdash_rs",
    title = "Efficiency-Energy Rebound Space",
    description = "Displayed here is the Efficiency-Energy rebound space of the selected nation"
  )
  
  
  # Cannot refer to id's wrapped in ns() in modules i.e. country below
  # step(
  #   el = "country",
  #   title = "Select a country",
  #   description = "..."
  # )$
  
  #'   isocode: 3-letter isocode
  #'   year: Year
  #'   rgdpe: Expenditure-side real GDP at chained PPPs (in million 2017 USD).
  #'   rgdpo: Output-side real GDP at chained PPPs (in million 2017 USD).
  #'   rgdpna: Real GDP at constant 2017 national prices (in million 2017 USD)
  #'   emp: Number of persons engaged (in millions)
  #'   avh: Average annual hours worked by persons engaged.
  #'   hc: Human capital index, based on years of schooling and returns to education;
  #'       see Human capital in PWT9.
  #'   rnna: Capital stock at constant 2017 national prices (in million 2017 USD).
  #'   rkna: Capital services at constant 2017 national prices (2017 = 1).