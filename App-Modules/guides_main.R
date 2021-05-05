# Establish guide for main mani-menu
guide_main <- Cicerone$
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
      id = "guide_main",
      mathjax = FALSE)$
  step(
    el = "[data-value='outline']",
    title = "Outline",
    description = "Click on the Outline tab to view a general description of the 
                  app.",
    is_id = FALSE
  )$
  step(
    el = "[data-value='pfu_menu']",
    title = "PFU Database",
    description = "Click on the PFU Database tab to explore the 
                  Primary-Final-Useful database.",
    is_id = FALSE
  )$
  step(
    el = "[data-value='ee_menu']",
    title = "Energy-Economy",
    description = "Click on the Energy-Economy dashboard tab to explore 
                  energy-economy macroeconomic indicators.",
    is_id = FALSE
  )$
  step(
    el = "[data-value='rebound']",
    title = "Rebound",
    description = "Click on the Rebound tab to explore cutting-edge 
                  energy-economy rebound visualisations associated with 
                  the paper.......",
    is_id = FALSE
  )$
  step(
    el = "[data-value='relatedresources']",
    title = "Related resources",
    description = "Click on the rleated resources tab to view resources related 
    to energyg-economy analysis.",
    is_id = FALSE
  )$
  step(
    el = "[data-value='citation']",
    title = "Citation",
    description = "Click on the citation tab to view details of the people behind
                  the energy-economy decoupling app, and citation information.",
    is_id = FALSE
  )