intro_reboundUI <- function(id) {
  ns <- NS(id)
        fluidRow(
          box(width = 12,
              tags$h1("Introduction"),
              tags$hr(),
              tags$p("Matt Heun's rebound module")
              )
        )
        }