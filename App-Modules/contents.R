contentsUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    box(#title = "Table of contents",
      width = 12,
      tags$h1("Table of Contents"),
      tags$hr(),
      tags$h2("Visualisations"),
      tags$h3("Geographical Coverage"),
      tags$p("A World map detailing countries in this database 
                           which serve as 'exemplar' countries."),
      tags$h3("IEA WEEB"),
      tags$h3("Final-to-useful allocations"),
      tags$h3("Final-to-useful efficiencies"),
      tags$h3("Energy Conversion Chain"),
      tags$h3("Efficiency by GDP"),
      tags$h3("Energy-GDP Coupling"),
      tags$h2("Database Documentation", a("Search for it", 
                                          href = "http://www.google.com")),
      tags$h2("Citation"),
      
    ))
  
}