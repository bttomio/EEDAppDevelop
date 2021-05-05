# This module contains the database documentations

# Render r markdown database documentation file to HMTL
rmarkdown::render(input = "datadoctest.Rmd",
                  output_dir = "www"
                  # output_format = "HTML",
                  # output_options = "mathjax = https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS_CHTML.js"
)

# Establishes UI module function
documentationUI <- function(id) {
  ns <- NS(id)
  
  htmltools::tags$iframe(src = "datadoctest.html",
                         width = '100%',
                         height = 1000,
                         style = "border:none;")
  
  # <script type='text/x-mathjax-config'>
  #   MathJax.Hub.Config({
  #     showProcessingMessages: true,
  #     jax: ['input/TeX', 'output/CommonHTML'],
  #   });
  # </script >
  
}