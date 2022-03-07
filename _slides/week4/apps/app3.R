library(shiny)

ui <- fluidPage(
  h3("Part (c)"),
  sliderInput(
    "range", 
    "Slider range", 
    c(40, 60), 
    min = 0, 
    max = 100
  )
)
server <- function(input, output) {}

shinyApp(ui, server)
