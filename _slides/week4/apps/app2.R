library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  h3("Part (b)"),
  prettyToggle(
    "toggle", 
    label_on = "yes!", 
    label_off = "no..", 
    icon_on = icon("thumbs-up"), 
    icon_off = icon("thumbs-down")
  )
)
server <- function(input, output) {}

shinyApp(ui, server)
