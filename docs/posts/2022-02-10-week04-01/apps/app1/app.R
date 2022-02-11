library(shiny)

ui <- fluidPage(
  titlePanel("Hello!"),
  textInput("text_input", "Enter something")  # first arg is ID, second is label
)

server <- function(input, output) {}

app <- shinyApp(ui, server)
