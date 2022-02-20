library(shiny)
library(plotly)
library(tidyverse)

ui <- fluidPage(
  plotlyOutput("bars"),
  plotlyOutput("scatter")
)

server <- function(input, output) {
  brushed <- reactive({
    brush <- event_data("plotly_brushed")
    if (is.null(brush)) TRUE else between(diamonds$depth, brush$x[1], brush$x[2])
  })
  
  output$bars <- renderPlotly({
    plot_ly(diamonds, x = ~depth) %>%
      layout(dragmode = "select", selectdirection = "h")
  })
  
  output$scatter <- renderPlotly({
    print(head(diamonds %>% filter(brushed())))
    p <- diamonds %>%
      filter(brushed()) %>%
      ggplot() +
      geom_hex(aes(carat, price), alpha = 0.2)
    ggplotly(p)
  })
}

shinyApp(ui, server)