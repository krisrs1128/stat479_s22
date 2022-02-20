library(shiny)
library(crosstalk)
library(d3scatter)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  fluidRow(
    column(4, plotlyOutput("scatter1")),
    column(4, plotlyOutput("scatter2")),
    column(4, plotlyOutput("by_species"))
  )
)

server <- function(input, output, session) {
  shared_iris <- SharedData$new(iris)
  df <- reactive(shared_iris$data(withSelection = TRUE))
  
  output$scatter1 <- renderPlotly({
    p <- ggplot(df()) +
      geom_point(aes(Sepal.Length, Sepal.Width, col = Species, alpha = selection_factor(selected_)))
    
    ggplotly(p) %>%
      layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  })
  
  output$scatter2 <- renderPlotly({
    p <- ggplot(df()) +
      geom_point(aes(Petal.Length, Petal.Width, col = Species))
    ggplotly(p) %>%
      layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    
  })
  
  
  output$by_species <- renderPlotly({
    p <- ggplot(df(), aes(Species, fill = crosstalk::selection_factor(selected_))) +
      geom_bar(stat = "count") +
      crosstalk::scale_fill_selection("#444444", "#9999FF")
    ggplotly(p)
  })
}

shinyApp(ui, server)