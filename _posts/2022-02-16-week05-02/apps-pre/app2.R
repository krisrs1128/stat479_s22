library(shiny)
library(crosstalk)
library(d3scatter)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  fluidRow(
    column(4, d3scatterOutput("scatter1")),
    column(4, d3scatterOutput("scatter2")),
    column(4, plotlyOutput("by_species"))
  )
)

server <- function(input, output, session) {
  shared_iris <- SharedData$new(iris)
  
  output$scatter1 <- renderD3scatter({
    d3scatter(shared_iris, ~Petal.Length, ~Petal.Width, ~Species, width = "100%")
  })
  
  output$scatter2 <- renderD3scatter({
    d3scatter(shared_iris, ~Sepal.Length, ~Sepal.Width, ~Species, width = "100%")
  })
  
  df <- reactive(shared_iris$data(withSelection = TRUE))
  output$by_species <- renderPlotly({
    p <- ggplot(df(), aes(Species, fill = crosstalk::selection_factor(selected_))) +
      geom_bar(stat = "count") +
      crosstalk::scale_fill_selection("#444444", "#9999FF")
    ggplotly(p)
  })
}

shinyApp(ui, server)