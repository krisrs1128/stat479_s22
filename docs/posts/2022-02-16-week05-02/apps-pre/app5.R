# devtools::install_github("ropensci/plotly")
library(plotly)
library(crosstalk) 
library(shiny)

ui <- fluidPage(
  plotlyOutput("scatter1"),
  plotlyOutput("scatter2")
)

server <- function(input, output) {
  output$scatter1 <- renderPlotly({
    brush <- event_data("plotly_brushing", source = "scatter2")
    iris_ <- iris %>% mutate(selected_ = TRUE)
    if (!is.null(brush)) {
      iris_ <- iris_ %>%
        mutate(
          selected_ = between(Petal.Length, brush$x[1], brush$x[2]) & 
            between(Petal.Width, brush$y[1], brush$y[2])
        )
    }
    
    p <- ggplot(iris_) +
      geom_point(aes(Sepal.Length, Sepal.Width, alpha = selected_)) +
      theme(legend.position = "none")
    ggplotly(p, source = "scatter1") %>%
      layout(dragmode="select") 
  })
  
  output$scatter2 <- renderPlotly({
    brush <- event_data("plotly_brushing", source = "scatter1")
    iris_ <- iris %>% mutate(selected_ = TRUE)
    if (!is.null(brush)) {
      iris_ <- iris_ %>%
        mutate(
          selected_ = between(Sepal.Length, brush$x[1], brush$x[2]) & 
            between(Sepal.Width, brush$y[1], brush$y[2])
        )
    }
    
    p <- ggplot(iris_) +
      geom_point(aes(Petal.Length, Petal.Width, alpha = selected_)) +
      theme(legend.position = "none")
    ggplotly(p, source = "scatter2") %>%
      layout(dragmode="select") 
  })
}

shinyApp(ui, server)