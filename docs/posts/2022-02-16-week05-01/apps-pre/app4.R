# devtools::install_github("ropensci/plotly")
library(plotly)
library(crosstalk) 
library(shiny)

ui <- fluidPage(
  plotOutput("scatter1", brush = "plot_brush"),
  plotOutput("scatter2")
)

server <- function(input, output) {
  current_data <- reactiveVal(iris %>% mutate(selected_ = TRUE))
  
  observeEvent(
    input$plot_brush, {
    current_data(brushedPoints(iris, input$plot_brush, allRows = TRUE))
    }
  )
    
  output$scatter1 <- renderPlot({
    ggplot(current_data()) +
      geom_point(aes(Sepal.Length, Sepal.Width, col = as.factor(selected_)))
  })
  
  output$scatter2 <- renderPlot({
    ggplot(current_data()) +
      geom_point(aes(Petal.Length, Petal.Width, col = as.factor(selected_)))
  })
}

shinyApp(ui, server)