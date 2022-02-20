library(tidyverse)
library(shiny)
mtcars <- add_rownames(mtcars)

reset_selection <- function(x, brush) {
  brushedPoints(x, brush, allRows = TRUE)$selected_
}

make_scatter <- function(x, selected_) {
  x$selected_ <- selected_
  ggplot(x) +
    geom_point(aes(mpg, hp, alpha = as.numeric(selected_))) +
    scale_alpha(range = c(0.1, 1))
}

ui <- fluidPage(
  plotOutput("plot", brush = "plot_brush"),
  dataTableOutput("table")
)

server <- function(input, output) {
  selected <- reactiveVal(rep(TRUE, nrow(mtcars)))
  observeEvent(
    input$plot_brush,
    selected(reset_selection(mtcars, input$plot_brush))
  )
  
  output$plot <- renderPlot(make_scatter(mtcars, selected()))
  output$table <- renderDataTable(filter(mtcars, selected()))
}

shinyApp(ui, server)