library(plotly)
library(DT)
library(crosstalk)
library(shiny)
library(tidyverse)
library(lubridate)
library(nycflights13)

reset_selection <- function(x, brush) {
  brushedPoints(x, brush, allRows = TRUE)$selected_
}

ui <- fluidPage(
  plotOutput("h1", brush = brushOpts("plot_brush", direction = "x")),
  plotOutput("h2", brush = brushOpts("plot_brush", direction = "x")),
  plotOutput("h3", brush = brushOpts("plot_brush", direction = "x")),
  dataTableOutput("table")
)

server <- function(input, output) {
  selected <- reactiveVal(rep(TRUE, nrow(movies)))
  
  observeEvent(
    input$plot_brush,
    selected(reset_selection(flights, input$plot_brush))
  )
  
  output$h1 <- renderPlot({
    counts <- movies %>% count(year)
    sub_counts <- movies %>%
      filter(selected()) %>%
      count(year)
    
    ggplot(counts, aes(year, n)) +
      geom_bar(stat = "identity", width = 1) +
      geom_bar(data = sub_counts, fill = "red", stat = "identity", width = 1) +
      scale_y_continuous(expand = c(0, 0))
  })
  
  output$scatterplot <- renderPlot({
    movies %>%
      mutate(selected_ = selected()) %>%
      ggplot() +
      geom_point(aes(
        Rotten_Tomatoes_Rating, IMDB_Rating, 
        alpha = as.numeric(selected_),
        col = selected_
      )) +
      scale_color_manual(values = c("black", "red"), guide = FALSE) +
      scale_alpha(range = c(0.05, 0.6))
  })
  
  output$table <- renderDataTable({
    movies %>%
      filter(selected()) %>%
      select(Title, Major_Genre, Worldwide_Gross, Director, Release_Date)
  })
}

shinyApp(ui, server)