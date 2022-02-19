library(plotly)
library(DT)
library(crosstalk)
library(shiny)
library(tidyverse)
library(lubridate)

movies <- read_csv("https://raw.githubusercontent.com/krisrs1128/stat479_s22/main/_posts/2022-02-10-week04-03/apps/data/movies.csv") %>%
  mutate(
    id = row_number(),
    date = as_date(Release_Date, format = "%b %d %Y"),
    year = year(date),
    Major_Genre = fct_explicit_na(Major_Genre),
    MPAA_Rating = fct_explicit_na(MPAA_Rating),
  )

genres <- pull(movies, Major_Genre) %>%
  unique() %>%
  na.omit()

reset_selection <- function(x, brush) {
  brushedPoints(x, brush, allRows = TRUE)$selected_
}

ui <- fluidPage(
  fluidRow(
    column(6, plotOutput("histogram", brush = brushOpts("plot_brush", direction = "x"))),
    column(6, plotOutput("scatterplot"))
  ),
  dataTableOutput("table")
)

server <- function(input, output) {
  selected <- reactiveVal(rep(TRUE, nrow(movies)))
  
  observeEvent(
    input$plot_brush,
    selected(reset_selection(movies, input$plot_brush))
  )
  
  output$histogram <- renderPlot({
    movies %>%
      count(year) %>%
      ggplot() +
      geom_bar(aes(year, n), stat = "identity", width = 1) +
      scale_y_continuous(expand = c(0, 0))
    })
  
  output$scatterplot <- renderPlot({
    movies %>%
      mutate(selected_ = selected()) %>%
      ggplot() +
      geom_point(aes(Rotten_Tomatoes_Rating, IMDB_Rating, alpha = as.numeric(selected_))) +
      scale_alpha(range = c(0.05, 0.6))
  })
  
  output$table <- renderDataTable({
    movies %>%
      filter(selected()) %>%
      select(Title, Major_Genre, Worldwide_Gross, Director, Release_Date)
  })
}

shinyApp(ui, server)