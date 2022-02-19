library(plotly)
library(DT)
library(crosstalk)
library(shiny)
library(tidyverse)
library(lubridate)
library(nycflights13)


movies <- read_csv("https://raw.githubusercontent.com/krisrs1128/stat479_s22/main/_posts/2022-02-10-week04-03/apps/data/movies.csv") %>%
  mutate(
    id = row_number(),
    date = as_date(Release_Date, format = "%b %d %Y"),
    year = year(date),
    Major_Genre = fct_explicit_na(Major_Genre),
    MPAA_Rating = fct_explicit_na(MPAA_Rating),
  )


reset_selection <- function(x, brush) {
  brushedPoints(x, brush, allRows = TRUE)$selected_
}

ui <- fluidPage(
  fluidRow(
    column(6, plotOutput("histogram", brush = brushOpts("plot_brush", direction = "x"))),
    column(6, plotOutput("scatterplot", brush = "plot_brush"))
  ),
  dataTableOutput("table")
)

server <- function(input, output) {
  selected <- reactiveVal(rep(T, nrow(movies)))
  
  observeEvent(
    input$plot_brush,
    selected(reset_selection(movies, input$plot_brush))
  )
  
  output$histogram <- renderPlot({
    counts <- movies %>% count(year)
    sub_counts <- movies %>%
      filter(selected()) %>%
      count(year)
    
    ggplot(counts, aes(year, n)) +
      geom_bar(stat = "identity", fill = "#d3d3d3", width = 1) +
      geom_bar(data = sub_counts, stat = "identity", width = 1) +
      scale_y_continuous(expand = c(0, 0))
    })
  
  output$scatterplot <- renderPlot({
    movies %>%
      mutate(selected_ = selected()) %>%
      ggplot() +
      geom_point(aes(
        Rotten_Tomatoes_Rating, IMDB_Rating, 
        col = selected_
      )) +
      scale_color_manual(values = c("#d3d3d3", "black"), guide = "none")
  })
  
  output$table <- renderDataTable({
    movies %>%
      filter(selected()) %>%
      select(Title, Major_Genre, Worldwide_Gross, Director, Release_Date)
  })
}

shinyApp(ui, server)