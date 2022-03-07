library(shiny)
library(tidyverse)
library(plotly)
theme_set(theme_minimal())

# download and find a good ordering of the counties
fires <- read_csv("https://uwmadison.box.com/shared/static/k5vvekf1bhh9e16qb9s66owygc70t7dm.csv")
county_order <- fires %>%
  group_by(Counties) %>%
  summarise(latitude = median(Latitude)) %>% 
  arrange(latitude) %>%
  pull(Counties)

#' Function for making static plots
#' example usage:

plot_fires <- function(df) {
  p <- ggplot(df, aes(day_of_year, reorder(Counties, Latitude), size = AcresBurned)) +
    geom_point(data = df %>% filter(!selected), col = "#d3d3d3") +
    geom_point(data = df %>% filter(selected), aes(text = Name), col = "orange") +
    scale_y_discrete(limits = county_order)
  ggplotly(p)
}

histogram_fun <- function(df) {
  ggplot(df) +
    geom_histogram(data = df %>% filter(!selected), aes(log(AcresBurned)), fill = "#d3d3d3", bins=100) +
    geom_histogram(data = df %>% filter(selected), aes(log(AcresBurned)), fill="orange", bins = 100) 
}

#' The actual application
ui <- fluidPage(
  sliderInput("year", "Year", min = min(fires$year), max = max(fires$year), c(2014, 2018), sep = ""),
  plotlyOutput("year_plot"),
  plotOutput("histogram")
)

server <- function(input, output) {
  fires_subset <- reactive ({
    fires %>%
      mutate(selected = (
        (year >= input$year[1]) &
          (year <= input$year[2])
      ))
  })
  
  #outplut plot with selected subset
  output$year_plot <- renderPlotly({
    plot_fires(fires_subset())
  })
  output$histogram <- renderPlot(
    histogram_fun(fires_subset()))
}

app <- shinyApp(ui, server)
app