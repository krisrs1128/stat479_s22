library(shiny)
library(tidyverse)
library(plotly)
theme_set(theme_minimal())

fires <- read_csv("https://uwmadison.box.com/shared/static/k5vvekf1bhh9e16qb9s66owygc70t7dm.csv")
county_order <- fires %>%
  group_by(Counties) %>%
  summarise(latitude = median(Latitude)) %>% 
  arrange(latitude) %>%
  pull(Counties)
fires <- fires %>%
  mutate(Counties = factor(Counties, levels = county_order))

highlight_fires <- function(df, year_range) {
  mutate(df, selected = (year == year_range))
}

plot_fires <- function(df) {
  p <- ggplot(df, aes(day_of_year, Counties, size = AcresBurned)) +
    geom_point(data = df %>% filter(!selected), col = "#d3d3d3") +
    geom_point(data = df %>% filter(selected), aes(text = Name), col = "orange") +
    scale_y_discrete(limits = county_order)
  ggplotly(p, tooltip = "Name")
}

ui <- fluidPage(
  titlePanel("California Wildfires"),
  sliderInput("year", "Years", 2018, min = 2013, max = 2019, sep = "", step = 1),
  plotlyOutput("fires", height = 600)
)

server <- function(input, output) {
  fires_data <- reactive(highlight_fires(fires, input$year))
  output$fires <- renderPlotly(plot_fires(fires_data()))
}

shinyApp(ui, server)