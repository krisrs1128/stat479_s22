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
 fires %>%
   mutate(selected = year == 2018) %>%
   plot_fires()
plot_fires <- function(df) {
  p <- ggplot(df, aes(day_of_year, reorder(Counties, Latitude), size = AcresBurned)) +
    geom_point(data = df %>% filter(!selected), col = "#d3d3d3") +
    geom_point(data = df %>% filter(selected), aes(text = Name), col = "orange") +
    scale_y_discrete(limits = county_order)
  ggplotly(p)
}

#' The actual application
ui <- fluidPage(
  # fill in code here
)

server <- function(input, output) {
  # fill in code here...
}

#shinyApp(ui, server)