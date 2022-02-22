library(shiny)
library(tidyverse)

rentals <- read_csv("https://uwmadison.box.com/shared/static/zi72ugnpku714rbqo2og9tv2yib5xped.csv") %>%
  mutate(trunc_price = pmin(price, 1e3))

# Makes a scatterplot with points more or less transparent depending on vector
# of T/F selected_ values
scatterplot <- function(df, selected_) {
  df %>%
    mutate(selected = selected_) %>%
    ggplot() +
    geom_point(
      aes(
        longitude, latitude, col = room_type, 
        alpha = as.numeric(selected),
        size = as.numeric(selected)
      )
    ) +
    scale_alpha(range = c(0.1, .5), guide = FALSE) +
    scale_size(range = c(0.1, .9), guide = FALSE) +
    coord_fixed() +
    theme_void()
}

# Makes overlaid histograms, one filtered by T/F vector of selected_ values
overlay_histogram <- function(df, selected_) {
  sub_df <- filter(df, selected_)
  ggplot(df) +
    geom_histogram(aes(trunc_price), fill = "#d3d3d3", binwidth = 10) +
    geom_histogram(data = sub_df, aes(trunc_price), binwidth = 10) +
    scale_y_continuous(expand = c(0, 0, 0.1, 0))
}

ui <- fluidPage(
  h3("NYC Airbnb Rentals"),
  # ... fill in code here ...
)

server <- function(input, output) {
  ## define reactive value
  observeEvent(
    # ... fill in code here ...
  )
}

shinyApp(ui, server)