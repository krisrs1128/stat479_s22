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
    scale_color_brewer(palette = "Set2") +
    coord_fixed() +
    theme_void()
}

# Makes overlaid histograms, one filtered by T/F vector of selected_ values
overlay_histogram <- function(df, selected_) {
  sub_df <- filter(df, selected_)
  ggplot(df, aes(trunc_price, fill = room_type)) +
    geom_histogram(alpha = 0.3, binwidth = 25) +
    geom_histogram(data = sub_df, binwidth = 25) +
    scale_y_continuous(expand = c(0, 0, 0.1, 0)) +
    scale_fill_brewer(palette = "Set2", guide = "none")
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