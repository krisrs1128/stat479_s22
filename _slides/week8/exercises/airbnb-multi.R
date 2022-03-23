library(shiny)
library(glue)
library(bslib)
library(tidyverse)


ui <- fluidPage(
  plotOutput("histogram", brush = brushOpts("plot_brush", direction = "x"))
)

# code to set a clean theme
th <- theme_minimal() + 
  theme(
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#F7F7F7"),
    panel.border = element_rect(fill = NA, color = "#0c0c0c", size = 0.6),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.position = "bottom"
  )
theme_set(th)

rentals <- read_csv("https://uwmadison.box.com/shared/static/zi72ugnpku714rbqo2og9tv2yib5xped.csv") %>%
  mutate(trunc_price = pmin(price, 1e3))
rooms <- unique(rentals$room_type)

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
  fluidRow(
    column(6,
      selectInput("room_type", "Room Types", choices = rooms, selected = rooms, multiple = TRUE),
      plotOutput("histogram", brush = brushOpts("plot_brush", direction = "x"), height = 200),
    ),
    column(6, plotOutput("map", brush = "plot_brush", height = 600)),
  ),
  theme = bs_theme(bootswatch = "minty")
)

update_selection <- function(df, plot_brush, room_input) {
  sel1 <- brushedPoints(df, plot_brush, allRows = TRUE)$selected_
  sel2 <- df %>%
    mutate(selected_room = room_type %in% room_input) %>%
    pull(selected_room)
  sel1 & sel2
}

server <- function(input, output) {
  selected <- reactiveVal(rep(TRUE, nrow(rentals)))
  observe({
    current <- update_selection(rentals, input$plot_brush, input$room_type)
    selected(current)
  })
  output$histogram <- renderPlot(overlay_histogram(rentals, selected()))
  output$map <- renderPlot(scatterplot(rentals, selected()))
}

shinyApp(ui, server)