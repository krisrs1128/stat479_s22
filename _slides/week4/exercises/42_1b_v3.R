library(tidyverse)
library(shiny)

penguins <- read_csv("https://uwmadison.box.com/shared/static/ijh7iipc9ect1jf0z8qa2n3j7dgem1gh.csv")
islands <- unique(penguins$island)
species <- unique(penguins$species)

### helper functions
filtered_samples <- function(df, islands_, species_) {
  filter(df, island %in% islands_, species %in% species_)
}

scatterplot <- function(df, v1, v2) {
  ggplot(df) + geom_point(aes(.data[[v1]], .data[[v2]]))
}

histogram <- function(df, v) {
  ggplot(df) + geom_histogram(aes(.data[[v]]))
}

### the app
ui <- fluidPage(
  titlePanel("Penguins Plot"),
  selectInput("species", "Species", species, multiple = TRUE),
  selectInput("island", "Island", islands, multiple = TRUE),
  selectInput("var1", "First Variable", colnames(penguins)[3:6]),
  selectInput("var2", "Second Variable", colnames(penguins)[3:6]),
  plotOutput("scatterplot"),
  plotOutput("histogram1"),
  plotOutput("histogram2"),
)

server <- function(input, output) {
  current_data <- reactive(filtered_samples(penguins, input$island, input$species))  
  output$scatterplot <- renderPlot(scatterplot(current_data(), input$var1, input$var2))
  output$histogram1 <- renderPlot(histogram(current_data(), input$var1))
  output$histogram2 <- renderPlot(histogram(current_data(), input$var2))
}

shinyApp(ui, server)