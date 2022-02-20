library(tidyverse)
library(shiny)

penguins <- read_csv("https://uwmadison.box.com/shared/static/ijh7iipc9ect1jf0z8qa2n3j7dgem1gh.csv")
islands <- unique(penguins$island)
species <- unique(penguins$species)

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
  current_data <- reactive({
    penguins %>%
      filter(
        island %in% input$island,
        species %in% input$species
      )
  })  
  
  output$scatterplot <- renderPlot({
    ggplot(current_data()) +
      geom_point(aes(.data[[input$var1]], .data[[input$var2]]))
  })
  
  output$histogram1 <- renderPlot({
    ggplot(current_data()) +
      geom_histogram(aes(.data[[input$var1]]))
  })
  
  output$histogram2 <- renderPlot({
    ggplot(current_data()) +
      geom_histogram(aes(.data[[input$var2]]))
  })
  
}

shinyApp(ui, server)