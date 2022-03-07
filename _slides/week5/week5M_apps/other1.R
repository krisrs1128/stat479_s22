library(shiny)
library(tidyverse)
library(plotly)
library(ggplot2)
theme_set(theme_minimal())


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
generate_data <- function(y) {
    fires %>%
        mutate(selected = year == y)
}


current_data <- function(df){
    fires %>%
        filter(
            Name %in% df$name
        )
}
Name<-unique(fires$Name)
ui <- fluidPage(
    titlePanel("Wildfire in Califonia"),
    sliderInput("year","year",min=2013,max=2019,value=2013,step=1),
    plotlyOutput("plot_fires"),
    selectInput("name", "Fire Names", Name, multiple = TRUE),
    plotOutput("histogram")
)

server <- function(input, output) {
    output$plot_fires <- renderPlotly({
        
        generate_data(input$year) %>%
            plot_fires()
        
    })
    output$histogram <- renderPlot({
        ggplot(current_data(input)) +
            geom_bar(aes(x=Name,y=duration_days,fill=Name),stat = 'identity')
    })
}

shinyApp(ui, server)