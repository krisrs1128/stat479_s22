library(shiny)
library(shinyWidgets)
library(thematic)
library(bslib)
library(tidyverse)
library(plotly)
library(DT)
theme_set(theme_minimal())

# download and find a good ordering of the counties
fires <- read_csv("https://uwmadison.box.com/shared/static/k5vvekf1bhh9e16qb9s66owygc70t7dm.csv")
#fires <- read_csv("fires@5.csv")
county_order <- fires %>%
  group_by(Counties) %>%
  summarise(latitude = median(Latitude)) %>% 
  arrange(latitude) %>%
  pull(Counties)

#' Function for making static plots
plot_fires <- function(df) {
  p <- ggplot(df, aes(day_of_year, 
                      reorder(Counties, Latitude), 
                      size = AcresBurned,
                      )) +
    geom_point(data = df %>% filter(!selected), col = "#d3d3d3") +
    geom_point(data = df %>% filter(selected), aes(text=Name), col = "orange") +
    scale_y_discrete(limits = county_order) +
    ggtitle("Fires by counties and days of a year") +
    xlab("Day of Year") + ylab("County")
  ggplotly(p, tooltip = "text")
}

max_year = max(fires$year)
min_year = min(fires$year)

dark = bs_theme(bootswatch = "darkly", version = 4)
light = bs_theme(version = 4)

#' The actual application
ui <- fluidPage(
  theme = light,
  titlePanel("Ex4.3"),
  p("Group 12: Zhihao Lyu, Jingyun Jia, Danah Dykstra, Harshit Tummala"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      switchInput("dark_mode", "Dark mode", labelWidth = "80px"),
      sliderInput(
        "year_range", "Select years",
        value = c(max_year-1, max_year),
        min=min_year, max=max_year, sep="",
        animate = animationOptions(loop = TRUE),
        dragRange = TRUE),
      selectInput("cty_select", "Select a county",
                  choices = county_order, multiple = TRUE)
    ),
    mainPanel = mainPanel(
      plotlyOutput('plot', heigh="40rem"),
      tags$hr(class="m-2"),
      plotlyOutput('plot2')
    ),
  ),
  h3("Table of fire details"),
  DTOutput('table'),
  h3("Reactive Graph"),
  img(src="https://alexhaoge.xyz/images/479ex4.3reactive.png")
)

server <- function(input, output, session) {
  observe(session$setCurrentTheme(
    if (isTRUE(input$dark_mode)) dark else light
  ))
  
  df_select = reactive({
    tmp = fires %>%
      mutate(selected = between(
        year, input$year_range[1], input$year_range[2]))
    if(!is.null(input$cty_select))
      tmp = tmp %>%
        mutate(selected = selected & (Counties %in% input$cty_select))
    tmp
  })
  
  output$plot = renderPlotly({
    plot_fires(df_select())
  })
  
  output$plot2 = renderPlotly({
    g2 <- df_select() %>%
      ggplot(aes(duration_days, AcresBurned)) +
      geom_point(data = . %>% filter(!selected), col = "#d3d3d3") +
      geom_point(data = . %>% filter(selected), aes(text=Name), col = "orange") +
      xlim(0, NA) + xlab("Duration (Days)") + ylab("Acres Burned") +
      ggtitle("Acres Burned v.s. Duration (Extreme Value Removed)")
    ggplotly(g2, tooltip = 'text')
  })
  
  output$table = renderDT(
    df_select() %>% 
      select(!c("duration_days", "start_date", "day_of_year")),
    options = list(pageLength=5)
  )
}

thematic_shiny()
shinyApp(ui, server)
