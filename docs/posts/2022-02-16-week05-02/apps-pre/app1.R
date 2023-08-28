
# load libraries ----
library(ggplot2)
library(plotly)
library(DT)
library(leaflet)
library(crosstalk)

download.file("https://github.com/emilyriederer/demo-crosstalk/blob/master/data/stations.rds?raw=true", "stations.rds")
download.file("https://github.com/emilyriederer/demo-crosstalk/blob/master/data/trips_apr.rds?raw=true", "trips_apr.rds")
stations <- readRDS("stations.rds")
trips <- readRDS("trips_apr.rds")

# create SharedData ----
trips_ct <- SharedData$new(trips, key = ~station_id, group = "loc")
trips_sub_ct <- SharedData$new(trips[,c("station_name", "station_id", "pct_change")], 
                               key = ~station_id, group = "loc")
stations_ct <- SharedData$new(stations, key = ~station_id, group = "loc")

# create individual widgets ----

# map
lf <- leaflet(stations_ct) %>% addTiles() %>% addMarkers()

# table
dt <- datatable(trips_sub_ct,
                fillContainer = TRUE,
                rownames = FALSE,
                colnames = c("Station", "ID", "% Change"),
                options = list(autowidth = TRUE))

# plot
gg <- ggplot(trips_ct) +
  aes(
    x = prop_wend_2019, 
    y = pct_change,
    col = pct_change,
    name = station_name) +
  geom_point() +
  guides(col = FALSE) +
  labs(
    x = "% Apr 2019 Trips on Weekend",
    y = "% Change Apr 2020 vs 2019"
  ) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)
gg_ly <- 
  ggplotly(gg, tooltip = "name") %>%
  layout(dragmode = "lasso") %>%
  highlight(on = "plotly_selected",
            off = "plotly_doubleclick")

# create final output ----
bscols(gg_ly, lf, dt, widths = c(6, 6, 12))
