library(ggplot2)
library(plotly)
library(DT)
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

# basic ggplots
gg <- ggplot(trips_ct) +
  geom_point(aes(prop_wend_2019, pct_change, name = station_name)) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)

gg2 <- ggplot(trips_ct) +
  geom_bar(aes(year_2020, reorder(station_name, year_2020)), stat = "identity")

# extend to ggplotly
gg2_ly <- ggplotly(gg2) %>%
  layout(dragmode = "select", direction = "v") %>%
  highlight(on = "plotly_selected")

gg_ly <- ggplotly(gg) %>%
  layout(dragmode = "select") %>%
  highlight(on = "plotly_selected")

# create final output
dt <- datatable(trips_sub_ct)
bscols(gg_ly, gg2_ly, dt, widths = c(6, 6, 12))
