
# load libraries ----
library(ggplot2)
library(plotly)
library(DT)
library(tidyverse)
library(crosstalk)

scatter <- function(x, var1, var2) {
  p <- ggplot(x, aes_string(var1, var2)) +
    geom_point(aes(col = species)) +
    scale_color_brewer(palette = "Set2", guide = FALSE)
  ggplotly(p) %>%
    layout(dragmode = "lasso") %>%
    highlight(on = "plotly_selected")
}


# create SharedData ----
penguins <- read_csv("https://uwmadison.box.com/shared/static/ijh7iipc9ect1jf0z8qa2n3j7dgem1gh.csv") %>%
  mutate(id = row_number())
penguins <- SharedData$new(penguins, key = ~id)

# plotly scatterplots
p1 <- scatter(penguins, "bill_length_mm", "bill_depth_mm")
p2 <- scatter(penguins, "flipper_length_mm", "body_mass_g")

# create final output
dt <- datatable(penguins)
bscols(p1, p2, dt, widths = c(6, 6, 12))
