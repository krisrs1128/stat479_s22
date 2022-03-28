
# pollution problem
library(lubridate)
pollution <- read_csv("pollution.csv") %>%
  mutate(
    datetime = date,
    hour = hour(date),
    date = date(date)
  )
  
pollution %>%
  select(date, hour, pollution, temp, wnd_spd) %>%
  rename(wndspd = wnd_spd) %>%
  mutate(
    pollution = log(1 + pollution),
    wndspd = log(1 + wndspd)
  )  %>%
  pivot_wider(names_from = "hour", values_from = c("pollution", "temp", "wndspd")) %>%
  write_csv("pollution_wide.csv")

# crashes problem
# originally downloaded from https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if/data
crashes <- read_csv("Traffic_Crashes_-_Crashes.csv")
crashes %>%
  mutate(
    CRASH_DATE = as_datetime(CRASH_DATE, format = "%m/%d/%Y %H:%M:%S %p"),
    hour = str_pad(hour(CRASH_DATE), 2, pad = "0"),
    day_of_week = wday(CRASH_DATE),
    week = round_date(CRASH_DATE, "week")
  ) %>%
  count(week, day_of_week, hour) %>%
  arrange(day_of_week, hour) %>%
  write_csv("chicago_crashes.csv")