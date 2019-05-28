library(readr)
library(magrittr)
library(tidyverse)

deaths_by_hazard <- read_csv("deaths_by_hazard.csv")
df <-
  deaths_by_hazard %>%
  group_by(GeoAreaCode, GeoAreaName, TimePeriod) %>%
  mutate(
    worst = Hazard[which.max(Number_of_deaths)],
    sumofalldeaths = sum(Number_of_deaths)
  ) %>%
  filter(worst == Hazard) %>%
  select(- worst)
write.csv(df, "worsthazard.csv")
