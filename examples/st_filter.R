library(tidyverse)
library(sf)
library(tigris)
library(units)
library(mapview) # Used interactively

options("tigris_use_cache" = TRUE)

storms_sf <- storms |>
  st_as_sf(
    coords = c("long", "lat"),
    crs = 4326
  )

us_states <- states()

maryland <- filter(us_states, NAME == "Maryland")

maryland_smaller <- st_buffer(
  maryland,
  dist = units::set_units(-5000, "m"))

md_counties <- counties("MD")

st_filter(
  us_states,
  maryland,
  .predicate = st_touches
)

storms_observations <- storms_sf |>
  arrange(year, month, day, hour) |>
  group_by(year, name) |>
  summarise(
    avg_wind = mean(wind)
  )

storms_tracks <- st_cast(
  storms_observations,
  to = "LINESTRING"
)
