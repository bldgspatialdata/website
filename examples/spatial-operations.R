library(sf)
library(tigris)
library(tidyverse)

us_states <- states()

storms_sf <- storms |>
  sf::st_as_sf(
    coords = c("long", "lat"),
    crs = 4326
  )

storms_sf <- st_transform(storms_sf, crs = st_crs(us_states))

storms_sf |>
  st_filter(us_states) |>
  plot()

storms_sf |>
  st_join(us_states) |>
  ggplot() +
  geom_sf(aes(color = DIVISION))

storms_sf |>
  st_is(type = "POLYGON")

us_states |>
  st_is(type = "MULTIPOLYGON")

us_states |>
  filter(
    NAME == "Hawaii"
  ) |>
  ggplot() +
  geom_sf(fill = "green") +
  theme_void()

coords <- data.frame(
  long = c(-75, -78, -80, NA),
  lat = c(30, 40, 34, NA)
)

coords_sf <- coords |>
  st_as_sf(
    coords = c("long", "lat"),
    crs = 4326,
    na.fail = FALSE
  )

coords_sf |>
  st_make_valid()

st_intersects(storms_sf, us_states)


maryland <- us_states |>
  filter(NAME == "Maryland")

storms_sf |>
  st_filter(
    us_states,
    .predicate = st_disjoint
  ) |>
  plot()

storms_sf |>
  st_filter(
    maryland,
    dist = 1000000,
    .predicate = st_is_within_distance
  ) |>
  mapview::mapview()

florida <- us_states |>
  filter(NAME == "Florida")

storms_sf |>
  st_filter(
    florida,
    .predicate = st_covered_by
  ) |>
  mapview::mapview()
