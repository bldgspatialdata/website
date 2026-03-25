library(sf)
library(tigris)
library(tidyverse)

options("tigris_use_cache" = TRUE)

us_states <- states()

storms_sf <- storms |>
  sf::st_as_sf(
    coords = c("long", "lat"),
    crs = 4326
  )

storms_sf <- st_transform(storms_sf, crs = st_crs(us_states))

us_highways <- primary_roads()

maryland <- filter(us_states, NAME == "Maryland")

lower48 <- filter(
  us_states,
  !(NAME %in%
      c(
        "United States Virgin Islands",
        "Commonwealth of the Northern Mariana Islands",
        "Guam",
        "Alaska",
        "American Samoa",
        "Puerto Rico",
        "Hawaii"
      ))
)

storms_sf |>
  st_filter(us_states) |>
  plot()

st_join(maryland, us_highways)

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


c(-76.712838, 39.253383)

sfg <- st_point(c(-76.712838, 39.253383))

sfg

st_as_sfc(sfg)

sfc <- st_as_sfc(list(sfg), crs = 4326)

st_as_sf(sfc) |>
  mutate(
    name = "The point I made in GES668"
  )

sfc |>
  mutate(
    name = "This won't work"
  )

st_filter(
  us_states,
  maryland,
  .predicate = st_disjoint
)

us_highways |>
  mapview::mapview()

st_filter(
  us_highways,
  maryland
)

us_highways_joined <- st_join(
  us_highways,
  us_states,
  largest = TRUE
)

mapview::mapview(us_highways_joined, zcol = "NAME")

storms_sf_states <- st_join(
  storms_sf,
  us_states
)

storms_sf_states |>
  ggplot() +
  geom_sf(aes(color = NAME), size = 0.02)

st_filter(
  storms_sf,
  us_states
) |>
  ggplot() +
  geom_sf(size = 0.02)

st_is_within_distance(us_highways, maryland, dist = units::set_units(100, "mi"))

maryland_with_dist <- st_join(
  us_highways,
  maryland,
  join = st_is_within_distance,
  dist = units::set_units(100, "mi")
)

maryland_with_dist |>
  mapview::mapview(
    zcol = "NAME"
  )

st_centroid(
  us_states
) |>
  mapview::mapview()

st_point_on_surface(
  us_states
) |>
  mapview::mapview()

storms_buffered <- storms_sf |>
  slice_head(
    n = 1000
  ) |>
  st_buffer(
    dist = units::set_units(100, "km")
  )

storms_buffered |>
  mapview::mapview()

