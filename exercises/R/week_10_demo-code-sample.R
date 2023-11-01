# WIP

# Load libraries
library(tigris)
library(tidycensus)
library(osmdata)
library(tidyverse)

# Set option for tigris
options(tigris_use_cache = TRUE)

# Get American Community Survey data on population by race/ethnicity
baltcity_pop <- get_acs(
  geography = "tract",
  variables = c("B03002_003", "B03002_004", "B03002_012"),
  summary_var = "B03002_001",
  cache_table = TRUE,
  year = 2021,
  state = "MD",
  county = "Baltimore city",
  geometry = TRUE,
  survey = "acs5"
)

baltcity_pop_dots <- as_dot_density(
  input_data = baltcity_pop,
  value = "estimate",
  values_per_dot = 100,
  group = "variable"
)

baltcity_tracts <- tracts(
  state = "MD",
  county = "Baltimore city"
)

# Use geom_sf to make a map using the `baltimore_dots` data that is color-coded by race/ethnicity
ggplot() +
  geom_sf(data = baltcity_tracts) +
  geom_sf(
    data = baltcity_pop_dots,
    aes(color = variable)
    ) +
  theme_void()

# Get park and forested areas for the city
# baltcity_natural_area <-  getbb("Baltimore City, Maryland") |>
#   opq() |>
#   add_osm_features(
#     features = list(
#       "natural" = "wood",
#       "leisure" = "park",
#       "landuse" = "forest"
#     )
#   )  |>
#   osmdata_sf()

