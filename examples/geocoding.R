# pak::pak(c("tidyverse", "tidygeocoder", "arcgisgeocode"))

library(tidyverse)
library(tidygeocoder)
library(arcgisgeocode)

# create a dataframe with addresses
some_addresses <- tibble::tribble(
  ~name,                  ~addr,
  "White House",          "1600 Pennsylvania Ave NW, Washington, DC",
  "Transamerica Pyramid", "600 Montgomery St, San Francisco, CA 94111",
  "Willis Tower",         "233 S Wacker Dr, Chicago, IL 60606"
)

# geocode the addresses
some_addresses_geocoded <- some_addresses |>
  geocode(addr, method = "osm", lat = latitude, long = longitude)

glimpse(some_addresses_geocoded)

some_addresses_full_results <- some_addresses |>
  geocode(addr, full_results = TRUE)

glimpse(some_addresses_full_results)

fav_restaurants <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1Omrtv-8ADgIjfnf_YU6L-wpIijlRGeMHZJfx-ibHyJQ/edit?usp=sharing"
)

fav_restaurants_geocoded <- fav_restaurants |>
  geocode(`Address or Neighborhood`, method = "osm", lat = latitude, long = longitude)

egis_geocoder_url <- "https://egis.baltimorecity.gov/egis/rest/services/Locator/EGISCompositeLocator/GeocodeServer"

egis_geocoder <- geocode_server(
  url = egis_geocoder_url
)

fav_resturants_egis_geocoded <- geocode_addresses(
  single_line = fav_restaurants$`Address or Neighborhood`,
  geocoder = egis_geocoder
)

fav_restaurants_addr_xwalk <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1Omrtv-8ADgIjfnf_YU6L-wpIijlRGeMHZJfx-ibHyJQ/edit?usp=sharing",
  sheet = "Crosswalk"
)


