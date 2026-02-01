library(sf)
library(arcgislayers)
library(tidyverse)
library(tigris)

cb_counties <- counties(c("WV", "VA", "MD", "DE", "PA", "NY"))

loads <- arc_read(
  "https://gis.chesapeakebay.net/ags/rest/services/WIP/Loads_2018_01_07_20/MapServer/0"
)

loads |>
  glimpse()

fips_codes <- fips_codes |>
  mutate(
    GEOID = paste0(state_code, county_code)
  )

cb_counties_joined <- cb_counties |>
  left_join(
    fips_codes,
    by = join_by(GEOID)
  ) |>
  mutate(
    NAME = str_to_upper(NAME),
    state = str_to_upper(state)
  ) |>
  st_drop_geometry() |>
  distinct(
    NAME,
    state,
    .keep_all = TRUE
  )

loads |>
  left_join(
    cb_counties_joined,
    by = c(
      "ST" = "state",
      "CNTYNAME" = "NAME"
    )
  )


loads |>
  left_join(
    cb_counties_joined,
    by = join_by(
      ST == state,
      CNTYNAME == NAME
    )
  )
