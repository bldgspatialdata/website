# Load packages
library(tidyverse)
library(sf)
library(tidycensus)

options(tigris_use_cache = TRUE)

update_geom_defaults(ggplot2::GeomSf, list(color = "white", alpha = 0.85, linewidth = 0.05))

state_name <- "Maryland"
county_name <- c("Baltimore city", "Baltimore County")

states <- tigris::states()

counties <- tigris::counties(state = state_name)

tracts <- tigris::tracts(state = state_name, county = county_name)

block_groups <- tigris::block_groups(state = state_name, county = county_name)

blocks <- tigris::blocks(state = state_name, county = county_name)

ggplot() +
  geom_sf(data = states, fill = "slateblue") +
  geom_sf(data = counties, fill = "slateblue2") +
  geom_sf(data = tracts, fill = "slateblue4") +
  coord_sf(
    xlim = st_bbox(tracts)[c("xmin", "xmax")] + c(-0.35, 0.35),
    ylim = st_bbox(tracts)[c("ymin", "ymax")] + c(-0.35, 0.35)
  )

acs_vars <- load_variables(2022, "acs5")

edu_birth_place_src <- get_acs(
  geography = "tract",
  state = "Maryland",
  table = "B06009",
  geometry = TRUE
)

edu_birth_place <- edu_birth_place_src |>
  left_join(
    acs_vars |>
      select(name, label),
    by = c("variable" = "name")
  ) |>
  # NOTE: Some new-ish tidyverse functions unexpectedly drop the sf class
  separate_wider_delim(
    cols = label,
    delim = "!!",
    names_sep = "_",
    too_few = "align_start"
  ) |>
  st_as_sf() |>
  mutate(
    estimate_total = first(estimate),
    .by = GEOID
  ) |>
  mutate(
    pct_estimate = estimate / estimate_total
  )

grad_students <- edu_birth_place |>
  filter(
    variable == "B06009_006"
  )

grad_students |>
  st_filter(tracts) |>
  ggplot() +
  geom_sf(aes(fill = pct_estimate)) +
  scale_fill_continuous(labels = scales::label_percent()) +
  theme_void()

# Combining ACS data with OSM data
library(osmdata)

# https://www.openstreetmap.org/relation/12867319
q <- opq_osm_id("relation/12867319")

campus_osm <- osmdata_sf(q)

campus <- campus_osm$osm_multipolygons

# Demonstration of dm package

library(dm)

tigris_dm <- dm(states, counties, tracts, block_groups)

tigris_dm |>
  dm_add_pk(states, GEOID) |>
  dm_add_pk(counties, GEOID) |>
  dm_add_pk(tracts, GEOID) |>
  dm_add_pk(block_groups, GEOID) |>
  dm_add_fk(states, GEOID, counties, STATEFP) |>
  dm_add_fk(states, GEOID, tracts, STATEFP) |>
  dm_add_fk(states, GEOID, block_groups, STATEFP) |>
  dm_add_fk(counties, COUNTYFP, tracts, COUNTYFP) |>
  dm_add_fk(counties, COUNTYFP, block_groups, COUNTYFP) |>
  dm_add_fk(tracts, TRACTCE, block_groups, TRACTCE) |>
  dm_draw(
    view_type = "all"
  )
