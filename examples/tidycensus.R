# Load packages
library(tidyverse)
library(sf)
library(tidycensus)

options(tigris_use_cache = TRUE)

theme_set(theme_void())

update_geom_defaults(
  ggplot2::GeomSf,
  list(color = "white", alpha = 0.85, linewidth = 0.05)
)

state_name <- "Maryland"
county_name <- c("Baltimore city", "Baltimore County")

states <- tigris::states()

counties <- tigris::counties(state = state_name)

tracts <- tigris::tracts(state = state_name, county = county_name)

block_groups <- tigris::block_groups(state = state_name, county = county_name)

blocks <- tigris::blocks(state = state_name, county = county_name)

zctas <- tigris::zctas()

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
  state = state_name,
  table = "B06009",
  # summary_var = "B06009_001",
  cache_table = TRUE,
  geometry = TRUE
)

edu_birth_place_wide <- get_acs(
  geography = "tract",
  state = state_name,
  variables = c(
    "Total_" = "B06009_001",
    "Less than HS_" = "B06009_002",
    "HS or equivalent_" = "B06009_003",
    "Some college or associates_" = "B06009_004",
    "Bachelors_" = "B06009_005",
    "Graduate school_" = "B06009_006"
  ),
  cache_table = TRUE,
  geometry = TRUE,
  output = "wide"
)


edu_birth_place <- edu_birth_place_src |>
  left_join(
    acs_vars |>
      select(name, label),
    by = c("variable" = "name")
  ) |>
  separate_wider_delim(
    cols = label,
    delim = "!!",
    names_sep = "_",
    too_few = "align_start"
  ) |>
  st_as_sf() |>
  mutate(
    estimate_total = first(estimate),
    moe_total = first(moe),
    .by = GEOID
  ) |>
  mutate(
    pct_estimate = estimate / estimate_total,
    pct_moe = moe_prop(estimate, estimate_total, moe, moe_total),
    .after = moe
  )

grad_students <- edu_birth_place |>
  filter(
    variable == "B06009_006"
  )

grad_students |>
  st_filter(tracts) |>
  filter(estimate > 0) |>
  ggplot() +
  geom_sf(data = tracts) +
  geom_sf(aes(fill = pct_estimate)) +
  scale_fill_continuous(labels = scales::label_percent()) +
  theme_void()

var1 <- get_acs(
  geography = "county",
  state = state_name,
  table = "B06009",
  cache_table = TRUE
)

var2 <- get_acs(
  geography = "county",
  state = state_name,
  table = "B23022",
  cache_table = TRUE
)

# etc.

select_tables <- c(
  "B06009",
  "B23022",
  "B05001",
  "B06009",
  "B20004",
  "B08012",
  "B08133",
  "B25091",
  "B22002",
  "B07009"
)

table_data <- get_acs(
  geography = "county",
  state = state_name,
  table = select_tables,
  cache_table = TRUE
)

letter_fn <- function(x, ...) {
  # here is the body
}

map(
  letters,
  \(x) {
    paste0(str_to_title(x), " is a letter")
  }
)

select_tables <- set_names(
  select_tables,
  select_tables
)

acs_data_list <- map(
  select_tables,
  \(x) {
    get_acs(
      geography = "county",
      state = state_name,
      table = x,
      cache_table = TRUE
    )
  }
)

acs_data_bind_rows <- bind_rows(acs_data_list)

acs_data_list_rbind <- list_rbind(acs_data_list, names_to = "table")

# Combining ACS data with OSM data
library(osmdata)

# https://www.openstreetmap.org/relation/12867319
q <- opq_osm_id("relation/12867319")

campus_osm <- osmdata_sf(q)

campus <- campus_osm$osm_multipolygons |>
  st_transform(crs = st_crs(grad_students))

grad_students |>
  st_filter(campus) |>
  ggplot() +
  geom_sf(aes(fill = pct_estimate)) +
  geom_sf(data = campus, fill = NA, linewidth = 1.5)

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

library(arcgislayers)

parks <- arc_read(
  "https://bcgisdata.baltimorecountymd.gov/arcgis/rest/services/Recreation/ParkBoundaries/MapServer/1"
)

park_area <- parks |>
  st_buffer(dist = units::set_units(0.25, "mi")) |>
  st_union() |>
  st_as_sf() |>
  mutate(
    name = "Baltimore County Park Areas"
  ) |>
  st_transform(crs = st_crs(grad_students))

blocks <- blocks |>
  st_transform(crs = st_crs(grad_students))

grad_student_park_area_interp <- interpolate_pw(
  from = grad_students,
  to = park_area,
  extensive = TRUE,
  weights = blocks,
  crs = 3857,
  weight_column = "POP20"
)

grad_student_park_area_interp |>
  select(!contains("moe"))
