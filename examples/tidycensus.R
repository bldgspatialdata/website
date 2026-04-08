# Introduction to the tidycensus package for accessing the U.S. Census Bureau API
# See <https://www.census.gov/data/developers/data-sets.html> for available APIs
# See <https://data.census.gov/> to access Census data via the website
# Or check out <https://censusreporter.org/> for ACS data

# Install packages required for this example
# pak::pak(c(
#   "tidyverse",
#   "sf",
#   "tidycensus",
#   "tigris",
#   "osmdata",
#   "marquee",
#   "arcgislayers"
# ))

# Load packages
library(tidyverse)
library(sf)
library(tidycensus)

options(tigris_use_cache = TRUE)

# Set global default theme for plots
theme_set(theme_void())
theme_update(
  theme(
    plot.caption.position = "plot"
  )
)

# Set global defaults for fixed aesthetics in `geom_sf`
update_geom_defaults(
  ggplot2::GeomSf,
  list(color = "white", alpha = 0.75, linewidth = 0.03)
)

# Set geography and year for script
state_name <- "Maryland"
county_name <- c("Baltimore city", "Baltimore County")
year <- 2023

# Get Census spatial data at multiple levels
states <- tigris::states(year = year)

counties <- tigris::counties(state = state_name, year = year)

tracts <- tigris::tracts(state = state_name, county = county_name, year = year)

block_groups <- tigris::block_groups(
  state = state_name,
  county = county_name,
  year = year
)

blocks <- tigris::blocks(state = state_name, county = county_name, year = year)

# Look at Census boundary data
glimpse(tracts)

glimpse(block_groups)

# Note that blocks has a GEOID20 column - but no GEOID column
glimpse(blocks)

# Compare nested admin boundaries
ggplot() +
  geom_sf(data = states, fill = "wheat") +
  geom_sf(data = counties, fill = "slateblue2") +
  geom_sf(data = tracts, fill = "slateblue4") +
  coord_sf(
    xlim = st_bbox(tracts)[c("xmin", "xmax")] + c(-0.25, 0.25),
    ylim = st_bbox(tracts)[c("ymin", "ymax")] + c(-0.25, 0.25)
  )

mapview::mapview(
  list(
    states,
    counties,
    tracts,
    st_filter(block_groups, tracts[1, ])
  )
)

# Load variable definitions from Census
acs_vars_src <- load_variables(
  year = year,
  dataset = "acs5"
)

# Practice: find the variable for households with no internet access

# Load data for a single variable and summary variable
edu_birth_place_single_var <- get_acs(
  geography = "tract",
  state = state_name,
  variable = "B06009_006",
  summary_var = "B06009_001",
  year = year
)

# Load data for a full table
edu_birth_place_tbl <- get_acs(
  geography = "tract",
  state = state_name,
  table = "B06009",
  year = year,
  # Make sure to cache the results
  cache_table = TRUE,
  # Optionally include geometry (returns an sf object)
  geometry = TRUE
)

# Load data for selected variables
edu_birth_place_wide <- get_acs(
  geography = "tract",
  state = state_name,
  # Rename variables by using a named vector
  variables = c(
    "Total_" = "B06009_001",
    "Less than HS_" = "B06009_002",
    "HS or equivalent_" = "B06009_003",
    "Some college or associates_" = "B06009_004",
    "Bachelors_" = "B06009_005",
    "Graduate school_" = "B06009_006"
  ),
  year = year,
  cache_table = TRUE,
  # output defaults to "tidy"
  output = "wide"
)

# wide format data uses `E` suffix for estimate and `M` suffix for MOE
glimpse(edu_birth_place_wide)

# Format variable definitions
acs_vars <- acs_vars_src |>
  rename(variable = name) |>
  mutate(
    indent = str_count(label, "!!") - 1,
    table_id = str_sub(variable, end = -5)
  ) |>
  mutate(
    line_number = row_number(),
    .by = table_id
  ) |>
  separate_wider_delim(
    label,
    delim = "!!",
    names_sep = "_",
    too_few = "align_end",
    too_many = "drop"
  ) |>
  mutate(
    column_title = label_8,
    .after = variable
  ) |>
  relocate(
    starts_with("label"),
    .after = everything()
  )

# Join formatted variables to estimate data
edu_birth_place <- edu_birth_place_tbl |>
  left_join(
    acs_vars,
    by = join_by(variable)
  ) |>
  # Get the first estimate and MOE for each tract
  mutate(
    estimate_total = first(estimate),
    moe_total = first(moe),
    .by = GEOID
  ) |>
  mutate(
    perc_estimate = estimate / estimate_total,
    # moe_prop is one of a few helper functions for deriving a MOE
    perc_moe = moe_prop(estimate, estimate_total, moe, moe_total),
    .after = moe
  )

# Practice: use get_acs get both county and block group level data for
# no internet access

grad_deg <- edu_birth_place |>
  filter(
    variable == "B06009_006"
  )

grad_deg |>
  st_filter(tracts) |>
  ggplot() +
  geom_sf(aes(fill = perc_estimate)) +
  geom_sf(data = counties[c(3, 5), ], fill = NA, linewidth = 0.3) +
  scale_fill_continuous(
    name = "% w/ grad deg.",
    labels = scales::label_percent()
  )

# Multiple tables are not supported
select_tables <- c(
  "B06009",
  "B23022",
  "B05001"
)

table_data <- get_acs(
  geography = "county",
  state = state_name,
  table = select_tables,
  year = year,
  cache_table = TRUE
)

# `map` allows iterating over a vector
map(
  LETTERS,
  \(x) {
    str_glue("{x} is a letter")
  }
)

# Naming the input vector means the output keeps the names
select_tables <- set_names(
  select_tables,
  nm = select_tables
)

acs_data_list <- map(
  select_tables,
  \(x) {
    get_acs(
      geography = "county",
      state = state_name,
      table = x,
      year = year,
      cache_table = TRUE
    )
  }
)

# acs_data_list is a list of data frames
str(acs_data_list)

# Combine data frame lists by row with purrr::list_rbind
acs_data_list_rbind <- list_rbind(
  acs_data_list,
  names_to = "table"
)

# Combining ACS data with OSM data
library(osmdata)

# https://www.openstreetmap.org/relation/12867319
q <- opq_osm_id("relation/12867319")

campus_osm <- osmdata_sf(q)

campus <- campus_osm$osm_multipolygons |>
  st_transform(crs = st_crs(grad_deg))

grad_deg |>
  # Filter data to area w/in 1.5 miles of UMBC campus
  st_filter(
    st_buffer(campus, dist = units::set_units(1.5, "mi"))
  ) |>
  ggplot() +
  geom_sf(aes(fill = perc_estimate)) +
  geom_sf(data = campus, fill = NA, linewidth = 0.6) +
  scale_fill_viridis_c(
    name = "% of adults w/ grad or professional deg.",
    labels = scales::label_percent()
  ) +
  labs(
    caption = "Source: 2019-23 ACS 5-yr. Est. Table B06009.
    Adults >=25 years old included in table population."
  ) +
  theme(
    legend.title = marquee::element_marquee(
      width = unit(0.2, unit = "npc")
    ),
    plot.caption = marquee::element_marquee(
      width = unit(0.8, unit = "npc"),
      hjust = 0,
      margin = margin(b = 12)
    )
  )

# Interpolating ACS data
library(arcgislayers)

council_districts <- arc_read(
  "https://bcgisdata.baltimorecountymd.gov/arcgis/rest/services/Political/Political/MapServer/31"
)

blocks <- blocks |>
  st_transform(crs = st_crs(grad_students))

grad_student_council_district <- interpolate_pw(
  from = grad_students,
  to = council_districts,
  extensive = TRUE,
  weights = blocks,
  crs = 3857,
  weight_column = "POP20"
)
