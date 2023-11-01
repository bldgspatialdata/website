# Load libraries
library(tigris)
library(tidycensus)
library(osmdata)
library(tidyverse)

# Set option for tigris
options(tigris_use_cache = TRUE)

# Get American Community Survey tract level data on population by race/ethnicity
# Make sure to include a summary variable with total population
baltcity_pop <- get_acs(
  geography = ____,
  variables = ____,
  summary_var = ____,
  cache_table = TRUE,
  year = 2021,
  state = "MD",
  county = "Baltimore city",
  geometry = TRUE,
  survey = "acs5"
)

# If you are unsure what variables to use, use the `load_variables()` function
# to get a data frame with all of the variables
acs_vars <- load_variables(year = ____, dataset = ____)

# Use `tidycensus::as_dot_density()` to convert the geometry of your data into a
# dot density format and scale the values based on the values_per_dot parameter
baltimore_dots <- as_dot_density(
  input_data = ____,
  value = ____,
  values_per_dot = 100,
  group = "variable"
)

# Use tigris to get a basemap layer with the tract geometry
baltcity_tracts <- tracts(
  state = ____,
  county = ____
)

# Use geom_sf to make a map using the `baltimore_dots` data that is color-coded
# by race/ethnicity
ggplot() +
  geom_sf(data = ____) +
  geom_sf(data = ____, aes(color = ____)) +
  theme_void()

# Now try to improve the map:
#
# - Change the size and transparency of the dots to address over-plotting
# - Change the color scale to a color-blind friendly palette (I recommend `scale_color_brewer()`)
# - Change the values of the attribute mapped to color to reflect the categories used by the U.S. Census Bureau

ggplot() +
  geom_sf(data = ____) +
  geom_sf(data = ____, aes(color = ____)) +
  theme_void()
