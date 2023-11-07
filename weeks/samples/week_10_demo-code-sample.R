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

# If you are unsure what variables to use, use the `load_variables()` function
# to get a data frame with all of the variables

acs_vars <- load_variables(year = 2021, dataset = "acs5")

# You can visualize the estimate using fill and split variables into different
# panels of the plot

ggplot() +
  geom_sf(data = baltcity_pop, aes(fill = estimate)) +
  facet_wrap(~variable) +
  theme_void()


# Use `tidycensus::as_dot_density()` to convert the geometry of your data into a
# dot density format and scale the values based on the values_per_dot parameter

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

# Use geom_sf to make a map using the `baltimore_dots` data that is color-coded
# by race/ethnicity

ggplot() +
  geom_sf(data = baltcity_tracts) +
  geom_sf(
    data = baltcity_pop_dots,
    aes(color = variable)
  ) +
  theme_void()

# Now try to improve the map:
#
# - Change the size and transparency of the dots to address over-plotting
# - Change the color scale to a color-blind friendly palette (I recommend `scale_color_brewer()`)
# - Change the values of the attribute mapped to color to reflect the categories used by the U.S. Census Bureau

ggplot() +
  geom_sf(data = baltcity_tracts, fill = NA) +
  geom_sf(
    data = baltcity_pop_dots,
    aes(color = variable),
    alpha = 0.5,
    size = 0.35
  ) +
  scale_color_brewer(palette = "Accent", direction = -1) +
  theme_void() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    color = "Race/ethnicity"
  )
