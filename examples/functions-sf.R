library(tidyverse)
library(sf)
library(spData)

# Basic example to introduce the elements of functions: name, arguments, body
compare_nums <- function(x, y) {
  abs(y - x)
}

# Get states w/ tigris
us_states <- tigris::states()

# Look at non-spatial states data from spData
us_states_df

# Filter states by name *or* region
filter_states <- function(
    states,
    name = NULL,
    region = NULL,
    ...) {
  # Check if states is a data frame and name and region are not both provided
  stopifnot(
    is.data.frame(states),
    is.null(name) || is.null(region)
  )

  # Return states if no filter arguments provided
  # This pattern is known as an "early return"
  if (is.null(name) && is.null(region)) {
    return(states)
  }

  # Filter by name if available (or region if not)
  # Early returns help avoid nested if-else statements
  if (!is.null(name)) {
    return(dplyr::filter(states, NAME %in% name))
  }

  dplyr::filter(states, REGION %in% region)
}

# filter_states in action
filter_states(us_states, "Maryland")

filter_states(us_states, region = 4)

filter_states(us_states, c("Maryland", "Delaware"))

filter_states(us_states, name = "Maryland", region = 4)

filter_states(us_states, region = c(1, 3))

filter_states(us_states, region = TRUE)

# Combining multiple functions for interactive mapping
mapview_states <- function(data, name = NULL, region = NULL) {
  data |>
    filter_states(name = name, region = region) |>
    mapview::mapview()
}

# Combining multiple functions for static plotting
plot_states <- function(data, name = NULL, region = NULL) {
  data |>
    filter_states(name = name, region = region) |>
    ggplot() +
    geom_sf()
}



