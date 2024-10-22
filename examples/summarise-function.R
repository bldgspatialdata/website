library(tidyverse)
library(sf)

# First, put the summarise code in a function
summarise_minmax <- function(data) {
  data |>
    summarise(
      value_max = max(value),
      value_min = min(value)
    ) |>
    mutate(
      value_range = value_max - value_min
    )
}

# Using functions can help you write more consistent and readable code later on
us_states |>
  st_drop_geometry() |>
  pivot_us_states_longer() |>
  filter(REGION == "South") |>
  group_by(variable, NAME) |>
  summarise_minmax() |>
  ggplot() +
  geom_linerange(
    aes(
      y = NAME,
      xmin = value_min,
      xmax = value_max
    )
  ) +
  scale_x_continuous(labels = scales::label_number()) +
  facet_wrap(~variable) +
  theme_minimal()
