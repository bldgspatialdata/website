# Install packages if needed
# install.packages(c("skimr", "spData", "tidyverse", "sf", "gt"))

# Load packages
library(spData)
library(tidyverse)
library(sf)
library(gt)

# Set a default ggplot2 theme
theme_set(theme_minimal())

## General summaries ----

# Join us_states sf object to us_states_df with extra information
us_states <- left_join(
  us_states,
  us_states_df,
  by = c("NAME" = "state")
)

# Take a look at column names and types with glimpse or summary
glimpse(us_states)

summary(us_states)

# Take a look at more with skim
skimr::skim(us_states)

# Take a look at the "structure" of a data frame
str(us_states)

# `str()` highlights how data frames are a type of list
# Learn more: https://adv-r.hadley.nz/vectors-chap.html#tibble
str(list("A" = 1, "B" = list(1, 2, 3)))

## Tidying data ----

# First step to exploring your data is to make it tidy
# us_states is not "tidy" because important data including the year of the Census
# data is included in the column name

# Learn more: https://r4ds.hadley.nz/data-tidy.html#sec-billboard

# Pivot longer using a `tidyselect::ends_with()` to select columns that end with
# _10 or _15
us_states |>
  pivot_longer(
    cols = ends_with(c("_10", "_15")),
    names_to = "variable"
  ) |>
  mutate(
    # Extract the year from the new variable column
    year = paste0(20, str_sub(variable, end = 2)),
    # Coerce the character year into an integer
    year = as.integer(year)
  )

# You don't *have* to use tidyselect helpers
# This approach also works
us_states |>
  pivot_longer(
    cols = c(
      "total_pop_10", "total_pop_15",
      "median_income_10", "median_income_15",
      "poverty_level_10", "poverty_level_15"
    ),
    names_to = "variable"
  ) |>
  mutate(
    year = paste0(20, str_extract(variable, "[:digit:]+")),
    year = as.integer(year)
  )

# This final approach takes full advantage of the names_pattern and names_transform
# parameters to get the same result with less code
us_states |>
  pivot_longer(
    cols = ends_with(c("10", "15")),
    names_to = c("variable", "year"),
    # name_pattern take a regex pattern similar to `stringr::str_extract()`
    # Learn more about regex (regular expressions): https://r4ds.hadley.nz/regexps
    names_pattern = "([a-z|_]+)([0-9]+)",
    # name_transform can take a list of functions that are applied to each name
    names_transform = list(
      "variable" = \(x) {
        str_replace_all(x, "_", " ")
      },
      "year" = \(x) {
        as.integer(paste0(20, x))
      }
      # The \(x) {...} syntax a short way of writing "anonymous" functions (called
      # anonymous because they don't have names)
      # Learn more: https://adv-r.hadley.nz/functionals.html?q=anony#purrr-shortcuts
    )
  )

# We can also put the script in a function so we can use it anytime we need it
pivot_us_states_longer <- function(data) {
  data |>
    pivot_longer(
      cols = ends_with(c("10", "15")),
      names_to = c("variable", "year"),
      names_pattern = "([a-z|_]+)([0-9]+)",
      names_transform = list(
        "variable" = \(x) {
          str_replace_all(x, "_", " ")
        },
        "year" = \(x) {
          as.integer(paste0(20, x))
        }
      )
    )
}

# Review the vignette on pivoting for more examples: https://tidyr.tidyverse.org/articles/pivot.html

## Asking questions about our data ----

# Now that we have tidy data we can get back to the questions we came up with in
# class! Here are those questions:

# One variable at a time

# What is the max and min for all of the different demographic variables?
# How does median income vary by state?
# Which state has the highest pop in 2015?
# Which region is the poorest?
# Which region has greater land area?

# Two variables in combination

# How did poverty level change between 2010 and 2015?
# How has poverty changed over time in each state?

# Broader questions

# How does the Census Bureau measure poverty?
# How is change in poverty level related to policy changes?

## One variable at a time ----

us_states_minmax <- us_states |>
  # Make sure to drop your geometry if you don't need it
  st_drop_geometry() |>
  # Now reuse the pivot code we wrote earlier
  pivot_us_states_longer() |>
  # Group by variable
  group_by(variable) |>
  # And (remembering that min and max are *summary* functions) get the min and
  # max value for each
  # Review the description of summary functions for reference:
  # https://r4ds.hadley.nz/functions#summary-functions
  summarise(
    value_max = max(value),
    value_min = min(value)
  ) |>
  mutate(
    value_range = value_max - value_min
  )

# Here we can use the gt package (and gt function) to create a simple table
# based on the summary data frame we just made
us_states_minmax |>
  gt::gt()

# Using group_by + summarise + mutate isn't the only way to see the range of
# your data. ggplot helps us visualize the overall range for our variables
# without creating a summary data frame in advance
us_states |>
  st_drop_geometry() |>
  pivot_us_states_longer() |>
  ggplot() +
  geom_jitter(
    # Mapping color (or fill) to region may reveal new patterns
    aes(x = variable, y = value, color = REGION),
    size = 2, alpha = 0.7
  ) +
  # facet_wrap with "free" scales creates a panel for each different variable
  # this is only possible because we converted our data into a long format
  facet_wrap(~variable, scales = "free") +
  # Using `scales::label_number()` converts the y axis labels into a more
  # readable format
  scale_y_continuous(label = scales::label_number())

# First try it with the original wide format data
us_states |>
  ggplot() +
  geom_col(
    aes(
      # We can use reorder to sort NAME by median_income_15
      # This means the plot will also easily show min and max values
      y = reorder(NAME, median_income_15),
      x = median_income_15,
      fill = REGION
    ),
    alpha = 0.75
  ) +
  scale_y_continuous(label = scales::label_number())

# Next try it with the long format data
us_states |>
  pivot_us_states_longer() |>
  # Instead of selecting a column, we filter to the rows with observations of
  # the median income variable
  filter(
    variable == "median_income",
    year == 2015
  ) |>
  ggplot() +
  geom_col(
    aes(
      # We no longer map x to a specific variable but instead to a more general value column
      x = value,
      y = reorder(NAME, value),
      fill = REGION
    ),
    alpha = 0.75
  )

# Because this code is more generalizable it is easy to wrap in a function
plot_us_states_variable <- function(data,
                                    demographic = "median_income",
                                    year = 2015,
                                    alpha = 0.75) {
  data |>
    pivot_us_states_longer() |>
    filter(variable == demographic, year == year) |>
    ggplot() +
    geom_col(aes(value, reorder(NAME, value), fill = REGION), alpha = alpha)
}

# Now we can reuse the code to visualize total population This plot can help
# answer the question: what state has the smallest population? the largest?
plot_us_states_variable(us_states, "total_pop")

# And we can do take the same look at poverty level
plot_us_states_variable(us_states, "poverty_level")

## Two variables in combination ----

# The next question we discussed is: how has poverty changed over time in each
# state?

us_states_longer <- us_states |>
  st_drop_geometry() |>
  # Wide format data makes it easier to compare one variable to another
  mutate(
    # Using the same naming convention (putting "_15" at the end of each
    # variable name) let us reuse the same pivot function
    change_median_income_15 = median_income_15 - median_income_10,
    change_total_pop_15 = total_pop_15 - total_pop_10,
    change_poverty_level_15 = poverty_level_15 - poverty_level_10
  ) |>
  pivot_us_states_longer()

# mapping name to x and value to y makes it difficult to read the labels
us_states_longer |>
  filter(variable == "change_median_income") |>
  ggplot() +
  geom_col(aes(x = reorder(NAME, value), y = value)) +
  labs(
    x = "State",
    y = "Change in median income (2010-2015)"
  )

# Swapping the order makes an easier to read (and interpret) visual
us_states_longer |>
  filter(variable == "change_median_income") |>
  ggplot() +
  geom_col(aes(x = value, y = reorder(NAME, value))) +
  labs(
    x = "Change in median income (2010-2015)",
    y = "State"
  )

# Let's try to make a table and pivot back into wide format data
us_states_2015_changes <- us_states_longer |>
  pivot_wider(
    names_from = "variable"
  ) |>
  # We need to filter to 2015 because the change variables only exist for that
  # year
  filter(year == 2015) |>
  select(
    "NAME", "REGION", starts_with("change_")
  )

# gt supports grouped data nicely for making a basic table
# Learn more about gt: https://gt.rstudio.com/articles/gt.html
us_states_2015_changes |>
  group_by(REGION) |>
  gt()

# gt also has a variety of functions for formatting data
us_states_2015_changes |>
  group_by(REGION) |>
  gt() |>
  cols_label_with(
    fn = \(x){
      x |>
        str_remove("change_") |>
        str_replace("_", " ") |>
        str_to_sentence()
    }
  ) |>
  fmt_number(
    starts_with("change"),
    decimals = 0
  ) |>
  fmt_currency(
    contains("income"),
    decimals = 0
  ) |>
  tab_header(
    # labels and titles are a key part of documenting your exploratory analysis
    # as you go
    title = "Changes in U.S. states key demographics, 2010-2015"
  )
