# copied from _common.R in r4ds

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  # cache = TRUE,
  fig.retina = 2,
  fig.width = 6,
  fig.asp = 2 / 3,
  fig.show = "hold"
)

options(
  dplyr.print_min = 6,
  dplyr.print_max = 6,
  pillar.max_footer_lines = 2,
  pillar.min_chars = 15,
  stringr.view_n = 6,
  # Temporarily deactivate cli output for quarto
  cli.num_colors = 0,
  cli.hyperlink = FALSE,
  pillar.bold = TRUE,
  width = 77 # 80 - 3 for #> comment
)

ggplot2::theme_set(ggplot2::theme_gray(12))


course_schedule_tbl <- tibble::tribble(
  ~Week, ~Date, ~Topic, ~Slides, ~Exercise,
  "[1](weeks/week_01.html)", "2024-08-30", "Getting started with spatial data using sf and the tidyverse", "[📖](slides/spatial-data.html)", "[📝](exercises/exercise_01.html)",
  "[2](weeks/week_02.html)", "2024-09-06", "Visualizing spatial data with ggplot2", "[📖](slides/data-visualization.html)", "[📝](exercises/exercise_02.html)",
  "[3](weeks/week_03.html)", "2024-09-13", "Transforming data with dplyr", "[📖](slides/data-transformation.html)", "[📝](exercises/exercise_03.html)",
  "[4](weeks/week_04.html)", "2024-09-20", "Transforming spatial data attributes", "[📖](slides/spatial-data-attributes.html)", "[📝](exercises/exercise_04.html)",
  "[5](weeks/week_05.html)", "2024-09-27", "Applying spatial transformations and geometric operations using sf", "[📖](slides/feature-geometry.html)", "[📝](exercises/exercise_05.html)",
  "[6](weeks/week_06.html)", "2024-10-04", "Tidying and joining spatial data", "[📖](slides/tidy-data.html)", "[📝](exercises/exercise_06.html)",
  "[7](weeks/week_07.html)", "2024-10-11", "Building functions in R and literate programming with Quarto", "[📖](slides/functions.html)", "[📝](exercises/exercise_07.html)",
  "[8](weeks/week_08.html)", "2024-10-18", "Developing an exploratory data analysis with sf and the tidyverse", "[📖](slides/exploratory-analysis.html)", NA,
  "[9](weeks/week_09.html)", "2024-10-25", "Editing OpenStreetMap and exploring OpenStreetMap data with the osmdata package", "[📖](slides/open-street-map.html)", NA,
  "[10](weeks/week_10.html)", "2024-11-01", "Exploring American Community Survey data with the tidycensus package", NA, NA,
  "[11](weeks/week_11.html)", "2024-11-08", "Reading and writing spatial data files and services", "[📖](slides/spatial-data-io.html)", NA,
  "[12](weeks/week_12.html)", "2024-11-15", "Creating and managing spatial metadata", "[📖](slides/metadata.html)", NA,
  "[13](weeks/week_13.html)", "2024-11-22", "Project check-in meetings (no class)", NA, NA,
  "[14](weeks/week_14.html)", "2024-11-29", "Review and work session", NA, NA,
  "[15](weeks/week_15.html)", "2024-12-06", "Final project presentations", NA, NA,
  NA, "2024-12-14", "Due: Final project repository", NA, NA
)

week_num <- function(schedule, day = lubridate::today()) {
  schedule_dates <- as.Date(schedule[["Date"]])
  sum(schedule_dates < day) + 1
}
