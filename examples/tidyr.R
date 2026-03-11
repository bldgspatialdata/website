library(tidyverse)

# Pivoting ----
# https://tidyr.tidyverse.org/articles/pivot.html

students <- tribble(
  ~name,    ~degree,
  "Billy",  "BA",
  "Suzy",   "MFA",
  "Lionel", "MFA",
  "Jenny",  "BA"
)

classroom <- tribble(
  ~name,    ~quiz1, ~quiz2, ~test1, ~test2, ~test3,
  "Billy",  NA,     "D",    "C",  100, "80/100",
  "Suzy",   "F",    NA,     NA, 70, "95/100",
  "Lionel", "B",    "C",    "B", 100, "110/100",
  "Jenny",  "A",    "A",    "B", 65, "85/100"
)

students |>
  left_join(
    classroom,
    by = join_by(name)
  )

classroom |>
  separate_wider_delim(
    cols = test3,
    delim = "/",
    names = c("test3", "test3_score_possible")
  )

classroom_long <- classroom |>
  separate_wider_delim(
    cols = test3,
    delim = "/",
    names = c("test3", "test_score_possible")
  ) |>
  mutate(
    test3 = as.numeric(test3)
  ) |>
  pivot_longer(
    cols = c(test2, test3),
    names_to = "assessment"
  )

classroom_long |>
  pivot_wider(
    id_cols = "name",
    names_from = "assessment"
  ) |>
  mutate(
    total_test_score = sum(c(test2, test3)),
    diff_score = test3 - test2
  ) |>
  rename(
    student_name = name
  ) |>
  pivot_longer(
    cols = c("test2", "test3", "total_test_score", "diff_score"),
    names_to = "indicator"
  )

# Rectangling ----
# https://tidyr.tidyverse.org/articles/rectangle.html
# pak::pak("repurrrsive")

repurrrsive::gmaps_cities

repurrrsive::gmaps_cities |>
  unnest_wider(json)

repurrrsive::gmaps_cities |>
  unnest_wider(json) |>
  unnest_longer(results)

repurrrsive::gmaps_cities |>
  unnest_wider(json) |>
  unnest_longer(results) |>
  unnest_wider(results)

repurrrsive::gmaps_cities |>
  unnest_wider(json) |>
  unnest_longer(results) |>
  unnest_wider(results) |>
  unnest_wider(geometry)

repurrrsive::gmaps_cities |>
  unnest_wider(json) |>
  unnest_longer(results) |>
  unnest_wider(results) |>
  unnest_wider(geometry) |>
  unnest_wider(location)

# Nested data ----
# https://tidyr.tidyverse.org/articles/nest.html

df2 <- tribble(
  ~g, ~x, ~y,
  1,  1,  2,
  2,  4,  6,
  2,  5,  7,
  3, 10,  NA
)

df2 |> nest(data = c(x, y))

df2 |> group_by(g) |> nest()

storms_sf_nested <- storms |>
  st_as_sf(
    coords = c("long", "lat"),
    crs = 4326
  ) |>
  nest_by(year) |>
  ungroup()

storms_sf_nested |>
  slice_sample(n = 5) |>
  pmap(
    \(year, data) {
      ggplot(data = data) +
        geom_sf(aes(color = wind)) +
        labs(
          title = str_glue("Storm observations in {year}")
        )
    }
  )
