library(tidyverse)

# Pivoting ----
# https://tidyr.tidyverse.org/articles/pivot.html

students <- tribble(
  ~name    , ~degree ,
  "Billy"  , "BA"    ,
  "Suzy"   , "MFA"   ,
  "Lionel" , "MFA"   ,
  "Jenny"  , "BA"
)

classroom <- tribble(
  ~name    , ~quiz1 , ~quiz2 , ~test1 , ~test2 , ~test3    ,
  "Billy"  , NA     , "D"    , "C"    ,    100 , "80/100"  ,
  "Suzy"   , "F"    , NA     , NA     ,     70 , "95/100"  ,
  "Lionel" , "B"    , "C"    , "B"    ,    100 , "110/100" ,
  "Jenny"  , "A"    , "A"    , "B"    ,     65 , "85/100"
)

students |>
  left_join(
    classroom,
    by = join_by(name)
  )

# One approach to separating multiple values in a single cell
classroom |>
  mutate(
    test3 = stringr::str_extract(
      test3,
      "[:digit:]+(?=/)"
    )
  )

# Using separate_wider_delim is easier
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
    names = c("test3", "test3_score_possible")
  ) |>
  mutate(
    test3 = as.numeric(test3)
  ) |>
  pivot_longer(
    cols = c(test2, test3),
    names_to = "assessment",
    values_to = "score"
  )

# Moving data into a long format allows visualization w/ facet_wrap
classroom_long |>
  ggplot() +
  geom_point(aes(name, value, color = name)) +
  facet_wrap(~assessment)

classroom_long |>
  pivot_wider(
    id_cols = "name",
    names_from = "assessment",
    values_from = "score"
  ) |>
  mutate(
    total_test_score = test2 + test3,
    diff_score = test3 - test2
  ) |>
  rename(
    student_name = name
  ) |>
  pivot_longer(
    cols = c("test2", "test3", "total_test_score", "diff_score"),
    names_to = "indicator"
  ) |>
  ggplot() +
  geom_point(aes(student_name, value)) +
  facet_wrap(
    ~indicator,
    # Try setting scales to free_y
    scales = "free_y"
  )

spData::us_states_df |>
  pivot_longer(
    # cols supports the same tidyselect syntax as dplyr::select
    cols = c(
      ends_with("10"),
      ends_with("15")
    )
  ) |>
  mutate(
    # case_when can be used to derive new values
    # based on the column names
    variable = case_when(
      str_detect(
        name,
        "median_income"
      ) ~ "Median income",
      str_detect(
        name,
        "poverty_level"
      ) ~ "Median income"
    )
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
  unnest_wider(location) |>

  # Final unnested data can be converted to an sf object and visualized
  repurrrsive::gmaps_cities |>
  unnest_wider(json) |>
  unnest_longer(results) |>
  unnest_wider(results) |>
  unnest_wider(geometry) |>
  unnest_wider(location) |>
  sf::st_as_sf(
    coords = c("lng", "lat"),
    crs = 4326
  ) |>
  dplyr::select(city) |>
  mapview::mapview()

# Nested data ----
# https://tidyr.tidyverse.org/articles/nest.html

df2 <- tribble(
  ~g , ~x , ~y ,
   1 ,  1 ,  2 ,
   2 ,  4 ,  6 ,
   2 ,  5 ,  7 ,
   3 , 10 , NA
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

# Example showing how to use pmap on a
# nested list column of sf objects
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
