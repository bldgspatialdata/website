library(dplyr)

storms |>
  arrange(
    desc(year)
  ) |>
  filter(
    name %in% c("Maria", "Marie", "Mary",
                "Marcus", "Michael", "Mike", "Mortimer")
  ) |>
  distinct(name, .keep_all = TRUE)


# =
# <-
# ==

storms |>
  filter(
    category >= 3
  )


storms |>
  select(year, name) |>
  distinct()

# base R examples
storms[, c("year", "name", "yr", "date")]

#
storms |>
  select(
    any_of(
      c("year", "name", "yr", "date")
    )
  )

storms |>
  select(
    where(is.numeric)
  )

storms |>
  ggplot(aes(wind, pressure)) +
  geom_point()

storms |>
  mutate(
    wind_pressure_ratio = wind / pressure
  ) |>
  ggplot(aes(wind_pressure_ratio)) +
  geom_histogram()
