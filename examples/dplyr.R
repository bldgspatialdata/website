library(dplyr)

select(storms, wind)

select(storms, year, name)

storms |>
  select(year)

storms |>
  select(
    !c(year, month, day)
  )

storms |>
  select(
    !year,
    !month
  )

storms |>
  select(year:hour)

?any_of

select(
  storms,
  any_of(c("latitude", "y", "lon", "longitude", "x"))
)

# storms[, c("lat", "latitude", "y", "lon", "long", "longitude", "x")]

select(storms, wnd)

storms |>
  filter(
    year == 1978
  ) |>
  select(wind)


storms[["wind"]] >= 50

nrow(storms)

filter(storms, wind >= 50)

1 == 1

2 > 1

2 <= 2

is.na(storms$category)
!is.na(1)
1 %in% c(1, 2, 3)

!TRUE

filter(
  storms,
  !is.na(category)
)

storms |>
  filter(
    year %in% c(1980, 1990, 2000)
  ) |>
  View()

storms |>
  filter(
    name %in% c("Alicia", "Fran", "Katrina", "Sandy", "Eli", "Burt", "Lauren", "Chase")
  ) |>
  distinct(name)

filter(storms, month == 9, day == 13)

TRUE | FALSE

FALSE | FALSE

TRUE | TRUE

filter(
  storms,
  name == "Eloise" | name == "Evelyn",
  day == 13
)

filter_out(
  storms,
  name %in% c("Eloise", "Evelyn")
)

filter(
  storms,
  hurricane_force_diameter >= 115,
  tropicalstorm_force_diameter >= 240
) |>
  distinct(name, year)
