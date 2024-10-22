library(sf)
library(tidyverse)
library(gt)

nc = st_read(system.file("shape/nc.shp", package = "sf"))

glimpse(nc)

str(nc)

skimr::skim(nc)

nc |>
  janitor::tabyl(
    BIR74
  )

diamonds |>
  janitor::tabyl(
    cut,
    color,
    clarity
  )

nc |>
  select(!c(NAME, starts_with(c("FIPS", "CNTY")))) |>
  sf::st_drop_geometry() |>
  gtsummary::tbl_summary()

diamonds |>
  gtsummary::tbl_summary(
    by = cut
  )

diamonds |>
  slice_head(n = 10) |>
  group_by(color) |>
  gt() |>
  summary_rows(
    columns = where(is.numeric),
    fns = list(
      "min",
      "max",
      list(label = "avg", fn = "mean")
    ),
    fmt = ~ fmt_number(., use_seps = FALSE)
  )


ggplot(data) +
  aes(x = bill_length_mm, y = bill_depth_mm) +
  geom_point(colour = "#112446") +
  theme_minimal()

