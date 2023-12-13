library(packageRank)
library(ggplot2)
library(dplyr)

pkg_downloads <- packageRank::cranDownloads(
  packages = c("sp", "sf", "dplyr", "ggplot2"),
  from = "2018-08-01",
  to = "2023-07-31"
)

pkg_downloads_data <- pkg_downloads$cranlogs.data

# readr::write_csv(pkg_downloads$cranlogs.data, "files/pkg_downloads_data.csv")
# pkg_downloads_data <- readr::read_csv("files/pkg_downloads_data.csv", show_col_types = FALSE)

pkg_plot_data <- pkg_downloads_data |>
  dplyr::mutate(
    date = lubridate::date(date),
    month = lubridate::month(date),
    year = lubridate::year(date),
    date_month = lubridate::ymd(paste0(year, "-", stringr::str_pad(month, 2, pad = "0"), "-01"))
  ) |>
  dplyr::summarise(
    count = mean(count),
    .by = c("package", "date_month")
  )

pkg_plot <- pkg_plot_data |>
  ggplot2::ggplot(ggplot2::aes(x = date_month, y = count, color = package)) +
  ggplot2::stat_smooth(
    geom = "line", method = "loess", span = 0.05, alpha = 0.75, linewidth = 1.75
  ) +
  ggplot2::scale_x_date() +
  ggplot2::scale_y_continuous(
    labels = scales::label_number()
  ) +
  pilot::scale_color_pilot() +
  ggplot2::labs(
    color = "Package name",
    x = "Date",
    y = "Average daily downloads",
    caption = "Package downloads from CRAN (August 2018 - July 2023) via the {packageRank} package."
  ) +
  hrbrthemes::theme_ipsum_pub(
    base_family = "Atkinson Hyperlegible"
  )

ggplot2::ggsave(
  here::here("slides", "images", "pkg-downloads_08-2018_07-2023.png"),
  width = 1050,
  height = 700,
  scale = 2.75,
  units = "px",
  plot = pkg_plot,
  bg = "white"
)

suppressMessages(suppressWarnings(pkg_plot))
