#' Plot cranDownloads data for select packages
#'
#' Updated for Fall 2024 semester to show downloads by day instead of average
#' daily downloads by month.
#'
plot_cranDownloads_data <- function(
    data = NULL,
    packages = c("sp", "sf", "dplyr", "ggplot2"),
    from = "2018-08-01",
    to = "2024-07-31",
    plot_title = "Package downloads from CRAN",
    save = TRUE,
    plot_filename = "pkg-downloads_08-2018_07-2023.png") {
  if (is.null(data)) {
    pkg_downloads <- packageRank::cranDownloads(
      packages = packages,
      from = from,
      to = to
    )

    pkg_downloads_data <- pkg_downloads$cranlogs.data
  } else {
    pkg_downloads_data <- data
  }

  # TODO: Add support for caching downloads data
  # readr::write_csv(pkg_downloads$cranlogs.data, "files/pkg_downloads_data.csv")
  # pkg_downloads_data <- readr::read_csv("files/pkg_downloads_data.csv", show_col_types = FALSE)

  pkg_plot <- pkg_downloads_data |>
    dplyr::mutate(
      date = lubridate::date(date)
    ) |>
    ggplot2::ggplot(ggplot2::aes(
      x = date,
      y = count,
      color = package
    )) +
    ggplot2::stat_smooth(
      geom = "line",
      method = "loess",
      span = 0.05,
      alpha = 0.8,
      linewidth = 1.65
    ) +
    ggplot2::scale_x_date("Date") +
    ggplot2::scale_y_continuous(
      "Downloads",
      labels = scales::label_number()
    ) +
    pilot::scale_color_pilot() +
    ggplot2::labs(
      title = plot_title,
      caption = "Source: CRAN via {packageRank} package."
    ) +
    hrbrthemes::theme_ipsum_pub(
      base_family = "Atkinson Hyperlegible"
    ) +
    ggplot2::theme(
      plot.title.position = "plot"
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        title = "Package name",
        position = "top"
      )
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 11),
      legend.location = "plot",
      legend.title.position = "left",
      legend.justification.top = "left",
      # margin turned off for alignment
      # https://www.tidyverse.org/blog/2024/02/ggplot2-3-5-0-legends/
      legend.margin = ggplot2::margin(0, 0, 0, 0)
    )

  if (!save) {
    return(pkg_plot)
  }

  ggplot2::ggsave(
    here::here("slides", "images", plot_filename),
    width = 1050,
    height = 700,
    scale = 2.75,
    units = "px",
    plot = pkg_plot,
    bg = "white"
  )

  pkg_plot
}
