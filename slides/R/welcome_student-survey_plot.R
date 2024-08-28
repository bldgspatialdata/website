library(dplyr)
library(ggplot2)

url <- "https://docs.google.com/spreadsheets/d/1WS-2HR9MHTOKr-Bdz12vNnnGYCF6nZv8czeUucVCz48/edit?usp=sharing"

survey <- googlesheets4::read_sheet(url)

survey |>
  dplyr::select(tidyselect::starts_with("How much")) |>
  tidyr::pivot_longer(
    tidyselect::everything()
  ) |>
  dplyr::mutate(
    tool = dplyr::case_when(
      stringr::str_detect(name, "programming") ~ "Programming",
      stringr::str_detect(name, "with R") ~ "R",
      .default = "GIS"
    ),
    experience = dplyr::case_when(
      stringr::str_detect(value, "No") ~ "None",
      stringr::str_detect(value, "Some") ~ "Some",
      stringr::str_detect(value, "A lot") ~ "A lot"
    ),
    experience = factor(experience, c("None", "Some", "A lot"))
  ) |>
  dplyr::filter(!is.na(experience)) |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(
    aes(y = experience, fill = experience),
    alpha = 0.8
  ) +
  ggplot2::facet_wrap(~tool) +
  # scale_fill_viridis_d("Experience") +
  pilot::scale_fill_pilot() +
  hrbrthemes::theme_ipsum_pub(
    base_family = "Atkinson Hyperlegible",
    plot_title_family = "Atkinson Hyperlegible",
    caption_family = "Atkinson Hyperlegible",
    axis_title_size = 16,
    axis_text_size = 16,
    strip_text_size = 20
  ) +
  ggplot2::theme(
    plot.margin = margin(0.1, 0.1, 0.1, 0.1, "in"),
    axis.title.x = element_text(hjust = 1, vjust = 0),
    panel.spacing.x = unit(0.65, "in")
  ) +
  ggplot2::guides(fill = "none") +
  ggplot2::labs(
    title = "How much experience do you have with...",
    y = "",
    x = "Number of students",
    caption = "Source: GES 668 Fall 2023 Student Survey"
  )

ggsave(
  here::here("slides", "images", "2024-08-28_student-survey.png"),
  width = 1050,
  height = 700,
  scale = 2.75,
  units = "px",
  bg = "white"
)
