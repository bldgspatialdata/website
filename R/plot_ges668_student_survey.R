#' Plot results from the GES 668 student welcome survey
#'
#' Created for Fall 2023 semester. Updated for fall 2024 with the addition of a
#' level between none and some.
#'
plot_ges668_student_survey <- function(
    url = "https://docs.google.com/spreadsheets/d/1WS-2HR9MHTOKr-Bdz12vNnnGYCF6nZv8czeUucVCz48/edit?usp=sharing",
    plot_caption = "Source: GES 668 Fall 2024 Student Survey",
    plot_filename = "2024-08-28_student-survey.png",
    save = TRUE) {
  survey <- googlesheets4::read_sheet(url)

  experience_results <- survey |>
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
        stringr::str_detect(value, "little") ~ "A little",
        stringr::str_detect(value, "Some") ~ "Some",
        stringr::str_detect(value, "A lot") ~ "A lot"
      ),
      experience = factor(experience, c("None", "A little", "Some", "A lot"))
    ) |>
    dplyr::filter(!is.na(experience))

  experience_plot <- experience_results |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      ggplot2::aes(y = experience, fill = experience),
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
      plot.margin = ggplot2::margin(0.1, 0.1, 0.1, 0.1, "in"),
      axis.title.x = ggplot2::element_text(hjust = 1, vjust = 0),
      panel.spacing.x = ggplot2::unit(0.65, "in")
    ) +
    ggplot2::guides(fill = "none") +
    ggplot2::labs(
      title = "How much experience do you have with...",
      y = "",
      x = "Number of students",
      caption = plot_caption
    )

  if (!save) {
    return(experience_plot)
  }

  stopifnot(is.character(plot_filename))

  ggplot2::ggsave(
    plot = experience_plot,
    here::here("slides", "images", plot_filename),
    width = 1050,
    height = 700,
    scale = 2.75,
    units = "px",
    bg = "white"
  )
}
