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

week_frontmatter <- purrr::map(
  fs::dir_ls("weeks", regexp = "^weeks/week"),
  frontmatter::read_front_matter
)

week_schedule <- purrr::imap(
  week_frontmatter,
  \(x, nm) {
    week <- x[["data"]]
    slides <- week[["slides"]]
    exercise <- week[["exercise"]]

    data.frame(
      Week = glue::glue("[{week[['order']]}]({nm})"),
      Date = week[["date"]],
      Topic = week[["subtitle"]],
      Slides = if (is.null(slides)) {
        NA_character_
      } else {
        glue::glue("[📖](slides/{slides[['file']]})")
      },
      Exercise = if (is.null(exercise)) {
        NA_character_
      } else {
        glue::glue("[📝](exercises/{exercise[['file']]})")
      }
    )
  }
)

course_schedule_tbl <- purrr::list_rbind(week_schedule)
