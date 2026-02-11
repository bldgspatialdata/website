week_frontmatter <- purrr::map(
  fs::dir_ls(here::here("weeks"), regexp = "weeks/week"),
  frontmatter::read_front_matter
)

week_schedule <- purrr::imap(
  week_frontmatter,
  \(x, nm) {
    week <- x[["data"]]
    slides <- week[["slides"]]
    exercise <- week[["exercise"]]
    week_path <- fs::path("weeks", fs::path_file(nm))

    data.frame(
      Week = glue::glue("[{week[['order']]}]({week_path})"),
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

#' Create a gt table from a course schedule data frame
#'
#' @param main_font,icon_font Passed to `name` argument of [gt::google_font()]
gt_schedule <- function(
  data,
  main_font = "Atkinson Hyperlegible",
  icon_font = "Noto Color Emoji"
) {
  stopifnot(
    is.data.frame(data),
    all(hasName(data, c("Week", "Date", "Slides", "Exercise")))
  )

  data |>
    gt::gt() |>
    gt::fmt_url(
      columns = "Week",
      target = "_self"
    ) |>
    gt::cols_align(
      "center",
      columns = c("Week", "Date", "Slides", "Exercise")
    ) |>
    gt::fmt_date(
      columns = "Date",
      date_style = "MMMEd"
    ) |>
    gt::cols_width(
      "Date" ~ "120px"
    ) |>
    gt::fmt_url(
      columns = c("Slides", "Exercise"),
      target = "_self",
      show_underline = FALSE
    ) |>
    gt::sub_missing() |>
    gt::tab_style(
      style = gt::cell_text(font = gt::google_font(icon_font)),
      location = gt::cells_body(dplyr::all_of(c("Slides", "Exercise")))
    ) |>
    gt::tab_style(
      style = gt::cell_text(font = gt::google_font(main_font)),
      location = gt::cells_body(dplyr::all_of(c("Week", "Date", "Topic")))
    ) |>
    gt::tab_style(
      style = gt::cell_text(font = gt::google_font(main_font), weight = "bold"),
      location = gt::cells_column_labels()
    )
}
