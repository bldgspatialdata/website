scramble_lines <- function(x, copy = interactive()) {
  if (length(x) == 1) {
    x <- stringr::str_split_1(x, "\n")
  }

  x <- x[x != ""]

  x <- stringr::str_trim(x)

  x <- sample(x)

  if (!copy) {
    return(x)
  }

  clipr::write_clip(x)
}
