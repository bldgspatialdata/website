index_yml <- function(txt = NULL) {
  if (rlang::is_string(txt) && fs::is_file(txt)) {
    txt <- readLines(txt)
  }

  c(2:(which(txt == "---")[2] - 1))
}

read_yml <- function(path, remove = FALSE) {
  txt <- readLines(path)
  yml_index <- index_yml(txt)

  if (remove) {
    return(txt[c((max(yml_index) + 2):length(txt))])
  }

  ymlthis::as_yml(
    paste(txt[index_yml(txt)], collapse = "\n")
  )
}

# Function for renaming files ----
rename_files <- function(path,
                         pattern = "-",
                         replacement = "_",
                         glob = "*.qmd",
                         ...) {
  if (fs::is_dir(path)) {
    path_info <- fs::dir_info(path = path, glob = glob)
    path <- path_info[["path"]]
  }

  new_path <- stringr::str_replace(
    string = path,
    pattern = pattern,
    replacement = replacement
  )

  fs::file_move(path, new_path)
}

#' Update the YAML keys for a file with `{ymlthis}`
#'
update_file_yml <- function(path,
                            ...,
                            method = "add") {
  if (fs::is_dir(path)) {
    path_info <- fs::dir_info(path = path, glob = glob)
    path <- path_info[["path"]]
  }

  for (x in path) {
    yml <- read_yml(x)

    txt <- read_yml(x, remove = TRUE)

    if (method == "add") {
      new_yml <- ymlthis::yml_toplevel(
        yml,
        ...
      )
    } else if (method == "replace") {
      new_yml <- ymlthis::yml_replace(
        yml,
        ...
      )
    }


    updated_txt <- c(
      ymlthis:::capture_yml(new_yml),
      txt
    )

    writeLines(updated_txt, con = x)
  }
}

#' Use lightparser::split_to_tbl to read YAML keys from one or more Quarto
#' document
#'
#' Created 2024-08-29 to reconcile the file names and schedule of the slides,
#' week overview pages, and exercises.
read_qmd_params <- function(path,
                            ...,
                            recurse = FALSE,
                            keys = c("order",
                                     "title",
                                     "subtitle",
                                     "date",
                                     "date-due",
                                     "date-modified",
                                     "image",
                                     "abstract"),
                            regex = "qmd$",
                            perl = TRUE) {
  files <- path

  if (fs::is_dir(path)) {
    files <- fs::dir_ls(
      path,
      regexp = paste0("^", path ,"/_"),
      invert = TRUE,
      type = "file"
    )

    files <- stringr::str_subset(
      files,
      pattern = "^_",
      negate = TRUE
    )
  }

  files <- rlang::set_names(files, files)

  file_tbl <- purrr::map(
    files,
    \(x) {
      tbl <- withCallingHandlers(
        lightparser::split_to_tbl(x),
        error = NULL
      )

      if (is.null(tbl)) {
        return(NULL)
      }

      params <- tbl |>
        dplyr::filter(type == "yaml") |>
        dplyr::pull(params)

      # Select the frontmatter
      params <- params[[1]][keys]

      as.data.frame(vctrs::list_drop_empty(params))
    }
  )

  file_tbl |>
    purrr::list_rbind(
      names_to = "path"
    ) |>
    dplyr::mutate(
      filename = fs::path_ext(path),
      .after = dplyr::all_of("path")
    )
}
