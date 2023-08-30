index_yml <- function(txt = NULL) {
  if (rlang::is_string(txt) && fs::is_file(txt)) {
   txt <- readLines(txt)
  }

  c(2:(which(txt == '---')[2] - 1))
}

read_yml <- function(path, remove = FALSE) {
  txt <- readLines(path)
  yml_index <- index_yml(txt)

  if (remove) {
    return(txt[c((max(yml_index) + 2):length(txt))])
  }

  ymlthis::as_yml(
    paste(txt[index_yml(txt)], collapse = '\n')
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

# Function for renaming files ----
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
