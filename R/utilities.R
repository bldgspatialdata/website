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
