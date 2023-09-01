# roster <- tibble::tibble()

org <- "bldgspatialdata"
team <- "2023_Students"
# exercise_dir <- "~/exercises"

# Create exercise project
usethis::create_project(
  exercise_dir,
  open = FALSE
)

# Create repositories for each student in the roster
purrr::walk(
  roster[["repository"]],
  \(nm) {
    ghclass::repo_create(
      org = org,
      name = nm,
      private = TRUE,
      auto_init = FALSE,
      gitignore_template = "R"
    )
  }
)

purrr::walk(
  roster[["repository"]],
  \(repo_nm) {
    # Add all of the files from the repositories for each student in the roster
    purrr::walk(
      fs::dir_ls(exercise_dir),
      \(file) {
        if (fs::is_file(file)) {
          ghclass::repo_add_file(
            repo = paste0(org, "/", repo_nm),
            message = paste0("Add ", basename(file), " to exercises folder"),
            file = file,
            repo_folder = "exercises",
            overwrite = TRUE
          )
        }
      }
    )
  }
)

team_users <- ghclass::team_members(org, team = team)[["user"]]

purrr::walk2(
  roster[["repository"]],
  roster[["gh_username"]],
  function(repo_nm, user_nm) {
    if (!is.na(user_nm) && (user_nm %in% team_users)) {
      ghclass::repo_add_user(
        repo = paste0(org, "/", repo_nm),
        user = user_nm
      )
    }
  }
)
