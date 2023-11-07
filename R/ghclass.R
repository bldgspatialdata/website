org <- "bldgspatialdata"
team <- "2023_Students"
# exercise_dir <- "~/exercises"

roster <- tibble::tribble(
              ~repository,
         ""
  )

# Setup ----

# Create exercise project
usethis::create_project(
  exercise_dir,
  open = FALSE
)

# Create repositories for each student in the roster
purrr::walk(
  roster[["repo"]],
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

# Grant student team permissions to each repository
purrr::walk(
  roster[["repo"]],
  \(repo) {
    ghclass::repo_team_permission(
      repo = paste0(org, "/",  repo),
      team = team,
      permission = "pull"
    )
  }
)


# Add users who have joined the team to their repo
# NOTE: I ended up doing a lot of this manually after the initial setup as a
# large share students had not joined the Team prior to setup
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


# Updates ----

roster_add_file <- function(roster = NULL,
                            repos = NULL,
                            file = NULL,
                            repo_folder = "exercises",
                            path = NULL,
                            skip = NULL) {

  if (!is.null(roster)) {
    repos <- roster[["repository"]]
  }

  purrr::walk(
    repos,
    \(repo) {

      if (!is.null(skip) && (repo %in% skip)) {
        return(NULL)
      }

      if (is.null(file) && !is.null(path)) {
        file <- fs::dir_ls(path)
      }

      # Add all of the files from the repositories for each student in the roster
      purrr::walk(
        file,
        \(x) {
          if (fs::is_file(x)) {
            ghclass::repo_add_file(
              repo = paste0(org, "/", repo),
              message = paste0("Add ", basename(x), " to repository"),
              file = x,
              repo_folder = repo_folder,
              overwrite = TRUE
            )
          } else if (fs::is_dir(x)) {
            ghclass::repo_add_file(
              repo = paste0(org, "/", repo),
              message = paste0("Add ", basename(x), " to repository"),
              file = x,
              repo_folder = repo_folder,
              overwrite = TRUE
            )
          }
        }
      )
    }
  )
}
