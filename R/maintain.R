# Update imports in DESCRIPTION file ----

# List imports in Quarto documents
qmd_imports <- purrr::map(
  c(
    fs::dir_ls("slides", glob = "*.qmd"),
    fs::dir_ls("weeks", glob = "*.qmd"),
    # FIXME: exercises can't be parsed
    # fs::dir_ls("exercises", glob = "*.qmd"),
    fs::dir_ls("resources", glob = "*.qmd")
  ),
  attachment::att_from_qmd
) |>
  unlist() |>
  as.character()

# List imports in R scripts
r_imports <- purrr::map(
  c(
    fs::dir_ls("R", glob = "*.R"),
    fs::dir_ls("slides/R", glob = "*.R"),
    fs::dir_ls("exercises/R", glob = "*.R")
  ),
  attachment::att_from_rscript
) |>
  unlist() |>
  as.character()

imports <- unique(c(qmd_imports, r_imports))

# TODO: Remove this temporary work around when it is no longer needed
imports |>
  sort() |>
  paste0(collapse = ",\n") |>
  clipr::write_clip()

# FIXME: Figure out why att_to_desc_from_is is not working
# attachment::att_to_desc_from_is(
#   path.d = here::here("DESCRIPTION"),
#   imports = imports,
#   suggests = NULL,
#   normalize = FALSE,
#   must.exist = FALSE
# )
