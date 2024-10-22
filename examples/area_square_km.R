# This is a cleaned up version of your original function
area_square_km <- function(data, ...) {
  data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(...),
        ~ .x / 1000000
      )
    )
}

md_counties <- tigris::counties(state = "MD")

area_square_km(
  md_counties$AWATER
)

# Why doesn't it work?
# md_counties$AWATER is a numeric vector
# mutate requires a data frame input

area_square_km(
  md_counties
)

# Why doesn't it work?
# ... is empty
# all_of requires a string or character vector
# to specify the column names

# This works!
area_square_km(
  md_counties,
  "AWATER"
)


# This is the original code for your function
# Why doesn't it work?
area_square_km_original <- function(x, ...) {
  # The input is named `x` but within the function
  # you use the variable name `data`
  data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(...),
        # When you use the `~` tilde character notation for formulas the input
        # is `.x` not `.`
        ~ . / 1000000
      )
    )
}
