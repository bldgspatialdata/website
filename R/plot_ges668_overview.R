#' Create a map for the course overview page
#'
plot_ges668_overview <- function(...,
                                 save = TRUE) {
  rlang::check_installed(
    c("mapbaltimore", "mapmaryland", "sfext", "maplayer")
  )

  # FIXME: Swap this for prefixing functions
  require(ggplot2)
  require(patchwork)

  nhood <- ggplot() +
    geom_sf(data = mapbaltimore::neighborhoods, aes(fill = type), alpha = 0.8, color = "white") +
    theme_void() +
    scale_fill_brewer() +
    guides(fill = "none")


  region <- ggplot() +
    geom_sf(data = mapbaltimore::baltimore_msa_water, fill = "lightblue", alpha = 0.5, color = NA) +
    geom_sf(data = mapbaltimore::baltimore_msa_counties, fill = NA, color = "gray30", linewidth = 0.15) +
    theme_void()


  bus_network <- ggplot() +
    geom_sf(data = mapmaryland::us_states_near_md, fill = NA) +
    geom_sf(data = mapbaltimore::mta_bus_lines |>
      sfext::get_dist(to = c("xmid", "ymid"), drop = TRUE), linewidth = 0.25, aes(alpha = dist)) +
    geom_sf(data = mapbaltimore::mta_light_rail_lines, linewidth = 0.5) +
    geom_sf(data = mapbaltimore::mta_subway_lines, linewidth = 0.75) +
    maplayer::layer_neatline(data = mapbaltimore::baltimore_city, dist = 5, unit = "mi", crs = 2804, color = NA) +
    guides(alpha = "none") +
    theme_void()


  overview_plot <- region + bus_network

  if (save) {
    maplayer::ggsave_ext(
      plot = overview_plot,
      path = fs::path("images", "background_course-overview.png"),
      width = 8,
      height = 4.5
    )
  }

  overview_plot
}
