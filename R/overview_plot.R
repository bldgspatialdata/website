library(mapbaltimore)
library(ggplot2)
library(patchwork)

nhood <- ggplot() +
  geom_sf(data = neighborhoods, aes(fill = type), alpha = 0.8, color = "white") +
  theme_void() +
  scale_fill_brewer() +
  guides(fill = "none")


region <- ggplot() +
  geom_sf(data = baltimore_msa_water, fill = "lightblue", alpha = 0.5, color = NA) +
  geom_sf(data = baltimore_msa_counties, fill = NA, color = "gray30", linewidth = 0.15) +
  theme_void()


bus_network <- ggplot() +
  geom_sf(data = mapmaryland::us_states_near_md, fill = NA) +
  geom_sf(data = mta_bus_lines |>
            sfext::get_dist(to = c("xmid", "ymid"), drop= TRUE), linewidth = 0.25, aes(alpha = dist)) +
  geom_sf(data = mta_light_rail_lines, linewidth = 0.5) +
  geom_sf(data = mta_subway_lines, linewidth = 0.75) +
  maplayer::layer_neatline(data = baltimore_city, dist = 5, unit = "mi", crs = 2804, color = NA) +
  guides(alpha = "none") +
  theme_void()


region + bus_network

maplayer::ggsave_ext(
  name = here::here("images", "background_course-overview.png"),
  width = 8,
  height = 4.5
)
