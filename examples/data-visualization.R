# pak::pkg_install(c("rnaturalearth", "rnaturalearthdata"))
# install.packages(c("rnaturalearth", "rnaturalearthdata"))

library(tidyverse)
library(sf)
library(rnaturalearth)

coastline <- ne_coastline()

countries <- ne_countries()

# One variable visualizations

# continuous numeric
ggplot(storms, aes(wind)) +
  geom_histogram()

# discrete numeric
ggplot(storms, aes(year)) +
  geom_bar()

# discrete categorical
ggplot(storms, aes(status)) +
  geom_bar()

# locate text in y axis and count in x axis
ggplot(storms, aes(y = status)) +
  geom_bar()

# discrete categorical
ggplot(storms, aes(y = category)) +
  geom_bar()

# continuous numeric with vline and text annotation
ggplot(storms, aes(lat)) +
  geom_freqpoly(bins = 90) +
  geom_vline(xintercept = 39.2904) +
  geom_text(x = 39.2904 - 2, y = 500, angle = 90, label = "Baltimore City, MD")
# https://stackoverflow.com/questions/18091721/align-geom-text-to-a-geom-vline-in-ggplot2

# Two variable visualization
ggplot(storms, aes(wind, pressure)) +
  geom_point()

ggplot(storms, aes(wind, pressure)) +
  geom_point() +
  geom_smooth()

# spatial data is fancy point data
ggplot(storms, aes(long, lat)) +
  geom_point()

ggplot(storms, aes(long, lat)) +
  geom_point(aes(color = status))

# address over-plotting with transparency
ggplot(storms, aes(long, lat)) +
  geom_point(
    mapping = aes(color = status),
    alpha = 0.4
  )

# aesthetic mappings support functions as well as variables
ggplot(storms, aes(long, lat)) +
  geom_point(aes(color = status == "hurricane"))

ggplot(storms_sf) +
  geom_sf()

# convert storms to sf object with st_as_sf
storms_sf <- storms |>
  st_as_sf(
    coords = c("long", "lat"),
    crs = 4326
  )

# geom_sf works with sf objects
ggplot(storms_sf) +
  geom_sf()

# geom_sf doesn't work if geometry is not specified
ggplot(storms) +
  geom_sf()

# points support color
ggplot(data = storms_sf) +
  geom_sf(aes(color = status))

# points don't support fill
ggplot(data = storms_sf) +
  geom_sf(aes(fill = status))

# points support size
ggplot(data = storms_sf) +
  geom_sf(aes(size = tropicalstorm_force_diameter), alpha = 0.15)

# multiple data layers can be used, e.g. coastline
ggplot(data = storms_sf) +
  geom_sf(aes(color = status)) +
  geom_sf(data = coastline, color = "black")

# NOTE: view defaults to max extent


# coords_sf can be used to "zoom" map
storms_bbox <- st_bbox(storms_sf)

ggplot(data = storms_sf) +
  geom_sf(aes(color = status)) +
  geom_sf(data = coastline, color = "black") +
  coord_sf(
    xlim = storms_bbox[c("xmin", "xmax")],
    ylim = storms_bbox[c("ymin", "ymax")]
  )

# use appropriate scales
ggplot(data = storms_sf) +
  geom_sf(aes(color = status)) +
  geom_sf(data = coastline, color = "black") +
  coord_sf(
    xlim = storms_bbox[c("xmin", "xmax")],
    ylim = storms_bbox[c("ymin", "ymax")]
  ) +
  scale_color_brewer()

# order of layers matters (countries last)
ggplot(data = storms_sf) +
  geom_sf(aes(color = status)) +
  geom_sf(data = coastline, color = "black") +
  geom_sf(data = countries, fill = "white") +
  coord_sf(
    xlim = storms_bbox[c("xmin", "xmax")],
    ylim = storms_bbox[c("ymin", "ymax")]
  ) +
  scale_color_brewer()

# order of layers matters (countries first)
ggplot(data = storms_sf) +
  geom_sf(data = countries, fill = "white") +
  geom_sf(data = coastline, color = "black") +
  # Note: countries supports a fill attribute
  geom_sf(aes(color = status)) +
  coord_sf(
    xlim = st_bbox(storms_sf)[c("xmin", "xmax")],
    ylim = st_bbox(storms_sf)[c("ymin", "ymax")]
  ) +
  scale_color_brewer()


storms_bbox_lambert <- storms_bbox |>
  st_as_sfc() |>
  st_transform("ESRI:102009") |>
  st_bbox()

ggplot(data = storms_sf) +
  geom_sf(data = countries, fill = "white") +
  geom_sf(aes(color = status)) +
  geom_sf(data = coastline, color = "black") +
  coord_sf(
    xlim = storms_bbox_lambert[c("xmin", "xmax")],
    ylim = storms_bbox_lambert[c("ymin", "ymax")],
    crs = "ESRI:102009"
  ) +
  scale_color_brewer()

# convert from points to lines
storm_tracks <- storms_sf |>
  summarise(
    category = if_else(
      !all(is.na(category)),
      max(category, na.rm = TRUE),
      NA
    ),
    # combine geometry by year and name
    geometry = st_union(geometry),
    .by = c(year, name)
  ) |>
  # convert multi-point geometry to linestring
  st_cast("LINESTRING")

ggplot(storm_tracks) +
  geom_sf()

ggplot(storm_tracks) +
  geom_sf(
    data = \(x) {filter(x, !is.na(category))},
    mapping = aes(color = category),
    alpha = 0.25
  )

ggplot(data = storms_sf) +
  geom_sf(data = storm_tracks) +
  geom_sf(aes(color = status), alpha = 0.1)

ggplot(data = storms_sf) +
  geom_sf(aes(color = status)) +
  coord_sf(crs = "EPSG:3035")

storms_sf |>
  st_transform(2804) |>
  ggplot() +
  geom_sf(aes(color = status))

storms_bbox <- storms_sf |>
  st_transform("EPSG:3035") |>
  st_bbox()

storms_map <- ggplot() +
  geom_sf(data = countries, fill = "white") +
  geom_sf(data = storms_sf, aes(color = category), alpha = 0.3) +
  geom_sf(data = coastline, color = "black") +
  coord_sf(
    xlim = storms_bbox_lambert[c("xmin", "xmax")],
    ylim = storms_bbox_lambert[c("ymin", "ymax")],
    crs = "EPSG:3035"
  )

storms_map <- storms_map +
  scale_color_distiller(type = "seq", direction = 1, palette = "YlOrRd", na.value = "gray60")

storms_map <- storms_map +
  labs(
    title = "Atlantic hurricanes by category, 1975-2021",
    caption = "Data: NOAA Atlantic hurricane database best track data via the {dplyr package}"
  )

storms_map +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )
