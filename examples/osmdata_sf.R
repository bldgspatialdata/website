library(tidyverse)
library(osmdata)

# Get bounding box for UMBC
bbox <- getbb("University of Maryland Baltimore County")

# Add bbox to a query
q <- opq(bbox)

# Include amenity=bench in the query
# https://wiki.openstreetmap.org/wiki/Tag:amenity=bench
q <- q %>%
  add_osm_feature(
    key = "amenity",
    value = "bench"
  )

# Execute query and look at point data
benches <- q |>
  osmdata_sf()

benches$osm_points

# Get a citywide bounding box and add leisure=garden to query
q <- opq(bbox = 'Baltimore, Maryland') %>%
  add_osm_feature(key = 'leisure', value = 'garden')

# Execute the query
gardens <- q |>
  osmdata_sf()

gardens <- unique_osmdata(gardens)

# Combine
bind_rows(
  gardens$osm_points,
  gardens$osm_lines,
  gardens$osm_polygons,
  gardens$osm_multipolygons
)

# Does not work
ggplot() +
  geom_sf(data = gardens)

# Plot gardens
ggplot() +
  geom_sf(data = gardens$osm_polygons)

ggplot() +
  geom_sf(data = sf::st_centroid(gardens$osm_polygons))

# Get MD counties and filter to Baltimore City
md_counties <- tigris::counties("MD")

baltimore_city <- filter(
  md_counties,
  NAMELSAD == "Baltimore city"
)

# Query parks and gardens with the City spatial bbox
q <- opq (sf::st_bbox(baltimore_city)) %>%
  add_osm_features (features = list (
    "leisure" = "park",
    "leisure" = "garden"
  ))

# Execute the query
open_space <- q |>
  osmdata_sf()

open_space <- unique_osmdata(open_space)

# Example
q <- getbb("Baltimore city, Maryland") |>
  opq () %>%
  add_osm_features (features = list (
    "leisure" = "park",
    "leisure" = "garden"
  ))
