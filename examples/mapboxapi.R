library(mapboxapi)
library(mapgl)

# mb_access_token(
#   "<Your Mapbox API Token>"
# )

walk_5min <- mb_isochrone(
  "100 Holliday St, Baltimore, MD 21202",
  profile = "walking",
  time = 5
)

walk_5min

walk_5min |>
  mapboxgl_view()
