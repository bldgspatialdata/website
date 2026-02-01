abc <- list(
  "A" = 1,
  "B" = 2,
  "C" = 3
)

abc[1]

abc[[1]]

library(sf)

nc <- st_read(system.file("shape/nc.shp", package = "sf"), as_tibble = TRUE)

geometry <- nc[["geometry"]]


geometry[1]

class(geometry[[1]])
