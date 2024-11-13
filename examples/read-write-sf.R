#' Explanatory Code Samples showing use of st_read, st_as_sf, and st_write
#'
#' The code samples illustrate the process for reading data into an sf object
#' from a variety of local and remote sources.

library(readr)
library(sf)

#' read_sf_file ----

dsn <- "<file name>"

spatial_data <- st_read(dsn)

#' read_sf_url ----

dsn <- "<url>"

spatial_data <- st_read(dsn)

#' read_sf_geojson_text ----

dsn <- "<GeoJSON text>"

spatial_data <- st_read(dsn)

#' read_csv ----
file <- "<file path>"

data <- read_csv(file)

spatial_data <- st_as_sf(
  data,
  coords = c("lon", "lat"),
  crs = 4326
)

#' read_sheet ----
library(googlesheets4)

ss <- "<url>"

data <- read_sheet(ss)

spatial_data <- st_as_sf(
  data,
  coords = c("lon", "lat"),
  crs = 4326
)

#' read_excel ----
library(readxl)

path <- "<file>"

data <- read_excel(path)

spatial_data <- st_as_sf(
  data,
  coords = c("lon", "lat"),
  crs = 4326
)

#' arc_select ----
library(arcgislayers)

url <- "<url>"

feature_layer <- arc_open(url)

spatial_data <- arc_select(feature_layer)

# read.socrata
library(RSocrata)

url <- "<url>"

data <- read.socrata(url)

# Example with splitting location column
spatial_data <- st_as_sf(
  data,
  coords = c("lon", "lat"),
  crs = 4326
)

#' write_xlsx ----
library(openxlsx2)

data <- st_drop_geometry(spatial_data)

data_with_coords <- cbind(data, sf::st_coordinates(spatial_data))

write_xlsx(data_with_coords)

#' Reading spatial data from URLs
#'
#' The following code shows working examples of the reading sf data from URLs.
#' Combined with prior section of "non-working" examples on 2024-10-22.

library(sf)

drivers <- st_drivers()

st_read()

california <- st_read("https://github.com/ropensci/geojsonio/raw/main/inst/examples/california.geojson")

poa <- st_read(
  "https://github.com/ropensci/geojsonio/raw/main/inst/examples/poa_annua.kml"
)

url <- "https://github.com/r-spatial/sf/raw/main/inst/gpkg/nc.gpkg"

st_read(url)

url <- "https://github.com/r-spatial/sf/raw/main/inst/csv/pt.csv"

pt <- st_read(url)

url <- "https://github.com/elipousson/marylandedu/raw/main/data-raw/extdata/Enrollment_By_Grade_2023.csv"

enrollment <- st_read(url)

read_csv(url)

st_read("https://services1.arcgis.com/UWYHeuuJISiGmgXx/arcgis/rest/services/Cenus_Tracts/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

nc <- st_read(system.file("shape/nc.shp", package="sf"))

county_wkt <- nc[1,] |>
  st_buffer(10000) |>
  st_geometry() |>
  st_as_text()

nc <- st_read(system.file("shape/nc.shp", package="sf"), wkt_filter = county_wkt)

nc <- st_read(system.file("shape/nc.shp", package="sf"), query = "SELECT NAME, AREA FROM \"nc\" WHERE AREA < 0.1")

library(arcgislayers)

url <- "https://services1.arcgis.com/UWYHeuuJISiGmgXx/ArcGIS/rest/services/owner_occupancy/FeatureServer/0"

layer <- arc_open(url)

data <- layer |>
  arc_select(
    fields = c("PERMHOME")
  )
