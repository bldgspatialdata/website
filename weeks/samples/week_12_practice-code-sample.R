pkg_install(c("labelled", "googlesheets4", "DataEditR", "gtsummary", "pointblank"))

# Load required packages ----
library(tidyverse)
library(sf)
# library(arcgislayers)
library(labelled) # https://larmarange.github.io/labelled/

# Get started by reading data with arcgislayers (or sf if you prefer) ----

url <- "https://geodata.baltimorecity.gov/egis/rest/services/CitiMap/DOT_Layers/MapServer/5"

layer <- arc_open(url)

glimpse(layer)

data <- arc_select(layer)

# Creating a dictionary with generate_dictionary ----

data_dict <- generate_dictionary(data, details = "full")

# You could edit the dictionary within R (but I wouldn't recommend it)

# install.packages("DataEditR")
# library(DataEditR)
# data_edit(mtcars)

# You can save to a file or to a service like Google Sheets

##  Creating a data dictionary with Google Sheets ----

library(googlesheets4)

# If you have not used googlesheets4, you need to grant permission for the
# package to access your account
ss <- gs4_create("Bike Facility Data Dictionary")

write_sheet(data_dict, ss = ss, sheet = "dictionary")

gs4_browse(ss)

## Setting variable labels ----

updated_data_dict <- read_sheet("https://docs.google.com/spreadsheets/d/1NdOJ-_zpVjTH3bqrH1vp4u5VqEHkVw2oXDt5c5kcp1s/edit?usp=sharing")

print(updated_data_dict$label)

labelled_data <- set_variable_labels(data, .labels = updated_data_dict$label)

labelled_data <- st_drop_geometry(labelled_data)

# RStudio supports labelled variable names in View
View(labelled_data)

# gtsummary package supports labelled variable names with `tbl_summary()`
labelled_data |>
  select(FAC_TYPE, LENGTH, STATUS) |>
  gtsummary::tbl_summary()

## Try it out yourself ----

# Take a few minutes to read in data for your final project (or from another source)

# Create a data dictionary using generate_dictionary and fill in a few labels

## Creating metadata reports with pointblank or codebook ----

library(pointblank)
library(codebook)

labelled_data |>
  scan_data()

labelled_data |>
  codebook()
