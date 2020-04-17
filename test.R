library(readr)
type.list <- cols(
  LocationID = col_integer(),
  Longitude = col_double(),
  Latitude = col_double()
)
GPS <- read_csv("center_coordinates_needed.csv", col_types = type.list)