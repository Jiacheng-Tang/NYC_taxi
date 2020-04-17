# Function: taxi_cleaning(filename)

# New Variable:   trip_date       varialbe contains only the date of the month
#                 trip_time       total trip time in seconds
#                 trip_weekday    Sun-0, Mon-1, ..., Sat-6
#                 PU_long         longitude for pick up zone
#                 PU_lat          latitude for pick up zone
#                 DO_long         longtitude for drop off zone
#                 DO_lat          latitude for drop off zone

# Cleaning:       trip time       60 - 7200     seconds
#                 trip distance   0.1 - 20      miles
#                 fare amount     >0            dollars
#                 rate code ID    1             standard rate (*)
#                 passenger       1 - 6         remove data with NA or 0
#                 payment type    1,2           remove no charge or disputed data
#                 MTA tax         0.5           mandatory (*)
#                 toll amount     0             (*)
#                 improvement     0.3           mandatory (*)
#                 congestion      2.5           mandatory (*)
#   (*) deleted variable in the corrected version

taxi_cleaning <- function(filename)
{
  require("readr")
  require("stringr")
  library(stringr)
  library(readr)
  type.list <- cols(
    VendorID = col_integer(),
    tpep_pickup_datetime = col_datetime(format = ""),
    tpep_dropoff_datetime = col_datetime(format = ""),
    passenger_count = col_integer(),
    trip_distance = col_double(),
    RatecodeID = col_integer(),
    store_and_fwd_flag = col_character(),
    PULocationID = col_integer(),
    DOLocationID = col_integer(),
    payment_type = col_integer(),
    fare_amount = col_double(),
    extra = col_double(),
    mta_tax = col_double(),
    tip_amount = col_double(),
    tolls_amount = col_double(),
    improvement_surcharge = col_double(),
    total_amount = col_double(),
    congestion_surcharge = col_double()
  )
  get.data <- read_csv(filename, col_types = type.list)
 
  attach(get.data)
  dates <- as.numeric(strftime(tpep_pickup_datetime, format = "%d", tz=attr(tpep_pickup_datetime, "UTC")))
  get.data$trip_date = dates

  # Convert hour:minute:second to total seconds
  pickup.hours   <- as.numeric(strftime(tpep_pickup_datetime, format = "%H", tz=attr(tpep_pickup_datetime, "UTC")))
  pickup.minutes <- as.numeric(strftime(tpep_pickup_datetime, format = "%M", tz=attr(tpep_pickup_datetime, "UTC")))
  pickup.seconds <- as.numeric(strftime(tpep_pickup_datetime, format = "%S", tz=attr(tpep_pickup_datetime, "UTC")))
  pickup.time <- 3600*pickup.hours + 60*pickup.minutes + pickup.seconds

  dropoff.hours   <- as.numeric(strftime(tpep_dropoff_datetime, format = "%H", tz=attr(tpep_dropoff_datetime, "UTC")))
  dropoff.minutes <- as.numeric(strftime(tpep_dropoff_datetime, format = "%M", tz=attr(tpep_dropoff_datetime, "UTC")))
  dropoff.seconds <- as.numeric(strftime(tpep_dropoff_datetime, format = "%S", tz=attr(tpep_dropoff_datetime, "UTC")))
  dropoff.time <- 3600*dropoff.hours + 60*dropoff.minutes + dropoff.seconds

  trip.time <- dropoff.time-pickup.time
  get.data$trip_time <- trip.time

  # Calculate date to weekday table, add to dataframe
  year <- str_extract_all(filename,"[0-9]+")[[1]][1]
  month <- str_extract_all(filename,"[0-9]+")[[1]][2]
  n.days <- sort(unique(dates),decreasing = TRUE)[1]
  day1 <- paste0(year, "-", month, "-01")
  date2wday <- (as.POSIXlt(day1)$wday:40)[1:n.days] %% 7
  get.data$trip_weekday = date2wday[dates]

  # Add GPS location for each datepoint
  if (!file.exists("All_zones_centerGPS.csv")){
    source("taxi_map.R")
    print("Generating GPS information...")
    taxi_map()
    print("Done.")
  }

  type.list <- cols(
    x = col_double(),
    y = col_double(),
    id = col_integer()
  )
  zone.gps <- read_csv("All_zones_centerGPS.csv", col_types = type.list)
  zone.gps <- zone.gps[order(zone.gps$id),]
  get.data$PU_long = zone.gps$y[PULocationID]
  get.data$PU_lat  = zone.gps$x[PULocationID]
  get.data$DO_long = zone.gps$y[DOLocationID]
  get.data$DO_lat  = zone.gps$x[DOLocationID]

  # Add data cleaning criteria
  filtered.data <- get.data[
      trip.time > 60 & trip.time < 7200
    & trip_distance > 0.1 & trip_distance < 12
    & RatecodeID == 1
    & fare_amount > 0
    & passenger_count %in% 1:6
    & payment_type %in% 1:2
    & mta_tax == 0.5
    & tolls_amount == 0
    & improvement_surcharge == 0.3
    & congestion_surcharge == 2.5,]
  # print(names(filtered.data))
  get.filename <- paste0(substr(filename, 1, nchar(filename) - 4), '_corrected.csv')
  filtered.data <- filtered.data[,-c(2:3,6:7,12:16,18)]
  write.csv(filtered.data, get.filename, row.names = FALSE)
  # print(names(filtered.data))
  print(paste("Corrected version has been save to",get.filename))
  print(paste("Data points count:",nrow(filtered.data),"out of",nrow(get.data)))

  # trip_MPH = filtered.data$trip_distance*1609.34 / filtered.data$trip_time * 2.23694
  # hist(trip_MPH)
  # print(length(trip_MPH[trip_MPH>40]))
  detach(get.data)
}