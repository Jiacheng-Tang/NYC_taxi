taxi_filter <- function(filename, hour, location)
{
  require("readr")
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
  get.hours <- strftime(tpep_pickup_datetime, format = "%H", tz=attr(tpep_pickup_datetime, "UTC"))
  get.data.new <- get.data[get.hours == hour,]
  detach(get.data)
  
  get.filename <- paste0(substr(filename, 1, nchar(filename) - 4), '_', hour, '.csv')
  write.csv(get.data.new, get.filename, row.names = FALSE)
  print(paste("Filtered dataframe (by hour) has been saved to", get.filename))
  print(paste("Data points count:",nrow(get.data.new),"out of",nrow(get.data)))
  
  if (!(missing(location))){
    type.zone <- cols(
      LocationID = col_integer(),
      Borough = col_character(),
      Zone = col_character(),
      service_zone = col_character()
    )
    taxi.zone.lookup <- read_csv("taxi+_zone_lookup.csv", col_types = type.zone)
    
    attach(get.data.new)
    attach(taxi.zone.lookup)
    IDs <- LocationID[Borough==location]
    get.data.new2 <- get.data.new[PULocationID %in% IDs & DOLocationID %in% IDs,]
    detach(taxi.zone.lookup)
    detach(get.data.new)
    
    get.filename <- paste0(substr(get.filename, 1, nchar(get.filename) - 4), '_', location, '.csv')
    write.csv(get.data.new2, get.filename, row.names = FALSE)
    print(paste("Filtered dataframe (by hour & location) has been saved to", get.filename))
    print(paste("Data points count:",nrow(get.data.new2),"out of",nrow(get.data.new)))
  }
}