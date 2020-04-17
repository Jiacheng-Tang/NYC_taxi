# Read csv file, variable name: y(YEAR).(MONTH)
library(readr)
y19.dec <- read_csv("yellow_tripdata_2019-12.csv")


attach(y19.dec)

# Extract hour information from the dataset
hrs <- strftime(tpep_pickup_datetime, format = "%H", tz=attr(tpep_pickup_datetime, "UTC"))

# Filter dataset with specific hour, variable name: y(YEAR).(MONTH).(HOUR)
y19.dec.08 <- y19.dec[hrs=="08",]
rm(hrs)
detach(y19.dec)

# Save 8am dataset as a seperate file
write.csv(y19.dec.08,"yellow_tripdata_2019-12_08.csv", row.names = FALSE)

# Use filtered data directly
# y19.dec.08 <- read_csv("yellow_tripdata_2019-12_08.csv")

# Extract Manhattan Trips (i.e., trips from Manhattan to Manhattan)
# Delete low demand locations from the dataset (<100)
taxi_zone_lookup <- read_csv("taxi+_zone_lookup.csv")
names(taxi_zone_lookup)

attach(y19.dec.08)
attach(taxi_zone_lookup)
unique(Borough)
Manhattan.ID <- LocationID[Borough=="Manhattan"]
demand <- sort(table(PULocationID[PULocationID %in% Manhattan.ID]), decreasing=TRUE)
Manhattan.ID.high <- as.numeric(names(demand)[demand>100])
y19.dec.08.manhattan <- y19.dec.08[PULocationID %in% Manhattan.ID.high & DOLocationID %in% Manhattan.ID.high,]
detach(taxi_zone_lookup)
detach(y19.dec.08)

rm(demand)
rm(Manhattan.ID)
rm(Manhattan.ID.high)

write.csv(y19.dec.08.manhattan,"yellow_tripdata_2019-12_8am_manhattan.csv", row.names = FALSE)

# Add weekday variable to the dataset (Sun-0 Mon-1 Sat-6)
# Check numberof trips over weekdays for pattern
attach(y19.dec.08.manhattan)
names(y19.dec.08.manhattan)
dates <- as.numeric(strftime(tpep_pickup_datetime, format = "%d", tz=attr(tpep_pickup_datetime, "UTC")))
par(mfrow=c(1,1))
hist(dates, breaks = 0:31, main="Histogram of Dates (2019-12 8am-9am)")

pickup.day1 = strftime(tpep_pickup_datetime[match(1,dates)], format = "%Y-%m-%d", tz=attr(tpep_pickup_datetime, "UTC"))
n.days = sort(unique(dates),decreasing = TRUE)[1]
date2wday = (as.POSIXlt(pickup.day1)$wday:40)[1:n.days] %% 7
y19.dec.08.manhattan$wday = date2wday[dates]

rm(pickup.day1)

detach(y19.dec.08.manhattan)