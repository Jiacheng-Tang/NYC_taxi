taxi_map <- function(location = c("All", "Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island")){
  require(rgdal)
  require(rgeos)
  location <- match.arg(location)
  get.border <- readOGR(dsn = ".", layer = "taxi_zones")
  get.border <- spTransform(get.border, CRS("+proj=longlat +datum=WGS84"))
  if (location == "All") {
    zone.border <- get.border
  }
  else {
    zone.border <- get.border[get.border@data$borough == location,]
  }
  zone.center <- cbind.data.frame(data.frame(gCentroid(zone.border, byid=TRUE),
                                             id=zone.border@data$LocationID))
  get.filename <- paste0(location,"_zones_centerGPS.csv")
  write.csv(zone.center, get.filename, row.names=FALSE)

  require(ggmap)
  zone.border <- fortify(zone.border)
  map_box <- make_bbox(long, lat, data = zone.border, f = 0.05)
  map <- get_stamenmap(map_box, zoom = 12)

  zone.layer <- ggmap(map) +
  geom_polygon(aes(x=long, y=lat, group=group), size=.2,
               color='green', data=zone.border, alpha=0)+
  geom_point(data = zone.center, aes(x = x, y = y))

  return(zone.layer)
}