require(historydata)
require(ggmap)
data("catholic_dioceses")

dat.manhanttan <- read_csv("Manhattan_zones_GPS.csv")
bbox <- make_bbox(Latitude, Longitude, data = dat.manhanttan, f = 0.05)
map <- get_stamenmap(bbox, zoom = 12)
ggmap(map) +
  geom_vline(xintercept = -74)


Test = zone.border@polygons