taxi_binarySplits <- function(map, model){
  Split <- model$splits[!(duplicated(model$split[,1])|(model$split[,1]==0)),4]
  PUmap <- map
  DOmap <- map
  for (i in 1:length(Split)) {
    if (names(Split[i]) == "DO_long") {
      DOmap <- DOmap + geom_hline(yintercept = Split[i], col = "red")
    } else if (names(Split[i]) == "DO_lat") {
      DOmap <- DOmap + geom_vline(xintercept = Split[i], col = "red")
    } else if (names(Split[i]) == "PU_long") {
      PUmap <- PUmap + geom_hline(yintercept = Split[i], col = "blue")
    } else if (names(Split[i]) == "PU_lat") {
      PUmap <- PUmap + geom_vline(xintercept = Split[i], col = "blue")
    }
  }
  require(gridExtra)
  grid.arrange(PUmap,DOmap,nrow=1)
}