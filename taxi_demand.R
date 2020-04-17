taxi_demand <- function (dat) {
  # Define average interarrival time for PU(row)-DO(column) pairs, as a matrix
  attach(dat)
  IDs <- sort(unique(PULocationID))
  AAM <- matrix(0L, nrow = length(IDs), ncol = length(IDs))
  rownames(AAM) <- as.factor(IDs)
  colnames(AAM) <- as.factor(IDs)
  for (iPU in 1:length(IDs)) {
    for (iDO in 1:length(IDs)) {
      ID_PU <- IDs[iPU]
      ID_DO <- IDs[iDO]
      numDays <- length(unique(trip_date[trip_weekday %in% 1:5]))
      numTrips <- length(VendorID[trip_weekday %in% 1:5 & PULocationID==ID_PU & DOLocationID==ID_DO])
      if (numTrips<numDays){
        AAM[iPU,iDO] <- NA
      }
      else {
        AAM[iPU,iDO] <- 3600*numDays/numTrips
      }
    }
  }
  write.csv(AAM,"AverageArrivalMatrix.csv")
  print(paste("Average arrival matrix have been saved to", "AverageArrivalMatrix.csv"))
  return(AAM)
  detach(dat)
}




  # # Calculate mean interarrival time for PU-DO pairs
  # print("Calculation starts...")
  # IDs = sort(unique(PULocationID))
  # AAM = matrix(0L, nrow = length(IDs), ncol = length(IDs))
  # rownames(AAM) = as.factor(IDs)
  # colnames(AAM) = as.factor(IDs)
  # 

  # 
  # # Calculate mean travel time for PU-DO pairs
  # ATM = matrix(0L, nrow = length(IDs), ncol = length(IDs))
  # rownames(ATM) = as.factor(IDs)
  # colnames(ATM) = as.factor(IDs)
  # for (iPU in 1:length(IDs)) {
  #   for (iDO in 1:length(IDs)) {
  #     if (is.na(AAM[iPU,iDO])){
  #       ATM[iPU,iDO] = NA
  #     }
  #     else {
  #       ID_PU = IDs[iPU]
  #       ID_DO = IDs[iDO]
  #       numTrips = 0
  #       sumTrips = 0
  #       for (i in 1:n.days) {
  #         if (date2wday[i] > 0 & date2wday[i] < 6 & i < 21) {
  #           if (TRUE %in% unique(PULocationID==ID_PU & DOLocationID==ID_DO & dates==i)) {
  #             get.data = trip.time[PULocationID==ID_PU & DOLocationID==ID_DO & dates==i & trip.time>0 & trip.time<7200]
  #             numTrips = numTrips + length(get.data)
  #             sumTrips = sumTrips + sum(get.data)
  #           }
  #         }
  #       }
  #       ATM[iPU,iDO] = sumTrips/numTrips
  #     }
  #   }
  #   print(paste("ATM:",iPU,"out of",length(IDs),"has been processed."))
  # }
  # get.filename = paste(substr(filename,1,nchar(filename)-4), '_ATM', '.csv', sep = "")
  # write.csv(ATM,get.filename)
  # print(paste("Average trip time matrix have been saved to", get.filename))