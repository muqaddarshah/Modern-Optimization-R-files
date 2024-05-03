getData <- function() {
  
  distMatrix <- read.csv("IndianCitiesDataset.csv", row.names=1)
  # Convert the data frame to a matrix if necessary
  distMatrix <- as.matrix(distMatrix)
  return(distMatrix)
}


tourLength <- function(tour, distMatrix) {
  tour <- c(tour, tour[1])  # Complete the loop by adding the start city at the end.
  route <- embed(tour, 2)[,2:1]  # Convert the tour into a sequence of trips.
  
  # Calculate the tour length directly using the distance matrix
  tourlength <- sum(distMatrix[cbind(route[,1], route[,2])])
 
  return(tourlength)

}


tspFitness <- function(tour, ...){       #... allows passing some unspecified arguments to the function, which can be passed on further. 
  return (1/tourLength(tour, ...))    #Since the tour length must be minimsed, 1/tourlength can be maximised. 
  #We convert it into a maximisation problem because the GA package can only maximise. 

}

#To call this function, you must pass it on a GA produced solution. 
#For example:
#results <- runGA(problem = "tsp")
#solution <- getBestSolution()
# plotTSPSolution(solution)
plotTSPSolution<-function(solution){
  data("", package = "datasets")
  mds <- cmdscale("IndianCitiesDataset.csv")
  x <- mds[, 1]
  y <- -mds[, 2]
  plot(x, y, type = "n", asp = 1, xlab = "", ylab = "")
  abline(h = pretty(range(x), 10), v = pretty(range(y), 10),
         col = "light gray")
  tour <- solution[1, ]
  tour <- c(tour, tour[1])
  n <- length(tour)
  arrows(x[tour[-n]], y[tour[-n]], x[tour[-1]], y[tour[-1]],
         length = 0.15, angle = 25, col = "steelblue", lwd = 2)
  text(x, y, labels(Indian_cities_dataset), cex=0.8)
}

