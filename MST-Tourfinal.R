library(igraph)

# Load distance matrix from "IndianCitiesDataset.csv"
distMatrix <- as.matrix(read.csv("IndianCitiesDataset.csv", row.names=1))

# Create a graph from the distance matrix
g <- graph_from_adjacency_matrix(distMatrix, mode="undirected", weighted=TRUE)

# Compute the Minimum Spanning Tree
mst <- mst(g)

# Perform a depth-first search to get a tour
dfs_result <- dfs(mst, root=1, order=TRUE)
tour <- as.numeric(dfs_result$order)

# Ensure 'tour' is a valid numeric vector
if (!is.numeric(tour) || anyNA(tour)) {
  stop("Error: 'tour' is not a valid numeric vector")
}

# Add the start city at the end to complete the loop
tour <- c(tour, tour[1])

# Calculate the tour length
tourLength <- function(tour, distMatrix) {
  route <- embed(tour, 2)[, 2:1]
  sum(distMatrix[cbind(route[,1], route[,2])])
}
length <- tourLength(tour, distMatrix)

print(length)

install.packages("ggplot2")

# Load required packages
library(ggplot2)

# Example coordinates of cities (replace with your own data)
city_coordinates <- data.frame(
  City = dfs_result$order,  # Assuming dfs_result$order contains city names
  X = runif(length(dfs_result$order), min = 0, max = 10),  # Random X coordinates
  Y = runif(length(dfs_result$order), min = 0, max = 10)   # Random Y coordinates
)

# Create a dataframe with the tour path
tour_path <- data.frame(
  from = tour[-length(tour)],  # From city
  to = tour[-1]                 # To city
)

# Convert igraph.vs objects to data frames
city_coordinates <- data.frame(
  City = as.character(V(g)$name),  # Assuming city names are stored as 'name' attribute in the graph
  X = runif(vcount(g), min = 0, max = 10),  # Random X coordinates
  Y = runif(vcount(g), min = 0, max = 10)   # Random Y coordinates
)

# Create a dataframe with the tour path
tour_path <- data.frame(
  from = dfs_result$order[-length(dfs_result$order)],  # From city (excluding the last one)
  to = dfs_result$order[-1]                            # To city (excluding the first one)
)

# Plot the cities and tour
ggplot() +
  # Plot cities
  geom_point(data = city_coordinates, aes(x = X, y = Y), color = "blue", size = 3) +
  # Plot tour
  geom_path(data = tour_path, aes(x = city_coordinates$X[from], y = city_coordinates$Y[from], 
                                  xend = city_coordinates$X[to], yend = city_coordinates$Y[to]), 
            color = "red", size = 1) +
  # Add labels
  geom_text(data = city_coordinates, aes(x = X, y = Y, label = City), hjust = 0, vjust = 0) +
  # Set plot labels and title
  labs(x = "Longitude", y = "Latitude", title = "Traveling Salesman Problem Tour Visualization") +
  # Add theme
  theme_minimal()

