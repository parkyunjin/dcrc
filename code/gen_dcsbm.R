# Load the igraph package
if (!require("igraph")) {
  install.packages("igraph")
}
library(igraph)

# Function to generate a DCSBM graph with gamma, alpha, and B as inputs, and an optional seed
generate_dcsbm <- function(n, gamma_vector, alpha1, alpha2, B, seed = NULL) {
  # Set the random seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Determine the number of communities (k) from the dimensions of B
  k <- nrow(B)
  
  # Calculate community sizes based on gamma
  # Ensure gamma_vector sums to 1
  gamma_vector <- gamma_vector / sum(gamma_vector)
  
  # Calculate sizes based on gamma_vector
  sizes <- round(gamma_vector * n)
  
  # Check if sizes sum up to n, adjust if necessary
  while (sum(sizes) != n) {
    diff <- n - sum(sizes)
    sizes[which.max(sizes)] <- sizes[which.max(sizes)] + diff
  }
  
  # Create the membership vector based on sizes
  membership <- rep(1:k, sizes)
  
  # Generate degree correction parameters (theta) for each node
  # Range of theta is from alpha to 1
  theta <- runif(n, min = alpha1, max = alpha2)
  # Scale to ensure theta[i] * theta[j] * max(B) <= 1
  #max_theta_prod <- max(theta)^2
  #scale_factor <- sqrt(1 / (max(B) * max_theta_prod))
  #theta <- theta * scale_factor
  
  # Initialize the adjacency matrix
  adj_matrix <- matrix(0, n, n)
  
  # Generate the adjacency matrix for the DCSBM
  for (i in 1:n) {
    for (j in i:n) {  # Only upper triangle to avoid duplicates
      # Determine the blocks for nodes i and j
      block_i <- membership[i]
      block_j <- membership[j]
      
      # Calculate the probability of an edge
      p_ij <- pmin(1,B[block_i, block_j] * theta[i] * theta[j])
      
      # Generate edge based on probability
      adj_matrix[i, j] <- rbinom(1, 1, p_ij)
      adj_matrix[j, i] <- adj_matrix[i, j]  # Symmetric for undirected graph
    }
  }
  
  # Create igraph object from adjacency matrix
  dcsbm_graph <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)
  V(dcsbm_graph)$membership <- membership
  
  return(list(graph = dcsbm_graph, membership = membership, adjacency_matrix = adj_matrix))
}



# Example usage
n <- 100                          # Number of nodes
gamma <- c(0.5, 0.5)                      # Ratio of two community sizes
alpha1 <- 0.5                      # Lower bound for degree correction (theta)
alpha2 <- 1
B <- matrix(c(0.1, 0.05,
              0.05, 0.2), 
            nrow = 2, ncol = 2, byrow = TRUE)  # Block connectivity matrix for 2 communities
seed <- 123                       # Optional seed for reproducibility

# Generate DCSBM graph
dcsbm_result <- generate_dcsbm(n, gamma, alpha1, alpha2, B, seed)

# Plot the graph
plot(dcsbm_result$graph, vertex.color = dcsbm_result$membership, vertex.size = 5, edge.arrow.size = 0.5, main = "DCSBM Network")
