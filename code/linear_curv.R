library(igraph)

cal_linear_curv <- function(g) {
  # Ensure graph is undirected
  if (is.directed(g)) {
    stop("Function only supports undirected graphs.")
  }
  
  if (is.null(V(g)$membership)) {
    stop("Vertex attribute 'membership' must be defined.")
  }
  
  # Step 1: Prepare adjacency matrix and neighbor stats
  adj_matrix <- as_adjacency_matrix(g, sparse = FALSE)
  member <- V(g)$membership
  num_neighbors <- rowSums(adj_matrix)
  num_nodes <- vcount(g)
  mean_n <- mean(num_neighbors)
  
  # Step 2: Common neighbors matrix
  common_neighbors_matrix <- adj_matrix %*% adj_matrix
  
  # Step 3: Get the indices of edges in the adjacency matrix
  edges <- which(adj_matrix == 1, arr.ind = TRUE)
  edges <- edges[edges[,1] < edges[,2], ]  # Unique edges for undirected
  
  # Data frame with metrics
  df <- data.frame(
    node_i = edges[, 1],
    node_j = edges[, 2],
    membership = ifelse(member[edges[,1]] == member[edges[,2]], "Same", "Different"),
    num_neighbors_i = num_neighbors[edges[, 1]],
    num_neighbors_j = num_neighbors[edges[, 2]],
    num_common_neighbors = common_neighbors_matrix[cbind(edges[, 1], edges[, 2])],
    frc = 4 - num_neighbors[edges[,1]] - num_neighbors[edges[, 2]] + 3*common_neighbors_matrix[cbind(edges[, 1], edges[, 2])],
    lrc = 2/num_neighbors[edges[,1]] + 2/num_neighbors[edges[,2]] - 2 +
      2*common_neighbors_matrix[cbind(edges[, 1], edges[, 2])]/pmax(num_neighbors[edges[,1]],num_neighbors[edges[,2]]) +
      common_neighbors_matrix[cbind(edges[, 1], edges[, 2])]/pmin(num_neighbors[edges[,1]],num_neighbors[edges[,2]]),
    nrc = common_neighbors_matrix[cbind(edges[, 1], edges[, 2])]/(num_neighbors[edges[,1]]*num_neighbors[edges[,2]]), 
    jac_coef = common_neighbors_matrix[cbind(edges[, 1], edges[, 2])]/(num_neighbors[edges[,1]]+num_neighbors[edges[,2]]-common_neighbors_matrix[cbind(edges[, 1], edges[, 2])]),
    jac_curv = -2 + 3*common_neighbors_matrix[cbind(edges[, 1], edges[, 2])]/(num_neighbors[edges[,1]]+num_neighbors[edges[,2]]-common_neighbors_matrix[cbind(edges[, 1], edges[, 2])])
  )
  
  # Reorder the edge metrics to match igraph edge order
  graph_edges <- as.data.frame(get.edgelist(g))
  colnames(graph_edges) <- c("node_i", "node_j")
  df$key <- paste(df$node_i, df$node_j, sep = "_")
  graph_edges$key <- paste(graph_edges$node_i, graph_edges$node_j, sep = "_")
  df_ordered <- df[match(graph_edges$key, df$key), ]
  
  # Add edge attributes
  E(g)$frc <- df_ordered$frc
  E(g)$lrc <- df_ordered$lrc
  E(g)$nrc <- df_ordered$nrc
  E(g)$a2 <- df_ordered$num_common_neighbors
  E(g)$jac_coef <- df_ordered$jac_coef
  E(g)$jac_curv <- df_ordered$jac_curv
  E(g)$membership <- df_ordered$membership
  
  return(g)
}
