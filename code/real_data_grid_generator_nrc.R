# Load required libraries
library(ggplot2)
library(igraph)
library(R.matlab)

# Load graph data
g <- readRDS("/data/final_results.rds")
edge_list <- as.data.frame(get.edgelist(g))

# Construct results dataframe
data <- cbind(
  edge_list,
  nrc = E(g)$nrc,
  same_group = E(g)$membership,
  minni = E(g)$min,
  minratio = E(g)$min / E(g)$av
)
colnames(data) <- c("node_i", "node_j", "nrc", "same_group", "minni", "minratio")
data$sqrtmin <- 1 / sqrt(data$minni)
data$same_group <- as.factor(data$same_group)

E(g)$sqrtmin <- 1 / sqrt(E(g)$min)

total_red <- sum(data$same_group == "Different")
total_blue <- sum(data$same_group == "Same")
total_edges <- nrow(edge_list)

nrc_matrix <- as_adjacency_matrix(g, attr="nrc", sparse=FALSE)
# Differentiate no edges (set to NA) from edges with weight = 0
nrc_matrix[nrc_matrix == 0] <- NA  # Mark missing edges
# **Vectorized way to set edges with weight 0**
zero_weight_edges <- which(E(g)$nrc == 0)  # Get indices of edges with weight 0
edge_list_zero <- ends(g, E(g)[zero_weight_edges])  # Get edge endpoints
# Use matrix indexing to set these edges back to 0
nrc_matrix[cbind(edge_list_zero[,1], edge_list_zero[,2])] <- 0
nrc_matrix[cbind(edge_list_zero[,2], edge_list_zero[,1])] <- 0  # Ensure symmetry

sqrtmin_matrix <- as_adjacency_matrix(g, attr="sqrtmin", sparse=FALSE)
sqrtmin_matrix[sqrtmin_matrix == 0] <- NA  # Mark missing edges
# Use matrix indexing to set these edges back to 0
sqrtmin_matrix[cbind(edge_list_zero[,1], edge_list_zero[,2])] <- 0
sqrtmin_matrix[cbind(edge_list_zero[,2], edge_list_zero[,1])] <- 0  # Ensure symmetry

# Define grid search parameters using theta and x-intercept
thetas <- seq(88, 96, length.out = 50)  # θ in degrees

avg_degree <- mean(degree(g))
print(avg_degree)

xints <- seq(-1/mean(degree(g))*0.25, 1/mean(degree(g)), length.out = 50)      # x-intercept values

results <- list()
original_adj_matrix <- as_adjacency_matrix(g, sparse = TRUE)

for (theta in thetas) {
  # Compute slope as tan(theta) (convert degrees to radians)
  slope <- tan(theta * pi / 180)
  
  for (x_int in xints) {
    print(paste0("Theta is ", theta, " degrees and x-intercept is ", x_int))
    
    
    x_boundary <- function(y) y / slope + x_int
    
    mask <- nrc_matrix > x_boundary(sqrtmin_matrix)
    
    
    # Apply the mask to filter values in nrc_matrix
    filtered_nrc <- as.matrix(nrc_matrix)
    filtered_nrc[!mask] <- NA 
    filtered_adj_matrix <- as.matrix(original_adj_matrix) 
    filtered_adj_matrix[!mask] <- 0  
    
    # Check if locations of NA in filtered_nrc match 0 in filtered_adj_matrix
    sanity_check <- which(is.na(filtered_nrc)) == which(filtered_adj_matrix == 0)
    
    # Print result
    if (all(sanity_check, na.rm = TRUE)) {
      print("Sanity check passed: NA locations in filtered_nrc match 0 locations in filtered_adj_matrix!")
    } else {
      print("Sanity check failed: Some locations do not match!")
    }
    
    
    node_membership <- V(g)$membership  
    
   
    same_group_matrix <- outer(node_membership, node_membership, FUN = "==")  
    
    # Count total red (Different) and blue (Same) edges before filtering
    total_red <- sum(same_group_matrix == 0 & original_adj_matrix > 0, na.rm = TRUE)
    total_blue <- sum(same_group_matrix == 1 & original_adj_matrix > 0, na.rm = TRUE)
    total_edges <- sum(original_adj_matrix > 0, na.rm = TRUE)  
    
    # Count remaining red and blue edges after filtering
    remaining_red <- sum(same_group_matrix == 0 & filtered_adj_matrix > 0, na.rm = TRUE)
    remaining_blue <- sum(same_group_matrix == 1 & filtered_adj_matrix > 0, na.rm = TRUE)
    
    # Compute removal ratios
    removed_red_ratio <- 1 - (remaining_red / total_red)
    removed_blue_ratio <- 1 - (remaining_blue / total_blue)
    edges_removed <- total_edges - sum(filtered_adj_matrix > 0, na.rm = TRUE)
    
    # Print results
    print(paste0("Removed Red Ratio: ", removed_red_ratio))
    print(paste0("Removed Blue Ratio: ", removed_blue_ratio))
    print(paste0("Total Edges Removed: ", edges_removed))
    
    results <- append(results, list(
      list(theta = theta, x_intercept = x_int, removed_red_ratio = removed_red_ratio, removed_blue_ratio = removed_blue_ratio, edges_removed = edges_removed)
    ))
    
    # Save the filtered adjacency matrix with updated naming
    writeMat(paste0("/adj_matrix_theta_", round(theta, 2), "_xint_", round(x_int, 4), ".mat"), adj_matrix = filtered_adj_matrix)
    
    # Graph comparison check
    graph_diff <- !all(filtered_adj_matrix == original_adj_matrix, na.rm = TRUE)
    
    cat("Graph different from original:", graph_diff, "\n")
    cat("Edges removed:", edges_removed, "out of", total_edges, "\n")
    
    # Convert matrices into edge list format
    edge_indices <- which(!is.na(nrc_matrix), arr.ind = TRUE)  # Get valid edges
    
    # Extract values for existing edges
    plot_data <- data.frame(
      node_i = edge_indices[, 1],  # Source node
      node_j = edge_indices[, 2],  # Target node
      nrc = nrc_matrix[edge_indices],  # NRC values
      sqrtmin = sqrtmin_matrix[edge_indices],  # sqrtmin values
      same_group = factor(ifelse(same_group_matrix[edge_indices] == 1, "Same", "Different"))  # Convert to factor
    )
    
    # Create a mask for removed edges (filtered_adj_matrix == 0)
    removed_mask <- filtered_adj_matrix[edge_indices] == 0  
    
    # Check if ALL edges are removed
    all_removed <- all(removed_mask)  
    
    # Determine shape based on edge status
    plot_data$shape_type <- ifelse(removed_mask, "Removed", "Kept")  # 21 (hollow) for removed, 16 (filled) for kept
    if (all_removed) {
      plot_data$shape_type <- "Removed"
    }
    
    # Generate and save plots for each combination with updated labels
    plot_filename <- paste0("/boundary_plot_theta_", round(theta, 2), "_xint_", round(x_int, 4), ".pdf")
    pdf(plot_filename)
    
    plot <- ggplot(plot_data, aes(x = nrc, y = sqrtmin, color = same_group)) +
      geom_point(aes(shape = factor(shape_type)), alpha = 0.7, stroke = 1.1, size = 3) +  
      geom_abline(slope = slope, intercept = -slope * x_int, color = "blue", linewidth = 1.2) +
      labs(
        title = paste("Decision Boundary - Theta:", round(theta, 2), "°", "x-intercept:", round(x_int, 4)),
        subtitle = paste("Removed Red Ratio:", round(removed_red_ratio, 3),
                         "| Removed Blue Ratio:", round(removed_blue_ratio, 3)),
        x = expression(NRC),
        y = expression(SD),
        shape = "Edge Status", color = "Community Membership"
      ) +
      scale_shape_manual(values = c("Kept" = 16, "Removed" = 21)) +
      theme_minimal()
    
    print(plot)
    
    dev.off()
  }
}

# Convert results to dataframe
results_df <- do.call(rbind, lapply(results, as.data.frame))
write.csv(results_df, "/results/grid_results.csv", row.names = FALSE)


