library(ggplot2)
library(igraph)
library(R.matlab)

# Set graph path and name
graph_path <- "/data/caltech_curvs.gml"
output_path <- "/caltech"

# Load graph
g <- read_graph(graph_path, format = "gml")

# Membership info
node_membership <- V(g)$community
same_group_matrix <- outer(node_membership, node_membership, FUN = "==")
original_adj_matrix <- as_adjacency_matrix(g, sparse = TRUE)

# Metrics to process
metrics <- c("lrc", "bfc", "frc")

for (metric in metrics) {
  cat("Processing:", metric, "\n")
  
  # Create subfolders for each metric
  adj_dir <- file.path(output_path, metric, "adj")
  fig_dir <- file.path(output_path, metric, "fig")
  dir.create(adj_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
  
  # 1. Extract metric as adjacency matrix
  metric_matrix <- as_adjacency_matrix(g, attr = metric, sparse = FALSE)
  metric_matrix[metric_matrix == 0] <- NA
  
  zero_weight_edges <- which(edge_attr(g, metric) == 0)
  edge_list_zero <- ends(g, E(g)[zero_weight_edges])
  metric_matrix[cbind(edge_list_zero[,1], edge_list_zero[,2])] <- 0
  metric_matrix[cbind(edge_list_zero[,2], edge_list_zero[,1])] <- 0
  
  # 2. Define cutoffs using quantiles
  cutoffs <- quantile(metric_matrix[!is.na(metric_matrix)], probs = seq(0.01, 1, length.out = 100))
  
  results <- list()
  
  for (cutoff in cutoffs) {
    mask <- metric_matrix > cutoff
    filtered_matrix <- as.matrix(original_adj_matrix)
    filtered_matrix[!mask] <- 0
    
    total_red   <- sum(same_group_matrix == 0 & original_adj_matrix > 0, na.rm = TRUE)
    total_blue  <- sum(same_group_matrix == 1 & original_adj_matrix > 0, na.rm = TRUE)
    total_edges <- sum(original_adj_matrix > 0, na.rm = TRUE)
    
    remaining_red  <- sum(same_group_matrix == 0 & filtered_matrix > 0, na.rm = TRUE)
    remaining_blue <- sum(same_group_matrix == 1 & filtered_matrix > 0, na.rm = TRUE)
    
    removed_red_ratio   <- 1 - (remaining_red / total_red)
    removed_blue_ratio  <- 1 - (remaining_blue / total_blue)
    edges_removed       <- total_edges - sum(filtered_matrix > 0, na.rm = TRUE)
    
    results <- append(results, list(list(
      cutoff = cutoff,
      removed_red_ratio = removed_red_ratio,
      removed_blue_ratio = removed_blue_ratio,
      edges_removed = edges_removed
    )))
    
    # Save matrix
    mat_file <- file.path(adj_dir, paste0("adj_", metric, "_cutoff_", round(cutoff, 4), ".mat"))
    writeMat(mat_file, adj_matrix = filtered_matrix)
    
    # Prepare plot data
    edge_indices <- which(!is.na(metric_matrix), arr.ind = TRUE)
    plot_data <- data.frame(
      node_i = edge_indices[, 1],
      node_j = edge_indices[, 2],
      value = metric_matrix[edge_indices],
      same_group = factor(ifelse(same_group_matrix[edge_indices] == 1, "Same", "Different"))
    )
    
    removed_mask <- filtered_matrix[edge_indices] == 0
    all_removed <- all(removed_mask)
    plot_data$shape_type <- ifelse(removed_mask, "Removed", "Kept")
    if (all_removed) plot_data$shape_type <- "Removed"
    
    # Plot
    plot <- ggplot(plot_data, aes(x = value, y = same_group, color = same_group)) +
      geom_jitter(aes(shape = shape_type), height = 0.2, width = 0.001, alpha = 0.7, size = 3) +
      labs(
        title = paste("Threshold for", toupper(metric), "=", round(cutoff, 4)),
        subtitle = paste("Removed Red:", round(removed_red_ratio, 3),
                         "| Removed Blue:", round(removed_blue_ratio, 3)),
        x = toupper(metric),
        y = "Community Membership",
        shape = "Edge Status", color = "Community Membership"
      ) +
      scale_shape_manual(values = c("Kept" = 16, "Removed" = 21)) +
      theme_minimal()
    
    # Save plot
    fig_file <- file.path(fig_dir, paste0("plot_", metric, "_cutoff_", round(cutoff, 4), ".pdf"))
    ggsave(fig_file, plot, width = 8, height = 6)
  }
  
  # Save results
  results_df <- do.call(rbind, lapply(results, as.data.frame))
  write.csv(results_df, file.path(output_path, metric, paste0("grid_results_new_", metric, ".csv")), row.names = FALSE)
}
