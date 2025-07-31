#############################################################
# Title: Exp 3 example generating code - Part2              #
#                                                           #
# This code aims to generate graphs used for extra plots in #
# Exp 3.                                                    #
# FINAL CODE                                                #
#############################################################

library(igraph)
library(ggplot2)
library(R.matlab)


# following loop is for the rebuttal
for (seed in 145:(123+49)){
  workingdir <- paste0("/s_", seed)
  setwd(workingdir)
  print(paste0("Working for seed=", seed))
  
  # List of graphs to process
  graph_ids <- paste0("graph_n", c(1,2,3,4,5,6,7))
  
  # List of curvature attributes to process
  curvatures <- c("nrc") #, "lrc", "frc", "bfc")
  
  # Main loop over each graph
  for (graph_basename in graph_ids) {
    
    # Define output directories
    output_dir_adj_base <- paste0("adj_matrices_filtered_percentile_", graph_basename)
    output_dir_fig_base <- paste0("filtered_figures_percentile_", graph_basename)
    output_dir_result <- paste0("csv_percentile_conjecture_", graph_basename)
    
    # Load graph
    graph_path <- file.path(paste0(graph_basename, ".graphml"))
    cat("\n Processing", graph_path, "\n")
    g <- read_graph(graph_path, format = "graphml")
    
    # Prepare adjacency matrix
    original_adj_matrix <- as_adjacency_matrix(g, sparse = TRUE)
    
    # Node membership
    node_membership <- V(g)$membership
    
    if (is.null(node_membership)) {
      stop(paste("membership attribute not found in", graph_path))
    }
    same_group_matrix <- outer(node_membership, node_membership, FUN = "==")
    
    # Total edge counts
    total_red <- sum(same_group_matrix == 0 & original_adj_matrix > 0, na.rm = TRUE)
    total_blue <- sum(same_group_matrix == 1 & original_adj_matrix > 0, na.rm = TRUE)
    total_edges <- sum(original_adj_matrix > 0, na.rm = TRUE)
    
    for (curv_type in curvatures) {
      cat("\n=== Processing curvature:", curv_type, "===\n")
      
      if (!(curv_type %in% edge_attr_names(g))) {
        warning(paste("Skipping", curv_type, "- attribute not found in graph."))
        next
      }
      
      # Create output subfolders
      output_dir_adj <- file.path(output_dir_adj_base, curv_type)
      output_dir_fig <- file.path(output_dir_fig_base, curv_type)
      dir.create(output_dir_adj, showWarnings = FALSE, recursive = TRUE)
      dir.create(output_dir_fig, showWarnings = FALSE, recursive = TRUE)
      dir.create(output_dir_result, showWarnings = FALSE, recursive = TRUE)
      
      # Get curvature values and matrix
      curv_values <- edge_attr(g, curv_type)
      curv_matrix <- as_adjacency_matrix(g, attr = curv_type, sparse = FALSE)
      curv_matrix[curv_matrix == 0] <- NA
      
      # Restore zero-weight edges if applicable
      zero_weight_edges <- which(curv_values == 0)
      if (length(zero_weight_edges) > 0) {
        edge_list_zero <- ends(g, E(g)[zero_weight_edges])
        curv_matrix[cbind(edge_list_zero[,1], edge_list_zero[,2])] <- 0
        curv_matrix[cbind(edge_list_zero[,2], edge_list_zero[,1])] <- 0
      }
      
      # Generate quantile-based thresholds
      probs <- seq(0, 0.35, length.out = 100)
      xints <- quantile(curv_values, probs = probs, na.rm = TRUE)
      percentile_labels <- paste0("p", sprintf("%03d", round(probs * 1000)))
      
      # Store results
      graph_results <- list()
      
      for (i in seq_along(xints)) {
        x_int <- xints[i]
        percentile <- percentile_labels[i]
        
        cat("  Trying x-intercept:", x_int, "(percentile:", percentile, ")\n")
        
        mask <- curv_matrix >= x_int
        filtered_adj_matrix <- as.matrix(original_adj_matrix)
        filtered_adj_matrix[!mask] <- 0
        
        remaining_red <- sum(same_group_matrix == 0 & filtered_adj_matrix > 0, na.rm = TRUE)
        remaining_blue <- sum(same_group_matrix == 1 & filtered_adj_matrix > 0, na.rm = TRUE)
        
        removed_red_ratio <- 1 - (remaining_red / total_red)
        removed_blue_ratio <- 1 - (remaining_blue / total_blue)
        edges_removed <- total_edges - sum(filtered_adj_matrix > 0, na.rm = TRUE)
        
        # Save .mat file
        save_name <- file.path(output_dir_adj, paste0(graph_basename, "_", curv_type, "_", percentile, "_xint_", round(x_int, 7), ".mat"))
        writeMat(save_name,
                 adj_matrix = filtered_adj_matrix,
                 ground_truth = as.matrix(original_adj_matrix),
                 label = node_membership)
        
        # Store result
        graph_results <- append(graph_results, list(
          list(
            graph_name = graph_basename,
            curvature = curv_type,
            percentile = percentile,
            x_intercept = x_int,
            removed_red_ratio = removed_red_ratio,
            removed_blue_ratio = removed_blue_ratio,
            edges_removed = edges_removed
          )
        ))
        
        # Plot histogram
        edge_indices <- which(!is.na(curv_matrix), arr.ind = TRUE)
        plot_data <- data.frame(
          node_i = edge_indices[,1],
          node_j = edge_indices[,2],
          curv = curv_matrix[edge_indices],
          same_group = factor(ifelse(same_group_matrix[edge_indices] == 1, "Same", "Different")),
          shape_type = ifelse(filtered_adj_matrix[edge_indices] > 0, "Kept", "Removed")
        )
        
        plot_filename <- file.path(output_dir_fig, paste0(graph_basename, "_", curv_type, "_", percentile, "_xint_", round(x_int, 7), ".pdf"))
        
        pdf(plot_filename)
        p <- ggplot(plot_data, aes(x = curv, fill = same_group)) +
          geom_histogram(position = "identity", alpha = 0.6, bins = 50) +
          geom_vline(xintercept = x_int, color = "blue", linetype = "dashed") +
          labs(
            title = paste0(graph_basename, " | ", curv_type, " | ", percentile, " | x-int: ", round(x_int, 4)),
            subtitle = paste("Removed Red:", round(removed_red_ratio, 3),
                             " | Removed Blue:", round(removed_blue_ratio, 3)),
            x = toupper(curv_type),
            y = "Count",
            fill = "Community Membership"
          ) +
          theme_minimal()
        print(p)
        dev.off()
      }
      
      # Save results for this curvature
      final_result_file <- file.path(output_dir_result, paste0(graph_basename, "_", curv_type, "_grid_results_filtered.csv"))
      all_results_df <- do.call(rbind, lapply(graph_results, as.data.frame))
      write.csv(all_results_df, final_result_file, row.names = FALSE)
    }
  }
  
  cat("All graphs and curvatures processed.\n")
}
