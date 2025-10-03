#############################################################
# Title: Exp 1 example generating code - Part1              #
#                                                           #
# This code aims to generate graphs used for extra plots in #
# Exp 1.                                                    #
# FINAL CODE                                                #
#############################################################

# Import necessary library
source("/code/gen_dcsbm.R")
source("/code/linear_curv.R")
library(igraph)
library(R.matlab)

# Settings
B1 <- matrix(c(0.6, 0.3, 0.3,
               0.3, 0.6, 0.3,
               0.3, 0.3, 0.6), 
             nrow = 3, byrow = TRUE) 
B2 <- matrix(c(0.6, 0.3, 0.3, 0.3,
               0.3, 0.6, 0.3, 0.3,
               0.3, 0.3, 0.6, 0.3,
               0.3, 0.3, 0.3, 0.6),
             nrow = 4, byrow = TRUE)
B3 <- matrix(0.3, nrow = 8, ncol = 8)
diag(B3) <- 0.6



for (seed in 123:223){
  
  # Generate three graphs
  # More nodes
  n1 <- generate_dcsbm(n = 300, gamma = c(0.04, 0.07, 0.11, 0.14, 0.18, 0.21, 0.25, 0.29) , alpha1 = 0.5, alpha2 = 1.5, B3, seed)
  n2 <- generate_dcsbm(n = 600, gamma = c(0.04, 0.07, 0.11, 0.14, 0.18, 0.21, 0.25, 0.29) , alpha1 = 0.5, alpha2 = 1.5, B3, seed)
  n3 <- generate_dcsbm(n = 1200, gamma = c(0.04, 0.07, 0.11, 0.14, 0.18, 0.21, 0.25, 0.29) , alpha1 = 0.5, alpha2 = 1.5, B3, seed)
  # More community
  n4 <- generate_dcsbm(n = 300, gamma = c(0.1, 0.2, 0.7), alpha1 = 0.5, alpha2 = 1.5, B1, seed)
  n5 <- generate_dcsbm(n = 300, gamma = c(0.10, 0.20, 0.30, 0.40), alpha1 = 0.5, alpha2 = 1.5, B2, seed)
  n6 <- generate_dcsbm(n = 300, gamma = c(0.04, 0.07, 0.11, 0.14, 0.18, 0.21, 0.25, 0.29) , alpha1 = 0.5, alpha2 = 1.5, B3, seed)

  
  # Calculate FRC, LRC, NRC, Jac_coef
  n1_g <- cal_linear_curv(n1$graph)
  n2_g <- cal_linear_curv(n2$graph)
  n3_g <- cal_linear_curv(n3$graph)
  n4_g <- cal_linear_curv(n4$graph)
  n5_g <- cal_linear_curv(n5$graph)
  n6_g <- cal_linear_curv(n6$graph)
  
  # Convert igraph to adjacency matrix
  A_n1 <- as.matrix(as_adjacency_matrix(n1_g, sparse = FALSE))
  A_n2 <- as.matrix(as_adjacency_matrix(n2_g, sparse = FALSE))
  A_n3 <- as.matrix(as_adjacency_matrix(n3_g, sparse = FALSE))
  A_n4 <- as.matrix(as_adjacency_matrix(n4_g, sparse = FALSE))
  A_n5 <- as.matrix(as_adjacency_matrix(n5_g, sparse = FALSE))
  A_n6 <- as.matrix(as_adjacency_matrix(n6_g, sparse = FALSE))
  
  
  # Save membership
  label_n1 <- V(n1_g)$membership
  label_n2 <- V(n2_g)$membership
  label_n3 <- V(n3_g)$membership
  label_n4 <- V(n4_g)$membership
  label_n5 <- V(n5_g)$membership
  label_n6 <- V(n6_g)$membership
  
  # Set save directory
  savefolder <- paste0("/s_", seed)
  # Create folder if it doesn't exist
  if (!dir.exists(savefolder)) {
    dir.create(savefolder, recursive = TRUE)
  }
  setwd(savefolder)
  
  # Save each graph as GraphML
  write_graph(n1_g, file = "graph_n1.graphml", format = "graphml")
  write_graph(n2_g, file = "graph_n2.graphml", format = "graphml")
  write_graph(n3_g, file = "graph_n3.graphml", format = "graphml")
  write_graph(n4_g, file = "graph_n4.graphml", format = "graphml")
  write_graph(n5_g, file = "graph_n5.graphml", format = "graphml")
  write_graph(n6_g, file = "graph_n6.graphml", format = "graphml")
  
  # Save adjacency matrices as .mat files
  writeMat("adj_matrix_n1.mat", adj_matrix = A_n1)
  writeMat("adj_matrix_n2.mat", adj_matrix = A_n2)
  writeMat("adj_matrix_n3.mat", adj_matrix = A_n3)
  writeMat("adj_matrix_n4.mat", adj_matrix = A_n4)
  writeMat("adj_matrix_n5.mat", adj_matrix = A_n5)
  writeMat("adj_matrix_n6.mat", adj_matrix = A_n6)
  
  # Save ground truth community labels as .mat files
  writeMat("label_n1.mat", label = label_n1)
  writeMat("label_n2.mat", label = label_n2)
  writeMat("label_n3.mat", label = label_n3)
  writeMat("label_n4.mat", label = label_n4)
  writeMat("label_n5.mat", label = label_n5)
  writeMat("label_n6.mat", label = label_n6)
  
  print(paste0("Finished seed=", seed))
}
