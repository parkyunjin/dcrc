############################################################
# Title: Figure 1 generating code - Part1                  #
#                                                          #
# This code aims to generate graphs used to plot Figure 1. #
############################################################

# Import necessary library
source("/code/gen_dcsbm.R")
source("/code/linear_curv.R")
library(igraph)
library(R.matlab)

# Settings                  
B <- matrix(c(0.9, 0.3, 0.3,
              0.3, 0.9, 0.3,
              0.3, 0.3, 0.9), 
            nrow = 3, byrow = TRUE) 
seed <- 123

# Generate two graphs
balanced_sbm <- generate_dcsbm(n = 150, gamma = c(0.3, 0.3, 0.3), alpha1 = 1, alpha2 = 1, B, seed)
dcsbm <- generate_dcsbm(n = 150, gamma = c(0.1, 0.2, 0.7), alpha1 = 0.5, alpha2 = 1.5, B, seed)

# Calculate FRC, LRC, NRC
balanced_sbm_g <- cal_linear_curv(balanced_sbm$graph)
dcsbm_g <- cal_linear_curv(dcsbm$graph)

# Convert igraph to adjacency matrix
A_balanced <- as.matrix(as_adjacency_matrix(balanced_sbm_g, sparse = FALSE))
A_dcsbm <- as.matrix(as_adjacency_matrix(dcsbm_g, sparse = FALSE))

# Extract community membership
label_dcsbm <- V(dcsbm_g)$membership

# Save the graphs

write_graph(balanced_sbm_g, file = "balanced_sbm.graphml", format = "graphml")
write_graph(dcsbm_g, file = "dcsbm.graphml", format = "graphml")

# Save each matrix as .mat
writeMat("balanced_sbm.mat", adj_matrix = A_balanced)
writeMat("dcsbm.mat", adj_matrix = A_dcsbm)

# Save DCSBM membership
writeMat("dcsbm_label.mat", label = label_dcsbm)
