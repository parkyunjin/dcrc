library(igraph)
library(ggplot2)
source("/code/linear_curv.R")

# —————— Functions ——————
gen_thm_dcsbm <- function(n, gamma_vector, alpha1, alpha2, B, seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  k <- nrow(B)
  gamma_vector <- gamma_vector / sum(gamma_vector)
  sizes <- round(gamma_vector * n)
  while(sum(sizes)!=n) {
    diff <- n - sum(sizes)
    sizes[which.max(sizes)] <- sizes[which.max(sizes)] + diff
  }
  membership <- rep(seq_len(k), sizes)
  theta <- runif(n, min=alpha1, max=alpha2)
  
  adj <- matrix(0, n, n)
  for(i in seq_len(n)) for(j in i:n) {
    p_ij <- min(1, B[membership[i], membership[j]] * theta[i] * theta[j])
    e <- rbinom(1,1,p_ij)
    adj[i,j] <- e; adj[j,i] <- e
  }
  
  g <- graph_from_adjacency_matrix(adj, mode="undirected", diag=FALSE)
  V(g)$membership <- membership
  V(g)$theta      <- theta
  comm_mat <- diag(ncol(B))[membership, ]
  
  list(graph=g, comm_mat=comm_mat)
}

compute_M <- function(P, comm_mat, theta) {
  n <- nrow(comm_mat); K <- ncol(comm_mat)
  inv2 <- 1/sum(theta^2)
  THE  <- diag(theta); THE2 <- diag(theta^2)
  one  <- matrix(1,n,1)
  G    <- inv2 * t(comm_mat) %*% THE2 %*% comm_mat
  eta  <- K * 1/sum(abs(theta)) * t(comm_mat) %*% THE %*% one
  Peta <- P %*% eta
  Dinv <- diag(1 / as.numeric(Peta))
  Dinv %*% P %*% G %*% P %*% Dinv
}

compute_omega <- function(theta, comm_mat, P) {
  diag(theta) %*% comm_mat %*% P %*% t(comm_mat) %*% diag(theta)
}

compute_all_dcrc_star <- function(g, theta, comm_mat, P, membership) {
  n <- length(theta); K <- ncol(comm_mat)
  l1 <- sum(abs(theta)); l2 <- sum(theta^2)
  M  <- compute_M(P, comm_mat, theta)
  C  <- K^2 * l2 / (l1^2)
  
  el <- get.edgelist(g, names=FALSE)
  ks <- membership[el[,1]]; ls <- membership[el[,2]]
  vals <- C * M[cbind(ks, ls)]
  
  E(g)$dcrc_star <- vals
  edge_df <- data.frame(i=el[,1], j=el[,2], dcrc_star=vals)
  list(graph=g, edge_table=edge_df)
}

compute_sn <- function(omega) {
  K <- ncol(omega)
  sn <- matrix(NA, K, K)
  for(i in seq_len(K)) for(j in seq_len(K)) {
    if(i==j) next
    ks <- setdiff(seq_len(K), c(i,j))
    num <- sum(omega[ks,i] * omega[ks,j] * (1-omega[ks,i]) * (1-omega[ks,j]))
    den <- sum(omega[,i]*omega[,j])^2
    sn[i,j] <- num/den
  }
  sn
}

compute_all_sn <- function(g, theta, comm_mat, P, membership) {
  omega  <- compute_omega(theta, comm_mat, P)
  sn_mat <- compute_sn(omega)
  el     <- get.edgelist(g, names=FALSE)
  vals   <- sqrt(sn_mat[cbind(el[,1], el[,2])])
  
  E(g)$sn <- vals
  edge_df <- data.frame(i=el[,1], j=el[,2], sn=vals)
  list(graph=g, edge_table=edge_df)
}

# —————— Main ——————

# parameters
n     <- 1000 #3000, 5000
P     <- matrix(0.07, nrow=2, ncol=2); diag(P) <- 0.1
gamma <- c(0.5, 0.5)
alpha1 <- 1; alpha2 <- 1
seed <- 123


# generate DCBM
res1 <- gen_thm_dcsbm(n, gamma, alpha1, alpha2, P, seed=seed)

# dcrc_star
res2 <- compute_all_dcrc_star(
  res1$graph, V(res1$graph)$theta,
  res1$comm_mat, P, V(res1$graph)$membership
)

# sn
res3 <- compute_all_sn(
  res2$graph, V(res2$graph)$theta,
  res1$comm_mat, P, V(res1$graph)$membership
)

# linear curvature
final_g <- cal_linear_curv(res3$graph)

# build edges_df
edges_df <- as_data_frame(final_g, what="edges")
edges_df$thm4 <- (edges_df$nrc - edges_df$dcrc_star) / (edges_df$dcrc_star * edges_df$sn)
edges_df$thm5 <- (edges_df$nrc - edges_df$dcrc_star) / (edges_df$nrc / sqrt(edges_df$minni))

# save .rds
rds_file <- sprintf("edges_df_n_%d.rds", n)
saveRDS(edges_df, file=rds_file)

# compute and save summary
df1 <- data.frame(x = edges_df$thm4)
df2 <- data.frame(x = edges_df$thm5)

sum1 <- list(
  mean = mean(df1$x),
  sd   = sd(df1$x)
)

sum2 <- list(
  mean = mean(df2$x),
  sd   = sd(df2$x)
)

txt_file <- sprintf("summary_n_%d.txt", n)
sink(txt_file)
cat("=== thm4 ===\n")
cat("Mean: ", sum1$mean,   "\n")
cat("SD:   ", sum1$sd,     "\n")


cat("\n=== thm5 ===\n")
cat("Mean: ", sum2$mean,   "\n")
cat("SD:   ", sum2$sd,     "\n")
sink()

message("Done. Saved:\n - ", rds_file, "\n - ", txt_file, "\n")
