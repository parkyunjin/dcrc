library(ggplot2)

# One-hot vector for community of i
one_hot <- function(z, K) {
  h <- rep(0, K)
  h[z] <- 1
  return(h)
}

# Compute eta and G
compute_eta_G <- function(theta, Pi, K) {
  n <- length(theta)
  theta1_norm <- sum(theta)
  theta2_norm <- sum(theta^2)
  
  eta <- K * (1 / theta1_norm) * t(Pi) %*% theta
  G <- (1 / theta2_norm) * t(Pi) %*% diag(theta^2) %*% Pi
  
  list(eta = eta, G = G)
}

# Compute expected n_i
compute_En_i <- function(theta, z, P) {
  K <- max(z)
  n <- length(theta)
  Pi <- model.matrix(~ factor(z) - 1)  # community indicator matrix
  norms <- compute_eta_G(theta, Pi, K)
  eta <- norms$eta
  
  theta1_norm <- sum(theta)
  
  En_i <- numeric(n)
  for (i in 1:n) {
    ez <- one_hot(z[i], K)
    eta_term <- as.numeric(t(ez) %*% P %*% eta)
    En_i[i] <- (1 / K) * theta[i] * theta1_norm * eta_term - theta[i]^2
    #En_i[i] <- (1 / K) * theta[i] * theta1_norm * eta_term
  }
  return(En_i)
}

# Compute expected n_ij | A_ij = 1
compute_En_ij <- function(i, j, theta, z, P) {
  K <- max(z)
  Pi <- model.matrix(~ factor(z) - 1)
  norms <- compute_eta_G(theta, Pi, K)
  G <- norms$G
  theta2_norm <- sum(theta^2)
  
  ez_i <- one_hot(z[i], K)
  ez_j <- one_hot(z[j], K)
  
  triple_term <- as.numeric(t(ez_i) %*% P %*% G %*% P %*% ez_j)
  penalty_term <- (theta[i]^2 + theta[j]^2) * P[z[i], z[j]]
  
  En_ij <- theta[i] * theta[j] * theta2_norm * triple_term - theta[i] * theta[j] * penalty_term
  #En_ij <- theta[i] * theta[j] * theta2_norm * triple_term 
  return(En_ij)
}

FRC_diff <- function(i, j, k, theta, z, P){
  Eni_vector <- compute_En_i(theta, z, P)
  Enj <- Eni_vector[j]
  Enk <- Eni_vector[k]
  
  Enij <- compute_En_ij(i, j, theta, z, P)
  Enik <- compute_En_ij(i, k, theta, z, P)
  
  result <- -Enj + Enk + 3*Enij - 3*Enik
  
  return(result)
}

LRC_diff <- function(i, j, k, theta, z, P){
  Eni_vector <- compute_En_i(theta, z, P)
  Eni <- Eni_vector[i]
  Enj <- Eni_vector[j]
  Enk <- Eni_vector[k]
  
  Enij <- compute_En_ij(i, j, theta, z, P)
  Enik <- compute_En_ij(i, k, theta, z, P)
  
  result <- 2/Enj - 2/Enk + 2*Enij/max(Eni, Enj) + Enij/min(Eni, Enj) - 2*Enik/max(Eni, Enk) - Enik/min(Eni, Enk)
  
  return(result)
}

DCRC_diff <- function(i, j, k, theta, z, P){
  Eni_vector <- compute_En_i(theta, z, P)
  Eni <- Eni_vector[i]
  Enj <- Eni_vector[j]
  Enk <- Eni_vector[k]
  
  Enij <- compute_En_ij(i, j, theta, z, P)
  Enik <- compute_En_ij(i, k, theta, z, P)
  
  result <-  Enij/(Eni*Enj) - Enik/(Eni*Enk)
  
  return(result)
}

library(ggplot2)
library(tidyr)
library(dplyr)

plot_all_diffs <- function(theta_range = seq(0.1, 3.0, length.out = 1000), save_path = "curvature_diffs.pdf") {
  n <- 100
  K <- 2
  z <- c(rep(1, n), rep(2, n))
  P <- matrix(c(1, 0.5,
                0.5, 1), nrow = K, byrow = TRUE)
  
  i <- 1     # node i in community 1
  j <- 2     # another node in same community
  k <- n + 1 # node in other community (community 2)
  
  results <- data.frame()
  
  for (val in theta_range) {
    theta <- rep(1, 2 * n)
    theta[n + 1] <- val  # vary this one only
    
    frc_val <- FRC_diff(i, j, k, theta, z, P) / 200
    lrc_val <- LRC_diff(i, j, k, theta, z, P)
    dcrc_val <- DCRC_diff(i, j, k, theta, z, P) * 200
    
    results <- rbind(results, data.frame(
      theta_nplus1 = val,
      FRC = frc_val,
      LRC = lrc_val,
      DCRC = dcrc_val
    ))
  }
  
  results_long <- pivot_longer(results, cols = c(FRC, LRC, DCRC),
                               names_to = "Metric", values_to = "Value")
  
  results_long <- results_long %>%
    mutate(Metric = recode(Metric,
                           FRC = "FRC",
                           LRC = "LRC",
                           DCRC = "DCRC"))
  

  p <- ggplot(results_long, aes(x = theta_nplus1, y = Value, color = Metric)) +
    geom_line(linewidth = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 2) +
    #geom_point() +
    labs(x = "Degree of heterogeneity",
         y = "(Within) - (across) curvature",
         color = "Curvature") +
    theme_minimal()+
    theme(axis.text = element_text(size = 26),          
          axis.title = element_text(size = 28),         
          legend.position = c(0.87, 0.85),
          legend.background = element_rect(fill = "white", color = "gray"),
          legend.title = element_text(size = 26),
          legend.text = element_text(size = 25),
          legend.key.width = unit(2.5, "cm"))+
    scale_color_manual( 
      values = c("FRC" = "#00BA38", "DCRC" = "#F8766D", "LRC" = "#619CFF"),# Assign colors
      breaks = c("FRC", "LRC", "DCRC")  # Order in legend
    )
  
  ggsave(save_path, p, width = 10, height = 6)
  
  return(results)
}

# Run and save
plot_all_diffs()
