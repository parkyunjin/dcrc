library(ggplot2)
library(dplyr)
library(stringr)

# Set the path to your folder
folder_path <- "/results"  # <-- CHANGE THIS
file_list <- list.files(folder_path, pattern = "eval_.*_permloss.txt", full.names = TRUE)

# Helper functions to extract parts of the filename
extract_metric <- function(fname) str_match(fname, "eval_(.*?)_.*?_permloss")[, 2]
extract_algorithm <- function(fname) str_match(fname, "eval_.*?_(.*?)_permloss")[, 2]

# Read all files
all_data <- lapply(file_list, function(file) {
  df <- read.table(file, header = FALSE, stringsAsFactors = FALSE)
  colnames(df) <- c("filename", "value")
  df$p <- as.numeric(str_match(basename(df$filename), "_p([0-9]+)_xint")[,2])
  df$metric <- extract_metric(basename(file))
  df$algorithm <- extract_algorithm(basename(file))
  return(df)
})

# Combine into one dataframe
combined_df <- bind_rows(all_data)

# Get best value per algorithm-metric
best_df <- combined_df %>%
  group_by(algorithm, metric) %>%
  slice_min(order_by = value, n = 1) %>%
  ungroup() %>%
  select(algorithm, metric, p, value)

# Add is_best = TRUE to best_df
best_df_tagged <- best_df %>%
  mutate(is_best = TRUE)

# Join and mark best rows
combined_df <- combined_df %>%
  left_join(best_df_tagged, 
            by = c("algorithm", "metric", "p", "value")) %>%
  mutate(is_best = ifelse(is.na(is_best), FALSE, is_best))



# Plot with emphasized best points
p <- ggplot(combined_df, aes(x = p, y = value, color = metric)) +
  geom_line(aes(group = metric)) +
  #geom_point(aes(size = is_best), shape = 21, stroke = 1.2, fill = "white") +
  scale_size_manual(values = c("TRUE" = 4, "FALSE" = 2), guide = "none") +
  facet_wrap(~algorithm, scales = "free_x") +
  labs(title = "Performance Comparison of Different Curvatures by Algorithm",
       x = "Percentile Threshold", y = "Performance", color = "Metric") +
  theme_minimal()

# Set where to save the files
setwd("/results_percentile")

# Save plot as PDF
ggsave("algorithm_performance_plot.pdf", plot = p, width = 10, height = 6)

# Save best performance summary
write.table(best_df, file = "best_performance_summary.txt", row.names = FALSE, quote = FALSE, sep = "\t")

# List only NRC files
file_list <- list.files(folder_path, pattern = "eval_nrc_.*_permloss.txt", full.names = TRUE)

# Extract algorithm from filename
extract_algorithm <- function(fname) {
  str_match(fname, "eval_nrc_(.*?)_permloss\\.txt")[,2]
}

# Load all NRC result files
all_data <- lapply(file_list, function(file) {
  df <- read.table(file, header = FALSE, stringsAsFactors = FALSE)
  colnames(df) <- c("filename", "value")
  
  # Extract percentile from filename
  df$percentile <- as.numeric(str_match(basename(df$filename), "_p([0-9]+)_xint")[,2])
  
  # Extract algorithm name from file
  df$algorithm <- extract_algorithm(basename(file))
  
  return(df)
})

# Combine
combined_df <- bind_rows(all_data)

combined_df <- combined_df %>%
  group_by(algorithm) %>%
  arrange(percentile, .by_group = TRUE) %>%
  mutate(xrank = row_number()) %>%
  ungroup()

baseline_df <- combined_df %>%
  filter(xrank == 1) %>%
  select(algorithm, baseline_value = value)

combined_df <- combined_df %>%
  left_join(baseline_df, by = "algorithm") %>%
  mutate(delta = value - baseline_value)

# Plot
p2 <- ggplot(combined_df, aes(x = percentile, y = delta, color = algorithm)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "NRC Performance Improvement from Baseline",
    x = "Percentile Threshold", y = "Change in Mismatch Number",
    color = "Algorithm"
  ) +
  theme_minimal()



# Save plot as PDF
ggsave("nrc_performance_plot.pdf", plot = p2, width = 10, height = 6)
