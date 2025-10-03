library(ggplot2)
library(dplyr)
library(stringr)

folder_path <- "/results" 
file_list <- list.files(folder_path, pattern = "eval_.*_permloss.txt", full.names = TRUE)

extract_metric <- function(fname) str_match(fname, "eval_(.*?)_.*?_permloss")[, 2]
extract_algorithm <- function(fname) str_match(fname, "eval_.*?_(.*?)_permloss")[, 2]

all_data <- lapply(file_list, function(file) {
  df <- read.table(file, header = FALSE, stringsAsFactors = FALSE)
  colnames(df) <- c("filename", "value")
  df$p <- as.numeric(str_match(basename(df$filename), "_p([0-9]+)_xint")[,2])
  df$metric <- extract_metric(basename(file))
  df$algorithm <- extract_algorithm(basename(file))
  return(df)
})

combined_df <- bind_rows(all_data)

best_df <- combined_df %>%
  group_by(algorithm, metric) %>%
  slice_min(order_by = value, n = 1) %>%
  ungroup() %>%
  select(algorithm, metric, p, value)

best_df_tagged <- best_df %>%
  mutate(is_best = TRUE)

combined_df <- combined_df %>%
  left_join(best_df_tagged, 
            by = c("algorithm", "metric", "p", "value")) %>%
  mutate(is_best = ifelse(is.na(is_best), FALSE, is_best))



p <- ggplot(combined_df, aes(x = p, y = value, color = metric)) +
  geom_line(aes(group = metric)) +
  scale_size_manual(values = c("TRUE" = 4, "FALSE" = 2), guide = "none") +
  facet_wrap(~algorithm, scales = "free_x") +
  labs(title = "Performance Comparison of Different Curvatures by Algorithm",
       x = "Percentile Threshold", y = "Performance", color = "Metric") +
  theme_minimal()

setwd("/results_percentile")

ggsave("algorithm_performance_plot.pdf", plot = p, width = 10, height = 6)

write.table(best_df, file = "best_performance_summary.txt", row.names = FALSE, quote = FALSE, sep = "\t")

file_list <- list.files(folder_path, pattern = "eval_nrc_.*_permloss.txt", full.names = TRUE)

extract_algorithm <- function(fname) {
  str_match(fname, "eval_nrc_(.*?)_permloss\\.txt")[,2]
}

all_data <- lapply(file_list, function(file) {
  df <- read.table(file, header = FALSE, stringsAsFactors = FALSE)
  colnames(df) <- c("filename", "value")
  

  df$percentile <- as.numeric(str_match(basename(df$filename), "_p([0-9]+)_xint")[,2])
  
  df$algorithm <- extract_algorithm(basename(file))
  
  return(df)
})


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


p2 <- ggplot(combined_df, aes(x = percentile, y = delta, color = algorithm)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "NRC Performance Improvement from Baseline",
    x = "Percentile Threshold", y = "Change in Mismatch Number",
    color = "Algorithm"
  ) +
  theme_minimal()




ggsave("nrc_performance_plot.pdf", plot = p2, width = 10, height = 6)
