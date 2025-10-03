library(igraph)
library(ggplot2)
library(nortest)

# Read data
rds_dir <- "/thm"

rds_files <- list.files(
  path       = rds_dir,
  pattern    = "_\\d+\\.rds$",
  full.names = TRUE
)
if (length(rds_files) == 0) stop("No files matching *_{n}.rds found.")
get_n_from_path <- function(p) {
  as.integer(sub(".*_(\\d+)\\.rds$", "\\1", basename(p)))
}
ns <- vapply(rds_files, get_n_from_path, integer(1))
ord <- order(ns)
rds_files <- rds_files[ord]
ns <- ns[ord]

read_thms <- function(p) {
  obj <- tryCatch(readRDS(p), error = function(e) NULL)
  if (is.null(obj)) return(list(thm4 = numeric(0), thm5 = numeric(0)))
  thm4 <- as.numeric(obj$thm4); thm4 <- thm4[is.finite(thm4)]
  thm5 <- as.numeric(obj$thm5); thm5 <- thm5[is.finite(thm5)]
  list(thm4 = thm4, thm5 = thm5)
}
thms_list <- lapply(rds_files, read_thms)

x4_all <- unlist(lapply(thms_list, `[[`, "thm4")); x4_all <- x4_all[is.finite(x4_all)]
x5_all <- unlist(lapply(thms_list, `[[`, "thm5")); x5_all <- x5_all[is.finite(x5_all)]
h4 <- max(2*IQR(x4_all)/length(x4_all)^(1/3),  diff(range(x4_all))/30)
h5 <- max(2*IQR(x5_all)/length(x5_all)^(1/3),  diff(range(x5_all))/30)
b4_min <- floor(min(x4_all)/h4)*h4; b4_max <- ceiling(max(x4_all)/h4)*h4
b5_min <- floor(min(x5_all)/h5)*h5; b5_max <- ceiling(max(x5_all)/h5)*h5
breaks4 <- seq(b4_min, b4_max, by=h4)
breaks5 <- seq(b5_min, b5_max, by=h5)



# Make histogram
pdf_path <- file.path(rds_dir, "thm4_thm5.pdf")
grDevices::cairo_pdf(filename = pdf_path, width = 18, height = 4,
                     family = "Helvetica", fallback_resolution = 300)

op <- par(
  mfrow = c(1, 6),
  mar = c(3.5, 3.5, 1.8, 0.8),
  mgp = c(2.5, 0.7, 0),
  oma = c(0.5, 0, 2.5, 0),
  tcl = -0.2,
  cex = 0.9,
  cex.lab = 1.5 
)

plot_order <- order(ns)

corner_tag <- function(tag, side = 3, adj = 0, line = 0.5, cex = 1.5, font = 2) {
  mtext(tag, side = side, adj = adj, line = line, cex = cex, font = font)
}
letters_tags <- paste0("(", letters[1:6], ")")

for (i in seq_along(plot_order)) {
  data_idx <- plot_order[i]
  
  # Plot thm4
  hist(thms_list[[data_idx]]$thm4, freq = FALSE, main = "", xlab = expression(T[4*','*n]),
       breaks = breaks4)
  curve(dnorm(x, 0, 1), add = TRUE, lwd = 2)
  corner_tag(letters_tags[2*i - 1])
  
  # Plot thm5
  hist(thms_list[[data_idx]]$thm5, freq = FALSE, main = "", xlab = expression(T[5*','*n]),
       breaks = breaks5,)
  curve(dnorm(x, 0, 1), add = TRUE, lwd = 2)
  corner_tag(letters_tags[2*i])
}

col_labels <- paste0("n = ", ns)
num_pairs <- length(ns)
xpos <- (2 * seq_along(ns) - 1) / (2 * num_pairs)

for (i in seq_along(plot_order)) {
  label_idx <- plot_order[i]
  mtext(col_labels[label_idx], side = 3, outer = TRUE, at = xpos[i],
        line = 1.0, cex = 1.7, font = 2, adj = 0.5)
}


par(op); dev.off()
message("Saved 1x6 histogram to: ", pdf_path)