############################################################
# Title: Figure 1 plotting code - Part3.                   #
#                                                          #
#                                                          #
# This code aims to generate graphs used to plot Figure 1. #
############################################################

library(gridExtra)
library(ggplot2)
library(showtext)
library(grid)
library(igraph)


## BALANCED SBM
# Load graph data
balanced_sbm_g <- read_graph("balanced_sbm_curved.graphml", format = "graphml")
# Make edge dataframe
edge_list <- as.data.frame(get.edgelist(balanced_sbm_g))
results1 <- cbind(edge_list, bfc = E(balanced_sbm_g)$bfc,  frc = E(balanced_sbm_g)$frc, lrc = E(balanced_sbm_g)$lrc, nrc = E(balanced_sbm_g)$nrc, same_group = E(balanced_sbm_g)$membership)
colnames(results1) <- c("node_i", "node_j","bfc", "frc", "lrc", "nrc", "same_group")

## UNBALANCED DCBM
# Load graph data
dcsbm_g <- read_graph("dcsbm_curved.graphml", format = "graphml")
# Make edge dataframe
edge_list <- as.data.frame(get.edgelist(dcsbm_g))
results3 <- cbind(edge_list,  bfc = E(dcsbm_g)$bfc, frc = E(dcsbm_g)$frc, lrc = E(dcsbm_g)$lrc, nrc = E(dcsbm_g)$nrc, same_group = E(dcsbm_g)$membership)
colnames(results3) <- c("node_i", "node_j","bfc", "frc", "lrc", "nrc", "same_group")


## PLOTTING

# Import Times New Roman font
showtext_auto()
font_add("Times New Roman", regular = "/usr/share/fonts/urw-base35/NimbusRoman-Regular.t1")

# Add subplot labels
label_theme <- theme(
  plot.title = element_text(
    hjust = -0.1, vjust = 1.5, size = 12
  )
)

# Plot for BALANCED SBM
 p1 <- ggplot(results1, aes(x = bfc, fill = same_group)) +
   geom_histogram(bins = 30, color = "black", alpha = 0.5, position = "identity") +
   labs(
     x = "BFC value",
     y = "Frequency") +
   theme_minimal()+
   scale_fill_manual(values = c("Same" = "blue", "Different" = "red"), guide = "none") +
   #ggtitle("(a)") + label_theme+
   theme(
     axis.title = element_text(size = 32),    # x and y axis titles
     axis.text = element_text(size = 8.5)
   )
   #theme(text = element_text(family = "Times New Roman"))


p2 <- ggplot(results1, aes(x = frc, fill = same_group)) +
  geom_histogram(bins = 30,  color = "black", alpha = 0.5, position = "identity") +
  labs(
    x = "FRC value",
    y = "Frequency") +
  theme_minimal()+
  scale_fill_manual(values = c("Same" = "blue", "Different" = "red"), guide = "none")+
  #ggtitle("(b)") + label_theme+
  theme(
    axis.title = element_text(size = 32),    # x and y axis titles
    axis.text = element_text(size = 8.5)
  )
  #theme(text = element_text(family = "Times New Roman"))


p3 <- ggplot(results1, aes(x = lrc, fill = same_group)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.5, position = "identity") +
  labs(
    x = "LRC value",
    y = "Frequency") +
  theme_minimal()+
  scale_fill_manual(values = c("Same" = "blue", "Different" = "red"), guide = "none")+
  #ggtitle("(c)") + label_theme+
  theme(
    axis.title = element_text(size = 32),    # x and y axis titles
    axis.text = element_text(size = 8.5)
  )
  #theme(text = element_text(family = "Times New Roman"))


p4 <- ggplot(results1, aes(x = nrc, fill = same_group)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.5, position = "identity") +
  labs(
    x = "DCRC value",
    y = "Frequency") +
  theme_minimal()+
  scale_fill_manual(values = c("Same" = "blue", "Different" = "red"), guide = "none")+
  #ggtitle("(d)") + label_theme+
  theme(
    axis.title = element_text(size = 32),    # x and y axis titles
    axis.text = element_text(size = 8.5)
  )
  #theme(text = element_text(family = "Times New Roman"))



# Plot for DCSBM
 p5 <- ggplot(results3, aes(x = bfc, fill = same_group)) +
   geom_histogram(bins = 30, color = "black", alpha = 0.5, position = "identity") +
   labs(
     x = "BFC value",
     y = "Frequency") +
   theme_minimal()+
   scale_fill_manual(values = c("Same" = "blue", "Different" = "red"), guide = "none")+
   #ggtitle("(e)") + label_theme+
   theme(
     axis.title = element_text(size = 32),    # x and y axis titles
     axis.text = element_text(size = 8.5)
   )
   #theme(text = element_text(family = "Times New Roman"))


p6 <- ggplot(results3, aes(x = frc, fill = same_group)) +
  geom_histogram(bins = 30,  color = "black", alpha = 0.5, position = "identity") +
  labs(
    x = "FRC value",
    y = "Frequency") +
  theme_minimal()+
  scale_fill_manual(values = c("Same" = "blue", "Different" = "red"), guide = "none")+
  #ggtitle("(f)") + label_theme+
  theme(
    axis.title = element_text(size = 32),    # x and y axis titles
    axis.text = element_text(size = 8.5)
  )
  #theme(text = element_text(family = "Times New Roman"))


p7 <- ggplot(results3, aes(x = lrc, fill = same_group)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.5, position = "identity") +
  labs(
    x = "LRC value",
    y = "Frequency") +
  theme_minimal()+
  scale_fill_manual(values = c("Same" = "blue", "Different" = "red"), guide = "none")+
  #ggtitle("(g)") + label_theme+
  theme(
    axis.title = element_text(size = 32),    # x and y axis titles
    axis.text = element_text(size = 8.5)
  )
  
  #theme(text = element_text(family = "Times New Roman"))


p8 <- ggplot(results3, aes(x = nrc, fill = same_group)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.5, position = "identity") +
  labs(
    x = "DCRC value",
    y = "Frequency") +
  theme_minimal()+
  scale_fill_manual(values = c("Same" = "blue", "Different" = "red"), guide = "none")+
  #ggtitle("(h)") + label_theme+
  theme(
    axis.title = element_text(size = 32),    # x and y axis titles
    axis.text = element_text(size = 8.5)
  )
  #theme(text = element_text(family = "Times New Roman"))

# Arrange plots in a single window

# Titles as grobs
title0 <- textGrob("SBM", gp = gpar(fontsize = 36, fontface = "bold"))
title1 <- textGrob("DCBM", gp = gpar(fontsize = 36, fontface = "bold"))
pdf("figure_1_two_row.pdf", width = 10, height = 8)
grid.arrange(title0,arrangeGrob(p1, p2, p3, p4, ncol = 4),title1, arrangeGrob(p5, p6, p7, p8, ncol = 4), nrow = 4,heights = c(0.1, 1, 0.1, 1))
dev.off()

pdf("figure_1_one_row.pdf", width = 32, height = 4)

remove_y_axis_title <- function(p) {
  p + theme(axis.title.y = element_blank())
}



grid.arrange(
  arrangeGrob(  
    title0,
    arrangeGrob(p1, p2, p3, p4, ncol = 4),
    nrow = 2,
    heights = c(0.1, 1)
  ),
  arrangeGrob(  
    title1,
    arrangeGrob(p5, p6, p7, p8, ncol = 4),
    nrow = 2,
    heights = c(0.1, 1)
  ),
  ncol = 2
  
)

dev.off()
