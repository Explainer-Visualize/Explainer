# Load necessary libraries
library(ggplot2)
library(patchwork)  # For combining plots
library(gridExtra)

# Create the data frame with updated data (including standard deviation)
data <- data.frame(
  Method = rep(c("GradNorm", "Gram", "ODIN", "MDS", "KNN", "Explainer"), each = 5),
  Dataset = rep(c("MNIST", "SVHN", "Textures", "Places365", "Mean"), times = 6),
  AUROC_mean = c(63.72, 53.91, 52.07, 60.50, 57.55, 
                 72.64, 91.52, 62.34, 60.44, 71.73, 
                 95.24, 84.58, 86.94, 85.07, 87.96, 
                 90.10, 91.18, 92.69, 84.90, 89.72, 
                 94.26, 92.67, 93.16, 91.77, 92.96, 
                 94.58, 93.41, 93.68, 92.65, 93.58),
  AUROC_sd = c(7.37, 6.36, 4.09, 5.33, 3.22, 
               2.34, 4.45, 8.27, 3.41, 3.20, 
               1.96, 0.77, 2.26, 1.24, 0.61, 
               2.41, 0.47, 1.06, 2.54, 1.36, 
               0.38, 0.30, 0.24, 0.23, 0.14, 
               0.27, 0.24, 0.19, 0.18, 0.10),
  FPR_95_mean = c(85.41, 91.65, 98.09, 92.46, 91.90, 
                  70.30, 33.91, 94.64, 90.49, 72.34, 
                  23.83, 68.61, 67.70, 70.36, 57.62, 
                  27.30, 25.96, 27.94, 47.67, 32.22, 
                  20.05, 22.60, 24.06, 30.38, 24.27, 
                  17.35, 18.64, 18.12, 23.21, 19.33),
  FPR_95_sd = c(4.85, 2.42, 0.49, 2.28, 2.23, 
                8.96, 17.35, 2.71, 1.93, 6.73, 
                12.34, 0.52, 11.06, 6.96, 4.24, 
                3.55, 2.52, 4.20, 4.54, 3.40, 
                1.36, 1.26, 0.55, 0.63, 0.40, 
                1.02, 0.94, 0.32, 0.47, 0.29)
)

# Set factor levels for ordering in the plot
data$Method <- factor(data$Method, levels = c("GradNorm", "Gram", "ODIN", "MDS", "KNN", "Explainer"))
data$Dataset <- factor(data$Dataset, levels = c("MNIST", "SVHN", "Textures", "Places365", "Mean"))

# Create AUROC Plot with Standard Deviation
plot_auroc <- ggplot(data, aes(x = Dataset, y = AUROC_mean, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = AUROC_mean - AUROC_sd, ymax = AUROC_mean + AUROC_sd),
                position = position_dodge(width = 0.8), width = 0.25) +
  coord_cartesian(ylim = c(50, 100)) +
  labs(title = "AUROC",
       y = "",
       x = "") +
  theme_minimal() +
  theme(
    axis.ticks = element_line(color = "black", size = 0.6),
    axis.ticks.length = unit(0.15, "cm"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c(
    "#19415c", "#e6d3a3", "#7cac94",
    "#32747a", "#74476f", "#c67b74"
  )) +
  scale_x_discrete(expand = c(0.1, 0.1)) 

# Create FPR@95 Plot with Standard Deviation
plot_fpr95 <- ggplot(data, aes(x = Dataset, y = FPR_95_mean, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = FPR_95_mean - FPR_95_sd, ymax = FPR_95_mean + FPR_95_sd),
                position = position_dodge(width = 0.8), width = 0.25) +
  coord_cartesian(ylim = c(10, 100)) +
  labs(title = "FPR@95",
       y = "",
       x = "", fill = NULL) +
  theme_minimal() +
  theme(
    axis.ticks = element_line(color = "black", size = 0.6),
    axis.ticks.length = unit(0.15, "cm"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 14),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c(
    "#19415c", "#e6d3a3", "#7cac94",
    "#32747a", "#74476f", "#c67b74"
  )) +
  scale_x_discrete(expand = c(0.1, 0.1))

# Combine both plots side by side
combined_plot <- (plot_auroc + plot_fpr95 + plot_layout(ncol = 2, guides = "collect")) &
  theme(legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.justification = "center",
    legend.text = element_text(size = 14, face = "bold"),
    legend.title = element_blank()
  ) &
  guides(fill = guide_legend(nrow = 1))  # Force legend into one row

# Save the combined plot with the specified size
ggsave("OOD-cifar10.pdf", plot = combined_plot, width = 11, height = 4)
