# Load necessary libraries
library(ggplot2)
library(patchwork)  # For combining plots
library(gridExtra)

# Create the updated data frame
data <- data.frame(
  Method = rep(c("GradNorm", "Gram", "ODIN", "MDS", "KNN", "Explainer"), each = 4),
  Dataset = rep(c("iNaturalist", "Textures", "OpenImage-O", "Mean"), times = 6),
  
  AUROC_mean = c(86.06, 86.07, 80.66, 84.26,
                 65.30, 80.53, 67.72, 71.19,
                 94.37, 90.65, 90.11, 91.71,
                 75.03, 79.25, 69.87, 74.72,
                 93.99, 95.29, 90.19, 93.16,
                 95.48, 96.81, 93.74, 95.33),
  
  AUROC_sd = c(1.90, 0.36, 1.09, 0.87,
               0.20, 0.37, 0.58, 0.24,
               0.41, 0.20, 0.15, 0.19,
               0.76, 0.33, 0.14, 0.26,
               0.36, 0.02, 0.32, 0.22,
               0.26, 0.04, 0.17, 0.13),

  FPR_95_mean = c(61.31, 66.88, 71.16, 66.45,
                  85.54, 80.87, 86.66, 84.36,
                  22.39, 42.99, 37.30, 34.23,
                  58.53, 58.16, 68.29, 61.66,
                  24.46, 24.45, 32.90, 27.27,
                  18.53, 19.33, 26.55, 21.47),

  FPR_95_sd = c(2.86, 3.59, 0.23, 0.22,
                0.40, 1.20, 1.27, 0.78,
                1.87, 1.56, 0.59, 1.05,
                0.75, 0.84, 0.28, 0.27,
                1.06, 0.29, 1.12, 0.75,
                0.75, 0.19, 0.83, 0.56)
)

# Set factor levels for correct plot ordering
data$Method <- factor(data$Method, levels = c("GradNorm", "Gram", "ODIN", "MDS", "KNN", "Explainer"))
data$Dataset <- factor(data$Dataset, levels = c("iNaturalist", "Textures", "OpenImage-O", "Mean"))

# Create AUROC Plot with Standard Deviation
plot_auroc <- ggplot(data, aes(x = Dataset, y = AUROC_mean, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = AUROC_mean - AUROC_sd, ymax = AUROC_mean + AUROC_sd),
                position = position_dodge(width = 0.8), width = 0.25) +
  coord_cartesian(ylim = c(60, 100)) +
  labs(title = "AUROC", y = "", x = "") +
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
  scale_x_discrete(expand = c(0.15, 0.15))

# Create FPR@95 Plot with Standard Deviation
plot_fpr95 <- ggplot(data, aes(x = Dataset, y = FPR_95_mean, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = FPR_95_mean - FPR_95_sd, ymax = FPR_95_mean + FPR_95_sd),
                position = position_dodge(width = 0.8), width = 0.25) +
  coord_cartesian(ylim = c(10, 100)) +
  labs(title = "FPR@95", y = "", x = "", fill = NULL) +
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
  scale_x_discrete(expand = c(0.15, 0.15))

# Combine both plots side by side
combined_plot <- (plot_auroc + plot_fpr95 + plot_layout(ncol = 2, guides = "collect")) &
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.justification = "center",
        legend.text = element_text(size = 14, face = "bold"),
        legend.title = element_blank()) &
  guides(fill = guide_legend(nrow = 1))

# Save the combined plot
ggsave("OOD-ImageNet200.pdf", plot = combined_plot, width = 11, height = 4)
