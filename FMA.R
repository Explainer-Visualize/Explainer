# Load necessary libraries
library(ggplot2)
library(patchwork)
library(gridExtra)

# Updated data frame based on the LaTeX-style table
data <- data.frame(
  Method = rep(c("Flat Classifier", "Hybrid Classifier", "DenseNet-121", "DenseNet-Explainer"), each = 2),
  Classes = rep(c("Non-leaf Genre", "Leaf Genre"), times = 4),
  Precision = c(0.8662, 0.6762, 0.8971, 0.7579, 0.9124, 0.7923, 0.9302, 0.8128),
  Recall = c(0.8739, 0.6610, 0.8913, 0.7738, 0.8956, 0.8012, 0.9116, 0.8104),
  F1 = c(0.8615, 0.6688, 0.8833, 0.7657, 0.9037, 0.7867, 0.9228, 0.8075),
  Precision_std = c(0.0132, 0.0212, 0.0114, 0.0158, 0.0092, 0.0143, 0.0041, 0.0078),
  Recall_std = c(0.0119, 0.0227, 0.0092, 0.0153, 0.0081, 0.0113, 0.0052, 0.0068),
  F1_std = c(0.0142, 0.0189, 0.0123, 0.0142, 0.0077, 0.0132, 0.0039, 0.0082)
)

# Set factor levels to control order
data$Method <- factor(data$Method, levels = c("Flat Classifier", "Hybrid Classifier", "DenseNet-121", "DenseNet-Explainer"))
data$Classes <- factor(data$Classes, levels = c("Non-leaf Genre", "Leaf Genre"))

# Precision Plot
plot_precision <- ggplot(data, aes(x = Classes, y = Precision, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.55), width = 0.50) +
  geom_errorbar(aes(ymin = Precision - Precision_std, ymax = Precision + Precision_std),
                position = position_dodge(width = 0.55), width = 0.15, linewidth = 0.5) +
  coord_cartesian(ylim = c(0.65, 0.95)) +
  labs(title = "Precision", y = "", x = "") +
  theme_minimal() +
  theme(
    panel.spacing = unit(2, "lines"),
    axis.ticks = element_line(color = "black", size = 0.6),
    axis.ticks.length = unit(0.15, "cm"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("#19415c", "#e6d3a3", "#32747a", "#c67b74")) +
  scale_x_discrete(expand = c(0.25, 0.25))

# Recall Plot
plot_recall <- ggplot(data, aes(x = Classes, y = Recall, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.55), width = 0.50) +
  geom_errorbar(aes(ymin = Recall - Recall_std, ymax = Recall + Recall_std),
                position = position_dodge(width = 0.55), width = 0.15, linewidth = 0.5) +
  coord_cartesian(ylim = c(0.65, 0.95)) +
  labs(title = "Recall", y = "", x = "", fill = NULL) +
  theme_minimal() +
  theme(
    panel.spacing = unit(2, "lines"),
    axis.ticks = element_line(color = "black", size = 0.6),
    axis.ticks.length = unit(0.15, "cm"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    legend.text = element_text(size = 12, face = "bold"),
    # legend.title = element_text(size = 10),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.justification = "center"
  ) +
  scale_fill_manual(values = c("#19415c", "#e6d3a3", "#32747a", "#c67b74")) +
  scale_x_discrete(expand = c(0.25, 0.25))

# F1 Score Plot
plot_f1 <- ggplot(data, aes(x = Classes, y = F1, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.55), width = 0.50) +
  geom_errorbar(aes(ymin = F1 - F1_std, ymax = F1 + F1_std),
                position = position_dodge(width = 0.55), width = 0.15, linewidth = 0.5) +
  coord_cartesian(ylim = c(0.65, 0.95)) +
  labs(title = "F1 Score", y = "", x = "") +
  theme_minimal() +
  theme(
    panel.spacing = unit(2, "lines"),
    axis.ticks = element_line(color = "black", size = 0.6),
    axis.ticks.length = unit(0.15, "cm"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("#19415c", "#e6d3a3", "#32747a", "#c67b74")) +
  scale_x_discrete(expand = c(0.25, 0.25))

# Combine the plots horizontally using patchwork
combined_plot <- plot_precision + plot_recall + plot_f1 + plot_layout(ncol = 3)

# Save the combined plot as PDF
ggsave("FMA.pdf", plot = combined_plot, width = 10, height = 4)
