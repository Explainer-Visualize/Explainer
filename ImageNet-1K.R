library(ggplot2)
library(dplyr)
library(tidyr)

# Dataset without Backbone
data <- data.frame(
  Method = c("ResNet", "Guided", "HiMulConE", "Explainer"),
  Level = c(1, 2, 3, 4),
  `X1` = c(96.52, 97.03, 97.12, 97.42),
  `X2` = c(94.23, 94.62, 94.78, 95.06),
  `X3` = c(93.17, 94.10, 94.43, 94.76),
  `X4` = c(92.06, 92.68, 92.79, 92.93),
  `X5` = c(91.25, 92.09, 92.42, 92.88),
  `X6` = c(89.33, 89.95, 90.08, 90.22),
  `X7` = c(90.27, 91.09, 91.23, 92.03), 
  `X8` = c(87.28, 87.91, 88.19, 88.54),
  `X9` = c(84.49, 85.26, 85.58, 86.65),
  `X10` = c(85.67, 86.37, 86.94, 87.39),
  `X11` = c(83.75, 84.76, 85.22, 85.92),
  `X12` = c(76.55, 76.05, 77.52, 78.26)
)

# Reshape to long format
data_long <- pivot_longer(data, cols = starts_with("X"), names_to = "Index", values_to = "Score")

# Clean index labels
data_long$Index <- gsub("X", "", data_long$Index)

# Preserve order
data_long$Method <- factor(data_long$Method, levels = c("ResNet", "Guided", "HiMulConE", "Explainer"))
data_long$Index <- factor(data_long$Index, levels = as.character(1:12))

# Bar chart
new_plot <- ggplot(data_long, aes(x = Index, y = Score, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  labs(title = "", y = "Top-1 Accuraccy", x = "# of Hierarchical Level", fill = NULL) +
  theme_minimal() +
  coord_cartesian(ylim = c(75, 100)) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(
    axis.ticks = element_line(color = "black", size = 1),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 28, face = "bold"),
    axis.text.y = element_text(size = 26, face = "bold"),
    axis.title.x = element_text(size = 28, face = "bold"),
    axis.title.y = element_text(size = 28, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 28, face = "bold"),
    legend.text = element_text(size = 26),
    legend.title = element_text(size = 26),
    legend.spacing.x = unit(3, "cm"),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    legend.position = c(0.5, 0.95),
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  ) +
  scale_fill_manual(values = c(
    "ResNet" = "#19415c",
    "Guided" = "#e6d3a3",
    "HiMulConE" = "#32747a",
    "Explainer" = "#c67b74"
  ))

# Save it
ggsave("ImageNet-1K.pdf", plot = new_plot, width = 24, height = 8)
