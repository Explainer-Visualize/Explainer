# Load necessary libraries
library(ggplot2)
library(patchwork)  # For combining plots
library(gridExtra)

# Create the data frame
data <- data.frame(
  Method = rep(c("Naive Bayes", "Logistic Regression", "Support Vector", "Explainer"), each = 2),
  Classes = rep(c("General Topic", "Subtopic"), times = 4),
  Precision = c(0.674, 0.521, 0.752, 0.590, 0.807, 0.634, 0.879, 0.696),
  Recall = c(0.684, 0.513, 0.736, 0.592, 0.802, 0.658, 0.864, 0.721),
  F1 = c(0.673, 0.492, 0.741, 0.584, 0.803, 0.619, 0.872, 0.683),
  Precision_std = c(0.014, 0.016, 0.018, 0.013, 0.012, 0.010, 0.009, 0.007),
  Recall_std = c(0.016, 0.015, 0.017, 0.015, 0.013, 0.011, 0.007, 0.008),
  F1_std = c(0.018, 0.019, 0.019, 0.016, 0.014, 0.013, 0.009, 0.006)
)

# Set factor levels for ordering in the plot
data$Method <- factor(data$Method, levels = c("Naive Bayes", "Logistic Regression", "Support Vector", "Explainer"))
data$Classes <- factor(data$Classes, levels = c("General Topic", "Subtopic"))

# Precision Plot
plot_precision <- ggplot(data, aes(x = Classes, y = Precision, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.55), width = 0.50) +
  geom_errorbar(aes(ymin = Precision - Precision_std, ymax = Precision + Precision_std),
                position = position_dodge(width = 0.55),
                width = 0.15,
                linewidth = 0.55) +
  coord_cartesian(ylim = c(0.5, 0.9)) +
  labs(title = "Precision", y = "", x = "") +
  scale_fill_manual(values = c(
    "Naive Bayes" = "#19415c",
    "Logistic Regression"="#e6d3a3",
    "Support Vector" = "#32747a",
    "Explainer" = "#c67b74"
  )) +
  theme_minimal() +
  theme(
    panel.spacing = unit(2, "lines"),
    axis.ticks = element_line(color = "black", size = 0.6),
    axis.ticks.length = unit(0.15, "cm"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    legend.position = "none"
  ) +
  scale_x_discrete(expand = c(0.25, 0.25))

# Recall Plot
plot_recall <- ggplot(data, aes(x = Classes, y = Recall, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.55), width = 0.50) +
  geom_errorbar(aes(ymin = Recall - Recall_std, ymax = Recall + Recall_std),
                position = position_dodge(width = 0.55),
                width = 0.15,
                linewidth = 0.55) +
  coord_cartesian(ylim = c(0.5, 0.9)) +
  labs(title = "Recall", y = "", x = "", fill = NULL) +
  scale_fill_manual(values = c(
    "Naive Bayes" = "#19415c",
    "Logistic Regression"="#e6d3a3",
    "Support Vector" = "#32747a",
    "Explainer" = "#c67b74"
  )) +
  theme_minimal() +
  theme(
    panel.spacing = unit(2, "lines"),
    axis.ticks = element_line(color = "black", size = 0.6),
    axis.ticks.length = unit(0.15, "cm"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    legend.text = element_text(size = 12, face = "bold"),
    # legend.title = element_text(size = 16),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.justification = "center"
  ) +
  scale_x_discrete(expand = c(0.25, 0.25))

# F1 Score Plot
plot_f1 <- ggplot(data, aes(x = Classes, y = F1, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.55), width = 0.50) +
  geom_errorbar(aes(ymin = F1 - F1_std, ymax = F1 + F1_std),
                position = position_dodge(width = 0.55),
                width = 0.15,
                linewidth = 0.55) +
  coord_cartesian(ylim = c(0.5, 0.9)) +
  labs(title = "f1 Score", y = "", x = "") +
  scale_fill_manual(values = c(
    "Naive Bayes" = "#19415c",
    "Logistic Regression"="#e6d3a3",
    "Support Vector" = "#32747a",
    "Explainer" = "#c67b74"
  )) +
  theme_minimal() + 
  theme(
    panel.spacing = unit(2, "lines"),
    axis.ticks = element_line(color = "black", size = 0.6),
    axis.ticks.length = unit(0.15, "cm"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    legend.position = "none"
  ) +
  scale_x_discrete(expand = c(0.25, 0.25))

# Combine the plots horizontally using patchwork
combined_plot <- plot_precision + plot_recall + plot_f1 + plot_layout(ncol = 3)

# Save the combined plot
ggsave("MN-DS.pdf", plot = combined_plot, width = 10, height = 4)
