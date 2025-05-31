# Load necessary libraries
library(ggplot2)
library(patchwork)
library(ggpattern)  # For pattern features in ggplot2
library(stringr)

# Create the data frame based on your updated LaTeX table
data <- data.frame(
  Method = rep(c(
    "MVCNN", "MVCNN-Explainer",
    "RotationNet", "RotationNet-Explainer",
    "FG3DNet", "FG3DNet-Explainer"
  ), each = 3),
  
  Classes = rep(c("Airplane", "Chair", "Car"), times = 6),
  
  Class_Accuracy = c(
    82.57, 76.27, 71.88,
    84.92, 78.15, 73.56,
    89.11, 78.45, 72.53,
    90.54, 80.12, 74.31,
    89.44, 80.04, 74.03,
    90.83, 81.69, 76.19
  ),
  
  Instance_Accuracy = c(
    91.11, 82.90, 76.12,
    91.94, 84.23, 77.87,
    92.76, 82.07, 75.59,
    93.85, 83.79, 77.26,
    93.99, 83.94, 79.47,
    94.72, 85.28, 81.29
  ),
  Class_Accuracy_std = c(
    0.69, 1.04, 1.16,
    0.62, 0.93, 1.06,
    0.57, 0.72, 0.95,
    0.54, 0.68, 0.87,
    0.48, 0.65, 0.84,
    0.45, 0.61, 0.78
  ),
  
  Instance_Accuracy_std = c(
    0.63, 0.78, 1.13,
    0.52, 0.68, 1.02,
    0.53, 0.71, 0.96,
    0.47, 0.59, 0.81,
    0.45, 0.63, 0.83,
    0.39, 0.56, 0.74
  )
)

# Set factor levels for consistent ordering
data$Method <- factor(data$Method, levels = c(
  "MVCNN", "MVCNN-Explainer",
  "RotationNet", "RotationNet-Explainer",
  "FG3DNet", "FG3DNet-Explainer"
))
data$Classes <- factor(data$Classes, levels = c("Airplane", "Chair", "Car"))

# Define explicit pattern values for each method
pattern_values <- c(
  "MVCNN" = "stripe", 
  "MVCNN-Explainer" = "none", 
  "RotationNet" = "stripe", 
  "RotationNet-Explainer" = "none", 
  "FG3DNet" = "stripe", 
  "FG3DNet-Explainer" = "none"
)

# Plot: Class Accuracy with patterns and error bars
plot_class <- ggplot(data, aes(x = Classes, y = Class_Accuracy, fill = Method, pattern = Method)) +
  geom_bar_pattern(
    stat = "identity", 
    position = position_dodge(width = 0.8), 
    width = 0.7,
    pattern_density = 0.005, 
    pattern_spacing = 0.012, 
    pattern_fill = "white",
    pattern_color = "white",
    pattern_angle = 45
  ) +
  geom_errorbar(
    aes(ymin = Class_Accuracy - Class_Accuracy_std, ymax = Class_Accuracy + Class_Accuracy_std),
    position = position_dodge(width = 0.8),
    width = 0.25,
    linewidth = 0.4
  ) +
  coord_cartesian(ylim = c(70, 95)) +
  labs(title = "Class Accuracy", y = "", x = "") +
  theme_minimal() +
  theme(
    axis.ticks = element_line(color = "black", size = 0.6),
    axis.ticks.length = unit(0.15, "cm"),
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c(
    "MVCNN" = "#19415c", 
    "MVCNN-Explainer" = "#19415c", 
    "RotationNet" = "#7cac94", 
    "RotationNet-Explainer" = "#7cac94",
    "FG3DNet" = "#c67b74", 
    "FG3DNet-Explainer" = "#c67b74"
  )) +
  scale_pattern_manual(values = pattern_values) +
  scale_x_discrete(expand = c(0.25, 0.25))

# Plot: Instance Accuracy with patterns and error bars
plot_instance <- ggplot(data, aes(x = Classes, y = Instance_Accuracy, fill = Method, pattern = Method)) +
  geom_bar_pattern(
    stat = "identity", 
    position = position_dodge(width = 0.8), 
    width = 0.7,
    pattern_density = 0.005, 
    pattern_spacing = 0.012, 
    pattern_fill = "white",
    pattern_color = "white",
    pattern_angle = 45
  ) +
  geom_errorbar(
    aes(ymin = Instance_Accuracy - Instance_Accuracy_std, ymax = Instance_Accuracy + Instance_Accuracy_std),
    position = position_dodge(width = 0.8),
    width = 0.25,
    linewidth = 0.4
  ) +
  coord_cartesian(ylim = c(70, 95)) +
  labs(title = "Instance Accuracy", y = "", x = "") +
  theme_minimal() +
  theme(
    axis.ticks = element_line(color = "black", size = 0.6),
    axis.ticks.length = unit(0.15, "cm"),
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    legend.text = element_text(size = 12, face = "bold"),
    legend.title = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c(
    "MVCNN" = "#19415c", 
    "MVCNN-Explainer" = "#19415c", 
    "RotationNet" = "#7cac94", 
    "RotationNet-Explainer" = "#7cac94",
    "FG3DNet" = "#c67b74", 
    "FG3DNet-Explainer" = "#c67b74"
  )) +
  scale_pattern_manual(values = pattern_values) +
  scale_x_discrete(expand = c(0.25, 0.25))

# Combine both plots side by side with a shared legend
combined_plot <- (plot_class + plot_instance + plot_layout(ncol = 2, guides = "collect")) &
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.justification = "center",
    legend.text = element_text(size = 9, face = "bold"),
    legend.title = element_blank()
  )

# Save the combined plot to a PDF file
# ggsave("FG3D.pdf", plot = combined_plot, width = 6, height = 4)
ggsave("FG3D.pdf", plot = combined_plot, width = 8, height = 4)
