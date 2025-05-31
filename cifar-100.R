# Load required libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpattern) 
library(patchwork)



# Create the data frame
data <- data.frame(
  Method = rep(c("ResNet-50", "ResNet-Explainer", 
                 "Mixer-B", "Mixer-Explainer",
                 "ViT-B", "ViT-Explainer"), each = 3),
  Classes = rep(c("Class 5", "Class 20", "Class 100"), times = 6),
  Accuracy = c(
    96.13, 92.38, 79.81,     # ResNet-50
    97.85, 93.56, 80.45,     # Explainer+ResNet-50
    95.38, 91.22, 76.44,     # MLPMixer
    96.45, 92.83, 77.03,
    95.71, 91.95, 78.67,     # Swin-S
    96.92, 92.75, 79.32      # Explainer+Swin-S
  ),
  Accuracy_std = c(
    0.21, 0.16, 0.14,     # ResNet-50
    0.17, 0.14, 0.12,     # Explainer+ResNet-50
    0.28, 0.23, 0.19,     # MLPMixer
    0.19, 0.17, 0.13,
    0.26, 0.21, 0.17,     # Swin-S
    0.16, 0.12, 0.10      # Explainer+Swin-S
  )
)

# Set factor levels (optional, for ordering in plot)
data$Method <- factor(data$Method, levels = c(
  "ResNet-50", "ResNet-Explainer", "Mixer-B", "Mixer-Explainer", "ViT-B", "ViT-Explainer"
))

data$Classes <- factor(data$Classes, levels = c("Class 5", "Class 20", "Class 100"))

pattern_values <- c(
  "ResNet-50" = "stripe", 
  "ResNet-Explainer" = "none", 
  "Mixer-B" = "stripe", 
  "Mixer-Explainer" = "none", 
  "ViT-B" = "stripe", 
  "ViT-Explainer" = "none"
)

# Plotting the data
plot_instance <- ggplot(data, aes(x = Classes, y = Accuracy, fill = Method, pattern = Method)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(width = 0.85), width = 0.8,
    pattern_density = 0.005, 
    pattern_spacing = 0.012, 
    pattern_fill = "white",
    pattern_color = "white",
    pattern_angle = 45
  ) +
  geom_errorbar(
    aes(ymin = Accuracy - Accuracy_std, ymax = Accuracy + Accuracy_std),
    position = position_dodge(width = 0.85),
    width = 0.20,
    linewidth = 0.6
  ) +
  coord_cartesian(ylim = c(75, 100)) +
  labs(title = "",
       y = "",
       x = "") +
  theme_minimal() +
  theme(
    axis.ticks = element_line(color = "black", size = 1),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.x = element_text(angle = 0, hjust =0.5, size = 16, face = "bold"),
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.text = element_text(size = 16),
    legend.title = element_blank(),
    strip.text = element_text(size = 16),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c(
    "ResNet-50" = "#19415c", 
    "ResNet-Explainer" = "#19415c", 
    "Mixer-B" = "#7cac94", 
    "Mixer-Explainer" = "#7cac94",
    "ViT-B" = "#c67b74", 
    "ViT-Explainer" = "#c67b74"
  )) + 
  scale_pattern_manual(values = pattern_values)

  # Combine both plots side by side with a shared legend
combined_plot <- (plot_instance + plot_layout(ncol = 1, guides = "collect")) &
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.justification = "center",
    legend.text = element_text(size = 15, face = "bold"),
    legend.title = element_blank()
  ) 

# Save the combined plot to a PDF file
ggsave("cifar-100.pdf", plot = combined_plot)

