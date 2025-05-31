# Load necessary libraries
library(ggplot2)
library(patchwork)  # For combining plots
library(gridExtra)

# Create the data frame with new values
data <- data.frame(
  Method = rep(c("Blast    ", "FunFams    ", "DeepFRI    ", "HEAL    ", "HEAL-Explainer    "), each = 3),
  Classes = rep(c("MF", "BP", "CC"), times = 5),
  AUPR = c(0.136, 0.067, 0.097, 0.367, 0.260, 0.288, 0.495, 0.261, 0.274, 0.653, 0.308, 0.432, 0.681, 0.319, 0.497),
  Fmax = c(0.328, 0.336, 0.448, 0.572, 0.500, 0.627, 0.625, 0.540, 0.613, 0.711, 0.581, 0.654, 0.744, 0.595, 0.669),
  Smin = c(0.632, 0.651, 0.628, 0.531, 0.579, 0.503, 0.437, 0.543, 0.527, 0.366, 0.509, 0.489, 0.343, 0.478, 0.468)
)

# Set factor levels for ordering in the plot
data$Method <- factor(data$Method, levels = c("Blast    ", "FunFams    ", "DeepFRI    ", "HEAL    ", "HEAL-Explainer    "))
data$Classes <- factor(data$Classes, levels = c("MF", "BP", "CC"))

# Create AUPR Plot
plot_aupr <- ggplot(data, aes(x = Classes, y = AUPR, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.72) +
  coord_cartesian(ylim = c(0.05, 0.8)) +
  labs(title = "AUPR", y = "", x = "") +
  theme_minimal() +
  theme(
    axis.ticks = element_line(color = "black", size = 0.6),
    axis.ticks.length = unit(0.15, "cm"),
    axis.text.x = element_text(angle = 0, hjust =0.5, size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("#19415c", "#e6d3a3", "#32747a", "#74476f", "#c67b74"))

# Create Fmax Plot
plot_fmax <- ggplot(data, aes(x = Classes, y = Fmax, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.72) +
  coord_cartesian(ylim = c(0.05, 0.8)) +
  labs(title = "Fmax", y = "", x = "", fill = NULL) +
  theme_minimal() +
  theme(
    axis.ticks = element_line(color = "black", size = 0.6),
    axis.ticks.length = unit(0.15, "cm"),
    axis.text.x = element_text(angle = 0, hjust =0.5, size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    legend.text = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 10),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.justification = "center"
  ) +
  scale_fill_manual(values = c("#19415c", "#e6d3a3", "#32747a", "#74476f", "#c67b74"))

# Create Smin Plot
plot_smin <- ggplot(data, aes(x = Classes, y = Smin, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.72) +
  coord_cartesian(ylim = c(0.05, 0.8)) +
  labs(title = "Smin", y = "", x = "") +
  theme_minimal() + 
  theme(
    axis.ticks = element_line(color = "black", size = 0.6),
    axis.ticks.length = unit(0.15, "cm"),
    axis.text.x = element_text(angle = 0, hjust =0.5, size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("#19415c", "#e6d3a3", "#32747a", "#74476f", "#c67b74"))

# Combine the plots horizontally using patchwork
combined_plot <- plot_aupr + plot_fmax + plot_smin + plot_layout(ncol = 3)

# Save the combined plot with the specified size
ggsave("PDBch.pdf", plot = combined_plot, width = 10, height = 4)
