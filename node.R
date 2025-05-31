library(ggplot2)
library(dplyr)
library(tidyr)

# Constructing the data
data <- data.frame(
  Model = c("Explainer", "ResNet-50"),
  A = c(85.4, 81.7),
  B = c(90.2, 86.1),
  C = c(92.6, 88.8),
  D = c(86.5, 84.3),
  E = c(91.2, 87.4),
  F = c(93.5, 89.3),
  G = c(92.8, 89.8),
  H = c(92.0, 88.1),
  I = c(94.1, 89.5),
  J = c(91.3, 88.5),
  K = c(87.5, 84.6),
  L = c(91.5, 89.3),
  M = c(88.2, 85.1),
  N = c(94.5, 90.5),
  O = c(89.4, 86.4),
  P = c(87.4, 84.5),
  Q = c(93.7, 90.2),
  R = c(95.2, 92.4),
  S = c(88.5, 85.1),
  T = c(92.3, 89.4),
  U = c(92.7, 90.5)
)

new_colnames <- c(
  "Model", "1~5", "6~10", "11~15", "16~20", "21~25", "26~30", "31~35",
  "36~40", "41~45", "46~50", "51~55", "56~60", "61~65", "66~70", "71~75",
  "76~80", "81~85", "86~90", "91~95", "96~100", ">100"
)
colnames(data) <- new_colnames

# Reshape long
data_long <- pivot_longer(data, cols = -Model, names_to = "Condition", values_to = "Score")
data_long$Model <- factor(data_long$Model, levels = unique(data$Model))
data_long$Condition <- factor(data_long$Condition, levels = unique(data_long$Condition))

# Calculate the difference
diff_data <- data_long %>%
  pivot_wider(names_from = Model, values_from = Score) %>%
  mutate(Diff = Explainer - `ResNet-50`) %>%
  select(Condition, Diff)

# Merge with original
combined_data <- left_join(data_long, diff_data, by = "Condition")

# Plot
new_plot <- ggplot(combined_data, aes(x = Condition)) +
  # Bar chart
  geom_bar(aes(y = Score, fill = Model),
           stat = "identity", position = position_dodge(width = 0.7), width = 0.65) +
  # Line chart mapped to secondary axis (transformed)
  geom_line(aes(y = Diff * (40 / 5) + 60, group = 1), color = "#cb3973", size = 3) +
  geom_point(aes(y = Diff * (40 / 5) + 60), size = 7.5,shape =21,  fill = "white", color = "white") +
  geom_point(aes(y = Diff * (40 / 5) + 60), color = "#cb3973", size = 4.5) + 
  
  scale_y_continuous( 
    name = "Top-1 Accuracy", 
    sec.axis = sec_axis(~ (. - 60) * (5 / 40), name = "Relative Improvement", breaks = seq(0, 10, 1))
  ) +
  labs(title = "", x = "# of Encompassed Leaf Nodes", fill = NULL) +
  theme_minimal() +
  coord_cartesian(ylim = c(60, 100)) +
  guides(fill = guide_legend(nrow = 1)) + 
  theme(
    axis.ticks = element_line(color = "black", size = 1),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.7, size = 28, face = "bold"),
    axis.text.y = element_text(size = 26, face = "bold"),
    axis.title = element_text(size = 28, face = "bold"),
    axis.title.y.right = element_text(size = 28, face = "bold"),
    axis.title.x = element_text(size = 28, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 28, face = "bold"),
    legend.text = element_text(size = 28),
    legend.title = element_text(size = 28),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    legend.position = c(0.5, 0.95),
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  ) +
  scale_fill_manual(values = c("#c67b74", "#19415c"))

ggsave("node.pdf", plot = new_plot, width = 24, height = 8)
