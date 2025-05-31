library(ggplot2)
library(dplyr)
library(tidyr)

# Constructing the data
data <- data.frame(
  Model = c(
    "Wang et al. (2017)", "Guan et al. (2020)", "Ma et al. (2019)", 
    "Hermoza et al. (2020)", "XProtoNet (2021)", "A3Net (2021)", 
    "Explainer"
  ),
  Atel = c(70.0, 77.9, 77.7, 77.5, 78.0, 77.9, 80.4),
  Card = c(81.0, 87.9, 89.4, 88.1, 88.7, 89.5, 91.3),
  Cons = c(75.9, 82.4, 82.9, 83.1, 83.5, 83.6, 86.3),
  Edema = c(66.1, 69.4, 69.6, 69.5, 71.0, 71.0, 75.1),
  Effu = c(69.3, 83.1, 83.8, 82.6, 83.1, 83.4, 86.1),
  Emph = c(66.9, 76.6, 77.1, 78.9, 80.4, 77.7, 80.6),
  Fibr = c(65.8, 72.6, 72.2, 74.1, 73.4, 73.7, 75.3),
  Hern = c(79.9, 85.8, 86.2, 87.9, 87.1, 87.8, 89.5),
  Infi = c(70.3, 75.8, 75.0, 74.7, 74.7, 75.9, 77.4),
  Mass = c(80.5, 85.0, 84.6, 84.6, 84.0, 85.5, 88.2),
  Nod = c(83.3, 90.9, 90.8, 93.6, 94.1, 93.3, 94.6),
  PT = c(78.6, 83.2, 82.7, 83.3, 81.5, 83.8, 85.4),
  Pne1 = c(68.4, 77.8, 77.9, 79.3, 79.9, 79.1, 82.2),
  Pne2 = c(87.2, 90.6, 93.4, 91.7, 90.9, 93.8, 94.9),
  Mean = c(74.5, 81.4, 81.7, 82.1, 82.2, 82.6, 85.7)
)

data_scores <- data %>%
  pivot_longer(cols = -Model, names_to = "Condition", values_to = "Score")

new_plot <- ggplot(data_scores, aes(x = Condition, y = Score, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  labs(title = "", y = "", x = "", fill = NULL) +
  theme_minimal() +
  coord_cartesian(ylim = c(60, 100)) +
  guides(fill = guide_legend(nrow = 1)) + 
  theme(
    axis.ticks = element_line(color = "black", size = 1),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 38, face = "bold"),
    axis.text.y = element_text(size = 36, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 38, face = "bold"),
    legend.text = element_text(size = 36),
    legend.title = element_text(size = 36),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 2),
    legend.position = c(0.5, 0.95), 
    legend.justification = "center",
    legend.direction = "horizontal", 
    legend.box = "horizontal"
  ) +
  scale_fill_manual(values = c(
    "#a5a5a5", "#19415c", "#e6d3a3", "#7cac94", 
    "#32747a", "#74476f", "#c67b74"
  ))

ggsave("ChestXray.pdf", plot = new_plot, width = 32, height = 8)