library(tidyr)
library(dplyr)
library(ggplot2)

# Updated data
data <- data.frame(
  Centroid_Number = rep(1:8, each = 3),
  Model = rep(c("ResNet-Explainer", "Mixer-Explainer", "ViT-Explainer"), 8),
  Accuracy = c(
   79.82, 76.23, 78.63,
80.23, 76.66, 78.95,  
80.42, 77.05, 79.28, 
80.45, 77.03, 79.32,   
80.38, 76.91, 79.35,  
80.39, 76.94, 79.08,  
80.32, 76.72, 78.82,  
80.21, 76.63, 78.83  
  ),
  Accuracy_std = c(
0.31, 0.62, 0.43,  
0.20, 0.32, 0.23,  
0.16, 0.24, 0.17,  
0.12, 0.13, 0.10,  
0.09, 0.14, 0.09,  
0.10, 0.12, 0.11, 
0.11, 0.13, 0.10,  
0.08, 0.12, 0.09 
  )
)

# Reshape the data
data_long <- data %>%
  rename(Score = Accuracy, std = Accuracy_std)

# Plotting
ggplot(data_long, aes(x = Centroid_Number, y = Score, group = Model, color = Model, fill = Model)) +
  geom_ribbon(aes(ymin = Score - std, ymax = Score + std), alpha = 0.2, color = NA) +
  geom_line(size = 2.0) +
  geom_point(aes(color = Model), size = 5.5, shape = 21, fill = "white", color = "white") + 
  geom_point(size = 3.8) +
  labs(title = "", x = "Centroid Number", y = "") +
  scale_x_continuous(breaks = 1:8) +
  scale_y_continuous(limits = c(75, 82), breaks = seq(75, 82, 1)) +
  scale_color_manual(values = c(
    "ResNet-Explainer" = "#32747a",
    "Mixer-Explainer" = "#74476f",
    "ViT-Explainer" = "#c67b74"
  )) +
  scale_fill_manual(values = c(
    "ResNet-Explainer" = "#32747a",
    "Mixer-Explainer" = "#74476f",
    "ViT-Explainer" = "#c67b74"
  )) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 16, face = "bold"),
    axis.ticks = element_line(color = "black", size = 0.8),
    axis.ticks.length = unit(0.12, "cm"),
    strip.text = element_text(size = 16, face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8)
  )
