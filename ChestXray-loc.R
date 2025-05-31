library(tidyr)
library(dplyr)
library(ggplot2)

# Create the data
data <- data.frame(
  IoU_Threshold = c(rep(0.3, 3), rep(0.5, 3), rep(0.7, 3)),
  Model = rep(c("Li et al.   ", "Liu et al.   ", "Explainer (Ours)   "), 3),
  Atel = c(0.36, 0.53, 0.62, 0.14, 0.32, 0.49, 0.04, 0.18, 0.35),
  Card = c(0.94, 0.88, 0.97, 0.84, 0.78, 0.88, 0.52, 0.70, 0.81),
  Effu = c(0.56, 0.57, 0.68, 0.22, 0.40, 0.53, 0.07, 0.28, 0.42),
  Infi = c(0.66, 0.73, 0.85, 0.30, 0.61, 0.74, 0.09, 0.41, 0.61),
  Mass = c(0.45, 0.48, 0.61, 0.22, 0.33, 0.49, 0.11, 0.27, 0.38),
  Nod = c(0.17, 0.10, 0.28, 0.07, 0.05, 0.21, 0.01, 0.04, 0.15),
  Pne1 = c(0.39, 0.49, 0.67, 0.17, 0.37, 0.56, 0.05, 0.25, 0.42),
  Pne2 = c(0.44, 0.40, 0.57, 0.19, 0.23, 0.48, 0.05, 0.18, 0.35),
  Mean = c(0.49, 0.53, 0.68, 0.27, 0.39, 0.58, 0.12, 0.29, 0.43),
  Atel_std = c(0.09, 0.06, 0.03, 0.05, 0.04, 0.03, 0.02, 0.03, 0.03),
  Card_std = c(0.13, 0.07, 0.04, 0.10, 0.08, 0.03, 0.08, 0.06, 0.03),
  Effu_std = c(0.09, 0.06, 0.03, 0.07, 0.08, 0.04, 0.02, 0.06, 0.05),
  Infi_std = c(0.10, 0.07, 0.06, 0.05, 0.08, 0.05, 0.03, 0.05, 0.05),
  Mass_std = c(0.06, 0.06, 0.04, 0.05, 0.07, 0.05, 0.04, 0.05, 0.03),
  Nod_std = c(0.04, 0.03, 0.03, 0.02, 0.02, 0.03, 0.00, 0.01, 0.02),
  Pne1_std = c(0.09, 0.06, 0.03, 0.05, 0.06, 0.03, 0.01, 0.04, 0.03),
  Pne2_std = c(0.09, 0.06, 0.03, 0.03, 0.04, 0.03, 0.01, 0.03, 0.03),
  Mean_std = c(0.10, 0.07, 0.03, 0.07, 0.06, 0.04, 0.03, 0.05, 0.03)
)

# Reshape the data: scores
data_long <- data %>%
  pivot_longer(cols = -c(IoU_Threshold, Model), names_to = "Condition", values_to = "Score") %>%
  filter(!grepl("_std", Condition))

# Reshape the data: standard deviations
data_std <- data %>%
  pivot_longer(cols = contains("_std"), names_to = "Condition", values_to = "std") %>%
  mutate(Condition = gsub("_std", "", Condition))

# Join the score and std data by IoU_Threshold, Model, and Condition
data_long <- data_long %>%
  left_join(data_std, by = c("IoU_Threshold", "Model", "Condition"))

# Set the order of Condition (optional)
data_long$Condition <- factor(data_long$Condition, 
                              levels = c("Atel", "Card", "Effu", "Infi", "Mass", "Nod", "Pne1", "Pne2", "Mean"))

# Plot the line chart with error envelope using geom_ribbon
ggplot(data_long, aes(x = IoU_Threshold, y = Score, group = Model, color = Model, fill = Model)) +
  geom_ribbon(aes(ymin = Score - std, ymax = Score + std), alpha = 0.2, color = NA) +
  geom_line(size = 1.0) +
  geom_point(aes(color = Model), size = 2.5, shape = 1, fill = "white", color = "white") + 
  geom_point(size = 1.8) +
  facet_wrap(~ Condition) +
  labs(title = "", x = "IoU Threshold", y = "Accuracy") +
  scale_x_continuous(breaks = c(0.3, 0.5, 0.7)) +
  scale_y_continuous(breaks = c(0, 0.3, 0.6, 0.9)) +
  scale_color_manual(values = c(
    "Li et al.   " = "#32747a",
    "Liu et al.   " = "#74476f",
    "Explainer (Ours)   " = "#c67b74"
  )) +
  scale_fill_manual(values = c(
    "Li et al.   " = "#32747a",
    "Liu et al.   " = "#74476f",
    "Explainer (Ours)   " = "#c67b74"
  )) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10, face = "bold"),
    axis.ticks = element_line(color = "black", size = 0.8),
    axis.ticks.length = unit(0.12, "cm"),
    strip.text = element_text(size = 10, face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8)
  )

