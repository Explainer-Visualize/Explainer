# # Load required libraries
library(ggplot2)
library(scales)

# Create the data frame with correct syntax
data <- data.frame(
  Scene = rep(c("Animal", "Disease", "Protein", "Music"), each = 5),
  Score = factor(rep(c("Score 1","Score 2","Score 3","Score 4","Score 5"), times = 4)),  # Fixed syntax
  Percentage = c(
    # Animal scores
    0.00, 0.03, 0.07, 0.19, 0.71,   
    # Disease scores 
    0.02, 0.03, 0.10, 0.20, 0.65,   
    # Protein scores
    0.01, 0.04, 0.13, 0.22, 0.60,  
    # Music scores
    0.00, 0.02, 0.10, 0.14, 0.74  
  )
)

# Set factor levels
data$Scene <- factor(data$Scene, 
                    levels = c("Animal", "Disease", "Protein", "Music"))
data$Score <- factor(data$Score, levels = c("Score 5","Score 4","Score 3","Score 2","Score 1"))  # Reverse for legend order

# Create color palette
score_colors <- c("#08306B", "#2171B5", "#6BAED6", "#BDD7E7", "#EFF3FF")

# Create the plot
ggplot(data, aes(x = Scene, y = Percentage, fill = Score)) +
  geom_col(position = "stack", width = 0.4, color = "white", linewidth = 0.3) +
  scale_y_continuous(
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(
    values = rev(score_colors),
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    x = "Category",
    y = "Percentage of Responses",
    fill = "Intelligibility Score"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(face = "bold", size = 16),
    axis.text.y = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 17),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
    legend.position = "top", # Or "right", "bottom"
    legend.direction = "horizontal", # if legend.position is "top" or "bottom"
    legend.text = element_text(size = 16),                       # Adjusted size
    legend.title = element_text(size = 18, face = "bold")
    # legend.key.size = unit(1.2, "lines")
  )

ggsave("user-study.pdf", width = 10, height = 6, dpi = 300)

