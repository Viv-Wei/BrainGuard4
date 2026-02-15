# =============================
# 1. Load package
# =============================
library(ggplot2)

# =============================
# 2. Example data
# =============================
df <- data.frame(
  group = c("Group A", "Group B", "Group C", "Group D", "Group E"),
  percent = c(2.4, 3.4, 2.5, 2.1, 1.5)
)

# Order from top to bottom
df$group <- factor(df$group, levels = rev(df$group))

# =============================
# 3. Create horizontal bar plot
# =============================
p <- ggplot(df, aes(x = percent, y = group)) +
  geom_col(
    fill = "#4A6F97",
    width = 0.7
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", percent)),
    hjust = -0.2,
    size = 5,
    fontface = "bold"
  ) +
  expand_limits(x = max(df$percent) * 1.25) +
  labs(
    title = "Incidence",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.text.y = element_text(face = "bold")
  )

# =============================
# 4. Save
# =============================
ggsave(
  "./results/incidence_barplot.pdf",
  plot = p,
  width = 4,
  height = 3,
  dpi = 300
)
