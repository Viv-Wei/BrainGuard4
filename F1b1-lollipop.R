# =============================
# 1. Load required package
# =============================
library(ggplot2)

# =============================
# 2. Create example data
# =============================
df <- data.frame(
  pathway = c(
    "Pathway A",
    "Pathway B",
    "Pathway C",
    "Pathway D",
    "Pathway E",
    "Pathway F"
  ),
  percent = c(39.2, 16.3, 8.8, 8.1, 6.6, 4.4),
  shape   = c(16, 17, 18, 15, 8, 3)
)

# =============================
# 3. Order pathways by percentage
# =============================
df$pathway <- factor(
  df$pathway,
  levels = df$pathway[order(df$percent)]
)

# =============================
# 4. Create lollipop plot
# =============================
p <- ggplot(df, aes(x = percent, y = pathway)) +
  geom_segment(
    aes(x = 0, xend = percent, yend = pathway),
    linewidth = 1.2,
    alpha = 0.7
  ) +
  geom_point(
    aes(shape = factor(shape)),
    size = 5,
    stroke = 1.2
  ) +
  scale_shape_manual(
    values = setNames(df$shape, df$shape),
    guide = "none"
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", percent)),
    hjust = -0.15,
    size = 5
  ) +
  expand_limits(
    x = max(df$percent) * 1.25
  ) +
  labs(
    x = "Pathway Percentage (%)",
    y = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.line.y  = element_blank(),
    axis.ticks.y = element_blank()
  )

# =============================
# 5. Save plot
# =============================
ggsave(
  "./plot/lollipop.pdf",
  plot = p,
  width = 8,
  height = 3,
  dpi = 300
)
