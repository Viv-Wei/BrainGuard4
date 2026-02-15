# =============================
# 1. Load data
# =============================
gd <- read.csv(
  "./data/Dementia-gradient_importance_Gradually_Decreasing_Deep_ordinal_regression.csv",
  stringsAsFactors = FALSE
)

gi <- read.csv(
  "./data/Dementia-gradient_importance_dementia_Gradually_Increasing_Deep_ordinal_regression.csv",
  stringsAsFactors = FALSE
)

fi <- read.csv(
  "./data/Dementia-gradient_importance_dementia_Fluctuating_Increasing_Deep_ordinal_regression.csv",
  stringsAsFactors = FALSE
)

fd <- read.csv(
  "./data/Dementia-gradient_importance_dementia_Fluctuating_Decreasing_Deep_ordinal_regression.csv",
  stringsAsFactors = FALSE
)

# =============================
# 2. Load libraries
# =============================
library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)

# =============================
# 3. Ring plot function
# =============================
plot_single_ring <- function(df, source_name, fill_colors) {
  
  # Compute axis limit
  max_value <- max(df$Gradient.Importance)
  
  axis_limit <- ifelse(
    max_value %% 1 == 0,
    max_value,
    ceiling(max_value * 2) / 2
  )
  
  axis_height <- axis_limit
  
  # Select top 10 proteins
  df_top <- df %>%
    slice_max(Gradient.Importance, n = 10) %>%
    mutate(
      height_ratio = Gradient.Importance / max_value,
      ymin  = 10,
      ymax  = ymin + height_ratio * axis_height,
      id    = row_number(),
      angle = 360 * (id - 1) / 10
    )
  
  axis_angle <- 360
  
  # Axis line
  axis_line <- data.frame(
    x    = axis_angle,
    xend = axis_angle,
    y    = 10,
    yend = 10 + axis_height
  )
  
  # Axis tick
  tick_height <- axis_height + 10
  
  ticks <- data.frame(
    x     = axis_angle,
    xend  = axis_angle,
    y     = tick_height - 0.2,
    yend  = tick_height + 0.2,
    label = round(axis_limit, 1)
  )
  
  # Plot
  ggplot(df_top) +
    geom_rect(
      aes(
        xmin = angle - 18,
        xmax = angle + 18,
        ymin = ymin,
        ymax = ymax,
        fill = Gradient.Importance
      ),
      color = "white",
      linewidth = 0.3
    ) +
    scale_fill_gradientn(
      colours = fill_colors,
      name = paste(source_name, "\nImportance")
    ) +
    geom_segment(
      data = axis_line,
      aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE,
      color = "gray20",
      linewidth = 0.5
    ) +
    geom_segment(
      data = ticks,
      aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE,
      color = "gray20",
      linewidth = 0.5
    ) +
    geom_text(
      data = ticks,
      aes(x = x, y = yend + 0.2, label = label),
      inherit.aes = FALSE,
      size = 2.5,
      color = "gray20"
    ) +
    geom_text(
      aes(x = angle, y = ymax + 0.3, label = Protein),
      size = 2.5
    ) +
    coord_polar(theta = "x", start = pi / 2) +
    theme_void() +
    ggtitle(paste("Ring Plot -", source_name)) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
      legend.position = "right",
      legend.title = element_text(size = 8),
      legend.text  = element_text(size = 7)
    )
}

# =============================
# 4. Color schemes
# =============================
fd_colors <- c("#d4cad4", "#c39cc2", "#9a6f98", "#7c4d72")
fi_colors <- c("#c4f0f0", "#6cc5c5", "#459999", "#1a6666")
gi_colors <- c("#f9d4dc", "#f07a95", "#db1846", "#a10f33")
gd_colors <- c("#dde8f2", "#8cbfe8", "#5a88b4", "#2a5680")

# =============================
# 5. Generate plots
# =============================
p1 <- plot_single_ring(fd, "FD", fd_colors)
p2 <- plot_single_ring(fi, "FI", fi_colors)
p3 <- plot_single_ring(gd, "GD", gd_colors)
p4 <- plot_single_ring(gi, "GI", gi_colors)

# =============================
# 6. Combine plots
# =============================
final_plot <- (p1 | p2) / (p3 | p4) +
  plot_annotation(
    title = "Comparative Ring Plots - Gradient Enhancement",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 13, face = "bold")
    )
  )

# =============================
# 7. Save output
# =============================
ggsave(
  "./results/Protein_selection-dementia-softened_gradient.pdf",
  final_plot,
  width  = 12,
  height = 8,
  units  = "in"
)
