# =============================
# 1. Load packages
# =============================
library(ggplot2)
library(dplyr)
library(readxl)
library(scales)

# =============================
# 2. Read data
# =============================
df_raw <- read_excel("./data/heatmap-MR_cox.xlsx")

# =============================
# 3. Rename variables
# =============================
df <- df_raw %>%
  rename(
    Protein = Outcome,
    Outcome = Source,
    HR      = OR,
    p_fdr   = P_value
  )

# =============================
# 4. Data preprocessing
# =============================
df <- df %>%
  mutate(
    Group = factor(
      Group,
      levels = c(
        "Gradually Increasing",
        "Fluctuating Increasing",
        "Gradually Decreasing",
        "Fluctuating Decreasing"
      )
    )
  ) %>%
  arrange(Group, Protein) %>%
  mutate(
    size_value = case_when(
      p_fdr <= 0.01 ~ 0.05,
      p_fdr <= 0.05 ~ 0.05 - p_fdr,
      TRUE ~ NA_real_
    ),
    HR_display = HR,
    Protein = factor(Protein, levels = unique(Protein)),
    Outcome = factor(Outcome, levels = unique(Outcome))
  )

# =============================
# 5. Check HR range
# =============================
min_hr <- min(df$HR, na.rm = TRUE)
max_hr <- max(df$HR, na.rm = TRUE)

# =============================
# 6. Define color palette
# =============================
custom_colors <- c(
  "#1f78b4", "#33a0cc", "#66c2a5", "#abdda4", "#e6f598",
  "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#b2182b"
)

offset <- max(abs(max_hr - 1), abs(min_hr - 1))
color_limits <- c(1 - offset, 1 + offset)

# =============================
# 7. Plot heatmap
# =============================
p <- ggplot(df, aes(x = Protein, y = Outcome)) +
  geom_point(
    data = df %>% filter(!is.na(size_value)),
    aes(size = size_value, fill = HR_display),
    shape = 21,
    color = "black",
    stroke = 0.4
  ) +
  scale_size_continuous(
    name = "P value",
    range = c(3, 8),
    breaks = c(0.05, 0.03, 0.02, 0.01),
    labels = c("0.01", "0.02", "0.03", "0.04")
  ) +
  scale_fill_gradientn(
    colours = custom_colors,
    name = "OR",
    limits = color_limits,
    oob = squish
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  labs(
    x = "Protein",
    y = "Outcome",
    title = "Association Heatmap (OR color & P-value size)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 10),
    plot.margin = margin(5.5, 10, 5.5, 20)
  )

# =============================
# 8. Save figure
# =============================
ggsave(
  "./results/heat_plot.pdf",
  plot = p,
  width = 8.4,
  height = 2.5,
  dpi = 300
)
