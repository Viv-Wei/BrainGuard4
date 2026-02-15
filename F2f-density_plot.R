# =============================
# 1. Load packages
# =============================
library(ggplot2)
library(ggridges)
library(dplyr)
library(readxl)
library(tidyr)

# =============================
# 2. Read data
# =============================
df <- read_excel("./data/density plot data.xlsx")

# =============================
# 3. Data preprocessing
# =============================
df_filtered <- df %>% 
  filter(group %in% c("Q1", "Q10")) %>% 
  mutate(
    group = factor(group, levels = c("Q1", "Q10")),
    type = factor(type, levels = c(
      "Pairs matching accuracy", "Numeric memory", 
      "Trail making numeric path", "Matrix pattern completion", 
      "Fluid intelligence", "Reaction time", 
      "Paired learning", "Picture vocabulary"
    )),
    predicted_cognition_norm = as.numeric(predicted_cognition_norm)
  ) %>%
  drop_na(predicted_cognition_norm)

# =============================
# 4. Mean value calculation
# =============================
mean_values <- df_filtered %>%
  group_by(protein, type, group) %>%
  summarise(
    mean_val = mean(predicted_cognition_norm, na.rm = TRUE),
    .groups = "drop"
  )

# =============================
# 5. Axis range
# =============================
x_range <- c(0, 1.2)

# =============================
# 6. Read pfdr and merge
# =============================
pp <- read_excel("./data/pfdr data.xlsx")

df_with_text <- df_filtered %>%
  left_join(pp, by = c("protein", "type")) %>%
  group_by(protein, type) %>%
  mutate(show_pfdr = row_number() == 1) %>%
  ungroup()

# =============================
# 7. Plot
# =============================
ggplot(df_with_text, aes(x = predicted_cognition_norm, y = group, fill = group)) +
  
  geom_density_ridges(
    alpha = 0.7, 
    scale = 0.9, 
    size = 0.3,
    panel_scaling = TRUE
  ) +
  
  geom_vline(
    data = mean_values,
    aes(xintercept = mean_val, color = group),
    linetype = "dashed",
    size = 0.6,
    show.legend = FALSE
  ) +
  
  geom_text(
    aes(label = ifelse(show_pfdr, as.character(pfdr), "")),
    position = position_jitter(width = 0.05, height = 0),
    size = 3,
    color = "black",
    show.legend = FALSE
  ) +
  
  facet_grid(
    protein ~ type, 
    scales = "free_y", 
    space = "free_y"
  ) +
  
  scale_fill_manual(values = c("Q1" = "#658eb2", "Q10" = "#c15c4f")) +
  scale_color_manual(values = c("Q1" = "#658eb2", "Q10" = "#c15c4f")) +
  
  scale_x_continuous(
    limits = x_range,
    breaks = seq(x_range[1], x_range[2], by = 0.2),
    expand = c(0, 0)
  ) +
  
  labs(
    x = "Predicted Cognition (Normalized)",
    y = "Group",
    title = "Q1/Q10 Distribution by Protein"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 10),
    strip.text.y = element_text(angle = 0, hjust = 0),
    panel.spacing = unit(0.2, "cm"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# =============================
# 8. Save figure
# =============================
ggsave("./plot/density_plot.pdf", width = 9, height = 6)
