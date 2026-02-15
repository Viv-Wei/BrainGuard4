# =============================
# 1. Load packages
# =============================
library(tidyverse)
library(data.table)
library(ggplot2)
library(scales)

# =============================
# 2. Read data
# =============================
j00_file <- fread("./data/data_fdr_trend.csv")

j00_clean <- j00_file %>%
  mutate(
    Trend = recode(
      Trend,
      "Gradually decreasing" = "Gradually Decreasing"
    )
  )

# =============================
# 3. Intra-group row selection
# =============================
j01_selected <- j00_clean %>%
  group_by(group, Protein) %>%
  mutate(
    has_sig = any(p_value < 0.05, na.rm = TRUE),
    keep_row = case_when(
      has_sig & p_value < 0.05 ~ TRUE,
      !has_sig & Outcome == "Dementia" ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(keep_row) %>%
  filter(
    if (n() > 1) {
      if (any(HR > 1, na.rm = TRUE)) {
        HR == max(HR, na.rm = TRUE)
      } else {
        HR == min(HR, na.rm = TRUE)
      }
    } else {
      TRUE
    }
  ) %>%
  ungroup() %>%
  select(-has_sig, -keep_row)

# =============================
# 4. Global significance filter
# =============================
valid_proteins <- j01_selected %>%
  group_by(Protein) %>%
  summarise(min_p = min(p_value, na.rm = TRUE), .groups = "drop") %>%
  filter(min_p < 0.05) %>%
  pull(Protein)

j01_final <- j01_selected %>%
  filter(Protein %in% valid_proteins)

# =============================
# 5. Data alignment
# =============================
skeleton <- tibble(Protein = valid_proteins)

j01_T2D <- skeleton %>%
  left_join(
    j01_final %>% filter(group == "T2D"),
    by = "Protein"
  )

j01_preDM <- skeleton %>%
  left_join(
    j01_final %>% filter(group == "Pre-DM"),
    by = "Protein"
  )

save(j01_T2D, j01_preDM, file = "./data/frost-filtered.Rdata")

# =============================
# 6. Custom axis transformation
# =============================
zoom_ratio <- 0.35

custom_split_trans <- function(ratio = 0.5, y_max = 6) {
  slope_upper <- (1 - ratio) / (y_max - 1)
  
  trans_new(
    name = "custom_split",
    transform = function(x) {
      ifelse(
        x <= 1,
        x * ratio,
        ratio + (x - 1) * slope_upper
      )
    },
    inverse = function(x) {
      ifelse(
        x <= ratio,
        x / ratio,
        1 + (x - ratio) / slope_upper
      )
    }
  )
}

# =============================
# 7. Forest plots
# =============================
for (f01 in c("T2D", "preDM")) {
  
  df <- get(paste0("j01_", f01)) %>%
    filter(!is.na(HR)) %>%
    arrange(Trend, Protein) %>%
    mutate(
      Protein = factor(Protein, levels = unique(Protein)),
      Group   = Trend,
      OR      = as.numeric(HR),
      CI_low  = as.numeric(conf_low),
      CI_high = as.numeric(conf_high),
      x_numeric = as.numeric(Protein)
    )
  
  p01 <- ggplot(df, aes(x = x_numeric, y = OR, color = Group)) +
    geom_errorbar(
      aes(ymin = CI_low, ymax = CI_high),
      width = 0.2,
      linewidth = 0.6
    ) +
    geom_point(size = 3) +
    geom_hline(
      yintercept = 1,
      linetype = "dashed",
      color = "grey50",
      linewidth = 0.5
    ) +
    scale_x_continuous(
      breaks = df$x_numeric,
      labels = df$Protein,
      expand = expansion(mult = 0.05)
    ) +
    scale_y_continuous(
      trans = custom_split_trans(ratio = zoom_ratio, y_max = 6),
      breaks = c(0, 0.5, 1, 2, 3, 4, 5, 6),
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_color_manual(
      values = c(
        "Fluctuating Decreasing" = "#A93435",
        "Fluctuating Increasing" = "#FF7F00",
        "Gradually Decreasing"   = "#383E89",
        "Gradually Increasing"   = "#3F9051"
      )
    ) +
    labs(
      x = "Outcome",
      y = "HR (95% CI)",
      title = paste("Forest Plot -", f01)
    ) +
    theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
    )
  
  file_name <- paste0("./results/", f01, "_ForestPlot_Dot.pdf")
  
  ggsave(
    file_name,
    plot = p01,
    width = 7,
    height = 3.5
  )
}
