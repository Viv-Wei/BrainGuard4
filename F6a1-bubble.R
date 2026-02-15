# =============================
# 1. Load required libraries
# =============================
library(ggplot2)
library(ggrepel)
library(dplyr)
library(ggbreak)
library(readxl)
library(scales)
library(stringr)

# =============================
# 2. Read data
# =============================
phewas_data <- read_excel("./data/data-F6.xlsx", sheet = "Sheet1")

# =============================
# 3. Filter categories
# =============================
phewas_data <- subset(
  phewas_data,
  Category %in% c("Food intake", "Lifestyles", "Psychosocial factors")
)

# =============================
# 4. Remove specified exposures
# =============================
strings_to_remove <- c(
  "No, I have not had this happen to me",
  "Above_moderate_vigorous_walking_recommendation",
  "Leisure_social_activities_binary",
  "Liking for taking the stairs",
  "Recent feelings of tiredness or low energy",
  "Difficulty with being emotionally affected by health problems",
  "Difficulty with standing for long periods",
  "Difficulty with doing your day-to-day work",
  "Frequency of depressed mood in last 2 weeks",
  "Difficulty with walking a long distance",
  "Difficulty with taking care of household responsibilities",
  "Frequency of tiredness / lethargy in last 2 weeks",
  "Difficulty with washing whole body",
  "Difficulty with getting dressed",
  "Health satisfaction"
)

phewas_data <- phewas_data %>%
  filter(!str_detect(exprosure1, paste(strings_to_remove, collapse = "|")))

# =============================
# 5. FDR correction
# =============================
phewas_data <- phewas_data %>%
  group_by(outcome) %>%
  mutate(fdr_2 = p.adjust(pvalue, method = "fdr")) %>%
  ungroup()

phewas_data$pvalue <- as.numeric(phewas_data$pvalue)

phewas_data <- phewas_data %>%
  filter(pvalue > 0) %>%
  mutate(logp = -log10(pvalue))

# =============================
# 6. Define special exposures
# =============================
special_exposures <- c(
  "Fed-up feelings",
  "Frequency of unenthusiasm / disinterest in last 2 weeks",
  "Recent poor appetite or overeating",
  "General happiness with own health",
  "Loneliness, isolation",
  "MET_minutes_per_week_for_moderate_activity",
  "MET_minutes_per_week_for_vigorous_activity",
  "Summed_MET_minutes_per_week_for_all_activity"
)

phewas_data <- phewas_data %>%
  mutate(
    x_label = paste(outcome, Category, sep = " - "),
    point_size = abs(estimate),
    is_special = exprosure1 %in% special_exposures,
    point_alpha = ifelse(is_special, 1.0, 0.7)
  )

phewas_data$fdr_2 <- as.numeric(phewas_data$fdr_2)
closest_index <- which.min(abs(phewas_data$fdr_2 - 0.05))
p_threshold <- phewas_data$logp[closest_index]

# =============================
# 7. Set factor order
# =============================
phewas_data$x_label <- factor(
  phewas_data$x_label,
  levels = c(
    "LGALS4 - Lifestyles",
    "LGALS4 - Psychosocial factors",
    "LGALS4 - Food intake",
    "GDF15 - Lifestyles",
    "GDF15 - Psychosocial factors",
    "GDF15 - Food intake",
    "APOM - Lifestyles",
    "APOM - Psychosocial factors",
    "APOM - Food intake",
    "GHRL - Lifestyles",
    "GHRL - Psychosocial factors",
    "GHRL - Food intake"
  )
)

# =============================
# 8. Color and shape settings
# =============================
category_colors <- c(
  "Lifestyles" = "#DC572C",
  "Psychosocial factors" = "#7E80A6",
  "Food intake" = "#93B83A"
)

colors_special <- c(
  "Lifestyles" = "#B23A1D",
  "Psychosocial factors" = "#332F59",
  "Food intake" = "#6B8C24"
)

special_symbols <- c(15, 16, 17, 18, 8, 10, 11, 12)
names(special_symbols) <- special_exposures

phewas_data <- phewas_data %>%
  mutate(point_shape = ifelse(is_special, special_symbols[exprosure1], 21))

# =============================
# 9. Custom y-axis transformation
# =============================
custom_trans <- function(x) {
  x_clean <- ifelse(is.na(x) | is.infinite(x), 0, x)
  max_y <- max(x_clean, na.rm = TRUE)
  
  if (max_y <= 70) {
    return(x_clean * 12 / max_y)
  }
  
  y_max <- 12
  y_break1 <- 1.5
  y_break2 <- 3
  range1 <- 10
  range2 <- 70
  
  ifelse(
    x_clean <= range1,
    x_clean * (y_break1 / range1),
    ifelse(
      x_clean <= range2,
      y_break1 + (x_clean - range1) *
        ((y_break2 - y_break1) / (range2 - range1)),
      y_break2 + (x_clean - range2) *
        ((y_max - y_break2) / (max_y - range2))
    )
  )
}

custom_inverse <- function(y) {
  max_y <- max(
    ifelse(is.na(phewas_data$logp) | is.infinite(phewas_data$logp), 0, phewas_data$logp),
    na.rm = TRUE
  )
  
  if (max_y <= 70) {
    return(y * max_y / 12)
  }
  
  y_max <- 12
  y_break1 <- 1.5
  y_break2 <- 3
  range1 <- 10
  range2 <- 70
  
  ifelse(
    y <= y_break1,
    y * range1 / y_break1,
    ifelse(
      y <= y_break2,
      range1 + (y - y_break1) *
        ((range2 - range1) / (y_break2 - y_break1)),
      range2 + (y - y_break2) *
        ((max_y - range2) / (y_max - y_break2))
    )
  )
}

custom_scale <- trans_new(
  name = "custom_y",
  transform = custom_trans,
  inverse = custom_inverse,
  breaks = function(x) {
    specified_breaks <- c(0, 5, 10, 50, 100, 200, 300)
    max_val <- max(
      ifelse(is.na(phewas_data$logp) | is.infinite(phewas_data$logp), 0, phewas_data$logp),
      na.rm = TRUE
    )
    valid_breaks <- specified_breaks[specified_breaks <= max_val]
    if (length(valid_breaks) == 0) return(c(0, max_val))
    valid_breaks
  }
)

# =============================
# 10. Jitter and labeling
# =============================
set.seed(123)

phewas_data <- phewas_data %>%
  group_by(x_label) %>%
  mutate(x_jitter = as.numeric(x_label) + runif(n(), -0.4, 0.4)) %>%
  ungroup()

max_logp <- max(
  ifelse(is.na(phewas_data$logp) | is.infinite(phewas_data$logp), 0, phewas_data$logp),
  na.rm = TRUE
)

phewas_data$abs_beta <- abs(phewas_data$point_size)

top3_data <- phewas_data %>%
  filter(!is_special) %>%
  group_by(outcome) %>%
  arrange(desc(logp)) %>%
  slice_head(n = 3) %>%
  ungroup()

special_data <- phewas_data %>%
  filter(is_special)

possible_label_cols <- c("exprosure1", "phenotype", "trait", "variable", "label", "exposure")

label_col <- NULL
for (col in possible_label_cols) {
  if (col %in% colnames(top3_data)) {
    label_col <- col
    break
  }
}

if (is.null(label_col)) {
  top3_data$label_text <- paste("Top", 1:nrow(top3_data))
} else {
  top3_data$label_text <- top3_data[[label_col]]
}

# =============================
# 11. Plot
# =============================
p <- ggplot() +
  geom_point(
    data = filter(phewas_data, !is_special),
    aes(
      x = x_jitter,
      y = logp,
      size = point_size,
      alpha = point_alpha,
      fill = Category
    ),
    shape = 21,
    stroke = 0.5,
    color = "white"
  ) +
  geom_point(
    data = filter(phewas_data, is_special),
    aes(
      x = x_jitter,
      y = logp,
      size = point_size,
      alpha = point_alpha,
      shape = exprosure1,
      fill = Category,
      color = Category
    ),
    stroke = 0.8
  ) +
  geom_text(
    data = top3_data,
    aes(x = x_jitter, y = logp, label = label_text),
    size = 2.8,
    color = "black",
    fontface = "bold",
    vjust = -0.8
  ) +
  geom_hline(
    yintercept = p_threshold,
    color = "gray50",
    linetype = "dashed",
    linewidth = 0.6,
    alpha = 0.7
  ) +
  scale_fill_manual(values = category_colors, name = "Category") +
  scale_color_manual(values = colors_special, guide = "none") +
  scale_shape_manual(values = special_symbols, guide = "none") +
  scale_size_continuous(
    range = c(2.3, 4.7),
    breaks = function(x) pretty(x, n = 4),
    labels = function(x) sprintf("%.2f", x),
    name = expression("|" * beta * "|"),
    guide = guide_legend(order = 2)
  ) +
  scale_alpha_identity(guide = "none") +
  scale_x_continuous(
    breaks = 1:length(levels(phewas_data$x_label)),
    labels = levels(phewas_data$x_label),
    limits = c(0.5, length(levels(phewas_data$x_label)) + 0.5),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    trans = custom_scale,
    expand = expansion(mult = c(0.02, 0.15)),
    limits = c(0, custom_trans(max_logp) * 1.2),
    minor_breaks = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 11, face = "bold"),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray80", linewidth = 0.5),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.4, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30", size = 12),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  labs(
    y = expression(-log[10](italic(P))),
    title = "PheWAS Analysis Results Visualization",
    subtitle = paste(
      "Point size indicates |β|; special exposures use distinct symbols.",
      "Max logP:", round(max_logp, 1)
    )
  )

print(p)

# =============================
# 12. Save figure
# =============================
ggsave(
  "./plot/bubble.pdf",
  plot = p,
  width = 8,
  height = 4,
  dpi = 300
)
