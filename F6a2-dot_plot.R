# =============================
# 1. Load required libraries
# =============================
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
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

# =============================
# 6. Filter target exposures
# =============================
target_exposures <- c(
  "Fed-up feelings",
  "Frequency of unenthusiasm / disinterest in last 2 weeks",
  "Recent poor appetite or overeating",
  "General happiness with own health",
  "Loneliness, isolation",
  "MET_minutes_per_week_for_moderate_activity",
  "MET_minutes_per_week_for_vigorous_activity",
  "Summed_MET_minutes_per_week_for_all_activity"
)

filtered_data <- phewas_data[phewas_data$exprosure1 %in% target_exposures, ]
filtered_data$fdr_2 <- as.numeric(filtered_data$fdr_2)

filtered_data <- filtered_data %>%
  mutate(
    neg_log10_pval = -log10(fdr_2),
    point_size = neg_log10_pval
  )

# =============================
# 7. Create color groups
# =============================
filtered_data <- filtered_data %>%
  mutate(
    color_group = case_when(
      estimate < -0.05 ~ "Dark Blue",
      estimate >= -0.05 & estimate < -0.01 ~ "Medium Blue",
      estimate >= -0.01 & estimate < 0 ~ "Light Blue",
      estimate == 0 ~ "Gray",
      estimate > 0 & estimate <= 0.01 ~ "Light Red",
      estimate > 0.01 & estimate <= 0.05 ~ "Medium Red",
      estimate > 0.05 ~ "Dark Red"
    ),
    color_group = factor(
      color_group,
      levels = c(
        "Dark Blue", "Medium Blue", "Light Blue",
        "Gray", "Light Red", "Medium Red", "Dark Red"
      )
    )
  )

# =============================
# 8. Set factor levels
# =============================
filtered_data$outcome <- factor(
  filtered_data$outcome,
  levels = c("LGALS4", "GDF15", "GHRL", "APOM")
)

filtered_data$exprosure1 <- factor(
  filtered_data$exprosure1,
  levels = c(
    "Summed_MET_minutes_per_week_for_all_activity",
    "MET_minutes_per_week_for_vigorous_activity",
    "MET_minutes_per_week_for_moderate_activity",
    "Loneliness, isolation",
    "General happiness with own health",
    "Recent poor appetite or overeating",
    "Frequency of unenthusiasm / disinterest in last 2 weeks",
    "Fed-up feelings"
  )
)

# =============================
# 9. Plot
# =============================
p <- ggplot(filtered_data, aes(x = outcome, y = exprosure1)) +
  geom_point(
    aes(size = neg_log10_pval, fill = color_group),
    shape = 21,
    color = "white",
    stroke = 0,
    alpha = 0.9
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.1),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40"),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    legend.position = "right",
    legend.box = "vertical",
    plot.margin = margin(15, 15, 15, 15)
  ) +
  scale_fill_manual(
    values = c(
      "Dark Blue" = "#16315E",
      "Medium Blue" = "#2171B5",
      "Light Blue" = "#6BAED6",
      "Gray" = "#D3D3D3",
      "Light Red" = "#FC9272",
      "Medium Red" = "#EF3B2C",
      "Dark Red" = "#C13920"
    ),
    name = "Estimate",
    drop = FALSE
  ) +
  scale_size_continuous(
    range = c(3, 10),
    breaks = function(x) {
      pretty_range <- pretty(x, n = 5)
      pretty_range[pretty_range > 0]
    },
    labels = function(x) {
      sapply(x, function(val) {
        if (val <= 0) return("")
        format(10^(-val), scientific = TRUE, digits = 1)
      })
    },
    name = "Pfdr",
    guide = guide_legend(override.aes = list(fill = "gray70"))
  ) +
  labs(
    x = "Protein (Outcome)",
    y = "Exposure",
    title = "Exposure-Protein Associations",
    subtitle = "Circle size: -log10(P-value) | Color: Effect estimate"
  ) +
  coord_fixed(ratio = 0.7) +
  scale_x_discrete(expand = expansion(mult = 0.1)) +
  scale_y_discrete(expand = expansion(mult = 0.1))

print(p)

# =============================
# 10. Save figure
# =============================
ggsave(
  "./results/dot_plot.pdf",
  plot = p,
  width = 8,
  height = 2,
  dpi = 300
)
