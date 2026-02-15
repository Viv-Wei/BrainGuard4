# =============================
# 1. Load packages
# =============================
library(ggplot2)
library(dplyr)
library(scales)
library(colorspace)

# =============================
# 2. Read data
# =============================
pie_data <- read.csv("./data/pie_data.csv")

# =============================
# 3. Filter exposures
# =============================
target_exposures <- c("LGALS4", "GDF15", "APOM", "GHRL")

pie_data <- pie_data %>%
  filter(exposure %in% target_exposures) %>%
  mutate(
    exposure = factor(exposure, levels = target_exposures),
    class_id = factor(class_id, levels = unique(class_id))
  )

# =============================
# 4. Define color palette
# =============================
all_colors <- c(
  "Immune cells & cytokines"            = "#CC3720",
  "Amino acids & derivatives"           = "#1EB2C2",
  "Energy & carbohydrate metabolism"    = "#163261",
  "Microbiota & metabolites"            = "#8083AD",
  "Sphingolipids & phospholipids"       = "#E9572A",
  "Fatty acid & cholesterol metabolism" = "#246BA8",
  "Vitamins & cofactors"                = "#E59067",
  "Nutrient metabolism"                 = "#98C034",
  "Nucleotide metabolism"               = "#F2D03D"
)

current_categories <- unique(pie_data$category)
fill_colors <- all_colors[names(all_colors) %in% current_categories]

# =============================
# 5. Faceted pie chart
# =============================
p <- ggplot(pie_data, aes(x = "", y = percentage, fill = category, group = class_id)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~ exposure, ncol = 2) +
  scale_fill_manual(values = fill_colors) +
  geom_text(aes(label = label, y = midpoint), color = "black", size = 4) +
  theme_void(base_size = 14) +
  theme(
    strip.text = element_text(size = 14, face = "bold", margin = margin(b = 10)),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(nrow = 3))

# =============================
# 6. Display & save
# =============================
print(p)

ggsave(
  "./results/Faceted_Pie_Charts.pdf",
  plot = p,
  width = 10,
  height = 10
)
