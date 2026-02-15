# =============================
# 1. Load packages
# =============================
library(ggplot2)
library(dplyr)
library(readxl)
library(forcats)
library(scales)

# =============================
# 2. Read data
# =============================
A <- read_excel("./data/figure1_d_Stacked Bar Plot.xlsx")
head(A)

# =============================
# 3. Define color palette
# =============================
my_colors <- c(
  "#4E79A7", "#F28E2B", "#E15759", "lightgrey", "#59A14F", 
  "#EDC948", "#B07AA1", "#FF9DA7", "#bcbd22", "#fdae61", 
  "#86BCB6", "#C70039", "#FABFD2", "#17becf", "#8A3B1B", 
  "#e377c2", "#7E6B3C", "#F0E6A2", "#6B4F8B", "#D35400", 
  "#B9D1E3", "#BB8D6F", "#D3B8E5", "#97C5E2"
)

# =============================
# 4. Percentage calculation
# =============================
A_percent <- A %>%
  group_by(Group) %>%
  mutate(Percent = Count / sum(Count) * 100) %>%
  arrange(Group, Percent) %>%
  ungroup() %>%
  mutate(Category = factor(Category))

# =============================
# 5. Stacked bar plot
# =============================
ggplot(
  A_percent,
  aes(
    x = Group,
    y = Percent,
    fill = fct_reorder2(Category, Group, Percent)
  )
) +
  geom_bar(
    stat = "identity",
    width = 0.7,
    color = "black"
  ) +
  geom_text(
    aes(label = paste0(round(Percent, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "white"
  ) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(
    labels = percent_format(scale = 1)
  ) +
  labs(
    x = "Protein",
    y = "Pathway Percentage (%)",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title  = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 10)
  )

# =============================
# 6. Save figure
# =============================
ggsave("./plot/stacked_bar.pdf", width = 9, height = 6)
