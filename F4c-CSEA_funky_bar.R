# =============================
# 1. Load packages
# =============================
library(tidyverse)
library(openxlsx)
library(data.table)
library(funkyheatmap)
library(Cairo)

# =============================
# 2. Read data
# =============================
j02_CSEA <- readRDS("./data/CSEA_res.RDS")
j02_col_info <- read.xlsx("./data/CSEA-funky_bar.xlsx")

# =============================
# 3. Define color palettes
# =============================
base_cols <- c("#c23921", "#26669f", "#2ca9b7", "#e48162")

yanse_list <- lapply(base_cols, function(cl) {
  colorRampPalette(c(cl, "#FCB461"))(69)
})

j02_palettes <- tibble(
  palette = c("color1", "color2", "color3", "color4"),
  colours = yanse_list
)

# =============================
# 4. Generate funky heatmap
# =============================
p02 <- funky_heatmap(
  data        = j02_CSEA,
  column_info = j02_col_info,
  palettes    = j02_palettes,
  scale_column = FALSE
)

# =============================
# 5. Save figure
# =============================
Cairo::CairoPDF(
  file = "./results/CSEA-funky_bar.pdf",
  width = 8,
  height = 6
)

print(p02)
dev.off()
