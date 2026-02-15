# =============================
# 1. Clear environment
# =============================
rm(list = ls())

# =============================
# 2. Load required packages
# =============================
library(readxl)
library(dplyr)
library(networkD3)
library(scales)
library(htmlwidgets)

# =============================
# 3. Read data
# =============================
a <- read_xlsx("./data/sankey_pathdata.xlsx")

# =============================
# 4. Filter valid outcomes
# =============================
valid_outcomes <- c(
  "Dementia in Alzheimer disease",
  "Dementia",
  "Vascular dementia"
)

a <- a %>% 
  filter(outcome %in% valid_outcomes)

# =============================
# 5. Extract step data
# =============================
step1_real <- a %>% 
  select(step1_from, step1_to_final, step1_beta, new1)

step2_real <- a %>% 
  select(step1_to_final, step2_to, step2_beta, new2)

# =============================
# 6. Build node order
# =============================
step1_order <- unique(step1_real$step1_from)

step1_to_order <- c()
seen <- c()

for (from in step1_order) {
  tos <- step1_real %>% 
    filter(step1_from == from) %>% 
    pull(step1_to_final)
  
  for (to in tos) {
    if (!(to %in% seen)) {
      step1_to_order <- c(step1_to_order, to)
      seen <- c(seen, to)
    }
  }
}

step2_to_order <- unique(step2_real$step2_to)

# =============================
# 7. Create nodes
# =============================
all_node_names <- c(step1_order, step1_to_order, step2_to_order)

nodes <- data.frame(
  name = all_node_names,
  stringsAsFactors = FALSE
)

nodes$node_id <- 0:(nrow(nodes) - 1)

get_node_id <- function(x) {
  match(x, nodes$name) - 1
}

# =============================
# 8. Create color palettes
# =============================
red_palette <- colorRampPalette(
  c("#F29091", "#BF1D2D", "#C5272D")
)(101)

blue_palette <- colorRampPalette(
  c("#9EAAD1", "#293890", "#0001A1")
)(101)

# =============================
# 9. Standardize beta values
# =============================
all_betas_df <- bind_rows(
  a %>% 
    select(
      name_from = step1_from,
      name_to   = step1_to_final,
      beta      = step1_beta,
      step      = new1
    ) %>% 
    mutate(link_type = "new1"),
  
  a %>% 
    select(
      name_from = step1_to_final,
      name_to   = step2_to,
      beta      = step2_beta,
      step      = new2
    ) %>% 
    mutate(link_type = "new2")
) %>%
  filter(!is.na(beta)) %>%
  mutate(std_beta = scale(beta)[, 1])

# =============================
# 10. Link color function
# =============================
get_color_by_step <- function(beta, std_beta, step_flag) {
  if (is.na(step_flag) || !step_flag) return("#BBBBBB")
  if (is.na(std_beta)) return("#BBBBBB")
  
  color_index <- min(
    100,
    max(1, round((std_beta + 3) * 100 / 6))
  )
  
  if (beta > 0) {
    red_palette[color_index]
  } else {
    blue_palette[color_index]
  }
}

# =============================
# 11. Build step1 links
# =============================
step1_links <- a %>%
  select(step1_from, step1_to_final, step1_beta, new1) %>%
  distinct() %>%
  left_join(
    all_betas_df %>% filter(link_type == "new1"),
    by = c(
      "step1_from"      = "name_from",
      "step1_to_final"  = "name_to",
      "step1_beta"      = "beta"
    )
  ) %>%
  mutate(
    source = get_node_id(step1_from),
    target = get_node_id(step1_to_final),
    value  = 70,
    color  = mapply(get_color_by_step, step1_beta, std_beta, new1),
    group  = "new1"
  ) %>%
  select(source, target, value, color, group)

# =============================
# 12. Build step2 links
# =============================
step2_links <- a %>%
  select(step1_to_final, step2_to, step2_beta, new2) %>%
  distinct() %>%
  left_join(
    all_betas_df %>% filter(link_type == "new2"),
    by = c(
      "step1_to_final" = "name_from",
      "step2_to"       = "name_to",
      "step2_beta"     = "beta"
    )
  ) %>%
  mutate(
    source = get_node_id(step1_to_final),
    target = get_node_id(step2_to),
    value  = 70,
    color  = mapply(get_color_by_step, step2_beta, std_beta, new2),
    group  = "new2"
  ) %>%
  select(source, target, value, color, group)

# =============================
# 13. Combine links
# =============================
all_links <- bind_rows(step1_links, step2_links)

# =============================
# 14. Create color scale
# =============================
unique_colors <- unique(all_links$color)

color_scale <- paste0(
  "d3.scaleOrdinal().domain([",
  paste0(sprintf("'%s'", unique_colors), collapse = ","),
  "]).range([",
  paste0(sprintf("'%s'", unique_colors), collapse = ","),
  "])"
)

# =============================
# 15. Create Sankey plot
# =============================
p <- sankeyNetwork(
  Links       = all_links,
  Nodes       = nodes,
  Source      = "source",
  Target      = "target",
  Value       = "value",
  NodeID      = "name",
  LinkGroup   = "color",
  colourScale = color_scale,
  fontSize    = 18,
  nodeWidth   = 80,
  nodePadding = 20,
  height      = 800,
  width       = 2000,
  sinksRight  = FALSE,
  iterations  = 128
)

# =============================
# 16. Style adjustment
# =============================
p <- htmlwidgets::onRender(
  p,
  "
  function(el, x) {
    d3.select(el).selectAll('.link')
      .style('stroke-width', function(d) {
        return Math.max(5, Math.min(12, d.value * 0.8)) + 'px';
      });
    d3.select(el).selectAll('.node text')
      .style('text-shadow', '0 0 2px white, 0 0 2px white')
      .style('font-weight', 'bold');
  }
  "
)

# =============================
# 17. Save output
# =============================
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

saveWidget(
  p,
  paste0("./plot/sankey_final_", timestamp, ".html"),
  selfcontained = TRUE
)
