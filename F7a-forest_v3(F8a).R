library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(readxl)
library(scales)
library(patchwork)
library(colorspace)

# ===========================
# 0. Path Configuration
# ===========================
input_dir <- "./data/F7/"
output_dir <- "./results/F7/"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ===========================
# 1. Color and Order Definitions
# ===========================
my_colors <- c(
  "UKB"     = "#E41A1C",  
  "CHARLS"  = "#518CB5",  
  "SHARE"   = "#F6914F", 
  "KLOSA"   = "#5FAE61", 
  "MHAS"    = "#48C2CB", 
  "HRS"     = "#D2575C", 
  "ELSA"    = "#9981BB", 
  "META"    = "#999999", "meta" = "#999999", 
  "MEGA"    = "#9C7774", "mega" = "#9C7774",
  "LASI"    = "#FFD92F", 
  "NHANES"  = "#8DA0CB", 
  "CLHLS"   = "#E7298A"   
)

# Define which databases to keep for Left and Right layouts
left_keep_dbs <- c("ELSA", "SHARE", "HRS", "NHANES")
right_keep_dbs <- c("MHAS", "CHARLS", "KLOSA", "META")

# Function to generate gradient colors for database quartiles (Q1-Q4)
make_db_colors <- function(db, base_col) {
  if(is.na(base_col)) return(c("grey", "grey", "grey", "grey"))
  cols <- colorRampPalette(c(lighten(base_col, 0.6), base_col))(4)
  names(cols) <- paste(db, paste0("Q", 1:4), sep = ".")
  return(cols)
}

# Robust HR extraction function: handles regex matching to prevent errors
clean_meta_data <- function(df) {
  df %>%
    mutate(
      # Extract first number as HR
      HR = as.numeric(str_extract(`HR (95% CI)`, "[0-9.]+")),
      # Extract part after '(' and before ',' as CI_low
      CI_low = as.numeric(str_extract(`HR (95% CI)`, "(?<=\\()[0-9.]+")),
      # Extract part after ',' and before ')' as CI_high
      CI_high = as.numeric(str_extract(`HR (95% CI)`, "(?<=, )[0-9.]+")),
      Label = paste(Database, Comparison, sep = " - "),
      is_ref = str_detect(Comparison, regex("reference", ignore_case = TRUE))
    )
}

# ===========================
# 2. Left Plot Generation Function
# ===========================
generate_left_plot <- function(file_path, save_path) {
  # Read and filter data for specific databases
  raw_df <- read_excel(file_path) %>% filter(Database %in% left_keep_dbs)
  if(nrow(raw_df) == 0) return()
  
  df_clean <- clean_meta_data(raw_df)
  db_factor <- factor(df_clean$Database, levels = left_keep_dbs)
  df_clean <- df_clean %>% arrange(db_factor)
  df_clean$Label <- factor(df_clean$Label, levels = rev(unique(df_clean$Label)))
  
  # Text component: Labels
  p_text_label <- ggplot(df_clean, aes(y = Label)) +
    geom_text(aes(x = 0, label = Label), hjust = 0, size = 3.5, fontface = "bold") +
    scale_y_discrete(limits = levels(df_clean$Label)) + theme_void()
  
  # Text component: Cases/N
  p_text_cases <- ggplot(df_clean, aes(y = Label)) +
    geom_text(aes(x = 0, label = `Cases/N`), hjust = 0.5, size = 3.5) +
    scale_y_discrete(limits = levels(df_clean$Label)) + theme_void() + labs(title = "Cases/N") +
    theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))
  
  # Text component: HR display
  p_text_hr <- ggplot(df_clean, aes(y = Label)) +
    geom_text(aes(x = 0, label = `HR (95% CI)`), hjust = 0.5, size = 3.5) +
    scale_y_discrete(limits = levels(df_clean$Label)) + theme_void() + labs(title = "HR (95% CI)") +
    theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))
  
  # Forest Plot Component
  p1 <- ggplot(df_clean, aes(y = Label, color = Database)) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_errorbar(data = subset(df_clean, !is_ref), aes(xmin = CI_low, xmax = CI_high), width = 0.2, color = "grey70", size = 0.9) +
    geom_point(data = subset(df_clean, !is_ref), aes(x = HR), size = 3.5) +
    geom_point(data = subset(df_clean, is_ref), aes(x = 1), shape = 18, size = 3.5, color = "black") +
    scale_y_discrete(limits = levels(df_clean$Label)) + theme_minimal(base_size = 15) + labs(x = "Hazard Ratio", y = "") +
    scale_color_manual(values = my_colors) + theme(panel.grid = element_blank(), axis.line.x = element_line(), axis.text.y = element_blank(), legend.position = "none")
  
  # Bar Chart Component: Incidence Rates
  df_rates <- raw_df %>%
    mutate(
      Cases = as.numeric(str_extract(`Cases/N`, "^[0-9]+")),
      N     = as.numeric(str_extract(`Cases/N`, "(?<=/)[0-9]+")),
      Rate_Raw = Cases / N, 
      Label = paste(Database, Comparison, sep = " - "),
      Group = interaction(Database, Comparison),
      Rate = ifelse(str_detect(Comparison, "Q[1-4]"), Rate_Raw, NA) 
    )
  df_rates$Label <- factor(df_rates$Label, levels = levels(df_clean$Label))
  db_list_present <- intersect(left_keep_dbs, unique(df_rates$Database))
  gradient_colors <- unlist(lapply(db_list_present, function(db) { make_db_colors(db, my_colors[db]) }))
  
  p2 <- ggplot(df_rates, aes(x = -Rate, y = Label, fill = Group)) +
    geom_col(width = 0.5) +
    geom_text(aes(label = percent(Rate, accuracy = 0.1), x = -Rate - 0.005), hjust = 1, size = 3.5, na.rm = TRUE) + 
    scale_fill_manual(values = gradient_colors) + scale_x_continuous(labels = function(x) percent(abs(x)), expand = expansion(mult = c(0.15, 0.05))) +
    scale_y_discrete(limits = levels(df_clean$Label)) + theme_minimal(base_size = 15) + labs(x = "Cases (%)", y = "") +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_blank(), axis.line.x = element_line(), legend.position = "none")
  
  # Assemble final patchwork plot
  final_plot <- p_text_label + p_text_cases + p_text_hr + p1 + p2 + plot_layout(nrow = 1, widths = c(1.2, 0.8, 1, 1.5, 1.2))
  # Dynamic height calculation based on row count
  ggsave(save_path, plot = final_plot, width = 7.5, height = 0.8 + nrow(df_clean)*0.3, units = "in")
}

# ===========================
# 3. Right Plot Generation Function
# ===========================
generate_right_plot <- function(file_path, save_path) {
  # Read and filter data
  raw_df <- read_excel(file_path) %>% filter(Database %in% right_keep_dbs)
  if(nrow(raw_df) == 0) return()
  
  df_clean <- clean_meta_data(raw_df)
  db_factor <- factor(df_clean$Database, levels = right_keep_dbs)
  df_clean <- df_clean %>% arrange(db_factor)
  df_clean$Label <- factor(df_clean$Label, levels = unique(df_clean$Label))
  plot_limits <- rev(levels(df_clean$Label))
  
  # Text component: Labels
  p_text_label <- ggplot(df_clean, aes(y = Label)) +
    geom_text(aes(x = 0, label = Label), hjust = 0, size = 3.5, fontface = "bold") +
    scale_y_discrete(limits = plot_limits) + theme_void()
  
  # Bar Chart Component: Incidence Rates
  df_rates <- raw_df %>%
    mutate(
      Cases = as.numeric(str_extract(`Cases/N`, "^[0-9]+")),
      N     = as.numeric(str_extract(`Cases/N`, "(?<=/)[0-9]+")),
      Rate_Raw = Cases / N,
      Label = paste(Database, Comparison, sep = " - "),
      Group = interaction(Database, Comparison),
      Rate = ifelse(str_detect(Comparison, "Q[1-4]"), Rate_Raw, NA)
    )
  df_rates$Label <- factor(df_rates$Label, levels = levels(df_clean$Label))
  db_list_present <- intersect(right_keep_dbs, unique(df_rates$Database))
  gradient_colors <- unlist(lapply(db_list_present, function(db) { make_db_colors(db, my_colors[db]) }))
  
  p2 <- ggplot(df_rates, aes(x = Rate, y = Label, fill = Group)) +
    geom_col(width = 0.5) +
    geom_text(aes(label = percent(Rate, accuracy = 0.1), x = Rate + 0.005), hjust = 0, size = 3.5, na.rm = TRUE) +
    scale_fill_manual(values = gradient_colors) + scale_x_continuous(labels = percent, expand = expansion(mult = c(0, 0.2))) +
    scale_y_discrete(limits = plot_limits) + theme_minimal(base_size = 15) + labs(x = "Cases (%)", y = "") +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_blank(), axis.line.x = element_line(), legend.position = "none")
  
  # Specialized Meta-analysis plotting (Diamonds)
  meta_df <- subset(df_clean, Database == "META" & !is_ref)
  p1 <- ggplot(df_clean, aes(y = Label, color = Database)) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_errorbar(data = subset(df_clean, !is_ref & Database != "META"), aes(xmin = CI_low, xmax = CI_high), width = 0.2, color = "grey70", size = 0.9) +
    geom_point(data = subset(df_clean, !is_ref & Database != "META"), aes(x = HR), size = 3.5) +
    # Add diamond shape for Meta rows
    {if(nrow(meta_df) > 0) geom_errorbarh(data = meta_df, aes(y = Label, xmin = CI_low, xmax = CI_high), height = 0.2, size = 1, color = "black")} +
    {if(nrow(meta_df) > 0) geom_point(data = meta_df, aes(y = Label, x = HR, fill = Database), shape = 23, size = 4, color = "black")} +
    geom_point(data = subset(df_clean, is_ref), aes(x = 1), shape = 18, size = 3.5, color = "black") +
    scale_color_manual(values = my_colors) + scale_fill_manual(values = my_colors) +
    scale_y_discrete(limits = plot_limits) + labs(x = "Hazard Ratio (95% CI)", y = "") +
    theme_minimal(base_size = 15) + theme(panel.grid = element_blank(), axis.line.x = element_line(), axis.text.y = element_blank(), legend.position = "none")
  
  # Text component: HR display
  p_text_hr <- ggplot(df_clean, aes(y = Label)) +
    geom_text(aes(x = 0, label = `HR (95% CI)`), hjust = 0.5, size = 3.5) +
    scale_y_discrete(limits = plot_limits) + theme_void() + labs(title = "HR (95% CI)") +
    theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))
  
  # Text component: Cases/N
  p_text_cases <- ggplot(df_clean, aes(y = Label)) +
    geom_text(aes(x = 0, label = `Cases/N`), hjust = 0.5, size = 3.5) +
    scale_y_discrete(limits = plot_limits) + theme_void() + labs(title = "Cases/N") +
    theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))
  
  # Assemble final patchwork plot
  final_plot <- p_text_label + p2 + p1 + p_text_hr + p_text_cases + plot_layout(nrow = 1, widths = c(1.2, 1.2, 1.8, 1, 0.8))
  ggsave(save_path, plot = final_plot, width = 7.5, height = 0.8 + nrow(df_clean)*0.3, units = "in")
}

# ===========================
# 4. Batch Execution
# ===========================
run_batch <- function() {
  files <- list.files(input_dir, pattern = "\\.xlsx$", full.names = TRUE)
  for (file in files) {
    filename <- basename(file)
    save_path <- file.path(output_dir, paste0(tools::file_path_sans_ext(filename), ".pdf"))
    tryCatch({
      # Detect orientation keyword in filename to apply correct function
      if (str_detect(filename, "左")) {
        message("Processing Left: ", filename)
        generate_left_plot(file, save_path)
      } else if (str_detect(filename, "右")) {
        message("Processing Right: ", filename)
        generate_right_plot(file, save_path)
      }
    }, error = function(e) message("❌ Error in ", filename, ": ", e$message))
  }
}

run_batch()