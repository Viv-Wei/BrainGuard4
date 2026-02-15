# =============================
# 1. Load packages & data
# =============================
load("./data/filtered_data.Rdata")

library(dplyr)
library(ggplot2)
library(patchwork)
library(rlang)

# =============================
# 2. P-value formatting
# =============================
format_p_value <- function(p_value, digits = 2) {
  if (is.na(p_value) || is.null(p_value)) {
    return("p = NA")
  }
  paste0("p = ", formatC(p_value, format = "e", digits = digits))
}

# =============================
# 3. Individual correlation plots
# =============================
create_individual_plots <- function(data, data_type = "All") {
  
  target_genes <- c("LGALS4", "GDF15", "APOM", "GHRL")
  
  plot_list <- list()
  results <- tibble()
  
  for (gene in target_genes) {
    
    csf_col    <- paste0("P9000_CSF_", gene, "_NPX")
    plasma_col <- paste0("P9000_Plasma_", gene, "_NPX")
    
    if (!all(c(csf_col, plasma_col) %in% colnames(data))) next
    
    plot_data <- data %>%
      filter(
        !is.na(!!sym(csf_col)),
        !is.na(!!sym(plasma_col))
      ) %>%
      mutate(
        !!csf_col    := as.numeric(!!sym(csf_col)),
        !!plasma_col := as.numeric(!!sym(plasma_col))
      ) %>%
      filter(
        !is.na(!!sym(csf_col)),
        !is.na(!!sym(plasma_col))
      )
    
    if (nrow(plot_data) < 3) next
    
    cor_test <- cor.test(
      plot_data[[csf_col]],
      plot_data[[plasma_col]]
    )
    
    r_value   <- round(cor_test$estimate, 3)
    p_raw     <- cor_test$p.value
    p_display <- format_p_value(p_raw)
    
    results <- bind_rows(
      results,
      tibble(
        Gene = gene,
        Data_Type = data_type,
        N_pairs = nrow(plot_data),
        Correlation_r = r_value,
        P_value_raw = p_raw,
        P_value_display = p_display,
        CI_lower = round(cor_test$conf.int[1], 3),
        CI_upper = round(cor_test$conf.int[2], 3)
      )
    )
    
    p <- ggplot(plot_data, aes_string(x = plasma_col, y = csf_col)) +
      geom_point(
        alpha = 0.7,
        color = "#3678B1",
        size = 2.5
      ) +
      geom_smooth(
        method = "lm",
        se = TRUE,
        color = "#DA4C35",
        fill = "#B63336",
        alpha = 0.2
      ) +
      labs(
        title = paste0(gene, " - CSF vs Plasma (", data_type, ")"),
        subtitle = paste0(
          "r = ", r_value, ", ",
          p_display,
          " (n = ", nrow(plot_data), ")"
        ),
        x = paste0("Plasma ", gene, " (NPX)"),
        y = paste0("CSF ", gene, " (NPX)")
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
      ) +
      annotate(
        "text",
        x = -Inf,
        y = Inf,
        label = paste0("r = ", r_value, "\n", p_display),
        hjust = -0.1,
        vjust = 1.5,
        size = 4,
        fontface = "bold",
        color = ifelse(p_raw < 0.05, "red", "black")
      )
    
    plot_list[[gene]] <- p
    
    ggsave(
      paste0("./results/Correlation_", gene, "_", data_type, ".pdf"),
      p,
      width = 6,
      height = 6
    )
  }
  
  write.csv(
    results,
    paste0("./data/CSF_Plasma_Correlation_Results_", data_type, ".csv"),
    row.names = FALSE
  )
  
  list(
    plots = plot_list,
    results = results
  )
}

# =============================
# 4. Combined plot
# =============================
create_combined_plot <- function(plot_list, data_type = "All") {
  
  if (length(plot_list) < 2) return(NULL)
  
  combined_plot <- wrap_plots(plot_list, ncol = 2)
  
  ggsave(
    paste0("./results/All_Genes_Correlation_Comparison_", data_type, ".pdf"),
    combined_plot,
    width = 14,
    height = 16
  )
  
  combined_plot
}

# =============================
# 5. Run analysis
# =============================
filtered_results <- create_individual_plots(
  filtered_data,
  data_type = "Stable_HC"
)
