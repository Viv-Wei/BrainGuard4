# =============================
# 1. Clear environment
# =============================
rm(list = ls())

# =============================
# 2. Load required packages
# =============================
library(survival)
library(rms)
library(ggplot2)
library(dplyr)

# =============================
# 3. Read data 
# [can replace the data with other cohort data]
# =============================
df <- read.csv("./data/F7/RCS_share.csv")

# =============================
# 4. Create survival object
# =============================
surv_obj <- Surv(
  time  = df$dementia_to_jinzu_days,
  event = df$dementia_type_flag
)

# =============================
# 5. Prepare model data
# =============================
df_model <- df %>%
  select(
    total_score,
    dementia_to_jinzu_days,
    dementia_type_flag,
    sex,
    age,
    BMI
  )

dd <- datadist(df_model)
options(datadist = "dd")

# =============================
# 6. Fit Cox model with RCS
# =============================
fit <- cph(
  surv_obj ~ rcs(total_score, 4) + sex + age + BMI,
  data = df,
  x = TRUE,
  y = TRUE
)

# =============================
# 7. Extract P values
# =============================
anova_fit  <- anova(fit)
p_overall   <- signif(anova_fit[1, "P"], 1)
p_nonlinear <- signif(anova_fit[2, "P"], 2)

# =============================
# 8. Generate predictions (HR)
# =============================
pred <- Predict(
  fit,
  total_score,
  ref.zero = TRUE,
  fun = exp
)

plot_df <- as.data.frame(pred)

# =============================
# 9. Create RCS plot
# =============================
rcs_fig <- ggplot(plot_df, aes(x = total_score, y = yhat)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    fill = "lightblue",
    alpha = 0.4
  ) +
  geom_line(color = "blue", size = 1.2) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "red"
  ) +
  labs(
    x = "Total Score",
    y = "Hazard Ratio (HR) for Dementia",
    title = "Restricted Cubic Spline of Total Score (Cox Model)"
  ) +
  annotate(
    "text",
    x = min(plot_df$total_score) +
      0.1 * diff(range(plot_df$total_score)),
    y = max(plot_df$yhat) * 0.95,
    label = paste0(
      "P for overall = ", p_overall,
      "\nP for nonlinear = ", p_nonlinear
    ),
    hjust = 0,
    size = 5,
    fontface = "italic"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title   = element_text(face = "bold", hjust = 0.5),
    axis.title   = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line    = element_line(color = "black"),
    panel.border = element_blank()
  )

# =============================
# 10. Save plot
# =============================
ggsave(
  "./results/RCS_TotalScore_HR_share.pdf",
  plot = rcs_fig,
  width = 2.66,
  height = 2.99,
  units = "in"
)
