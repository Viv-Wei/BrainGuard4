#----------------------------- forest -----------------------------


# =============================
# 1. Load packages
# =============================
library(tidyverse)
library(readxl)

# =============================
# 2. Read data
# =============================
df <- read_xlsx("./data/Figure1a_forest_data_0805.xlsx")

# =============================
# 3. Data preprocessing
# =============================
df <- df %>%
  separate(CI, into = c("CI_low", "CI_high"), sep = "-", convert = TRUE) %>%
  mutate(
    OR      = as.numeric(OR),
    CI_low  = as.numeric(CI_low),
    CI_high = as.numeric(CI_high),
    Index   = as.numeric(factor(Outcome))
  )

str(df)

# =============================
# 4. Forest plot
# =============================
ggplot(df, aes(x = OR, y = factor(Outcome, levels = rev(Outcome)))) +
  
  # Alternating background
  geom_rect(
    aes(
      xmin = -Inf, xmax = Inf,
      ymin = as.numeric(factor(Outcome, levels = rev(Outcome))) - 0.5,
      ymax = as.numeric(factor(Outcome, levels = rev(Outcome))) + 0.5
    ),
    fill  = rep(c("grey95", "white"), length.out = nrow(df)),
    alpha = 0.5
  ) +
  
  # 95% CI
  geom_segment(
    aes(x = CI_low, xend = CI_high,
        y = Outcome, yend = Outcome),
    color = "black",
    linewidth = 0.8
  ) +
  
  # OR point
  geom_point(size = 5, color = "#E41A1C") +
  
  # Reference line
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  
  scale_y_discrete(expand = expansion(mult = 0.000001)) +
  labs(x = "OR (95% CI)", y = "Outcome") +
  theme_minimal() +
  theme(
    panel.grid   = element_blank(),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
    axis.text    = element_text(size = 12, color = "black"),
    axis.title   = element_text(size = 14, color = "black")
  )

# =============================
# 5. Save figure
# =============================
ggsave("./results/forest.pdf", width = 9, height = 6)

#----------------------------- Restricted Cubic Spline -----------------------------

# =============================
# 1. Load packages
# =============================
library(survival)
library(rms)
library(ggplot2)
library(nhanesR)

# =============================
# 2. Read data
# =============================
d <- read_rds("./data/figure1a_data.rds")

# =============================
# 3. Variable recoding
# =============================
d$APOEe4_carrier <- as_factor(d$APOEe4_carrier)

d$APOEe4_carrier <- Recode(
  d$APOEe4_carrier,
  "0::0",
  "1::1",
  "9::9",
  "NA::9",
  to.numeric = FALSE
)

# =============================
# 4. Setup rms environment
# =============================
dd <- datadist(d)
options(datadist = "dd")

# =============================
# 5. Fit Cox model with RCS
# =============================
k <- 4  # number of knots

fit_rcs <- cph(
  Surv(Dementia_interval, Dementia_type) ~
    rcs(glucose, k) +
    age + sex + edu + race + TDI +
    APOEe4_carrier +
    baseline_cancer + baseline_stroke + baseline_ckd +
    High_cholesterol_hypertension_DM_Medicine +
    Aspirin_other_anti_inflammatory_drugs,
  data = d,
  x = TRUE,
  y = TRUE,
  surv = TRUE
)

# =============================
# 6. Prediction (HR scale)
# =============================
ref_glu <- median(d$glucose, na.rm = TRUE)

pdat <- Predict(fit_rcs, glucose, ref.zero = TRUE, fun = exp)
pdat <- as.data.frame(pdat)

# =============================
# 7. Plot HR curve
# =============================
ggplot(pdat, aes(x = glucose, y = yhat)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(
    x = "Glucose",
    y = paste0("Hazard Ratio (ref = ", round(ref_glu, 2), ")")
  ) +
  theme_classic()

# =============================
# 8. Save figure
# =============================
ggsave("./results/RCS_plot.pdf", width = 9, height = 6)

