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

