# Quick visual check. Run after: devtools::load_all(".")
# Produces three PNGs in the working directory.
library(ggplot2)

set.seed(42)
n <- 40
df <- data.frame(
  USUBJID = rep(paste0("S", 1:n), each = 2),
  TRTP    = rep(c(rep("Placebo", n/2), rep("Deramiocel", n/2)), each = 2),
  AVISIT  = rep(c("Baseline", "Month 12"), times = n),
  AVAL    = c(rbind(rep(0, n),
                    c(rnorm(n/2, -10.7, 5), rnorm(n/2, -6.5, 5)))),
  SAFFL   = "Y",
  stringsAsFactors = FALSE
)

md <- prep_mirror_data(
  df, trt_levels = c("Placebo", "Deramiocel"),
  time_baseline = "Baseline", time_endpoint = "Month 12",
  pop_flags = list(SAFFL = "Y"), population_label = "Safety",
  value_label = "% Change from Baseline"
)
md <- add_summary(md, c("Placebo" = -10.7, "Deramiocel" = -6.5),
                  stat_label = "LS-Mean")
md <- add_precision(md, ci = list("Placebo" = c(-13.8, -7.6),
                                  "Deramiocel" = c(-8.9, -4.1)))
md <- add_difference(md, estimate = 4.2, ci_lower = 1.1, ci_upper = 7.3,
                     p_value = 0.012)

# 1. Minimal
p1 <- mirror_slope_plot(md, title = "Minimal")
ggsave("plot1_minimal.png", p1, width = 8, height = 6, dpi = 120)

# 2. Full annotations
p2 <- mirror_slope_plot(
  md, show_difference = TRUE, show_slowing = TRUE, show_precision = TRUE,
  trt_colors = mirror_palette("capricor"),
  title = "PUL 2.0 - Percent Change from Baseline at Month 12",
  footnote = "LS-means from MMRM. ITT population. Positive difference favours treatment."
)
ggsave("plot2_full.png", p2, width = 9, height = 6, dpi = 120)

# 3. Spaghetti
p3 <- mirror_slope_plot(
  md, show_spaghetti = TRUE, show_difference = TRUE, show_slowing = TRUE,
  title = "With individual trajectories"
)
ggsave("plot3_spaghetti.png", p3, width = 9, height = 6, dpi = 120)

cat("Rendered plot1_minimal.png, plot2_full.png, plot3_spaghetti.png\n")
