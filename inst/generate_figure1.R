# =============================================================================
# Generate Figure 1 for the mirrored slopegraph paper, using the mirrorslope
# package. Run from inside the package directory (the folder with DESCRIPTION),
# or after installing mirrorslope.
#
#   devtools::load_all(".")   # or: library(mirrorslope)
#   source("generate_figure1.R")
#
# Produces: figure1_mirrored_slopegraph.png (300 dpi, publication size)
# =============================================================================

library(ggplot2)

# ---- Simulated worked-example data -----------------------------------------
# Two arms, percent change from baseline at Month 12. Constructed so the arm
# means match the values reported in the paper's worked example:
#   Comparator (Placebo): mean -10.7%, n = 20
#   Treated (Active):     mean  -6.5%, n = 22
# Between-group difference = 4.2 percentage points, favouring the treated arm.

set.seed(2024)

make_arm <- function(ids, target_mean, sd = 6) {
  raw <- rnorm(length(ids), target_mean, sd)
  raw <- raw - mean(raw) + target_mean          # force exact arm mean
  raw
}

comp_ids <- paste0("C", 1:20)
trt_ids  <- paste0("T", 1:22)

comp_chg <- make_arm(comp_ids, -10.7)
trt_chg  <- make_arm(trt_ids,   -6.5)

df <- data.frame(
  USUBJID = c(rep(comp_ids, each = 2), rep(trt_ids, each = 2)),
  TRTP    = c(rep("Placebo", length(comp_ids) * 2),
              rep("Active",  length(trt_ids)  * 2)),
  AVISIT  = c(rep(c("Baseline", "Month 12"), length(comp_ids)),
              rep(c("Baseline", "Month 12"), length(trt_ids))),
  AVAL    = c(as.numeric(rbind(rep(0, length(comp_ids)), comp_chg)),
              as.numeric(rbind(rep(0, length(trt_ids)),  trt_chg))),
  stringsAsFactors = FALSE
)

# ---- Build the mirror_data object and the figure ---------------------------
md <- prep_mirror_data(
  df,
  trt_levels    = c("Placebo", "Active"),
  time_baseline = "Baseline",
  time_endpoint = "Month 12",
  value_label   = "% Change from Baseline"
)

md <- add_summary(
  md,
  estimates  = c("Placebo" = -10.7, "Active" = -6.5),
  stat_label = "Mean"
)

md <- add_difference(
  md,
  estimate = 4.2          # Active minus Placebo (less decline)
)

fig1 <- mirror_slope_plot(
  md,
  trt_colors      = mirror_palette("colorblind"),
  show_spaghetti  = TRUE,
  spaghetti_alpha = 0.25,
  show_difference = TRUE,
  show_slowing    = TRUE,
  show_n          = TRUE,
  jitter_points   = TRUE,
  seed            = 2024,
  title    = "Percent Change from Baseline at Month 12",
  subtitle = "Mirrored slope plot: Placebo and Active",
  footnote = paste(
    "Simulated illustrative data (Placebo n = 20, Active n = 22).",
    "Thin lines are individual trajectories; strip headers give arm means;",
    "the right annotation gives the between-group difference.",
    "The horizontal axis is positional, not a time axis.",
    sep = "\n"
  )
)

ggsave("figure1_mirrored_slopegraph.png", fig1,
       width = 8, height = 7, dpi = 300)

# Vector version for journal submission (fonts embedded by the device).
ggsave("figure1_mirrored_slopegraph.pdf", fig1,
       width = 8, height = 7)

cat("Wrote figure1_mirrored_slopegraph.png and .pdf\n")
