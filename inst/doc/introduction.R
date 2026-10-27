## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse   = TRUE,
  comment    = "#>",
  fig.width  = 8,
  fig.height = 6,
  dpi        = 150
)
library(mirrorslope)
library(ggplot2)

## ----data---------------------------------------------------------------------
set.seed(1)
n <- 40
df <- data.frame(
  USUBJID = rep(paste0("S", seq_len(n)), each = 2),
  TRTP    = rep(c(rep("Placebo", n / 2), rep("Active", n / 2)), each = 2),
  AVISIT  = rep(c("Baseline", "Week 24"), times = n),
  AVAL    = c(rbind(rep(0, n),
                    c(rnorm(n / 2, -10, 5), rnorm(n / 2, -6, 5)))),
  stringsAsFactors = FALSE
)
head(df, 4)

## ----minimal------------------------------------------------------------------
md <- prep_mirror_data(
  df,
  trt_levels    = c("Placebo", "Active"),
  time_baseline = "Baseline",
  time_endpoint = "Week 24",
  value_label   = "% Change from Baseline"
)

md <- add_summary(
  md,
  estimates  = c("Placebo" = -10.2, "Active" = -5.9),
  stat_label = "LS-Mean"
)

mirror_slope_plot(
  md,
  trt_colors = mirror_palette("colorblind"),
  title      = "Percent Change from Baseline at Week 24"
)

## ----difference---------------------------------------------------------------
md <- add_difference(
  md,
  estimate = 4.3,
  ci_lower = 1.2,
  ci_upper = 7.4,
  p_value  = 0.011
)

mirror_slope_plot(
  md,
  trt_colors      = mirror_palette("colorblind"),
  show_difference = TRUE,
  show_slowing    = TRUE,
  title           = "Percent Change from Baseline at Week 24",
  footnote        = "LS-means from a mixed model. Positive difference favours the active arm."
)

## ----precision----------------------------------------------------------------
md <- add_precision(
  md,
  ci = list("Placebo" = c(-13.3, -7.1),
            "Active"  = c(-8.4, -3.4))
)

mirror_slope_plot(
  md,
  trt_colors      = mirror_palette("colorblind"),
  show_difference = TRUE,
  show_slowing    = TRUE,
  show_precision  = TRUE,
  title           = "Percent Change from Baseline at Week 24"
)

## ----spaghetti----------------------------------------------------------------
mirror_slope_plot(
  md,
  trt_colors      = mirror_palette("colorblind"),
  show_spaghetti  = TRUE,
  spaghetti_alpha = 0.2,
  show_difference = TRUE,
  show_slowing    = TRUE,
  title           = "With individual trajectories"
)

## ----inspect------------------------------------------------------------------
md
summary(md)

