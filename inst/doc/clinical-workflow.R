## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse   = TRUE,
  comment    = "#>",
  fig.width  = 9,
  fig.height = 6,
  dpi        = 150
)
library(mirrorslope)
library(ggplot2)

## ----simulate-----------------------------------------------------------------
set.seed(7)
n_subj <- 60

adsl <- data.frame(
  USUBJID = paste0("SUBJ-", sprintf("%03d", seq_len(n_subj))),
  TRT01P  = rep(c("Placebo", "Active"), each = n_subj / 2),
  SAFFL   = "Y",
  ITTFL   = "Y",
  stringsAsFactors = FALSE
)
# A few subjects out of the safety population
adsl$SAFFL[c(3, 17, 42)] <- "N"

base_val <- rnorm(n_subj, 0, 4)
chg_val  <- ifelse(adsl$TRT01P == "Placebo",
                   rnorm(n_subj, -11, 6),
                   rnorm(n_subj,  -6, 6))

adqs <- data.frame(
  USUBJID  = rep(adsl$USUBJID, each = 2),
  AVISIT   = rep(c("Baseline", "Month 12"), times = n_subj),
  PARAMCD  = "FUNCSCORE",
  CHG      = as.numeric(rbind(rep(0, n_subj), chg_val)),
  stringsAsFactors = FALSE
)
head(adqs, 4)

## ----merge--------------------------------------------------------------------
adqs <- merge(
  adqs,
  adsl[, c("USUBJID", "TRT01P", "SAFFL", "ITTFL")],
  by = "USUBJID",
  all.x = TRUE
)
head(adqs, 4)

## ----prep---------------------------------------------------------------------
md <- prep_mirror_data(
  adqs,
  subject_var      = "USUBJID",
  trt_var          = "TRT01P",
  trt_levels       = c("Placebo", "Active"),
  time_var         = "AVISIT",
  time_baseline    = "Baseline",
  time_endpoint    = "Month 12",
  value_var        = "CHG",
  pop_flags        = list(SAFFL = "Y", ITTFL = "Y"),
  population_label = "Safety / ITT",
  value_label      = "Change from Baseline (Functional Score)"
)
md

## ----estimates----------------------------------------------------------------
md <- add_summary(
  md,
  estimates  = c("Placebo" = -11.3, "Active" = -6.1),
  stat_label = "LS-Mean"
)

md <- add_precision(
  md,
  ci = list("Placebo" = c(-14.0, -8.6),
            "Active"  = c(-8.7, -3.5))
)

md <- add_difference(
  md,
  estimate = 5.2,
  ci_lower = 1.9,
  ci_upper = 8.5,
  p_value  = 0.003
)
md

## ----figure-------------------------------------------------------------------
mirror_slope_plot(
  md,
  trt_colors      = mirror_palette("colorblind"),
  show_difference = TRUE,
  show_slowing    = TRUE,
  show_precision  = TRUE,
  title           = "Functional Score: Change from Baseline at Month 12",
  footnote        = paste(
    "LS-means and 95% CIs from a mixed model for repeated measures.",
    "Safety/ITT population. Positive difference favours the active arm."
  )
)

