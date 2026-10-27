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

## ----shared-data, include = FALSE---------------------------------------------
set.seed(3)
n <- 40
df <- data.frame(
  USUBJID = rep(paste0("S", seq_len(n)), each = 2),
  TRTP    = rep(c(rep("Placebo", n / 2), rep("Active", n / 2)), each = 2),
  AVISIT  = rep(c("Baseline", "Week 24"), times = n),
  AVAL    = c(rbind(rep(0, n),
                    c(rnorm(n / 2, -10, 5), rnorm(n / 2, -6, 5)))),
  stringsAsFactors = FALSE
)
md <- prep_mirror_data(df, trt_levels = c("Placebo", "Active"),
                       time_baseline = "Baseline", time_endpoint = "Week 24",
                       value_label = "% Change from Baseline")
md <- add_summary(md, c("Placebo" = -10.2, "Active" = -5.9),
                  stat_label = "LS-Mean")
md <- add_difference(md, estimate = 4.3, ci_lower = 1.2, ci_upper = 7.4,
                     p_value = 0.011)

## ----extend-------------------------------------------------------------------
p <- mirror_slope_plot(
  md,
  trt_colors      = mirror_palette("colorblind"),
  show_difference = TRUE,
  show_slowing    = TRUE
)

p +
  labs(subtitle = "Exploratory analysis") +
  theme(
    plot.background = element_rect(fill = "grey98", color = NA),
    axis.text       = element_text(size = 11)
  )

## ----palettes-----------------------------------------------------------------
mirror_palette("default")
mirror_palette("colorblind")

# Reverse if you want the stronger colour on the reference arm
mirror_palette("colorblind", reversed = TRUE)

## ----custom-colors------------------------------------------------------------
mirror_slope_plot(
  md,
  trt_colors      = c("#4D4D4D", "#1B7837"),
  show_difference = TRUE,
  show_slowing    = TRUE,
  title           = "Custom colours"
)

## ----base-size----------------------------------------------------------------
mirror_slope_plot(
  md,
  trt_colors      = mirror_palette("colorblind"),
  show_difference = TRUE,
  show_slowing    = TRUE,
  base_size       = 14,
  title           = "Larger base size for presentation"
)

## ----terminology--------------------------------------------------------------
mirror_slope_plot(
  md,
  trt_colors      = mirror_palette("colorblind"),
  show_slowing    = TRUE,
  outcome_label   = "worsening",
  title           = "Custom outcome terminology"
)

## ----patchwork, eval = requireNamespace("patchwork", quietly = TRUE)----------
library(patchwork)

# A second endpoint
set.seed(9)
df2 <- df
df2$AVAL <- c(rbind(rep(0, n),
                    c(rnorm(n / 2, -8, 4), rnorm(n / 2, -5, 4))))
md2 <- prep_mirror_data(df2, trt_levels = c("Placebo", "Active"),
                        time_baseline = "Baseline", time_endpoint = "Week 24",
                        value_label = "% Change (Secondary Endpoint)")
md2 <- add_summary(md2, c("Placebo" = -8.1, "Active" = -4.8),
                   stat_label = "LS-Mean")
md2 <- add_difference(md2, estimate = 3.3, p_value = 0.04)

pa <- mirror_slope_plot(md,  trt_colors = mirror_palette("colorblind"),
                        show_difference = TRUE, show_slowing = TRUE,
                        title = "Primary endpoint")
pb <- mirror_slope_plot(md2, trt_colors = mirror_palette("colorblind"),
                        show_difference = TRUE, show_slowing = TRUE,
                        title = "Secondary endpoint")

pa / pb

## ----saving, eval = FALSE-----------------------------------------------------
# mirror_slope_plot(
#   md,
#   trt_colors      = mirror_palette("colorblind"),
#   show_difference = TRUE,
#   show_slowing    = TRUE,
#   save_path       = "figure1.pdf",
#   width           = 9,
#   height          = 6
# )

