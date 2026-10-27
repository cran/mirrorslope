build_full <- function() {
  md <- add_summary(make_md(), c("Placebo" = -10.2, "Active" = -5.9),
                    stat_label = "LS-Mean")
  md <- add_precision(md, ci = list("Placebo" = c(-13, -7),
                                    "Active"  = c(-8, -4)))
  add_difference(md, estimate = 4.3, ci_lower = 1.1, ci_upper = 7.5,
                 p_value = 0.012)
}

test_that("minimal plot returns a ggplot", {
  md <- add_summary(make_md(), c("Placebo" = -10, "Active" = -6))
  p <- mirror_slope_plot(md)
  expect_s3_class(p, "ggplot")
})

test_that("full annotation plot returns a ggplot", {
  p <- mirror_slope_plot(build_full(), show_difference = TRUE,
                         show_slowing = TRUE, show_precision = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("spaghetti plot returns a ggplot", {
  p <- mirror_slope_plot(build_full(), show_spaghetti = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("show_difference without attached difference errors", {
  md <- add_summary(make_md(), c("Placebo" = -10, "Active" = -6))
  expect_error(mirror_slope_plot(md, show_difference = TRUE),
               "no difference is attached")
})

test_that("plot on object without summary errors", {
  expect_error(mirror_slope_plot(make_md()), "no summary rows")
})

test_that("returned ggplot can be extended", {
  p <- mirror_slope_plot(add_summary(make_md(), c("Placebo" = -10, "Active" = -6)))
  expect_s3_class(p + ggplot2::theme_bw(), "ggplot")
})

test_that("return_data gives plot and data", {
  out <- mirror_slope_plot(build_full(), show_difference = TRUE,
                           return_data = TRUE)
  expect_named(out, c("plot", "data"))
  expect_s3_class(out$plot, "ggplot")
  expect_s3_class(out$data, "data.frame")
})

test_that("custom colors accepted named or unnamed", {
  md <- add_summary(make_md(), c("Placebo" = -10, "Active" = -6))
  expect_s3_class(mirror_slope_plot(md, trt_colors = c("#333", "#0A0")),
                  "ggplot")
  expect_s3_class(
    mirror_slope_plot(md, trt_colors = c("Placebo" = "#333", "Active" = "#0A0")),
    "ggplot")
})

test_that("absolute-scale plot with custom benefit label works", {
  md <- add_summary(make_md(), c("Placebo" = 38, "Active" = 42),
                    baseline = c("Placebo" = 45, "Active" = 45))
  p <- mirror_slope_plot(md, show_slowing = TRUE,
                         benefit_label = "% less decline",
                         y_label = "Functional score")
  expect_s3_class(p, "ggplot")
})

test_that("mirror_palette returns two colors", {
  expect_length(mirror_palette("default"), 2)
  expect_length(mirror_palette("colorblind"), 2)
  expect_equal(mirror_palette("default"),
               rev(mirror_palette("default", reversed = TRUE)))
})

test_that("theme_mirror returns a theme", {
  expect_s3_class(theme_mirror(), "theme")
})

test_that("y_percent toggle does not error either way", {
  md <- add_summary(make_md(), c("Placebo" = -10, "Active" = -6))
  expect_s3_class(mirror_slope_plot(md, y_percent = TRUE),  "ggplot")
  expect_s3_class(mirror_slope_plot(md, y_percent = FALSE), "ggplot")
})

test_that("jitter_points with seed is reproducible and returns a ggplot", {
  md <- add_summary(make_md(), c("Placebo" = -10, "Active" = -6))
  p1 <- mirror_slope_plot(md, show_spaghetti = TRUE, jitter_points = TRUE,
                          seed = 1)
  expect_s3_class(p1, "ggplot")
})

test_that("subtitle renders without error", {
  md <- add_summary(make_md(), c("Placebo" = -10, "Active" = -6))
  expect_s3_class(mirror_slope_plot(md, subtitle = "A subtitle"), "ggplot")
})
