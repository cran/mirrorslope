test_that("direction detection compares estimate to baseline", {
  expect_equal(.detect_direction(-5, 0),  "decrease")
  expect_equal(.detect_direction(5, 0),   "increase")
  expect_equal(.detect_direction(38, 45), "decrease")  # absolute scale
  expect_equal(.detect_direction(50, 45), "increase")
  expect_true(is.na(.detect_direction(10, 10)))
  expect_true(is.na(.detect_direction(NA, 0)))
})

test_that("auto labels follow direction", {
  d <- .auto_labels("decrease")
  expect_equal(d$outcome, "decline")
  expect_equal(d$benefit, "% less decline")
  i <- .auto_labels("increase")
  expect_equal(i$outcome, "increase")
  expect_equal(i$benefit, "% less increase")
  u <- .auto_labels(NA)
  expect_equal(u$outcome, "change")
})

test_that("relative benefit uses change from each baseline", {
  # change-from-baseline (baseline 0)
  expect_equal(.relative_benefit(-10, 0, -6, 0), 40)
  # absolute scale: ref change -7, trt change -3 -> |(-7 - -3)/-7|*100
  expect_equal(round(.relative_benefit(38, 45, 42, 45), 1), 57.1)
  expect_equal(.relative_benefit(-10, 0, -10, 0), 0)
})

test_that("relative benefit warns and NA on near-zero reference change", {
  expect_warning(r <- .relative_benefit(5, 5, 3, 5), "near zero")
  expect_true(is.na(suppressWarnings(.relative_benefit(5, 5, 3, 5))))
})

test_that("x layout is symmetric and auto-computes", {
  xl <- .compute_x_layout(12)
  expect_equal(xl$ref_x, 12 - xl$x_offset)
  expect_equal(xl$trt_x, 12 + xl$x_offset)
  expect_true(xl$ref_x > xl$x_min)
  expect_true(xl$trt_x < xl$x_max_data)
})

test_that("x layout respects user x_offset", {
  xl <- .compute_x_layout(12, x_offset = 8)
  expect_equal(xl$ref_x, 4)
  expect_equal(xl$trt_x, 20)
})

test_that("annotation zone inactive with zero rows", {
  xl <- .compute_x_layout(12)
  az <- .compute_annotation_zone(xl, 0)
  expect_false(az$zone_active)
  expect_equal(az$x_max, xl$x_max_data)
})

test_that("annotation zone expands with rows", {
  xl <- .compute_x_layout(12)
  az1 <- .compute_annotation_zone(xl, 1)
  az3 <- .compute_annotation_zone(xl, 3)
  expect_true(az1$zone_active)
  expect_true(az3$x_max > az1$x_max)
  expect_true(az1$rule_x > xl$x_max_data)
})

test_that("manual x_max too small warns and overrides", {
  xl <- .compute_x_layout(12)
  expect_warning(az <- .compute_annotation_zone(xl, 3, user_x_max = 15),
                 "too small")
  expect_true(az$x_max > 15)
})

test_that("manual x_max large enough is honored", {
  xl <- .compute_x_layout(12)
  az <- .compute_annotation_zone(xl, 3, user_x_max = 100)
  expect_equal(az$x_max, 100)
})

test_that("y limits expand with padding and handle flat data", {
  yl <- .compute_y_lim(c(-20, 0, 10))
  expect_true(yl[1] < -20 && yl[2] > 10)
  expect_silent(.compute_y_lim(c(5, 5, 5)))
})

test_that("row counting matches show flags", {
  expect_equal(.count_annotation_rows(TRUE, FALSE, FALSE), 1)
  expect_equal(.count_annotation_rows(TRUE, FALSE, TRUE),  2)
  expect_equal(.count_annotation_rows(TRUE, TRUE,  TRUE),  3)
  expect_equal(.count_annotation_rows(FALSE, TRUE, FALSE), 1)
  expect_equal(.count_annotation_rows(FALSE, FALSE, FALSE), 0)
})

test_that("label formatters produce expected strings", {
  expect_equal(.format_diff_label(4.2),  'Delta ~ "= +4.2%"')
  expect_equal(.format_diff_label(-3.1), 'Delta ~ "= -3.1%"')
  # the plotmath string must parse (it is drawn with annotate(parse = TRUE))
  expect_silent(parse(text = paste0("bold(", .format_diff_label(-3.1), ")")))
  expect_equal(.format_p_value(0.012),   "p = 0.012")
  expect_equal(.format_p_value(0.0001),  "p < 0.001")
  expect_equal(.format_benefit_label(42.2, "% less decline"), "42% less decline")
  expect_match(.format_diff_precision(ci_low = 1.1, ci_high = 7.3,
                                      p_value = 0.01), "95% CI 1.1, 7.3")
  expect_match(.format_diff_precision(se = 1.8), "SE 1.8")
})
