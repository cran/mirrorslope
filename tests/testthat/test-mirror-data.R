test_that("prep_mirror_data returns a mirror_data with OBS rows", {
  md <- make_md()
  expect_s3_class(md, "mirror_data")
  expect_true(all(md$ROWTYPE == "OBS"))
  expect_equal(sort(unique(md$ARMROLE)), c("COMPARATOR", "REFERENCE"))
  expect_equal(sort(unique(md$TIMEROLE)), c("BASELINE", "ENDPOINT"))
})

test_that("metadata records schema, arms, and N", {
  md <- make_md(n = 20)
  m <- attr(md, "meta")
  expect_equal(m$schema_version, "1.0")
  expect_equal(m$reference_arm, "Placebo")
  expect_equal(m$comparator_arm, "Active")
  expect_equal(m$n_reference, 10)
  expect_equal(m$n_comparator, 10)
})

test_that("population flags filter and are recorded", {
  df <- make_df(n = 20)
  df$SAFFL[df$USUBJID == "S1"] <- "N"
  md <- prep_mirror_data(df, trt_levels = c("Placebo", "Active"),
                         time_baseline = "Baseline", time_endpoint = "Month 12",
                         pop_flags = list(SAFFL = "Y"))
  expect_false("S1" %in% md$SUBJID)
  expect_match(attr(md, "meta")$pop_filters, "SAFFL=Y")
})

test_that("missing required column errors", {
  df <- make_df()
  df$AVAL <- NULL
  expect_error(
    prep_mirror_data(df, trt_levels = c("Placebo", "Active"),
                     time_baseline = "Baseline", time_endpoint = "Month 12"),
    "not found"
  )
})

test_that("more than two arms errors clearly", {
  df <- make_df(n = 30)
  df$TRTP[df$USUBJID %in% c("S1","S2")] <- "ArmC"
  expect_error(
    prep_mirror_data(df, trt_levels = c("Placebo", "Active", "ArmC"),
                     time_baseline = "Baseline", time_endpoint = "Month 12"),
    "length 2"
  )
})

test_that("non-numeric value errors", {
  df <- make_df()
  df$AVAL <- as.character(df$AVAL)
  expect_error(
    prep_mirror_data(df, trt_levels = c("Placebo", "Active"),
                     time_baseline = "Baseline", time_endpoint = "Month 12"),
    "must be numeric"
  )
})

test_that("incomplete subjects are dropped with warning", {
  df <- make_df(n = 20)
  df <- df[!(df$USUBJID == "S1" & df$AVISIT == "Month 12"), ]
  expect_warning(
    md <- prep_mirror_data(df, trt_levels = c("Placebo", "Active"),
                           time_baseline = "Baseline",
                           time_endpoint = "Month 12"),
    "lack both"
  )
  expect_false("S1" %in% md$SUBJID)
})

test_that("missing AVAL warns and is excluded", {
  # A subject whose endpoint value is NA triggers the missing-value warning
  # (the row is excluded). Capture all warnings and confirm the missing-value
  # message is among them.
  df <- make_df(n = 20)
  df$AVAL[df$USUBJID == "S2" & df$AVISIT == "Month 12"] <- NA
  warns <- character(0)
  withCallingHandlers(
    prep_mirror_data(df, trt_levels = c("Placebo", "Active"),
                     time_baseline = "Baseline", time_endpoint = "Month 12"),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("missing", warns)))
})

test_that("add_summary attaches SUMM rows with baseline default 0", {
  md <- make_md()
  md <- add_summary(md, c("Placebo" = -10, "Active" = -6),
                    stat_label = "LS-Mean")
  summ <- md[md$ROWTYPE == "SUMM", ]
  expect_equal(nrow(summ), 2)
  expect_equal(summ$BASELINE, c(0, 0))
  expect_equal(unique(summ$STATTYPE), "LS-Mean")
})

test_that("add_summary honors supplied baseline for absolute scales", {
  md <- make_md()
  md <- add_summary(md, c("Placebo" = 38, "Active" = 42),
                    baseline = c("Placebo" = 45, "Active" = 45))
  summ <- md[md$ROWTYPE == "SUMM", ]
  expect_equal(summ$BASELINE, c(45, 45))
})

test_that("add_summary errors on name mismatch naming the arm", {
  md <- make_md()
  expect_error(add_summary(md, c("Placebo" = -10, "Wrong" = -6)), "Wrong")
})

test_that("add_summary errors on NA estimate naming the arm", {
  md <- make_md()
  expect_error(add_summary(md, c("Placebo" = NA, "Active" = -6)), "Placebo")
})

test_that("add_difference stores on comparator row", {
  md <- add_summary(make_md(), c("Placebo" = -10, "Active" = -6))
  md <- add_difference(md, estimate = 4, p_value = 0.01)
  comp <- md[md$ROWTYPE == "SUMM" & md$ARMROLE == "COMPARATOR", ]
  expect_equal(comp$DIFF, 4)
  expect_equal(comp$PVALUE, 0.01)
})

test_that("add_difference warns on sign inconsistency", {
  md <- add_summary(make_md(), c("Placebo" = -10, "Active" = -6))
  # observed comparator - reference = +4; supplying -4 is inconsistent
  expect_warning(add_difference(md, estimate = -4), "inconsistent")
})

test_that("add_difference before add_summary errors", {
  expect_error(add_difference(make_md(), estimate = 4), "no summary rows")
})

test_that("add_precision stores CI on summ rows", {
  md <- add_summary(make_md(), c("Placebo" = -10, "Active" = -6))
  md <- add_precision(md, ci = list("Placebo" = c(-13, -7),
                                    "Active"  = c(-8, -4)))
  ref <- md[md$ROWTYPE == "SUMM" & md$ARMROLE == "REFERENCE", ]
  expect_equal(ref$CI_LOWER, -13)
  expect_equal(ref$CI_UPPER, -7)
})

test_that("print and summary methods run", {
  md <- add_summary(make_md(), c("Placebo" = -10, "Active" = -6))
  expect_output(print(md), "mirror_data")
  expect_output(s <- summary(md), "ARM")
})

test_that("as.data.frame strips class and meta", {
  md <- add_summary(make_md(), c("Placebo" = -10, "Active" = -6))
  d <- as.data.frame(md)
  expect_false(inherits(d, "mirror_data"))
  expect_null(attr(d, "meta"))
})
