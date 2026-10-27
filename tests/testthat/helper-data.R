# Shared synthetic dataset builder for tests
make_df <- function(n = 20, seed = 1, flag = TRUE) {
  set.seed(seed)
  d <- data.frame(
    USUBJID = rep(paste0("S", seq_len(n)), each = 2),
    TRTP    = rep(c(rep("Placebo", n / 2), rep("Active", n / 2)), each = 2),
    AVISIT  = rep(c("Baseline", "Month 12"), times = n),
    AVAL    = c(rbind(rep(0, n),
                      c(rnorm(n / 2, -10, 4), rnorm(n / 2, -6, 4)))),
    stringsAsFactors = FALSE
  )
  if (flag) d$SAFFL <- "Y"
  d
}

make_md <- function(...) {
  prep_mirror_data(make_df(...),
                   trt_levels    = c("Placebo", "Active"),
                   time_baseline = "Baseline",
                   time_endpoint = "Month 12")
}
