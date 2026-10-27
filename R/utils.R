# mirrorslope — internal utility functions
# Not exported. Implements helper logic for the mirror_data spec v1.0.
# These functions carry no user-facing documentation; they are the internal
# contracts that prep_mirror_data(), the add_*() functions, and
# mirror_slope_plot() rely on.

# ── Null-coalescing helper ──────────────────────────────────────────────────
# Small convenience so auto-computation reads cleanly. rlang exports `%||%`
# but we define a local copy to avoid taking a hard import solely for this.
`%||%` <- function(x, y) if (is.null(x)) y else x

# ── Direction detection (spec section 3.1) ──────────────────────────────────

#' Detect outcome direction by comparing endpoint estimate to baseline
#'
#' Spec rule: compare the reference arm endpoint ESTIMATE to its BASELINE.
#'   estimate < baseline -> "decrease"  (declined; less decline is better)
#'   estimate > baseline -> "increase"  (rose; less increase is better)
#'   estimate == baseline -> NA          (ambiguous; caller must error)
#'
#' This is correct for both change-from-baseline data (baseline = 0) and
#' absolute-scale data (baseline is the real starting value).
#'
#' @param ref_estimate Numeric scalar — reference arm endpoint estimate.
#' @param ref_baseline Numeric scalar — reference arm baseline value.
#' @return "decrease", "increase", or NA_character_.
#' @keywords internal
#' @noRd
.detect_direction <- function(ref_estimate, ref_baseline) {
  if (is.na(ref_estimate) || is.na(ref_baseline)) return(NA_character_)
  if (ref_estimate == ref_baseline) return(NA_character_)
  if (ref_estimate < ref_baseline) "decrease" else "increase"
}

# ── Auto labels ──────────────────────────────────────────────────────────────

#' Build default outcome and benefit labels from direction
#'
#' Used when the user has not supplied outcome_label / benefit_label.
#'
#' @param direction "decrease" or "increase".
#' @return Named list with elements `outcome` and `benefit`.
#' @keywords internal
#' @noRd
.auto_labels <- function(direction) {
  if (is.na(direction) || !direction %in% c("decrease", "increase")) {
    return(list(outcome = "change", benefit = "% relative difference"))
  }
  if (direction == "decrease") {
    list(outcome = "decline",  benefit = "% less decline")
  } else {
    list(outcome = "increase", benefit = "% less increase")
  }
}

# ── Relative benefit ─────────────────────────────────────────────────────────

#' Compute relative benefit percentage
#'
#' Measures the comparator's change relative to the reference's change, where
#' "change" is measured from each arm's baseline. Returns a positive percentage.
#'
#'   ref_change = ref_estimate - ref_baseline
#'   trt_change = trt_estimate - trt_baseline
#'   benefit    = abs((ref_change - trt_change) / ref_change) * 100
#'
#' Warns and returns NA if the reference change is at or near zero (unstable).
#'
#' @param ref_estimate,ref_baseline Reference arm endpoint and baseline.
#' @param trt_estimate,trt_baseline Comparator arm endpoint and baseline.
#' @return Numeric scalar (percentage, non-negative), or NA_real_.
#' @keywords internal
#' @noRd
.relative_benefit <- function(ref_estimate, ref_baseline,
                              trt_estimate, trt_baseline) {
  ref_change <- ref_estimate - ref_baseline
  trt_change <- trt_estimate - trt_baseline
  if (abs(ref_change) < 1e-8) {
    warning(
      "Reference arm change from baseline is at or near zero; relative ",
      "benefit is unstable and will not be computed. Consider show_slowing = FALSE.",
      call. = FALSE
    )
    return(NA_real_)
  }
  abs((ref_change - trt_change) / ref_change) * 100
}

# ── X-axis layout ────────────────────────────────────────────────────────────

#' Compute the data-panel x layout
#'
#' Resolves the symmetric placement of the two arm baselines around x_center,
#' and the right edge of the data panel (x_max_data). The annotation zone is
#' handled separately by .compute_annotation_zone().
#'
#' Auto rules (when arguments are NULL):
#'   x_offset = x_center * 0.75
#'   margin   = x_offset * 0.25
#'   x_min      = x_center - x_offset - margin
#'   x_max_data = x_center + x_offset + margin
#'
#' @param x_center Numeric — shared endpoint x position.
#' @param x_offset Numeric or NULL — distance from center to each baseline.
#' @param x_min    Numeric or NULL — left boundary.
#' @param x_max    Numeric or NULL — right boundary of the DATA panel.
#' @return Named list of resolved coordinates.
#' @keywords internal
#' @noRd
.compute_x_layout <- function(x_center, x_offset = NULL,
                              x_min = NULL, x_max = NULL) {

  x_offset <- x_offset %||% (x_center * 0.75)
  margin   <- x_offset * 0.25

  x_min      <- x_min %||% (x_center - x_offset - margin)
  x_max_data <- x_max %||% (x_center + x_offset + margin)

  ref_x <- x_center - x_offset   # reference (control) baseline, left
  trt_x <- x_center + x_offset   # comparator (treatment) baseline, right

  if (ref_x <= x_min) {
    warning(
      "Reference baseline position (", round(ref_x, 2), ") is at or left of ",
      "x_min (", round(x_min, 2), "). Increase x_offset or decrease x_min.",
      call. = FALSE
    )
  }
  if (trt_x >= x_max_data) {
    warning(
      "Comparator baseline position (", round(trt_x, 2), ") is at or right of ",
      "the data panel edge (", round(x_max_data, 2), "). Increase x_offset.",
      call. = FALSE
    )
  }

  list(
    x_center        = x_center,
    x_offset        = x_offset,
    x_min           = x_min,
    x_max_data      = x_max_data,
    ref_x           = ref_x,
    trt_x           = trt_x,
    ref_header_mid  = (x_min + x_center) / 2,
    trt_header_mid  = (x_center + x_max_data) / 2
  )
}

# ── Annotation row count ─────────────────────────────────────────────────────

#' Count active annotation text rows
#'
#' Determines how many rows of text the annotation zone will contain, which
#' drives the horizontal expansion of the zone. Rows:
#'   1. difference value      (show_difference)
#'   2. precision/p-value line (any of diff SE, CI, p-value supplied & shown)
#'   3. relative benefit      (show_slowing)
#'
#' @param show_difference Logical.
#' @param show_slowing Logical.
#' @param has_diff_precision Logical — diff SE/CI/p available and to be shown.
#' @return Integer count of rows (0 to 3).
#' @keywords internal
#' @noRd
.count_annotation_rows <- function(show_difference, show_slowing,
                                   has_diff_precision) {
  n <- 0L
  if (isTRUE(show_difference)) n <- n + 1L
  if (isTRUE(show_difference) && isTRUE(has_diff_precision)) n <- n + 1L
  if (isTRUE(show_slowing)) n <- n + 1L
  n
}

# ── Annotation zone geometry ─────────────────────────────────────────────────

#' Compute the annotation zone geometry and final x_max
#'
#' The zone sits to the right of the data panel. x_max expands to fit it; the
#' data panel geometry is unchanged. zone_scale grows with the number of text
#' rows. If the user supplied a manual x_max smaller than required, we override
#' and warn (spec section 5.2).
#'
#' @param x_layout Result of .compute_x_layout().
#' @param n_rows   Integer rows from .count_annotation_rows().
#' @param user_x_max Numeric or NULL — a manually supplied x_max, if any.
#' @return Named list: rule_x, text_x, x_max, zone_active, zone_width.
#' @keywords internal
#' @noRd
.compute_annotation_zone <- function(x_layout, n_rows, user_x_max = NULL) {

  if (n_rows == 0L) {
    return(list(
      zone_active = FALSE,
      rule_x      = NA_real_,
      text_x      = NA_real_,
      x_max       = x_layout$x_max_data,
      zone_width  = 0
    ))
  }

  x_offset <- x_layout$x_offset
  gap      <- x_offset * 0.15
  scale    <- switch(as.character(n_rows),
                     "1" = 0.9,
                     "2" = 1.2,
                     "3" = 1.5,
                     1.8)               # 4+ fallback
  zone_width <- x_offset * scale

  rule_x <- x_layout$x_max_data + gap
  text_x <- rule_x + x_offset * 0.05
  x_max  <- rule_x + zone_width

  # Honor a manual x_max only if it is large enough; otherwise override + warn.
  if (!is.null(user_x_max)) {
    if (user_x_max < x_max) {
      warning(
        "Supplied x_max (", round(user_x_max, 2), ") is too small for the ",
        "annotation zone; overriding to ", round(x_max, 2),
        ". Remove the manual x_max to silence this.",
        call. = FALSE
      )
    } else {
      x_max <- user_x_max
    }
  }

  list(
    zone_active = TRUE,
    rule_x      = rule_x,
    text_x      = text_x,
    x_max       = x_max,
    zone_width  = zone_width
  )
}

# ── Y-axis limits ────────────────────────────────────────────────────────────

#' Compute y-axis limits from supplied values with padding
#'
#' @param values  Numeric vector of all values that must be visible.
#' @param padding Fractional expansion each side. Default 0.15.
#' @return Numeric length-2 vector c(lower, upper).
#' @keywords internal
#' @noRd
.compute_y_lim <- function(values, padding = 0.15) {
  values <- values[is.finite(values)]
  if (length(values) == 0) {
    stop("No finite values available to compute y-axis limits.", call. = FALSE)
  }
  rng  <- range(values)
  span <- rng[2] - rng[1]
  if (span == 0) span <- abs(rng[1])
  if (span == 0) span <- 1
  c(rng[1] - padding * span, rng[2] + padding * span)
}

# ── Label formatting ─────────────────────────────────────────────────────────

#' Format the difference value label
#'
#' Produces a plotmath string such as 'Delta ~ "= +4.2%"' or 'Delta ~ "= -3.1%"'
#' with explicit sign and one decimal place. The delta is drawn by plotmath
#' (annotate(..., parse = TRUE)) rather than as a literal U+0394 glyph, and the
#' minus is ASCII, so the label renders on every graphics device including the
#' pdf() device used by R CMD check.
#'
#' @param diff_estimate Numeric scalar.
#' @param unit Character appended after the number. Default "%".
#' @return Character string (plotmath), or NA_character_ if input is NA.
#' @keywords internal
#' @noRd
.format_diff_label <- function(diff_estimate, unit = "%") {
  if (is.na(diff_estimate)) return(NA_character_)
  sign_chr <- if (diff_estimate >= 0) "+" else "-"
  paste0('Delta ~ "= ', sign_chr, sprintf("%.1f", abs(diff_estimate)), unit, '"')
}

#' Format the difference precision line (CI and/or SE), optionally with p-value
#'
#' Builds a single line such as "95% CI 1.1, 7.3" or "SE 1.8" and appends a
#' p-value segment when supplied. CI takes precedence over SE for display.
#'
#' @param se Numeric or NA — SE of the difference.
#' @param ci_low,ci_high Numeric or NA — difference CI bounds.
#' @param ci_level Numeric — confidence level (e.g. 0.95) for the label.
#' @param p_value Numeric or NA — p-value for the difference.
#' @return Character string, or NA_character_ if nothing to show.
#' @keywords internal
#' @noRd
.format_diff_precision <- function(se = NA_real_,
                                   ci_low = NA_real_, ci_high = NA_real_,
                                   ci_level = 0.95, p_value = NA_real_) {
  parts <- character(0)

  if (!is.na(ci_low) && !is.na(ci_high)) {
    pct <- round(ci_level * 100)
    parts <- c(parts, sprintf("%d%% CI %.1f, %.1f", pct, ci_low, ci_high))
  } else if (!is.na(se)) {
    parts <- c(parts, sprintf("SE %.1f", se))
  }

  if (!is.na(p_value)) parts <- c(parts, .format_p_value(p_value))

  if (length(parts) == 0) return(NA_character_)
  paste(parts, collapse = "   ")
}

#' Format a p-value (APA-style, floor at 0.001)
#'
#' "p < 0.001" below the floor; otherwise "p = 0.012" to three decimals.
#'
#' @param p Numeric scalar between 0 and 1.
#' @return Character string, or NA_character_ if input is NA.
#' @keywords internal
#' @noRd
.format_p_value <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < 0.001) return("p < 0.001")
  paste0("p = ", sprintf("%.3f", p))
}

#' Format the relative benefit label
#'
#' Produces e.g. "39% less decline".
#'
#' @param pct Numeric scalar (percentage).
#' @param benefit_label Character suffix, e.g. "% less decline". The numeric
#'   percentage replaces a leading "%" placeholder if present, otherwise the
#'   value is prepended.
#' @return Character string, or NA_character_ if input is NA.
#' @keywords internal
#' @noRd
.format_benefit_label <- function(pct, benefit_label) {
  if (is.na(pct)) return(NA_character_)
  # benefit_label like "% less decline" -> "39% less decline"
  if (grepl("^%", benefit_label)) {
    paste0(sprintf("%.0f", pct), benefit_label)
  } else {
    paste0(sprintf("%.0f%%", pct), " ", benefit_label)
  }
}

# ── Timestamp helper ─────────────────────────────────────────────────────────

#' ISO 8601 timestamp for provenance metadata
#' @return Character scalar.
#' @keywords internal
#' @noRd
.iso_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
}
