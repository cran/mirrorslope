# mirrorslope — the renderer
# Reads a fully-built mirror_data object and produces a ggplot, implementing
# the rendering contract in section 6 of the mirror_data specification v1.0.

#' Render a mirror slope plot
#'
#' Produces a publication-quality mirror slope plot from a \code{mirror_data}
#' object built with \code{\link{prep_mirror_data}} and \code{\link{add_summary}}
#' (and optionally \code{\link{add_precision}} and \code{\link{add_difference}}).
#'
#' Both arms share one x-axis and converge at a central endpoint position, so
#' the between-arm gap at the endpoint is the primary visual quantity. All
#' optional elements are off by default: the object stores everything available,
#' and this function exposes only what is requested.
#'
#' @section Direction and labels:
#' \code{direction} describes the direction of numeric movement of the value,
#' derived by comparing the reference arm endpoint estimate to its baseline
#' (\code{"decrease"} if the value moved down, \code{"increase"} if up). The
#' default benefit label is phrased accordingly (e.g. \dQuote{percent less decline}).
#' For outcomes where the favourable direction is unusual (for example a score
#' where a lower number is better), supply \code{outcome_label} and
#' \code{benefit_label} explicitly rather than relying on the defaults.
#'
#' @param mirror_data A \code{mirror_data} object with summary rows attached.
#' @param show_spaghetti Logical. Draw individual subject trajectories beneath
#'   the mean lines. Default \code{FALSE}.
#' @param spaghetti_alpha,spaghetti_size Numeric. Transparency and line width of
#'   the individual trajectory lines.
#' @param show_difference Logical. Show the difference value (and the difference
#'   band). Requires a difference attached via \code{\link{add_difference}}.
#'   Default \code{FALSE}.
#' @param show_slowing Logical. Show the relative benefit percentage (and the
#'   difference band). Default \code{FALSE}.
#' @param show_precision Logical. Draw per-arm precision bars at the endpoint
#'   markers, if precision is attached. Default \code{FALSE}.
#' @param show_n Logical. Show the analyzed N per arm in the strip header.
#'   Default \code{TRUE}.
#' @param direction Character or \code{NULL}. \code{"decrease"} or
#'   \code{"increase"}; if \code{NULL}, derived from the data (see section
#'   above).
#' @param outcome_label,benefit_label Character or \code{NULL}. Override the
#'   auto-generated terminology in the benefit line of the annotation zone.
#' @param x_center Numeric. The x position at which both arm lines converge.
#'   Default \code{12}. A positional coordinate only; not a unit of time.
#' @param x_offset Numeric or \code{NULL}. Distance from \code{x_center} to each
#'   baseline; auto if \code{NULL}.
#' @param x_min,x_max Numeric or \code{NULL}. Plot boundaries; auto if
#'   \code{NULL}. \code{x_max} auto-expands to fit the annotation zone.
#' @param y_lim Numeric length-2 or \code{NULL}. Y-axis limits; auto if
#'   \code{NULL}.
#' @param trt_colors Length-2 character vector of arm colors
#'   \code{c(reference, comparator)}, or a named vector matching the arms.
#'   Default \code{mirror_palette("default")}.
#' @param band_color Character or \code{NULL}. Difference band fill; defaults to
#'   the comparator color.
#' @param band_alpha Numeric. Difference band transparency. Default \code{0.12}.
#' @param line_size Numeric. Mean line width. Default \code{1.2}.
#' @param point_shape Integer. Endpoint marker shape. Default \code{18}.
#' @param point_size Numeric. Endpoint marker size. Default \code{4}.
#' @param base_size Numeric. Base font size passed to \code{\link{theme_mirror}}.
#'   Default \code{11}.
#' @param y_percent Logical. If \code{TRUE} (default), y-axis tick labels and the
#'   header statistics are formatted with a percent sign, appropriate for
#'   change-from-baseline outcomes. Set \code{FALSE} for absolute-scale outcomes.
#' @param jitter_points Logical. If \code{TRUE}, individual endpoints are given a
#'   small horizontal jitter (reference arm toward the left, comparator toward
#'   the right) so overlapping subjects are visible. Default \code{FALSE}. Use
#'   \code{seed} for reproducibility.
#' @param title,subtitle,caption Character or \code{NULL}. Standard labels.
#' @param y_label Character or \code{NULL}. Y-axis label; defaults to the value
#'   label stored in the object.
#' @param footnote Character or \code{NULL}. A figure footnote rendered in a
#'   smaller font below the caption.
#' @param seed Integer or \code{NULL}. Seed for reproducibility of any jittered
#'   elements. Default \code{NULL}. The plot is otherwise deterministic.
#' @param return_data Logical. If \code{TRUE}, return a list with \code{$plot}
#'   and \code{$data}. Default \code{FALSE}.
#' @param save_path Character or \code{NULL}. If given, save via
#'   \code{ggplot2::ggsave()}. The format is taken from the file extension; use
#'   \code{.pdf} or \code{.svg} for a vector figure suitable for journal
#'   submission, or \code{.png}/\code{.tiff} at \code{dpi} 300 or higher for
#'   raster.
#' @param width,height Numeric. Output size in inches when saving.
#' @param dpi Numeric. Output resolution when saving. Default \code{300}.
#'
#' @return A \code{ggplot} object, or a list of \code{$plot} and \code{$data}
#'   when \code{return_data = TRUE}. The plot can be extended with standard
#'   \code{ggplot2} syntax.
#'
#' @seealso \code{\link{prep_mirror_data}}, \code{\link{add_summary}},
#'   \code{\link{add_difference}}, \code{\link{add_precision}},
#'   \code{\link{theme_mirror}}, \code{\link{mirror_palette}}
#'
#' @examples
#' df <- data.frame(
#'   USUBJID = rep(paste0("S", 1:20), each = 2),
#'   TRTP    = rep(c("Placebo", "Active"), each = 20),
#'   AVISIT  = rep(c("Baseline", "Month 12"), times = 20),
#'   AVAL    = c(rbind(rep(0, 20),
#'                     c(rnorm(10, -10, 3), rnorm(10, -6, 3))))
#' )
#' md <- prep_mirror_data(df, trt_levels = c("Placebo", "Active"),
#'                        time_baseline = "Baseline", time_endpoint = "Month 12")
#' md <- add_summary(md, c("Placebo" = -10.1, "Active" = -5.8),
#'                   stat_label = "LS-Mean")
#' md <- add_difference(md, estimate = 4.3, p_value = 0.01)
#' mirror_slope_plot(md, show_difference = TRUE, show_slowing = TRUE)
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_segment geom_point geom_rect geom_hline annotate coord_cartesian scale_color_manual scale_x_continuous scale_y_continuous labs ggsave arrow unit
#' @importFrom rlang .data
#' @importFrom stats runif
#' @export
mirror_slope_plot <- function(mirror_data,
                              show_spaghetti  = FALSE,
                              spaghetti_alpha = 0.15,
                              spaghetti_size  = 0.4,
                              show_difference = FALSE,
                              show_slowing    = FALSE,
                              show_precision  = FALSE,
                              show_n          = TRUE,
                              direction       = NULL,
                              outcome_label   = NULL,
                              benefit_label   = NULL,
                              x_center        = 12,
                              x_offset        = NULL,
                              x_min           = NULL,
                              x_max           = NULL,
                              y_lim           = NULL,
                              trt_colors      = mirror_palette("default"),
                              band_color      = NULL,
                              band_alpha      = 0.12,
                              line_size       = 1.0,
                              point_shape     = 18,
                              point_size      = 3.2,
                              base_size       = 11,
                              y_percent       = TRUE,
                              jitter_points   = FALSE,
                              title           = NULL,
                              subtitle        = NULL,
                              y_label         = NULL,
                              caption         = NULL,
                              footnote        = NULL,
                              seed            = NULL,
                              return_data     = FALSE,
                              save_path       = NULL,
                              width           = 8,
                              height          = 6,
                              dpi             = 300) {

  if (!is.null(seed)) set.seed(seed)

  # ---- Validate object state ----
  .check_is_mirror_data(mirror_data)
  .check_has_summary(mirror_data)
  if (isTRUE(show_difference)) .check_has_difference(mirror_data)

  meta <- mirror_meta(mirror_data)
  ref_arm <- meta$reference_arm
  trt_arm <- meta$comparator_arm

  # ---- Resolve arm colors -> named c(reference, comparator) ----
  if (is.null(names(trt_colors))) {
    if (length(trt_colors) != 2) {
      stop("trt_colors must have length 2 or be named to match the arms.",
           call. = FALSE)
    }
    trt_colors <- stats::setNames(trt_colors, c(ref_arm, trt_arm))
  } else {
    bad <- setdiff(c(ref_arm, trt_arm), names(trt_colors))
    if (length(bad) > 0) {
      stop("trt_colors is missing color(s) for arm(s): ",
           paste(bad, collapse = ", "), ".", call. = FALSE)
    }
  }
  ref_color <- trt_colors[[ref_arm]]
  trt_color <- trt_colors[[trt_arm]]
  band_color <- band_color %||% trt_color

  # ---- Pull summary rows ----
  is_summ <- mirror_data$ROWTYPE == "SUMM"
  ref_row <- mirror_data[is_summ & mirror_data$ARMROLE == "REFERENCE", ]
  trt_row <- mirror_data[is_summ & mirror_data$ARMROLE == "COMPARATOR", ]

  ref_est  <- ref_row$ESTIMATE[1]
  trt_est  <- trt_row$ESTIMATE[1]
  ref_base <- ref_row$BASELINE[1]
  trt_base <- trt_row$BASELINE[1]
  stat_label <- ref_row$STATTYPE[1]

  # ---- Resolve direction (data movement) ----
  if (is.null(direction)) {
    direction <- if (nzchar(meta$direction)) meta$direction else
      .detect_direction(ref_est, ref_base)
    if (is.na(direction)) {
      stop("Cannot derive direction: reference estimate equals its baseline. ",
           "Supply direction = 'decrease' or 'increase'.", call. = FALSE)
    }
  }
  auto_lbl <- .auto_labels(direction)
  if (is.null(outcome_label)) outcome_label <- auto_lbl$outcome

  # ---- X layout and annotation zone ----
  xl <- .compute_x_layout(x_center, x_offset, x_min, x_max)

  # difference precision present?
  diff_se   <- .col_or_na(trt_row, "DIFF_SE")[1]
  diff_low  <- .col_or_na(trt_row, "DIFF_CILOW")[1]
  diff_high <- .col_or_na(trt_row, "DIFF_CIHIGH")[1]
  diff_lvl  <- .col_or_na(trt_row, "DIFF_CILEVEL")[1]
  diff_p    <- .col_or_na(trt_row, "PVALUE")[1]
  diff_est  <- .col_or_na(trt_row, "DIFF")[1]
  has_diff_precision <- isTRUE(show_difference) &&
    (!is.na(diff_se) || (!is.na(diff_low) && !is.na(diff_high)) || !is.na(diff_p))

  n_rows <- .count_annotation_rows(show_difference, show_slowing,
                                   has_diff_precision)
  az <- .compute_annotation_zone(xl, n_rows, user_x_max = x_max)
  show_band <- az$zone_active

  # ---- Y limits: include observed values, estimates, baselines ----
  obs_vals <- mirror_data$AVAL[mirror_data$ROWTYPE == "OBS"]
  all_vals <- c(obs_vals, ref_est, trt_est, ref_base, trt_base)
  # include per-arm CI bounds if precision shown
  if (isTRUE(show_precision)) {
    all_vals <- c(all_vals,
                  .col_or_na(ref_row, "CI_LOWER")[1],
                  .col_or_na(ref_row, "CI_UPPER")[1],
                  .col_or_na(trt_row, "CI_LOWER")[1],
                  .col_or_na(trt_row, "CI_UPPER")[1])
  }
  if (is.null(y_lim)) y_lim <- .compute_y_lim(all_vals)
  y_span <- y_lim[2] - y_lim[1]

  # ---- Strip header geometry (two-line band above the panel) ----
  header_h    <- y_span * 0.15
  header_ymin <- y_lim[2]
  header_ymax <- y_lim[2] + header_h
  header_line1_y <- header_ymin + header_h * 0.64
  header_line2_y <- header_ymin + header_h * 0.32

  # ---- Mean line data frame (each arm: baseline -> endpoint) ----
  mean_lines <- data.frame(
    arm    = c(ref_arm, trt_arm),
    x_base = c(xl$ref_x, xl$trt_x),
    y_base = c(ref_base, trt_base),
    x_end  = xl$x_center,
    y_end  = c(ref_est, trt_est),
    stringsAsFactors = FALSE
  )

  endpoint_pts <- data.frame(
    arm = c(ref_arm, trt_arm),
    x   = xl$x_center,
    y   = c(ref_est, trt_est),
    stringsAsFactors = FALSE
  )

  # ---- Spaghetti data (OBS rows mapped to x positions) ----
  spag <- NULL
  if (isTRUE(show_spaghetti)) {
    obs <- mirror_data[mirror_data$ROWTYPE == "OBS", ]
    obs$x_pos <- ifelse(
      obs$TIMEROLE == "ENDPOINT", xl$x_center,
      ifelse(obs$ARMROLE == "REFERENCE", xl$ref_x, xl$trt_x)
    )
    # Optional horizontal jitter at the endpoint so overlapping subjects are
    # visible. Reference points jitter left of centre, comparator right, so the
    # two arms stay on their own sides. seed (set earlier) makes it reproducible.
    if (isTRUE(jitter_points)) {
      is_end <- obs$TIMEROLE == "ENDPOINT"
      jw <- xl$x_offset * 0.06
      side <- ifelse(obs$ARMROLE == "REFERENCE", -1, 1)
      obs$x_pos[is_end] <- xl$x_center +
        side[is_end] * runif(sum(is_end), 0, jw)
    }
    # Each subject line runs from its own baseline to the central endpoint only.
    # Group by arm + subject so a subject id shared across arms (crossover, etc.)
    # never connects one arm's baseline to the other arm's, and order each pair
    # baseline -> endpoint so the segment is drawn cleanly.
    obs$.grp <- paste(obs$ARMROLE, obs$SUBJID, sep = "\r")
    obs <- obs[order(obs$.grp, obs$TIMEROLE == "ENDPOINT"), , drop = FALSE]
    spag <- obs
  }

  # ---- Annotation zone text rows ----
  zone_rows <- character(0)
  if (isTRUE(show_difference)) {
    zone_rows <- c(zone_rows, .format_diff_label(diff_est))
    if (has_diff_precision) {
      prec_line <- .format_diff_precision(
        se = diff_se, ci_low = diff_low, ci_high = diff_high,
        ci_level = if (is.na(diff_lvl)) 0.95 else diff_lvl, p_value = diff_p
      )
      if (!is.na(prec_line)) zone_rows <- c(zone_rows, prec_line)
    }
  }
  if (isTRUE(show_slowing)) {
    pct <- .relative_benefit(ref_est, ref_base, trt_est, trt_base)
    bl  <- if (!is.null(benefit_label)) benefit_label else
      .format_benefit_label(pct, auto_lbl$benefit)
    if (!is.na(bl)) zone_rows <- c(zone_rows, bl)
  }
  # Row 1 is the plotmath difference label when show_difference is on; flag it
  # so the renderer parses that row (Delta via plotmath draws on every device).
  attr(zone_rows, "parse") <- seq_along(zone_rows) == 1 & isTRUE(show_difference)

  # ---- Header label strings ----
  val_sfx <- if (isTRUE(y_percent)) "%" else ""
  ref_stat_txt <- paste0(stat_label, ": ", sprintf("%.1f", ref_est), val_sfx)
  trt_stat_txt <- paste0(stat_label, ": ", sprintf("%.1f", trt_est), val_sfx)
  ref_name_txt <- if (isTRUE(show_n))
    paste0(ref_arm, "  (n=", meta$n_reference, ")") else ref_arm
  trt_name_txt <- if (isTRUE(show_n))
    paste0(trt_arm, "  (n=", meta$n_comparator, ")") else trt_arm

  # ---- y label ----
  if (is.null(y_label)) {
    y_label <- if (nzchar(meta$value_label)) meta$value_label else "Value"
  }

  # ---- Hand off to layer assembly ----
  p <- .build_mirror_layers(
    meta = meta, xl = xl, az = az,
    y_lim = y_lim, y_span = y_span,
    header_ymin = header_ymin, header_ymax = header_ymax,
    header_line1_y = header_line1_y, header_line2_y = header_line2_y,
    mean_lines = mean_lines, endpoint_pts = endpoint_pts, spag = spag,
    ref_arm = ref_arm, trt_arm = trt_arm,
    ref_est = ref_est, trt_est = trt_est,
    ref_color = ref_color, trt_color = trt_color,
    trt_colors = trt_colors, band_color = band_color, band_alpha = band_alpha,
    show_band = show_band, show_precision = show_precision,
    show_spaghetti = show_spaghetti,
    spaghetti_alpha = spaghetti_alpha, spaghetti_size = spaghetti_size,
    line_size = line_size, point_shape = point_shape, point_size = point_size,
    ref_row = ref_row, trt_row = trt_row,
    zone_rows = zone_rows,
    ref_name_txt = ref_name_txt, trt_name_txt = trt_name_txt,
    ref_stat_txt = ref_stat_txt, trt_stat_txt = trt_stat_txt,
    base_size = base_size,
    y_percent = y_percent,
    title = title, subtitle = subtitle, y_label = y_label,
    caption = caption, footnote = footnote
  )

  # ---- Save if requested ----
  if (!is.null(save_path)) {
    ggplot2::ggsave(filename = save_path, plot = p,
                    width = width, height = height, dpi = dpi)
  }

  if (isTRUE(return_data)) {
    list(plot = p, data = as.data.frame(mirror_data))
  } else {
    p
  }
}

# ── Layer assembly (internal) ────────────────────────────────────────────────

#' Assemble the ggplot layers for a mirror slope plot
#'
#' Pure rendering: takes resolved geometry and data frames and stacks ggplot2
#' layers in draw order. All decision logic lives in mirror_slope_plot().
#' @keywords internal
#' @noRd
.build_mirror_layers <- function(meta, xl, az, y_lim, y_span,
                                 header_ymin, header_ymax,
                                 header_line1_y, header_line2_y,
                                 mean_lines, endpoint_pts, spag,
                                 ref_arm, trt_arm, ref_est, trt_est,
                                 ref_color, trt_color, trt_colors,
                                 band_color, band_alpha,
                                 show_band, show_precision, show_spaghetti,
                                 spaghetti_alpha, spaghetti_size,
                                 line_size, point_shape, point_size,
                                 ref_row, trt_row, zone_rows,
                                 ref_name_txt, trt_name_txt,
                                 ref_stat_txt, trt_stat_txt,
                                 base_size, y_percent, title, subtitle, y_label,
                                 caption, footnote) {

  p <- ggplot2::ggplot()

  # 1. Difference band (between the two endpoint estimates, full data width)
  if (isTRUE(show_band)) {
    p <- p + ggplot2::annotate(
      "rect",
      xmin = xl$x_min, xmax = xl$x_max_data,
      ymin = min(ref_est, trt_est), ymax = max(ref_est, trt_est),
      fill = band_color, alpha = band_alpha
    )
  }

  # 2. Slope echoes: brief, muted dashed stubs just past each endpoint. Kept
  #    short and light so they hint at continuation without reading as data.
  if (az$zone_active) {
    echo_len <- xl$x_offset * 0.28
    for (i in seq_len(nrow(mean_lines))) {
      ml <- mean_lines[i, ]
      slope <- (ml$y_end - ml$y_base) / (ml$x_end - ml$x_base)
      echo_x <- ml$x_end + echo_len
      echo_y <- ml$y_end + slope * (echo_x - ml$x_end)
      col <- if (ml$arm == ref_arm) ref_color else trt_color
      p <- p + ggplot2::annotate(
        "segment",
        x = ml$x_end, y = ml$y_end, xend = echo_x, yend = echo_y,
        color = col, linetype = "dashed", linewidth = 0.3, alpha = 0.25
      )
    }
  }

  # 3. Spaghetti (under mean lines): each subject drawn baseline -> centre only
  if (isTRUE(show_spaghetti) && !is.null(spag)) {
    p <- p + ggplot2::geom_line(
      data = spag,
      mapping = ggplot2::aes(x = .data$x_pos, y = .data$AVAL,
                             group = .data$.grp, color = .data$ARM),
      alpha = spaghetti_alpha, linewidth = spaghetti_size
    )
  }

  # 4. Mean slope lines
  p <- p + ggplot2::geom_segment(
    data = mean_lines,
    mapping = ggplot2::aes(x = .data$x_base, y = .data$y_base,
                           xend = .data$x_end, yend = .data$y_end,
                           color = .data$arm),
    linewidth = line_size
  )

  # 5. Per-arm precision bars at the endpoint (optional)
  if (isTRUE(show_precision)) {
    prec <- data.frame(
      arm   = c(ref_arm, trt_arm),
      x     = xl$x_center,
      lower = c(.col_or_na(ref_row, "CI_LOWER")[1],
                .col_or_na(trt_row, "CI_LOWER")[1]),
      upper = c(.col_or_na(ref_row, "CI_UPPER")[1],
                .col_or_na(trt_row, "CI_UPPER")[1]),
      se_lo = c(ref_est, trt_est) - c(.col_or_na(ref_row, "SE")[1],
                                      .col_or_na(trt_row, "SE")[1]),
      se_hi = c(ref_est, trt_est) + c(.col_or_na(ref_row, "SE")[1],
                                      .col_or_na(trt_row, "SE")[1]),
      stringsAsFactors = FALSE
    )
    # Prefer CI; fall back to +/-1 SE
    prec$lo <- ifelse(!is.na(prec$lower), prec$lower, prec$se_lo)
    prec$hi <- ifelse(!is.na(prec$upper), prec$upper, prec$se_hi)
    prec <- prec[!is.na(prec$lo) & !is.na(prec$hi), ]
    if (nrow(prec) > 0) {
      prec$color <- ifelse(prec$arm == ref_arm, ref_color, trt_color)
      p <- p + ggplot2::geom_segment(
        data = prec,
        mapping = ggplot2::aes(x = .data$x, y = .data$lo,
                               xend = .data$x, yend = .data$hi),
        color = prec$color, linewidth = 0.6
      )
    }
  }

  # 6. Endpoint markers, each on a small white halo so they read cleanly where
  #    the converging mean lines and individual trajectories crowd the knot.
  p <- p +
    ggplot2::geom_point(
      data = endpoint_pts,
      mapping = ggplot2::aes(x = .data$x, y = .data$y),
      shape = point_shape, size = point_size * 1.7, color = "white"
    ) +
    ggplot2::geom_point(
      data = endpoint_pts,
      mapping = ggplot2::aes(x = .data$x, y = .data$y, color = .data$arm),
      shape = point_shape, size = point_size
    )

  # 7. Reference lines: light dashed baseline at y = 0 and a light dashed
  #    vertical guide at the central convergence axis. Kept subordinate to data.
  p <- p +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        color = "grey55", linewidth = 0.35) +
    ggplot2::annotate("segment", x = xl$x_center, xend = xl$x_center,
                      y = y_lim[1], yend = header_ymin,
                      linetype = "dashed", color = "grey70", linewidth = 0.3)

  # 7b. Directional arrows: both arms flow inward toward the endpoint. These
  #     counter the most common misreading (that the horizontal axis is time).
  arrow_y   <- y_lim[1] + y_span * 0.04
  arrow_gap <- xl$x_offset * 0.18
  endpoint_label <- meta$endpoint_label
  p <- p +
    ggplot2::annotate("segment",
      x = xl$ref_x, xend = xl$x_center - arrow_gap, y = arrow_y, yend = arrow_y,
      color = "grey55", linewidth = 0.35,
      arrow = ggplot2::arrow(length = ggplot2::unit(0.07, "inches"),
                             type = "closed")) +
    ggplot2::annotate("segment",
      x = xl$trt_x, xend = xl$x_center + arrow_gap, y = arrow_y, yend = arrow_y,
      color = "grey55", linewidth = 0.35,
      arrow = ggplot2::arrow(length = ggplot2::unit(0.07, "inches"),
                             type = "closed")) +
    ggplot2::annotate("text",
      x = (xl$ref_x + xl$x_center) / 2, y = arrow_y + y_span * 0.04,
      label = paste0("to ", endpoint_label), color = "grey50",
      size = base_size * 0.24) +
    ggplot2::annotate("text",
      x = (xl$trt_x + xl$x_center) / 2, y = arrow_y + y_span * 0.04,
      label = paste0("to ", endpoint_label), color = "grey50",
      size = base_size * 0.24)

  # 8. Strip headers: full-width comparator color, left half reference color
  p <- p +
    ggplot2::annotate("rect", xmin = xl$x_min, xmax = xl$x_max_data,
                      ymin = header_ymin, ymax = header_ymax,
                      fill = trt_color) +
    ggplot2::annotate("rect", xmin = xl$x_min, xmax = xl$x_center,
                      ymin = header_ymin, ymax = header_ymax,
                      fill = ref_color) +
    ggplot2::annotate("text", x = xl$ref_header_mid, y = header_line1_y,
                      label = ref_name_txt, color = "white",
                      fontface = "bold", size = base_size * 0.32) +
    ggplot2::annotate("text", x = xl$ref_header_mid, y = header_line2_y,
                      label = ref_stat_txt, color = "white",
                      size = base_size * 0.28) +
    ggplot2::annotate("text", x = xl$trt_header_mid, y = header_line1_y,
                      label = trt_name_txt, color = "white",
                      fontface = "bold", size = base_size * 0.32) +
    ggplot2::annotate("text", x = xl$trt_header_mid, y = header_line2_y,
                      label = trt_stat_txt, color = "white",
                      size = base_size * 0.28)

  # 9. Annotation zone: bracket sized to the difference band + text rows beside it
  if (az$zone_active && length(zone_rows) > 0) {
    n <- length(zone_rows)
    band_lo <- min(ref_est, trt_est)
    band_hi <- max(ref_est, trt_est)
    mid_y   <- (band_lo + band_hi) / 2

    # Bracket: height equals the treatment gap exactly, with short inward serifs
    # and a thin line matching the axis weight, so it reads as a quiet
    # measurement mark rather than a heavy UI element.
    serif_w <- xl$x_offset * 0.025
    p <- p +
      ggplot2::annotate("segment", x = az$rule_x, xend = az$rule_x,
                        y = band_lo, yend = band_hi,
                        color = band_color, linewidth = 0.45) +
      ggplot2::annotate("segment", x = az$rule_x, xend = az$rule_x + serif_w,
                        y = band_hi, yend = band_hi,
                        color = band_color, linewidth = 0.45) +
      ggplot2::annotate("segment", x = az$rule_x, xend = az$rule_x + serif_w,
                        y = band_lo, yend = band_lo,
                        color = band_color, linewidth = 0.45)

    # Text rows: stacked and vertically centred on the band midpoint,
    # independent of the bracket height.
    row_gap <- y_span * 0.06
    total_h <- (n - 1) * row_gap
    top_y   <- mid_y + total_h / 2
    row_ys  <- top_y - (seq_len(n) - 1) * row_gap

    text_start <- az$rule_x + xl$x_offset * 0.12
    for (i in seq_len(n)) {
      txt   <- zone_rows[i]
      # first row bold + comparator color; benefit row comparator color; rest grey
      is_first <- i == 1
      col <- if (is_first) trt_color else "grey35"
      face <- if (is_first) "bold" else "plain"
      # The difference label (row 1 when show_difference) is a plotmath string
      # from .format_diff_label(); parse it so Delta renders on every device.
      is_math <- isTRUE(attr(zone_rows, "parse")[i])
      if (is_math) {
        p <- p + ggplot2::annotate(
          "text", x = text_start, y = row_ys[i],
          label = paste0("bold(", txt, ")"), parse = TRUE,
          hjust = 0, color = col,
          size = base_size * 0.34
        )
      } else {
        p <- p + ggplot2::annotate(
          "text", x = text_start, y = row_ys[i], label = txt,
          hjust = 0, color = col, fontface = face,
          size = base_size * (if (is_first) 0.34 else 0.3)
        )
      }
    }
  }

  # 10. Scales, coordinates (clip off so headers/zone render outside panel)
  y_scale <- if (isTRUE(y_percent)) {
    ggplot2::scale_y_continuous(labels = function(v) paste0(v, "%"))
  } else {
    ggplot2::scale_y_continuous()
  }
  p <- p +
    ggplot2::scale_color_manual(values = trt_colors) +
    y_scale +
    ggplot2::scale_x_continuous(
      breaks = c(xl$ref_x, xl$x_center, xl$trt_x),
      labels = c(meta$baseline_label, meta$endpoint_label, meta$baseline_label)
    ) +
    ggplot2::coord_cartesian(
      xlim = c(xl$x_min, az$x_max),
      ylim = c(y_lim[1], header_ymax),
      clip = "off"
    ) +
    ggplot2::labs(title = title, subtitle = subtitle,
                  x = NULL, y = y_label, caption = caption) +
    theme_mirror(base_size = base_size)

  # 11. Footnote: append below caption using a second caption-like annotation
  if (!is.null(footnote)) {
    cap <- if (is.null(caption)) footnote else paste0(caption, "\n", footnote)
    p <- p + ggplot2::labs(caption = cap)
  }

  p
}
