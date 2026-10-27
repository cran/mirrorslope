# mirrorslope — mirror_data object: construction pipeline and accessors
# Implements the mirror_data specification v1.0.

MIRRORSLOPE_SCHEMA_VERSION <- "1.0"

# ── Internal constructor ─────────────────────────────────────────────────────

#' Construct a mirror_data object from a data frame and metadata
#'
#' Internal. Wraps a conforming data frame as an S3 "mirror_data" object with
#' a metadata list stored in the "meta" attribute.
#'
#' @param data Conforming data frame (OBS and/or SUMM rows).
#' @param meta Named list of metadata fields (spec section 3).
#' @return An object of class c("mirror_data", "data.frame").
#' @keywords internal
#' @noRd
new_mirror_data <- function(data, meta) {
  structure(
    data,
    meta  = meta,
    class = c("mirror_data", "data.frame")
  )
}

#' Retrieve the metadata list from a mirror_data object
#' @param x A mirror_data object.
#' @return The metadata list.
#' @keywords internal
#' @noRd
mirror_meta <- function(x) attr(x, "meta")

#' Replace the metadata list on a mirror_data object
#' @keywords internal
#' @noRd
`mirror_meta<-` <- function(x, value) {
  attr(x, "meta") <- value
  x
}

# ── prep_mirror_data() ───────────────────────────────────────────────────────

#' Prepare analysis data for a mirror slope plot
#'
#' First step of the mirror slope pipeline. Validates and structures a long
#' analysis data frame into a \code{mirror_data} object containing subject-level
#' (\code{OBS}) rows, ready to receive group-level summaries via
#' \code{\link{add_summary}}.
#'
#' The function filters to exactly two treatment arms, applies optional
#' population flags, retains only the baseline and endpoint visits, assigns the
#' \code{ARMROLE} and \code{TIMEROLE} role columns, counts the analyzed N per
#' arm, and records provenance metadata. It does not compute any statistics.
#'
#' @param data A data frame in long format, one row per subject per visit.
#' @param subject_var Character. Column name for the subject identifier.
#'   Default \code{"USUBJID"}.
#' @param trt_var Character. Column name for the treatment arm. Default
#'   \code{"TRTP"}.
#' @param trt_levels Character vector of length 2 in the form
#'   \code{c(reference, comparator)}. The first element is the reference
#'   (control) arm and plots on the left; the second is the comparator
#'   (treatment) arm and plots on the right. Values must be present in
#'   \code{trt_var}.
#' @param time_var Character. Column name for the visit/time variable. Default
#'   \code{"AVISIT"}.
#' @param time_baseline Character. The \code{time_var} value identifying the
#'   baseline visit.
#' @param time_endpoint Character. The \code{time_var} value identifying the
#'   endpoint visit.
#' @param value_var Character. Column name for the analysis value. Default
#'   \code{"AVAL"}.
#' @param pop_flags Optional named list of population flag filters applied with
#'   AND logic, e.g. \code{list(SAFFL = "Y", ANL01FL = "Y")}. Each name is a
#'   column and each value the level to retain. Default \code{NULL} (no
#'   filtering).
#' @param value_label Character. Human-readable description of \code{value_var},
#'   used as the default y-axis label. Default \code{"Change from Baseline"}.
#' @param population_label Character. Description of the analysis population for
#'   metadata and figure footnotes, e.g. \code{"ITT"}. Default \code{NULL}.
#'
#' @return A \code{mirror_data} object containing \code{OBS} rows and populated
#'   metadata, including \code{schema_version} and provenance.
#'
#' @seealso \code{\link{add_summary}}, \code{\link{add_difference}},
#'   \code{\link{add_precision}}, \code{\link{mirror_slope_plot}}
#'
#' @examples
#' df <- data.frame(
#'   USUBJID = rep(paste0("S", 1:6), each = 2),
#'   TRTP    = rep(c("Placebo", "Active"), each = 6),
#'   AVISIT  = rep(c("Baseline", "Week 24"), times = 6),
#'   AVAL    = c(0, -8, 0, -11, 0, -9, 0, -4, 0, -6, 0, -5),
#'   SAFFL   = "Y"
#' )
#' md <- prep_mirror_data(
#'   df,
#'   trt_levels    = c("Placebo", "Active"),
#'   time_baseline = "Baseline",
#'   time_endpoint = "Week 24",
#'   pop_flags     = list(SAFFL = "Y")
#' )
#' md
#'
#' @export
prep_mirror_data <- function(data,
                             subject_var      = "USUBJID",
                             trt_var          = "TRTP",
                             trt_levels,
                             time_var         = "AVISIT",
                             time_baseline,
                             time_endpoint,
                             value_var        = "AVAL",
                             pop_flags        = NULL,
                             value_label      = "Change from Baseline",
                             population_label = NULL) {

  call <- match.call()

  # ---- Required columns present (spec 5.1) ----
  req_cols <- c(subject_var, trt_var, time_var, value_var)
  missing_cols <- setdiff(req_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # ---- trt_levels well-formed ----
  if (length(trt_levels) != 2) {
    stop("trt_levels must have length 2: c(reference, comparator).",
         call. = FALSE)
  }
  reference_arm  <- trt_levels[1]
  comparator_arm <- trt_levels[2]

  # ---- Coerce factor arm to character, warning on dropped levels (spec 5.2) ----
  if (is.factor(data[[trt_var]])) {
    present <- unique(as.character(data[[trt_var]]))
    dropped <- setdiff(levels(data[[trt_var]]), present)
    if (length(dropped) > 0) {
      warning("Factor '", trt_var, "' has levels not present in the data; ",
              "coercing to character. Dropped levels: ",
              paste(dropped, collapse = ", "), call. = FALSE)
    }
    data[[trt_var]] <- as.character(data[[trt_var]])
  }

  # ---- More than two arms requested is fine; more than two PRESENT after
  #      subsetting to requested arms is the spec error. We subset first. ----
  arm_vals <- unique(data[[trt_var]])
  missing_arms <- setdiff(trt_levels, arm_vals)
  if (length(missing_arms) > 0) {
    stop("trt_levels value(s) not found in '", trt_var, "': ",
         paste(missing_arms, collapse = ", "), call. = FALSE)
  }

  # ---- Apply population flags (AND logic) ----
  pop_filter_str <- ""
  if (!is.null(pop_flags)) {
    if (is.null(names(pop_flags)) || any(names(pop_flags) == "")) {
      stop("pop_flags must be a named list, e.g. list(SAFFL = 'Y').",
           call. = FALSE)
    }
    flag_missing <- setdiff(names(pop_flags), names(data))
    if (length(flag_missing) > 0) {
      stop("pop_flags column(s) not found in data: ",
           paste(flag_missing, collapse = ", "), call. = FALSE)
    }
    keep <- rep(TRUE, nrow(data))
    for (fl in names(pop_flags)) {
      keep <- keep & (data[[fl]] %in% pop_flags[[fl]])
    }
    data <- data[keep, , drop = FALSE]
    pop_filter_str <- paste(
      vapply(names(pop_flags),
             function(fl) paste0(fl, "=", paste(pop_flags[[fl]], collapse = "/")),
             character(1)),
      collapse = "; "
    )
  }

  # ---- Subset to the two requested arms ----
  data <- data[data[[trt_var]] %in% trt_levels, , drop = FALSE]

  # ---- Guard against >2 arms surviving (should not happen post-subset, but
  #      defensive per spec messaging) ----
  surviving_arms <- unique(data[[trt_var]])
  if (length(surviving_arms) > 2) {
    stop("prep_mirror_data() supports exactly 2 arms. Found ",
         length(surviving_arms), ": ",
         paste(surviving_arms, collapse = ", "),
         ". Subset to your reference and comparator arms before calling.",
         call. = FALSE)
  }

  # ---- Timepoints present ----
  time_vals <- unique(data[[time_var]])
  if (!time_baseline %in% time_vals) {
    stop("time_baseline '", time_baseline, "' not found in '", time_var, "'.",
         call. = FALSE)
  }
  if (!time_endpoint %in% time_vals) {
    stop("time_endpoint '", time_endpoint, "' not found in '", time_var, "'.",
         call. = FALSE)
  }

  # ---- Retain only baseline and endpoint visits ----
  data <- data[data[[time_var]] %in% c(time_baseline, time_endpoint), ,
               drop = FALSE]

  # ---- value_var numeric (spec 5.1) ----
  if (!is.numeric(data[[value_var]])) {
    stop("value_var '", value_var, "' must be numeric; got ",
         class(data[[value_var]])[1], ".", call. = FALSE)
  }

  # ---- Build the OBS rows in canonical structure ----
  obs <- data.frame(
    ROWTYPE  = "OBS",
    SUBJID   = as.character(data[[subject_var]]),
    ARM      = as.character(data[[trt_var]]),
    ARMROLE  = ifelse(data[[trt_var]] == reference_arm,
                      "REFERENCE", "COMPARATOR"),
    VISIT    = as.character(data[[time_var]]),
    TIMEROLE = ifelse(data[[time_var]] == time_baseline,
                      "BASELINE", "ENDPOINT"),
    AVAL     = as.numeric(data[[value_var]]),
    stringsAsFactors = FALSE
  )

  # ---- Missing AVAL: warn with count, exclude from OBS (spec 5.2) ----
  n_missing <- sum(is.na(obs$AVAL))
  if (n_missing > 0) {
    warning(n_missing, " observation(s) have missing ", value_var,
            " and are excluded from individual trajectory lines.",
            call. = FALSE)
    obs <- obs[!is.na(obs$AVAL), , drop = FALSE]
  }

  # ---- Duplicate subject/visit within an arm: warn (spec 5.2) ----
  dup_key <- paste(obs$ARM, obs$SUBJID, obs$TIMEROLE, sep = "\r")
  if (any(duplicated(dup_key))) {
    warning("Duplicate subject/visit combinations detected within an arm. ",
            "This may reflect a multiple-record design; all rows are retained.",
            call. = FALSE)
  }

  # ---- Same subject in both arms: warn (spec 5.2) ----
  subj_arms <- tapply(obs$ARM, obs$SUBJID, function(a) length(unique(a)))
  if (any(subj_arms > 1)) {
    warning("One or more subjects appear under both arms. This may reflect a ",
            "crossover or before/after design; all rows are retained.",
            call. = FALSE)
  }

  # ---- Incomplete subjects (one timepoint only): warn + drop (spec 5.2) ----
  has_base <- tapply(obs$TIMEROLE == "BASELINE", obs$SUBJID, any)
  has_end  <- tapply(obs$TIMEROLE == "ENDPOINT", obs$SUBJID, any)
  complete <- names(has_base)[has_base & has_end]
  incomplete <- setdiff(unique(obs$SUBJID), complete)
  if (length(incomplete) > 0) {
    warning(length(incomplete), " subject(s) lack both baseline and endpoint ",
            "and are dropped: ", paste(utils::head(incomplete, 10),
                                       collapse = ", "),
            if (length(incomplete) > 10) ", ..." else "", call. = FALSE)
    obs <- obs[obs$SUBJID %in% complete, , drop = FALSE]
  }

  # ---- N per arm (analyzed subjects, post-filter) ----
  n_reference  <- length(unique(obs$SUBJID[obs$ARMROLE == "REFERENCE"]))
  n_comparator <- length(unique(obs$SUBJID[obs$ARMROLE == "COMPARATOR"]))

  if (n_reference == 1 || n_comparator == 1) {
    warning("An arm contains only one analyzed subject.", call. = FALSE)
  }

  # ---- Assemble metadata (spec section 3) ----
  meta <- list(
    schema_version  = MIRRORSLOPE_SCHEMA_VERSION,
    reference_arm   = reference_arm,
    comparator_arm  = comparator_arm,
    baseline_label  = time_baseline,
    endpoint_label  = time_endpoint,
    value_label     = value_label,
    population_label = population_label %||% "",
    pop_filters     = pop_filter_str,
    direction       = "",            # derived later unless set
    n_reference     = n_reference,
    n_comparator    = n_comparator,
    created         = .iso_timestamp(),
    source_tool     = paste0("mirrorslope (R) ",
                             as.character(utils::packageVersion("mirrorslope"))),
    call            = paste(deparse(call), collapse = " ")
  )

  new_mirror_data(obs, meta)
}

# ── add_summary() ────────────────────────────────────────────────────────────

#' Attach group-level summary estimates to a mirror_data object
#'
#' Second step of the pipeline. Adds one \code{SUMM} row per arm carrying the
#' endpoint summary estimate, the baseline value the mean line is drawn from,
#' and a free-text statistic label.
#'
#' @param mirror_data A \code{mirror_data} object from
#'   \code{\link{prep_mirror_data}}.
#' @param estimates Named numeric vector of length 2 giving the endpoint
#'   summary estimate per arm. Names must match the reference and comparator
#'   arm labels.
#' @param stat_label Character. Free-text label for the estimate type, e.g.
#'   \code{"Mean"}, \code{"LS-Mean"}, \code{"Median"}. Default \code{"Mean"}.
#' @param baseline Named numeric vector of length 2 giving the baseline value
#'   per arm from which each mean line is drawn. Defaults to \code{0} for both
#'   arms (appropriate for change-from-baseline outcomes). For absolute-scale
#'   outcomes, supply the group baseline estimates.
#'
#' @return The \code{mirror_data} object with two \code{SUMM} rows added.
#'
#' @details
#' This function accepts pre-computed estimates and is deliberately
#' model-agnostic. A future release may add a method to populate estimates
#' directly from a model object (e.g. an \code{emmGrid}); the named-vector
#' interface here is forward-compatible with that addition.
#'
#' @seealso \code{\link{prep_mirror_data}}, \code{\link{add_difference}}
#' @export
add_summary <- function(mirror_data,
                        estimates,
                        stat_label = "Mean",
                        baseline   = NULL) {

  .check_is_mirror_data(mirror_data)
  meta <- mirror_meta(mirror_data)
  arms <- c(meta$reference_arm, meta$comparator_arm)

  # ---- estimates: named, length 2, names match arms ----
  if (length(estimates) != 2 || is.null(names(estimates))) {
    stop("estimates must be a named numeric vector of length 2.", call. = FALSE)
  }
  bad <- setdiff(names(estimates), arms)
  if (length(bad) > 0) {
    stop("estimates name(s) do not match the arms in this object: ",
         paste(bad, collapse = ", "),
         ". Expected: ", paste(arms, collapse = ", "), ".", call. = FALSE)
  }

  # ---- NA estimate is an error (decided this round) ----
  if (anyNA(estimates)) {
    na_arm <- names(estimates)[is.na(estimates)]
    stop("estimate is NA for arm(s): ", paste(na_arm, collapse = ", "),
         ". A model may have failed to converge; supply finite estimates.",
         call. = FALSE)
  }

  # ---- baseline: default 0 for both arms ----
  if (is.null(baseline)) {
    baseline <- stats::setNames(c(0, 0), arms)
  } else {
    if (length(baseline) != 2 || is.null(names(baseline))) {
      stop("baseline must be a named numeric vector of length 2.", call. = FALSE)
    }
    bad_b <- setdiff(names(baseline), arms)
    if (length(bad_b) > 0) {
      stop("baseline name(s) do not match the arms: ",
           paste(bad_b, collapse = ", "), ".", call. = FALSE)
    }
  }

  summ <- data.frame(
    ROWTYPE  = "SUMM",
    SUBJID   = NA_character_,
    ARM      = arms,
    ARMROLE  = c("REFERENCE", "COMPARATOR"),
    VISIT    = NA_character_,
    TIMEROLE = NA_character_,
    AVAL     = NA_real_,
    ESTIMATE = c(estimates[[meta$reference_arm]],
                 estimates[[meta$comparator_arm]]),
    BASELINE = c(baseline[[meta$reference_arm]],
                 baseline[[meta$comparator_arm]]),
    STATTYPE = stat_label,
    N        = c(meta$n_reference, meta$n_comparator),
    stringsAsFactors = FALSE
  )

  combined <- .row_bind_mirror(mirror_data, summ)
  mirror_meta(combined) <- meta
  combined
}

# ── add_precision() ──────────────────────────────────────────────────────────

#' Attach per-arm precision (SE or CI) to a mirror_data object
#'
#' Optional pipeline step. Adds standard errors or confidence intervals for
#' each arm's estimate to the existing \code{SUMM} rows. Either \code{se} or
#' \code{ci} may be supplied; if both are given, \code{ci} is used for display.
#'
#' @param mirror_data A \code{mirror_data} object that already has summary rows.
#' @param se Named numeric vector of length 2 (one per arm), or \code{NULL}.
#' @param ci A named list of length 2; each element a numeric
#'   \code{c(lower, upper)}; or \code{NULL}.
#' @param ci_level Numeric confidence level for labelling, default \code{0.95}.
#'
#' @return The \code{mirror_data} object with precision columns populated on the
#'   \code{SUMM} rows.
#' @seealso \code{\link{add_summary}}
#' @export
add_precision <- function(mirror_data, se = NULL, ci = NULL, ci_level = 0.95) {

  .check_is_mirror_data(mirror_data)
  .check_has_summary(mirror_data)
  meta <- mirror_meta(mirror_data)
  arms <- c(meta$reference_arm, meta$comparator_arm)

  if (is.null(se) && is.null(ci)) {
    stop("Supply either se or ci.", call. = FALSE)
  }

  is_summ <- mirror_data$ROWTYPE == "SUMM"

  if (!is.null(ci)) {
    if (length(ci) != 2 || is.null(names(ci))) {
      stop("ci must be a named list of length 2, each c(lower, upper).",
           call. = FALSE)
    }
    for (arm in arms) {
      idx <- is_summ & mirror_data$ARM == arm
      mirror_data$CI_LOWER[idx] <- ci[[arm]][1]
      mirror_data$CI_UPPER[idx] <- ci[[arm]][2]
    }
  } else {
    if (length(se) != 2 || is.null(names(se))) {
      stop("se must be a named numeric vector of length 2.", call. = FALSE)
    }
    for (arm in arms) {
      idx <- is_summ & mirror_data$ARM == arm
      mirror_data$SE[idx] <- se[[arm]]
    }
  }

  meta$ci_level <- ci_level
  mirror_meta(mirror_data) <- meta
  mirror_data
}

# ── add_difference() ─────────────────────────────────────────────────────────

#' Attach the between-arm difference to a mirror_data object
#'
#' Optional pipeline step. Stores the comparator-minus-reference difference and
#' its optional precision and p-value on the comparator \code{SUMM} row, per the
#' specification.
#'
#' @param mirror_data A \code{mirror_data} object that already has summary rows.
#' @param estimate Numeric scalar. Difference, comparator minus reference, on
#'   the endpoint scale.
#' @param se Numeric scalar or \code{NULL}. Standard error of the difference.
#' @param ci_lower,ci_upper Numeric scalars or \code{NULL}. Difference CI bounds.
#' @param ci_level Numeric. Confidence level for labelling. Default \code{0.95}.
#' @param p_value Numeric scalar or \code{NULL}. P-value for the difference.
#'
#' @return The \code{mirror_data} object with difference fields populated on the
#'   comparator \code{SUMM} row.
#' @seealso \code{\link{add_summary}}, \code{\link{mirror_slope_plot}}
#' @export
add_difference <- function(mirror_data,
                           estimate,
                           se       = NULL,
                           ci_lower = NULL,
                           ci_upper = NULL,
                           ci_level = 0.95,
                           p_value  = NULL) {

  .check_is_mirror_data(mirror_data)
  .check_has_summary(mirror_data)
  meta <- mirror_meta(mirror_data)

  if (!is.numeric(estimate) || length(estimate) != 1 || is.na(estimate)) {
    stop("estimate must be a single non-missing numeric value.", call. = FALSE)
  }

  # ---- Sign-consistency warning (decided this round; spec 5.2) ----
  is_summ <- mirror_data$ROWTYPE == "SUMM"
  ref_est <- mirror_data$ESTIMATE[is_summ & mirror_data$ARMROLE == "REFERENCE"]
  trt_est <- mirror_data$ESTIMATE[is_summ & mirror_data$ARMROLE == "COMPARATOR"]
  observed_diff <- trt_est - ref_est
  if (sign(observed_diff) != sign(estimate) && observed_diff != 0 &&
      estimate != 0) {
    warning("Supplied difference sign (", ifelse(estimate >= 0, "+", "-"),
            ") is inconsistent with comparator minus reference estimates (",
            sprintf("%.2f", observed_diff),
            "). Verify the difference was entered correctly.", call. = FALSE)
  }

  idx <- is_summ & mirror_data$ARMROLE == "COMPARATOR"
  mirror_data$DIFF[idx]        <- estimate
  mirror_data$DIFF_SE[idx]     <- if (!is.null(se)) se else NA_real_
  mirror_data$DIFF_CILOW[idx]  <- if (!is.null(ci_lower)) ci_lower else NA_real_
  mirror_data$DIFF_CIHIGH[idx] <- if (!is.null(ci_upper)) ci_upper else NA_real_
  mirror_data$DIFF_CILEVEL[idx]<- ci_level
  mirror_data$PVALUE[idx]      <- if (!is.null(p_value)) p_value else NA_real_

  mirror_meta(mirror_data) <- meta
  mirror_data
}

# ── Internal helpers ─────────────────────────────────────────────────────────

#' Confirm an object is a mirror_data, else stop
#' @keywords internal
#' @noRd
.check_is_mirror_data <- function(x) {
  if (!inherits(x, "mirror_data")) {
    stop("Expected a 'mirror_data' object; got '", class(x)[1], "'. ",
         "Build one with prep_mirror_data().", call. = FALSE)
  }
  invisible(TRUE)
}

#' Confirm a mirror_data object already has SUMM rows
#' @keywords internal
#' @noRd
.check_has_summary <- function(x) {
  if (!any(x$ROWTYPE == "SUMM")) {
    stop("This mirror_data object has no summary rows yet. ",
         "Call add_summary() before add_difference()/add_precision().",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Is a difference present? (DIFF column exists and has a non-NA value)
#' @keywords internal
#' @noRd
.diff_present <- function(x) {
  "DIFF" %in% names(x) && any(!is.na(x$DIFF))
}

#' Confirm a difference has been attached, else stop with the fix
#'
#' Used by the renderer when a difference annotation is requested.
#' @keywords internal
#' @noRd
.check_has_difference <- function(x) {
  if (!.diff_present(x)) {
    stop("A difference annotation was requested (show_difference = TRUE) but ",
         "no difference is attached to this object. Call add_difference() first.",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Safely read an optional column from a mirror_data object
#'
#' Returns the column if present, otherwise a vector of NA of the right length.
#' Lets the renderer treat not-yet-added columns as all-NA without errors.
#' @keywords internal
#' @noRd
.col_or_na <- function(x, col) {
  if (col %in% names(x)) x[[col]] else rep(NA_real_, nrow(x))
}

#' Row-bind new rows to a mirror_data object, reconciling columns
#'
#' Aligns column sets between the existing object and the new rows, filling
#' absent columns with NA so base rbind succeeds. Caller restores meta.
#' @keywords internal
#' @noRd
.row_bind_mirror <- function(x, new_rows) {
  x_plain <- as.data.frame(x, stringsAsFactors = FALSE)
  all_cols <- union(names(x_plain), names(new_rows))
  for (cc in setdiff(all_cols, names(x_plain)))   x_plain[[cc]]  <- NA
  for (cc in setdiff(all_cols, names(new_rows)))  new_rows[[cc]] <- NA
  x_plain  <- x_plain[, all_cols, drop = FALSE]
  new_rows <- new_rows[, all_cols, drop = FALSE]
  bound <- rbind(x_plain, new_rows)
  new_mirror_data(bound, attr(x, "meta"))
}

# ── S3 methods ───────────────────────────────────────────────────────────────

#' Print a mirror_data object
#'
#' Shows a structured summary: arms and roles, timepoints, population, analyzed
#' N per arm, the statistic label, and which optional elements (summary,
#' precision, difference) are attached.
#'
#' @param x A \code{mirror_data} object.
#' @param ... Unused.
#' @return \code{x}, invisibly.
#' @export
print.mirror_data <- function(x, ...) {
  meta <- mirror_meta(x)
  n_obs  <- sum(x$ROWTYPE == "OBS")
  n_summ <- sum(x$ROWTYPE == "SUMM")
  has_summary    <- n_summ > 0
  has_difference <- .diff_present(x)
  has_precision  <- ("SE" %in% names(x) && any(!is.na(x$SE))) ||
                    ("CI_LOWER" %in% names(x) && any(!is.na(x$CI_LOWER)))
  stat <- if (has_summary) unique(stats::na.omit(x$STATTYPE))[1] else "(none)"

  cat("<mirror_data> schema ", meta$schema_version, "\n", sep = "")
  cat("  Reference (left)  : ", meta$reference_arm,
      "  (n=", meta$n_reference, ")\n", sep = "")
  cat("  Comparator (right): ", meta$comparator_arm,
      "  (n=", meta$n_comparator, ")\n", sep = "")
  cat("  Timepoints        : ", meta$baseline_label, "  ->  ",
      meta$endpoint_label, "\n", sep = "")
  if (nzchar(meta$population_label)) {
    cat("  Population        : ", meta$population_label, "\n", sep = "")
  }
  if (nzchar(meta$pop_filters)) {
    cat("  Filters applied   : ", meta$pop_filters, "\n", sep = "")
  }
  cat("  Observations      : ", n_obs, " OBS rows\n", sep = "")
  cat("  Summary           : ",
      if (has_summary) paste0("yes (", stat, ")") else "no", "\n", sep = "")
  cat("  Precision         : ", if (has_precision) "yes" else "no", "\n", sep = "")
  cat("  Difference        : ", if (has_difference) "yes" else "no", "\n", sep = "")
  if (nzchar(meta$direction)) {
    cat("  Direction         : ", meta$direction, "\n", sep = "")
  }
  invisible(x)
}

#' Summarize a mirror_data object
#'
#' Returns per-arm, per-timepoint N, mean, and range of the observed values.
#'
#' @param object A \code{mirror_data} object.
#' @param ... Unused.
#' @return A data frame, invisibly, with one row per arm/timepoint.
#' @export
summary.mirror_data <- function(object, ...) {
  obs <- object[object$ROWTYPE == "OBS", , drop = FALSE]
  if (nrow(obs) == 0) {
    message("No observation rows to summarize.")
    return(invisible(data.frame()))
  }
  grp <- interaction(obs$ARM, obs$TIMEROLE, drop = TRUE)
  out <- do.call(rbind, lapply(split(obs, grp), function(d) {
    data.frame(
      ARM      = d$ARM[1],
      TIMEROLE = d$TIMEROLE[1],
      N        = length(unique(d$SUBJID)),
      MEAN     = mean(d$AVAL),
      MIN      = min(d$AVAL),
      MAX      = max(d$AVAL),
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  print(out)
  invisible(out)
}

#' Coerce a mirror_data object to a plain data frame
#'
#' Strips the \code{mirror_data} class and metadata, returning the underlying
#' tabular structure as an ordinary data frame.
#'
#' @param x A \code{mirror_data} object.
#' @param ... Unused.
#' @return A plain data frame.
#' @export
as.data.frame.mirror_data <- function(x, ...) {
  attr(x, "meta") <- NULL
  class(x) <- "data.frame"
  x
}
