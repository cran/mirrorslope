# mirrorslope — publication theme

#' A publication theme for mirror slope plots
#'
#' A \code{ggplot2} theme tuned for journal figures: a clean sans typeface, a
#' coherent family of light hairline rules, no x-axis ticks (the x-axis is
#' positional, not numeric), a clear type hierarchy, and proportionally scaled
#' text. Because \code{\link{mirror_slope_plot}} returns a standard \code{ggplot}
#' object, you can apply this theme and then override any element with
#' \code{+ theme(...)}.
#'
#' @param base_size Numeric. Base font size in points; all text scales from it.
#'   Default \code{11}.
#' @param base_family Character. Base font family. Defaults to a clean sans
#'   stack resolved from what the graphics device provides (\code{"Helvetica"},
#'   then \code{"Arial"}, then \code{"sans"}). Pass a specific family to override.
#'
#' @return A \code{ggplot2} theme object.
#'
#' @details
#' The theme hides x-axis ticks and the minor grid because the horizontal axis
#' in a mirror slope plot encodes position (baseline versus endpoint for each
#' arm), not a continuous numeric scale. A single set of very light horizontal
#' guides is retained to aid reading values off the y-axis. Rule weights (axis
#' lines, ticks, gridlines) are deliberately kept to a light, consistent family
#' so that no chart furniture competes with the data.
#'
#' For submission-quality output, render to a vector format (PDF or SVG) or to a
#' raster at 300 dpi or higher, and ensure the chosen \code{base_family} is
#' available to and embedded by the graphics device.
#'
#' @examples
#' \donttest{
#' # Applied automatically inside mirror_slope_plot(), or layer it yourself:
#' # p + theme_mirror(base_size = 13)
#' }
#'
#' @importFrom grDevices postscriptFonts
#' @importFrom ggplot2 theme_classic theme element_blank element_line element_text element_rect margin unit rel
#' @export
theme_mirror <- function(base_size = 11, base_family = NULL) {

  # Resolve a clean sans family from what the device offers. Users can override
  # by passing base_family explicitly.
  if (is.null(base_family)) {
    fam_ok <- tryCatch(
      names(grDevices::postscriptFonts()),
      error = function(e) character(0)
    )
    base_family <- if ("Helvetica" %in% fam_ok) "Helvetica" else
      if ("Arial" %in% fam_ok) "Arial" else "sans"
  }

  ink        <- "grey20"   # primary text
  muted      <- "grey40"   # subtitle
  faint      <- "grey50"   # caption / footnote
  rule       <- "grey70"   # axis lines and ticks
  grid_light <- "grey93"   # horizontal guides

  ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      legend.position    = "none",

      # Coherent light rule family
      axis.line.x        = ggplot2::element_line(color = rule, linewidth = 0.35),
      axis.line.y        = ggplot2::element_line(color = rule, linewidth = 0.35),
      axis.ticks.x       = ggplot2::element_blank(),
      axis.ticks.y       = ggplot2::element_line(color = rule, linewidth = 0.3),
      axis.ticks.length  = ggplot2::unit(3, "pt"),
      panel.grid.major.y = ggplot2::element_line(color = grid_light,
                                                 linewidth = 0.3),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),

      # Type hierarchy: bold title, lighter subtitle, smallest footnote/caption
      plot.title         = ggplot2::element_text(face = "bold", color = ink,
                                                 size = ggplot2::rel(1.15),
                                                 margin = ggplot2::margin(b = 2)),
      plot.subtitle      = ggplot2::element_text(color = muted,
                                                 size = ggplot2::rel(0.88),
                                                 margin = ggplot2::margin(b = 8)),
      plot.caption       = ggplot2::element_text(color = faint,
                                                 size = ggplot2::rel(0.72),
                                                 hjust = 0, lineheight = 1.25,
                                                 margin = ggplot2::margin(t = 8)),
      plot.caption.position = "plot",
      plot.title.position   = "plot",

      axis.title.y       = ggplot2::element_text(color = ink,
                                                 size = ggplot2::rel(0.92)),
      axis.text          = ggplot2::element_text(color = ink,
                                                 size = ggplot2::rel(0.82)),
      plot.margin        = ggplot2::margin(t = 16, r = 14, b = 12, l = 14)
    )
}
