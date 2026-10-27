# mirrorslope — color palettes

#' Color palettes for mirror slope plots
#'
#' Returns a length-2 character vector of hex colors for the reference and
#' comparator arms, in that order. Pair the result with \code{trt_levels} by
#' naming it, or pass it directly to \code{\link{mirror_slope_plot}}, which
#' assigns the first color to the reference arm and the second to the
#' comparator arm.
#'
#' @param name Character. One of:
#'   \describe{
#'     \item{\code{"default"}}{Neutral gray reference, strong blue comparator.}
#'     \item{\code{"colorblind"}}{Okabe-Ito colorblind-safe pair (gray, blue).}
#'     \item{\code{"capricor"}}{House palette (gray reference, Capricor blue).}
#'   }
#' @param reversed Logical. If \code{TRUE}, swap the two colors so the stronger
#'   color is applied to the reference arm. Default \code{FALSE}.
#'
#' @return An unnamed length-2 character vector of hex color codes:
#'   \code{c(reference_color, comparator_color)}.
#'
#' @examples
#' mirror_palette("default")
#' mirror_palette("colorblind")
#' # Name it to bind explicitly to arms:
#' stats::setNames(mirror_palette("capricor"), c("Placebo", "Active"))
#'
#' @export
mirror_palette <- function(name = c("default", "colorblind", "capricor"),
                           reversed = FALSE) {
  name <- match.arg(name)

  pal <- switch(
    name,
    # Reference (left), Comparator (right)
    default    = c("#6B6B6B", "#005CB9"),
    colorblind = c("#999999", "#0072B2"),  # Okabe-Ito gray + blue
    capricor   = c("#6B6B6B", "#005CB9")   # house colors
  )

  if (isTRUE(reversed)) pal <- rev(pal)
  pal
}
