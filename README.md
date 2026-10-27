# mirrorslope

> Mirror slope plots for two-arm clinical trial data

A mirror slope plot compares two treatment arms over a single observation
interval. Both arms share one x-axis and converge at a central endpoint, making
the between-arm gap at the endpoint the primary visual quantity. The reference
arm is drawn on the left, the comparator on the right, and both mean lines meet
at the centre.

`mirror_slope_plot()` returns a standard **ggplot2** object, so the full ggplot2
ecosystem remains available for customisation.

## Installation

```r
# install.packages("devtools")
devtools::install_github("Sullivanstatistics/mirrorslope")
```

## Quick start

```r
library(mirrorslope)

# A long analysis dataset: one row per subject per visit
df <- data.frame(
  USUBJID = rep(paste0("S", 1:40), each = 2),
  TRTP    = rep(c("Placebo", "Active"), each = 40),
  AVISIT  = rep(c("Baseline", "Week 24"), times = 40),
  AVAL    = c(rbind(rep(0, 40),
                    c(rnorm(20, -10, 5), rnorm(20, -6, 5))))
)

df |>
  prep_mirror_data(
    trt_levels    = c("Placebo", "Active"),
    time_baseline = "Baseline",
    time_endpoint = "Week 24",
    value_label   = "% Change from Baseline"
  ) |>
  add_summary(
    estimates  = c("Placebo" = -10.2, "Active" = -5.9),
    stat_label = "LS-Mean"
  ) |>
  add_difference(estimate = 4.3, ci_lower = 1.2, ci_upper = 7.4,
                 p_value = 0.011) |>
  mirror_slope_plot(
    trt_colors      = mirror_palette("colorblind"),
    show_difference = TRUE,
    show_slowing    = TRUE,
    title           = "Percent Change from Baseline at Week 24"
  )
```

## The pipeline

The plot is built in composable, pipe-friendly stages. Each returns a
`mirror_data` object that stores everything available; the plot call exposes
only what is requested.

| Function | Role |
|----------|------|
| `prep_mirror_data()` | Validate and structure the analysis data |
| `add_summary()` | Attach group-level estimates and baselines |
| `add_precision()` | Attach per-arm SE or confidence intervals (optional) |
| `add_difference()` | Attach the between-arm difference and p-value (optional) |
| `mirror_slope_plot()` | Render the figure |

Helpers: `mirror_palette()` for colours (including a colourblind-safe pair) and
`theme_mirror()` for a publication theme.

## Documentation

Three vignettes are included:

* **Introduction to mirrorslope** — the core walkthrough.
* **Clinical workflow with ADaM data** — a realistic data-preparation pattern,
  including merging population flags from a subject-level dataset.
* **Customising the plot** — theming, palettes, and combining panels.

## Model-agnostic by design

The package does not fit statistical models. Estimates, intervals, and p-values
are computed upstream (for example from a mixed model for repeated measures) and
passed in. This keeps the tool independent of any particular analysis method and
keeps every figure traceable to the values supplied. The `mirror_data`
structure conforms to a versioned specification shared with a companion SAS
macro so that both tools produce equivalent figures.

## Citation and credit

The mirrored slopegraph was designed by Kati Maharry (Capricor Therapeutics). The
display is described in Maharry K, Sullivan A. *The Mirrored Slopegraph: A
Back-to-Back Composition for Comparing Two-Arm Change-from-Baseline Trajectories*
(2026, manuscript in preparation), and was first used in the supplementary
appendix of McDonald CM et al., *Lancet* 2026;408:721-733,
[doi:10.1016/S0140-6736(26)01385-1](https://doi.org/10.1016/S0140-6736(26)01385-1).
Run `citation("mirrorslope")` for a citation entry.

## License

MIT
