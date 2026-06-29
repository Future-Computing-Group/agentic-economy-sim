# plot_helpers.R
# ---------------------------------------------------------------------------
# Shared plotting utilities for all experiment figures.
#
# Provides:
#   - mean_ci95()    : compute mean and 95% CI from a numeric vector
#   - theme_ieee()   : compact ggplot2 theme for IEEE double-column figures
#   - Colour palettes (greyscale-safe with linetype/shape backups)
# ---------------------------------------------------------------------------

#' Compute mean and 95% confidence interval.
#'
#' @param x Numeric vector (non-finite values are dropped).
#' @return A single-row tibble with columns: mean, lo, hi.
mean_ci95 <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n == 0L) return(tibble::tibble(mean = NA_real_, lo = NA_real_, hi = NA_real_))
  if (n == 1L) return(tibble::tibble(mean = x, lo = x, hi = x))
  m  <- mean(x)
  se <- stats::sd(x) / sqrt(n)
  tibble::tibble(mean = m, lo = m - 1.96 * se, hi = m + 1.96 * se)
}

#' IEEE TSC-compliant ggplot2 theme for single-column figures (~3.5 in width).
#'
#' @param base_size Base font size (default: 8 pt for IEEE column-width).
#' @return A ggplot2 theme object.
theme_ieee <- function(base_size = 8) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor  = ggplot2::element_blank(),
      strip.background  = ggplot2::element_rect(fill = "grey90"),
      strip.text        = ggplot2::element_text(size = ggplot2::rel(0.95), face = "bold"),
      legend.key.size   = grid::unit(0.8, "lines"),
      legend.key.width  = grid::unit(1.0, "lines"),
      legend.spacing.x  = grid::unit(0.3, "lines"),
      legend.margin     = ggplot2::margin(1, 1, 1, 1),
      legend.box.margin = ggplot2::margin(0, 0, -2, 0),
      legend.text       = ggplot2::element_text(size = ggplot2::rel(0.85)),
      legend.title      = ggplot2::element_text(size = ggplot2::rel(0.90), face = "bold"),
      plot.title        = ggplot2::element_text(size = ggplot2::rel(1.0), face = "bold"),
      plot.margin       = ggplot2::margin(2, 6, 2, 2),
      axis.title        = ggplot2::element_text(size = ggplot2::rel(0.95)),
      axis.text         = ggplot2::element_text(size = ggplot2::rel(0.85))
    )
}


# ===========================================================================
# Colour palettes (greyscale-safe)
# ===========================================================================

# Architecture: red = naive, blue = hybrid (distinct in greyscale)
palette_arch <- c("naive" = "#E41A1C", "hybrid" = "#377EB8")

# Load levels: green/orange/purple (linetype backup for greyscale)
palette_load <- c("low" = "#4DAF4A", "medium" = "#FF7F00", "high" = "#984EA3")

# DAG topologies: blue/green/orange/red (shape backup for greyscale)
palette_topo <- c(
  "linear"    = "#377EB8",
  "tree"      = "#4DAF4A",
  "sp"        = "#FF7F00",
  "entangled" = "#E41A1C"
)

# Linetype for load levels (greyscale fallback)
linetype_load <- c("low" = "solid", "medium" = "dashed", "high" = "dotted")

# Point shape for topologies (greyscale fallback)
shape_topo <- c("linear" = 16, "tree" = 17, "sp" = 15, "entangled" = 18)
