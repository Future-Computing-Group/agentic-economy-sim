# Tests for the headline tail-dispersion statistic: the median per-cell
# reduction in the burn-in-trimmed price CV of the slice-and-EMA arm against the
# naive arm, its paired-seed bootstrap interval, and the two envelopes quoted
# beside it.
#
# The statistic is a RATIO, so it is defined only where the denominator is: the
# cell filter and the per-seed exclusion are the two places the arithmetic can
# go wrong silently, and each gets its own discriminating cell in the fixture
# below (a cell whose reduction would move the median if the filter leaked, and
# a seed row whose ratio is undefined).

# ---- fixture ----------------------------------------------------------------

# One cell of the factorial: four arms x four seeds, each arm's per-seed tail CV
# given explicitly so every reduction is exact rather than approximately known.
vr_cell <- function(graph_type, load_level, N, naive, naive_ema,
                    hybrid_noema, hybrid_ema) {
  arms <- list(naive = naive, naive_ema = naive_ema,
               hybrid_noema = hybrid_noema, hybrid_ema = hybrid_ema)
  dplyr::bind_rows(lapply(names(arms), function(a) tibble::tibble(
    architecture = a, graph_type = graph_type, load_level = load_level,
    N = N, seed = seq_along(arms[[a]]),
    mean_price_volatility_tail = arms[[a]]
  )))
}

vr_frame <- function() {
  four <- function(x) rep(x, 4L)
  dplyr::bind_rows(
    # Three live cells: reductions 0.5, 0.8, 0.9, so the median is the middle
    # one exactly and a leak in either direction is visible.
    vr_cell("sp", "medium", 20L, four(1.0), four(0.5), four(0.6), four(0.5)),
    vr_cell("sp", "medium", 40L, four(1.0), four(0.5), four(0.6), four(0.2)),
    # ... the third with one seed row whose naive tail CV is zero: the ratio is
    # undefined there and the row must leave the cell mean, not poison it.
    vr_cell("sp", "medium", 60L, c(1.0, 1.0, 1.0, 0.0), four(0.5), four(0.6),
            four(0.1)),
    # A cell with a zero-CV arm: excluded by the four-arm filter. Its reduction
    # is 0.99, which would move the median if the filter leaked.
    vr_cell("sp", "high", 20L, four(1.0), four(0.5), four(0.0), four(0.01)),
    # A cell whose naive arm never leaves the floor: excluded, and the only
    # source of the "at most" quantity the Discussion quotes.
    vr_cell("sp", "high", 40L, four(0.0), four(0.5), four(0.0), four(0.002))
  )
}

vr_value <- function(out, q) out$value[out$quantity == q]

# ---- the statistic ----------------------------------------------------------

test_that("the median reduction is taken over the cells with a live price in every arm", {
  out <- stat_exp4_volatility_reduction(vr_frame(), B = 200L, seed = 1L)

  cells <- dplyr::filter(out, quantity == "cell_reduction")
  expect_equal(nrow(cells), 3L)
  expect_equal(sort(cells$value), c(0.5, 0.8, 0.9))
  expect_equal(vr_value(out, "median_reduction"), 0.8)
  # The cell count travels with the median, since the manuscript quotes it.
  expect_equal(out$n[out$quantity == "median_reduction"], 3L)

  # The two excluded cells are absent, not merely down-weighted.
  expect_false(any(cells$load_level == "high"))
})

test_that("a seed whose naive tail CV is zero leaves its cell's mean", {
  out <- stat_exp4_volatility_reduction(vr_frame(), B = 200L, seed = 1L)
  cells <- dplyr::filter(out, quantity == "cell_reduction")

  excluded <- dplyr::filter(cells, N == 60L)
  expect_equal(excluded$value, 0.9)     # NaN if the undefined row were kept
  expect_equal(excluded$n, 3L)
  expect_equal(excluded$n_excluded, 1L)
  expect_true(all(dplyr::filter(cells, N != 60L)$n_excluded == 0L))
})

test_that("the bootstrap resamples seeds, not cells, and reports its own B", {
  out <- stat_exp4_volatility_reduction(vr_frame(), B = 200L, seed = 1L)
  med <- dplyr::filter(out, quantity == "median_reduction")

  # Every seed carries the same per-cell ratio in this fixture, so a resample of
  # the seeds cannot move any cell mean and the interval collapses on the point
  # estimate. An interval that moved would mean the resampling ran over some
  # other axis.
  expect_equal(med$lo, 0.8)
  expect_equal(med$hi, 0.8)
  expect_equal(vr_value(out, "bootstrap_resamples"), 200)
})

test_that("the envelopes cover every cell, floored ones included", {
  out <- stat_exp4_volatility_reduction(vr_frame(), B = 200L, seed = 1L)

  naive_env  <- dplyr::filter(out, quantity == "naive_tail_cv_envelope")
  hybrid_env <- dplyr::filter(out, quantity == "hybrid_ema_tail_cv_envelope")
  expect_equal(c(naive_env$lo, naive_env$hi), c(0, 1))
  expect_equal(c(hybrid_env$lo, hybrid_env$hi), c(0.002, 0.5))
  expect_equal(naive_env$n, 5L)         # all cells, not the live ones

  # What the slice-and-EMA arm costs where the naive arm never left the floor.
  expect_equal(vr_value(out, "hybrid_ema_tail_cv_max_at_zero_naive"), 0.002)
})
