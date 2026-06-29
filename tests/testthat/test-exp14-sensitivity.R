# Tests for the Exp.14 one-at-a-time sensitivity driver.
# These exercise the driver's contract on fast (small n_rounds) settings;
# the production sweep uses n_rounds = 200.

test_that("exp14_reduction_one returns a reduction <= 1 or NA", {
  r <- exp14_reduction_one("sp", "high", N = 40L, seed = 1L, n_rounds = 30L)
  expect_true(is.na(r) || r <= 1 + 1e-9)
})

test_that("a sub-floor naive cell returns NA (nothing to reduce)", {
  # With an absurdly high volatility floor, no naive baseline can exceed it,
  # so the reduction is undefined by construction -- this deterministically
  # exercises the NA branch without depending on the simulated magnitude.
  r <- exp14_reduction_one("sp", "high", N = 40L, seed = 1L, n_rounds = 30L,
                           vol_floor = 100)
  expect_true(is.na(r))
})

test_that("exp14_sensitivity_table is well-formed", {
  tab <- exp14_sensitivity_table(topologies = "sp", seeds = 1:2,
                                 N = 40L, n_rounds = 30L)
  expect_true(all(c("parameter", "level", "is_baseline", "n_volatile",
                    "median_reduction", "min_reduction",
                    "max_reduction") %in% names(tab)))
  # one row per (parameter, level): 4 params x 3 levels = 12
  expect_equal(nrow(tab), 12L)
  # exactly one baseline row per parameter (4 total)
  expect_equal(sum(tab$is_baseline), 4L)
  # any defined reduction must be <= 1
  defined <- tab$median_reduction[!is.na(tab$median_reduction)]
  expect_true(all(defined <= 1 + 1e-9))
})
