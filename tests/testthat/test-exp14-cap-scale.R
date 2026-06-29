# Tests for the cap_scale knob used by Exp.14 (parameter sensitivity).
# cap_scale multiplies per-tier capacities; smaller => more saturation
# (higher drop), the dominant driver of the price-volatility result. Exp.14
# probes the sensitivity of that result to the parameter choices.

test_that("cap_scale=1 is byte-identical to the default (no behaviour change)", {
  a <- exp4_run_single("hybrid", "sp", "high", N = 40L, seed = 5L,
                       n_rounds = 20L)
  b <- exp4_run_single("hybrid", "sp", "high", N = 40L, seed = 5L,
                       n_rounds = 20L, cap_scale = 1.0)
  expect_equal(digest::digest(a), digest::digest(b))
})

test_that("smaller cap_scale raises drop rate (more saturation)", {
  hi_cap <- exp4_run_single("naive", "sp", "high", N = 40L, seed = 5L,
                            n_rounds = 20L, cap_scale = 1.5)$drop_rate
  lo_cap <- exp4_run_single("naive", "sp", "high", N = 40L, seed = 5L,
                            n_rounds = 20L, cap_scale = 0.5)$drop_rate
  expect_true(lo_cap >= hi_cap - 1e-9)
})
