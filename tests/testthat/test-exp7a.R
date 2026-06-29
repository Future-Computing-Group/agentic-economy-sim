# Tests for Exp.7a — the strategic-bidding experiment that produces the DSIC
# empirics. Built on the validated vcg_allocate port.
#
# Design constraints: must run in a BINDING-capacity
# (saturated) regime so payments are positive and DSIC is observable; truthful
# must be the empirical best response (regret >= 0, = 0 at alpha = 1).

test_that("exp7a_run_single returns the expected schema", {
  r <- exp7a_run_single(graph_type = "tree", load_level = "high", N = 8L,
                        seed = 1L, cap = 30, shades = c(0.7, 1.0, 1.3),
                        n_rounds = 8L)
  expect_s3_class(r, "tbl_df")
  expect_true(all(c("graph_type", "load_level", "N", "seed",
                    "mean_payment", "binding") %in% names(r)))
  # one regret column per shade
  expect_true(any(grepl("^regret_", names(r))))
})

test_that("Exp.7a is run in a binding-capacity regime (non-vacuous DSIC)", {
  r <- exp7a_run_single(graph_type = "sp", load_level = "high", N = 8L,
                        seed = 2L, cap = 30, shades = c(0.5, 0.7, 0.9, 1.0, 1.1, 1.3),
                        n_rounds = 12L)
  # If capacity never binds, payments are ~0 and the DSIC result is vacuous.
  expect_gt(r$mean_payment, 1e-6)
  expect_true(r$binding)
})

test_that("truthful (alpha=1) is the empirical best response: regret >= 0, =0 at truthful", {
  shades <- c(0.5, 0.7, 0.9, 1.0, 1.1, 1.3)
  # Must be a BINDING regime: on a non-binding cell (e.g. tree at cap=30) VCG
  # payments are ~0 and every regret is ~0, so the test would pass for ANY
  # mechanism (vacuous). SP/high/cap=30 binds (cloud tier saturates). We assert
  # binding so the best-response claim is genuinely tested.
  r <- exp7a_run_single(graph_type = "sp", load_level = "high", N = 8L,
                        seed = 2L, cap = 30, shades = shades, n_rounds = 20L)
  expect_true(r$binding,
              info = "best-response test must run where capacity binds, else it is vacuous")
  reg <- vapply(shades, function(a) r[[sprintf("regret_%g", a)]], numeric(1))
  names(reg) <- as.character(shades)
  # Regret at truthful is exactly 0 (utility(1) - utility(1)).
  expect_equal(unname(reg["1"]), 0, tolerance = 1e-9)
  # Truthful is weakly best: no shade yields negative mean regret (= higher utility),
  # and at least one misreport is strictly punished (non-vacuous DSIC evidence).
  expect_true(all(reg >= -1e-6),
              info = "a shade with negative regret would mean misreport beats truthful (DSIC violation)")
  expect_gt(max(reg), 1e-6)
})
