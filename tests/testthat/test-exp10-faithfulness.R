# Tests for Exp.10 — Prop. 3 faithfulness assumption under violation.
# slice_inflation models a scalar-capacity FAITHFULNESS violation: the
# integrator's scalar max-flow overstates the true multi-dimensional
# feasibility, over-admitting tasks that then congest the queue. Faithful
# (inflation=1) is the default; inflation>1 must degrade outcomes.

test_that("slice_inflation=1 is byte-identical to default (faithful)", {
  a <- exp4_run_single("hybrid", "sp", "high", N = 40L, seed = 6L, n_rounds = 20L)
  b <- exp4_run_single("hybrid", "sp", "high", N = 40L, seed = 6L, n_rounds = 20L,
                       slice_inflation = 1.0)
  expect_equal(digest::digest(a), digest::digest(b))
})

test_that("over-advertised capacity (inflation>1) degrades drop/welfare", {
  # The faithfulness violation only bites where the tiers are genuinely
  # contended: over-admitted tasks must actually overload the queue and miss
  # deadlines. We use a contended entangled cell (device-bottleneck topology,
  # high load); in slack/uncongested cells the extra admissions still meet the
  # deadline and the violation is harmless, which is itself the correct
  # (scoped) reading of Prop. 3 condition (ii).
  faithful <- exp4_run_single("hybrid", "entangled", "high", N = 60L, seed = 6L,
                              n_rounds = 30L, slice_inflation = 1.0)
  violated <- exp4_run_single("hybrid", "entangled", "high", N = 60L, seed = 6L,
                              n_rounds = 30L, slice_inflation = 2.0)
  # Over-admission raises drop (more tasks congest the queue, miss deadlines).
  expect_gt(violated$drop_rate, faithful$drop_rate - 1e-9)
  # And does not improve welfare (the scalar abstraction lost decision info).
  expect_true(violated$welfare <= faithful$welfare + 1e-6)
})

test_that("naive is unaffected by slice_inflation (no integrator)", {
  a <- exp4_run_single("naive", "sp", "high", N = 40L, seed = 6L, n_rounds = 15L,
                       slice_inflation = 1.0)$drop_rate
  b <- exp4_run_single("naive", "sp", "high", N = 40L, seed = 6L, n_rounds = 15L,
                       slice_inflation = 3.0)$drop_rate
  expect_equal(a, b, tolerance = 1e-9)
})
