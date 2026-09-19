# Tests for Exp.10 — Prop. 3 faithfulness assumption under violation, and for
# served_among_admitted, the observable Exp.10 reports beside welfare.
#
# slice_inflation models a scalar-capacity FAITHFULNESS violation: the
# integrator's scalar max-flow overstates the true multi-dimensional
# feasibility, so it advertises a larger slice capacity than the tiers can
# serve. Faithful (inflation = 1) is the default.
#
# What the knob does at the current operating point, measured on 6 seeds x 30
# rounds at the hybrid arm, high load:
#
#   sp (N = 55):        inflation 1.0 and 2.0 give IDENTICAL summaries on 6 of
#                       6 seeds. served_among_admitted 1.0000 either way.
#   entangled (N = 35): served_among_admitted moves by at most 0.015 and in
#                       both directions -- lower under inflation 2 on 3 of 6
#                       seeds, higher on 3. Mean 0.9777 faithful against 0.9756
#                       violated.
#
# The mechanism is that slice_capacity is not the binding admission constraint
# here. Instrumenting integrator_clear over 30 rounds: on sp the integrator
# admits 58.4 of 84.2 generated tasks per round against an advertised slice
# capacity of 100, and the packing loop's `k >= slice_capacity` break fires in 0
# of 30 rounds; on entangled it fires in 2 of 30 at inflation 1 and 0 of 30 at
# inflation 2. Admission is set by the positive-slice-surplus test, i.e. by
# price, so inflating the advertised capacity cannot over-admit. It only shifts
# the tatonnement excess term, which nudges the slice price down a little.
#
# So there is no contrast to assert at this operating point, and none is
# manufactured. The tests below pin what served_among_admitted IS (a fraction
# of admitted tasks, distinct from 1 - drop_rate) and that it is live (it falls
# when admitted work genuinely becomes undeliverable). The test that pinned the
# pre-fix drop/welfare contrast at the constant bid latency of 50 ms is retired
# with the operating point it was calibrated on.

test_that("slice_inflation=1 is byte-identical to default (faithful)", {
  a <- exp4_run_single("hybrid", "sp", "high", N = 40L, seed = 6L, n_rounds = 20L)
  b <- exp4_run_single("hybrid", "sp", "high", N = 40L, seed = 6L, n_rounds = 20L,
                       slice_inflation = 1.0)
  expect_equal(digest::digest(a), digest::digest(b))
})

test_that("naive is unaffected by slice_inflation (no integrator)", {
  a <- exp4_run_single("naive", "sp", "high", N = 40L, seed = 6L, n_rounds = 15L,
                       slice_inflation = 1.0)$drop_rate
  b <- exp4_run_single("naive", "sp", "high", N = 40L, seed = 6L, n_rounds = 15L,
                       slice_inflation = 3.0)$drop_rate
  expect_equal(a, b, tolerance = 1e-9)
})

test_that("served_among_admitted separates rationing from deadline misses", {
  # drop_rate is over GENERATED tasks, so it charges the market for every task
  # it declined to admit. served_among_admitted is over ADMITTED tasks, so it
  # reports only what the tiers failed to deliver. At the operating point the
  # two differ by the rationed share: 0.31 on sp, 0.40 on entangled.
  for (cell in list(list(gt = "sp", N = 55L, eff = 0.75),
                    list(gt = "entangled", N = 35L, eff = 0.85))) {
    res <- exp4_run_single("hybrid", cell$gt, "high", N = cell$N, seed = 1L,
                           n_rounds = 30L, integ_efficiency = cell$eff,
                           integ_eta = 0.15)
    expect_gte(res$served_among_admitted, 0)
    expect_lte(res$served_among_admitted, 1)
    expect_gt(res$served_among_admitted, 1 - res$drop_rate + 0.2)
  }
})

test_that("served_among_admitted falls when admitted work becomes undeliverable", {
  # The liveness check the range assertion cannot give: a column pinned at 1.00
  # would pass every test above. 400 ms of encapsulation overhead on top of a
  # 135 ms critical path pushes admitted tasks past their deadlines, and the
  # column reports it. Measured on sp at the operating point, seeds 1 to 3:
  # 1.0000 -> 0.7417, 1.0000 -> 0.7173, 1.0000 -> 0.7366.
  clean <- exp4_run_single("hybrid", "sp", "high", N = 55L, seed = 1L,
                           n_rounds = 30L, integ_efficiency = 0.75,
                           integ_eta = 0.15)
  slow  <- exp4_run_single("hybrid", "sp", "high", N = 55L, seed = 1L,
                           n_rounds = 30L, integ_efficiency = 0.75,
                           integ_eta = 0.15, enc_overhead_ms = 400)
  expect_lt(slow$served_among_admitted, clean$served_among_admitted - 0.1)
})
