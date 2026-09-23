# The node substrate's default congestion level is the one it reports.
#
# The mechanism block runs both levels explicitly; every other node block runs
# at the driver's default. That default is the calibrated level, so every
# block is read at the same queue term. The steep level stays in the grids and
# the sensitivity sweep as an explicit setting.

test_that("the default congestion level is the calibrated row", {
  lv  <- node_congestion_levels()
  cal <- lv[lv$congestion == "calibrated", ]
  d   <- node_congestion_default()
  expect_equal(d$exec_clamp, cal$exec_clamp)
  expect_equal(d$queue_coef, cal$queue_coef)
  expect_equal(unname(unlist(d)), c(0.95, 0.75))
})

test_that("a bare node run is the explicit calibrated run", {
  d <- node_congestion_default()
  a <- node_run_single("tree", "high", N = 90L, seed = 2L, n_rounds = 6L)
  b <- node_run_single("tree", "high", N = 90L, seed = 2L, n_rounds = 6L,
                       exec_clamp = d$exec_clamp, queue_coef = d$queue_coef)
  expect_identical(a, b)
  expect_equal(a$exec_clamp, 0.95)
  expect_equal(a$queue_coef, 0.75)
})

test_that("the battery drivers take the same default", {
  d <- node_congestion_default()
  for (f in c("node_convergence_run", "node_determinacy_run",
              "node_report_stability_run", "node_shock_run")) {
    fm <- formals(get(f))
    expect_equal(eval(fm$exec_clamp), d$exec_clamp, info = f)
    expect_equal(eval(fm$queue_coef), d$queue_coef, info = f)
  }
})

test_that("the battery drivers pass the level to execution", {
  # A steeper queue term makes a loaded instance slower, so a driver that
  # dropped the argument would report the same latency at both levels.
  steep <- node_congestion_levels()
  steep <- steep[steep$congestion == "baseline", ]
  one <- function(...) node_shock_run("tree", N = 90L, seed = 1L,
                                      n_rounds = 6L, shock_start = 3L,
                                      capacity_end = 5L, ...)
  a <- one()
  b <- one(exec_clamp = steep$exec_clamp, queue_coef = steep$queue_coef)
  expect_false(isTRUE(all.equal(a, b)))
})
