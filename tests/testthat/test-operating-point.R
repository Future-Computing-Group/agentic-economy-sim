# Tests for the pipeline's operating point — the agent count per topology.
#
# The bid-time base latency is the zero-queue critical path rather than a
# constant, which moved the regime every experiment runs in. One shared agent
# count can no longer place all three topologies in the same regime: they differ
# by 2.5x in bottleneck demand per task, so a count that leaves tree slack
# collapses sp and entangled. The operating point is therefore a named vector in
# _targets.R, one N per topology, and these tests pin its criterion:
#
#   (1) at HIGH load the market contends -- bottleneck offered load
#       rho = max_r(w_r / C_r) * lambda * N in [1.1, 1.7];
#   (2) at LOW load it is slack (rho < 1), so the load sweep spans the
#       congestion transition rather than sitting on one side of it;
#   (3) neither high nor medium load collapses: a real share of generated tasks
#       still clears the market, and deadline misses stay well short of total.
#
# rho is the BOTTLENECK tier's offered load, not the across-tier mean: the mean
# cannot separate a contended topology from a collapsed one (entangled at N = 75
# has mean utilisation 1.66 and a drop rate of 1.00). rho_bottleneck() lives in
# R/sim_helpers.R so this test and R/calibrate_operating_point.R's sweep read one
# definition of the criterion.
#
# (1) and (2) are properties of the configuration and are checked exactly.
# (3) is a measurement and is checked by running the naive arm.

.targets_constant <- function(name) {
  exprs <- as.list(parse(here::here("_targets.R")))
  hit <- Filter(function(e) is.call(e) && identical(e[[1]], quote(`<-`)) &&
                  identical(e[[2]], as.name(name)), exprs)
  expect_length(hit, 1L)
  eval(hit[[1]][[3]])
}


test_that("the operating point contends at high load and is slack at low load", {
  n_agents <- .targets_constant("n_agents")
  expect_setequal(names(n_agents), c("tree", "sp", "entangled"))

  for (gt in names(n_agents)) {
    # sp sits on the band's lower edge (rho exactly 1.1) because the next step
    # up is the one cell with a seed-dependent collapse; see the comment below.
    expect_gte(rho_bottleneck(gt, n_agents[[gt]], "high"), 1.1)
    expect_lte(rho_bottleneck(gt, n_agents[[gt]], "high"), 1.7)
    expect_lt(rho_bottleneck(gt, n_agents[[gt]], "low"), 1)
  }
})

test_that("the operating point does not collapse at high or medium load", {
  # One grid step up the clearing fraction falls, but not uniformly, and only sp
  # collapses. Over seeds 1 to 6 at high load, 30 rounds: tree at N = 95 clears
  # 0.55 to 0.64, indistinguishable from N = 90's 0.61 to 0.68; entangled at
  # N = 40 degrades smoothly to 0.25 to 0.37; sp at N = 60 clears 0.27 to 0.58
  # on five seeds and collapses on the sixth, to 0.029 with a drop rate of
  # 0.983, the online success model having learnt that nothing succeeds so that
  # expected values fall under the clearing price and the market admits nothing
  # for tens of rounds. The chosen points clear from round one on every seed.
  n_agents <- .targets_constant("n_agents")
  for (gt in names(n_agents)) {
    for (load in c("medium", "high")) {
      res <- exp4_run_single("naive", gt, load, N = n_agents[[gt]],
                             seed = 1L, n_rounds = 30L)
      expect_gt(res$clearing_fraction, 0.3)
      expect_lt(res$drop_rate, 0.9)
      # Contended: the market is rationing, so it cannot be admitting
      # everything. Measured 0.51 to 0.66 at high load across the three points.
      if (load == "high") expect_lt(res$clearing_fraction, 0.95)
    }
  }
})

test_that("clearing_fraction counts admissions against generated tasks", {
  # The calibration rests on this column, so it is pinned against an independent
  # count rather than only bounded: every admitted-task and generated-task count
  # is taken from the clearing call itself, and the per-round ratios averaged
  # the way the harness averages them. A column that reported admitted over
  # admitted, or generated over generated, would be identically 1 and would
  # satisfy every bound in the suite.
  rec <- new.env()
  rec$n_gen <- numeric(0)
  rec$n_alloc <- numeric(0)
  orig <- clear_multitier_market
  rlang::local_bindings(
    clear_multitier_market = function(tasks_all, ...) {
      out <- orig(tasks_all, ...)
      rec$n_gen   <- c(rec$n_gen, nrow(tasks_all))
      rec$n_alloc <- c(rec$n_alloc, nrow(out$allocation))
      out
    },
    .env = globalenv()
  )
  res <- exp4_run_single("naive", "sp", "high", N = 55L, seed = 1L,
                         n_rounds = 12L)

  ratios <- (rec$n_alloc / rec$n_gen)[rec$n_gen > 0]
  expect_length(rec$n_gen, 12L)
  expect_equal(res$clearing_fraction, mean(ratios))
  # Independently: the market neither admits everything nor nothing here.
  expect_true(any(rec$n_alloc < rec$n_gen))
  expect_lt(res$clearing_fraction, 1)
  expect_gt(res$clearing_fraction, 0)
})

test_that("operating_point_sweep reproduces the criterion on one cell", {
  # Smoke on the committed sweep, so the script behind the chosen counts cannot
  # rot. The full sweep is 3 topologies x 2 loads x 10 counts x 3 seeds.
  tab <- operating_point_sweep(graph_types = "sp", n_grid = 55L,
                               load_levels = "high", seeds = 1L, n_rounds = 12L)
  expect_equal(nrow(tab), 1L)
  expect_equal(tab$rho, rho_bottleneck("sp", 55L, "high"))
  expect_equal(tab$clearing_mean, tab$clearing_min)   # one seed
  expect_gt(tab$clearing_mean, 0)
  expect_lt(tab$clearing_mean, 1)
})
