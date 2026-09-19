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
  # one row per (parameter, level): three params x 3 levels, plus
  # integ_efficiency's fourth level at 1.0 (the no-savings case)
  expect_equal(nrow(tab), 13L)
  # exactly one baseline row per parameter (4 total)
  expect_equal(sum(tab$is_baseline), 4L)
  # any defined reduction must be <= 1
  defined <- tab$median_reduction[!is.na(tab$median_reduction)]
  expect_true(all(defined <= 1 + 1e-9))
})

test_that("exp14_sensitivity_table indexes a named N by topology", {
  # The pipeline passes the per-topology operating point as a named vector
  # (_targets.R). Indexing it by anything but the topology -- taking the first
  # element, or passing the whole vector through -- would silently run the sweep
  # at the wrong agent count, so the decoy entry below must not be reachable.
  named  <- exp14_sensitivity_table(topologies = "entangled", seeds = 1L,
                                    N = c(sp = 90L, entangled = 40L),
                                    n_rounds = 5L)
  scalar <- exp14_sensitivity_table(topologies = "entangled", seeds = 1L,
                                    N = 40L, n_rounds = 5L)
  expect_equal(named, scalar)
  expect_equal(nrow(named), 13L)
})


test_that("the sweep grid carries the no-savings efficiency level", {
  grid <- exp14_sweep_grid()
  expect_equal(nrow(grid), 13L)
  expect_setequal(names(grid), c("parameter", "level", "is_baseline"))
  expect_equal(sort(grid$level[grid$parameter == "integ_efficiency"]),
               c(0.65, 0.75, 0.85, 1.0))
  expect_equal(sum(grid$is_baseline), 4L)
})


test_that("a branch computes the row the serial table computed", {
  # The dynamic target evaluates one (parameter, level) cell per branch. Each
  # cell reseeds inside exp4_run_single, so a cell's value cannot depend on how
  # many cells ran before it; if it ever did, the branched pipeline and the
  # serial table would disagree here.
  grid <- exp14_sweep_grid()
  i <- which(grid$parameter == "integ_efficiency" & grid$level == 1.0)
  expect_length(i, 1L)

  args <- list(topologies = "sp", seeds = 1L, N = 20L, n_rounds = 10L)
  row <- do.call(exp14_sensitivity_row,
                 c(list(parameter = grid$parameter[i], level = grid$level[i]),
                   args))
  tab <- do.call(exp14_sensitivity_table, args)

  expect_equal(nrow(row), 1L)
  expect_equal(as.list(row), as.list(tab[i, ]))
})


test_that("a branch's row is the median over its own volatile cells", {
  # The structural test above holds even where every cell is sub-floor (all NA).
  # This one runs a cell that is actually volatile and checks the number, so a
  # refactor that shifted an argument or dropped a cell cannot pass silently.
  row <- exp14_sensitivity_row("integ_efficiency", 0.85, topologies = "sp",
                               seeds = 1L, N = 55L, n_rounds = 40L)
  direct <- exp14_reduction_one("sp", "high", N = 55L, seed = 1L,
                                n_rounds = 40L, integ_efficiency = 0.85)
  expect_false(is.na(direct))
  expect_equal(row$n_volatile, 1L)
  expect_equal(row$median_reduction, direct)
  expect_equal(row$min_reduction, direct)
  expect_equal(row$max_reduction, direct)
})


test_that("the sweep holds efficiency at the headline configuration", {
  # Every other row of the table is a sensitivity around the baseline cell. If
  # the baseline were not the configuration the headline arms run at, those
  # rows would report the sensitivity of a result no experiment produces.
  k <- targets_constants(c("integ_efficiency_sp", "integ_efficiency_ent"))
  expect_equal(k$integ_efficiency_sp, k$integ_efficiency_ent)
  expect_equal(exp14_baseline()$integ_efficiency, k$integ_efficiency_sp)

  grid   <- exp14_sweep_grid()
  marked <- grid[grid$parameter == "integ_efficiency" & grid$is_baseline, ]
  expect_equal(nrow(marked), 1L)
  expect_equal(marked$level, k$integ_efficiency_sp)
})


test_that("the price-step sweep is centred on the common step", {
  # The levels are derived from the constant rather than written beside it: a
  # step that moved would otherwise leave the sweep bracketing a value no arm
  # runs at, and the row marked baseline would not be one.
  levels <- exp14_sweeps()$integ_eta
  expect_length(levels, 3L)
  expect_equal(levels[2], price_eta)
  expect_equal(mean(c(levels[1], levels[3])), price_eta)
  expect_true(exp14_is_baseline("integ_eta", levels[2]))
})
