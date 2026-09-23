# test-node-sensitivity.R
# ---------------------------------------------------------------------------
# The parameters the mechanism comparison's magnitude rests on, swept beside
# the numbers rather than after them.
#
# Three of them are the ones the triage named: the value-decay rate, the
# execution model's utilisation clamp and the bid-time congestion coefficient.
# The fourth setting is not a one-at-a-time level but a calibration: the queue
# term retuned so that the node simulator's elasticity of median latency to
# offered load matches what the emulated testbed measured, which is the one
# point in this parameter space with an outside measurement behind it.
# ---------------------------------------------------------------------------

test_that("the execution model takes its clamp and its queue coefficient", {
  env <- node_run_env("tree", "high", 90L, "uniform", "off")
  alloc <- tibble(task_id = sprintf("t%03d", 1:80), agent_id = rep_len(1:9, 80),
                  deadline = 1000, value_base = 1.5,
                  recipe = rep_len(rownames(env$anc), 80))
  at <- function(...) execute_allocation(alloc, env, latency_noise_cv = 0, ...)

  base <- at()
  expect_identical(at(util_clamp = 0.99, queue_coefficient = 2), base)
  # A looser clamp lets the queue at capacity cost more, and a larger
  # coefficient scales the same queue.
  expect_true(all(at(util_clamp = 0.995)$latency >= base$latency))
  expect_true(all(at(queue_coefficient = 4)$latency > base$latency))
  expect_true(all(at(util_clamp = 0.95)$latency <= base$latency))
})

test_that("the driver passes the queue term to the arm and to its references", {
  one <- function(...) node_run_single("tree", "high", N = 90L, seed = 1L,
                                       n_rounds = 6L,
                                       lambda_l_default = node_lambda_l(), ...)
  base <- one()
  # The default is the reported level, so naming it changes nothing.
  d <- node_congestion_default()
  expect_equal(one(exec_clamp = d$exec_clamp, queue_coef = d$queue_coef), base,
               tolerance = 1e-12)

  hot <- one(exec_clamp = 0.995, queue_coef = 4)
  expect_gt(hot$median_latency, base$median_latency)
  # The reference optima are scored through the same execution model, so a
  # queue term that changes the arm changes what the planner could have done.
  expect_lt(hot$optimum_ex_post, base$optimum_ex_post)
  expect_equal(hot$ceiling_zero_queue, base$ceiling_zero_queue)
  expect_equal(hot$exec_clamp, 0.995)
  expect_equal(hot$queue_coef, 4)
})

test_that("the load elasticity is the statistic the testbed comparison uses", {
  # (median latency ratio - 1) / (offered load ratio - 1), on rows that carry
  # the offered load as the admitted count over the clearing fraction.
  rows <- function(lat, admitted, frac) {
    tibble(median_latency = lat, tokens_admitted = admitted,
           clearing_fraction = frac)
  }
  lo <- rows(c(100, 100), c(50, 50), 0.5)     # offered 100
  hi <- rows(c(120, 120), c(60, 60), 0.4)     # offered 150
  expect_equal(node_load_elasticity(lo, hi), (120 / 100 - 1) / (150 / 100 - 1))
  # Two loads that offered the same work measure no sensitivity, not an
  # infinite one.
  expect_true(is.na(node_load_elasticity(lo, lo)))
})

test_that("the sweep varies one factor at a time from the headline setting", {
  s <- node_sensitivity_settings()
  base <- s[s$setting == "baseline", ]
  expect_equal(nrow(base), 1L)
  expect_equal(base$lambda_l, node_lambda_l())
  expect_equal(base$exec_clamp, 0.99)
  expect_equal(base$alpha, 50)
  expect_equal(base$queue_coef, 2)

  knobs <- c("lambda_l", "exec_clamp", "alpha", "queue_coef")
  moved <- vapply(seq_len(nrow(s)), function(i)
    sum(vapply(knobs, function(k) !isTRUE(all.equal(s[[k]][i], base[[k]])),
               logical(1))), numeric(1))
  # Every row but the baseline and the calibrated one moves exactly one knob;
  # the calibrated row moves the two that make up the queue term.
  expect_equal(moved[s$setting == "baseline"], 0)
  expect_true(all(moved[!s$setting %in% c("baseline", "calibrated")] == 1))
  expect_lte(moved[s$setting == "calibrated"], 2)
  expect_true("calibrated" %in% s$setting)
  expect_setequal(s$lambda_l[grepl("^lambda_l", s$setting)], c(0.0025, 0.005, 0.015))
  expect_setequal(s$exec_clamp[grepl("^exec_clamp", s$setting)], c(0.95, 0.995))
  expect_setequal(s$alpha[grepl("^alpha", s$setting)], 200)
})

test_that("the sweep runs four mechanisms on three instances at high load", {
  g <- node_sensitivity_grid(seeds = 1:5)
  expect_setequal(g$mechanism, c("market", "market_cc", "posted_price", "greedy_ev"))
  expect_setequal(g$p_post_k[g$mechanism == "posted_price"], 2)
  expect_setequal(g$graph_type, c("tree", "sp", "entangled"))
  expect_setequal(g$load_level, "high")
  expect_equal(nrow(g), nrow(node_sensitivity_settings()) * 4L * 3L * 2L * 5L)
  expect_equal(nrow(g), 960L)
})

test_that("the sweep runs the uncontracted and the contracted arm at every setting", {
  # A sensitivity taken on one architecture says nothing about whether the
  # ordering it reports survives the contraction, which is the other factor
  # the mechanism block crosses.
  g <- node_sensitivity_grid(seeds = 1:5)
  expect_setequal(g$architecture, c("naive", "hybrid_noema"))
  expect_true(all(table(g$setting, g$architecture) > 0))
  # The grid emits driver levels, so the mapping the mechanism block applies
  # is the identity on them.
  expect_identical(node_eval_architecture(g$architecture), g$architecture)

  src <- paste(readLines(here::here("_targets.R")), collapse = " ")
  expect_true(grepl(
    "node_eval_architecture\\(\\s*node_exp6_sensitivity_grid\\$architecture\\)",
    src))
})

test_that("the pipeline carries the sweep as its own target", {
  src <- paste(readLines(here::here("_targets.R")), collapse = "\n")
  expect_true(grepl("tar_target\\(\\s*node_exp6_sensitivity[,\\s]", src))
  expect_true(grepl("tar_target\\(\\s*node_exp6_sensitivity_grid[,\\s]", src))
})
