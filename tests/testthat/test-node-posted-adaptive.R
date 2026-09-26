# A demand-responsive posted price.
#
# The arrival-order posted price with its level re-set between rounds from the
# demand it saw: the level moves up after an over-demanded round and down
# after an under-demanded one, never below the path's reserve cost. It reads
# no reported value, so it sits in the truth-free tier.

test_that("the level rises after an over-demanded round and falls after an under-demanded one", {
  expect_gt(node_adaptive_level(1.5, rho = 1.4, eta = 0.1, target = 0.9), 1.5)
  expect_lt(node_adaptive_level(1.5, rho = 0.5, eta = 0.1, target = 0.9), 1.5)
  expect_equal(node_adaptive_level(1.5, rho = 0.9, eta = 0.1, target = 0.9), 1.5)
  expect_equal(node_adaptive_level(1.5, rho = 1.4, eta = 0.1, target = 0.9),
               1.5 * (1 + 0.1 * (1.4 - 0.9)))
  # Floored at the reserve: the anchor at level one is the path's reserve cost.
  expect_equal(node_adaptive_level(1.02, rho = 0, eta = 0.2, target = 1), 1)
})

test_that("the signal is the demand that clears the posted level, at the most loaded node", {
  env <- node_run_env("tree", "high", 90L, "uniform", "off")
  tasks <- tibble::tibble(task_id = sprintf("t%02d", 1:30), agent_id = 1L,
                          deadline = 1000, value_base = 1.5, recipe = "l2")
  ev <- c(rep(1, 20), rep(0.1, 10))
  p  <- rep(0.5, 30)
  rho <- node_screened_ratio(tasks, env, ev, p)
  Ctok <- node_token_capacity(env)
  # twenty screened tokens at l2 load l2 (50), e1 (50) and d (100); the ten
  # priced out are not demand at this level
  expect_equal(rho, 20 / min(Ctok[c("l2", "e1", "d")]))
  expect_equal(node_screened_ratio(tasks, env, ev, rep(2, 30)), 0)
})

test_that("screened demand above the target raises the level and below it lowers it", {
  env <- node_run_env("tree", "high", 90L, "uniform", "off")
  cap <- min(node_token_capacity(env)[c("l2", "e1", "d")])
  mk <- function(n) tibble::tibble(task_id = sprintf("t%03d", seq_len(n)),
                                   agent_id = 1L, deadline = 1000,
                                   value_base = 1.5, recipe = "l2")
  hi <- node_screened_ratio(mk(2 * cap), env, rep(1, 2 * cap), rep(0.5, 2 * cap))
  lo <- node_screened_ratio(mk(cap / 2), env, rep(1, cap / 2), rep(0.5, cap / 2))
  expect_gt(node_adaptive_level(1.4, hi, eta = 0.1, target = 1), 1.4)
  expect_lt(node_adaptive_level(1.4, lo, eta = 0.1, target = 1), 1.4)
})

test_that("under a steady demand the level settles instead of diverging", {
  # A demand curve at the current level: the screened demand falls as the
  # level rises, as it does when a higher price screens out more tasks.
  demand <- function(k) 1.5 / k
  for (eta in c(0.05, 0.1, 0.2)) for (target in c(0.9, 1.0, 1.1)) {
    k <- 1
    path <- numeric(200)
    for (t in 1:200) { k <- node_adaptive_level(k, demand(k), eta, target); path[t] <- k }
    tail <- path[151:200]
    expect_lt(diff(range(tail)), 0.01, label = sprintf("band at eta %g target %g", eta, target))
    expect_equal(tail[[50]], 1.5 / target, tolerance = 0.01)
  }
})

test_that("with a zero step it is the arrival-order posted price at its starting level", {
  run <- function(m, ...) node_run_single("tree", "high", N = 90L, seed = 2L,
                                          n_rounds = 8L, mechanism = m,
                                          p_post_k = 1.5, ...)
  a <- run("posted_price_adaptive", adapt_eta = 0, adapt_target = 0.9)
  b <- run("posted_price_fcfs")
  cols <- c("welfare", "tokens_admitted", "median_latency", "mean_unit_cost",
            "alloc_ratio_true", "drop_rate")
  expect_equal(as.list(a[cols]), as.list(b[cols]))
})

test_that("a moving level moves the run", {
  run <- function(eta) node_run_single("tree", "high", N = 90L, seed = 2L,
                                       n_rounds = 8L, p_post_k = 1.5,
                                       mechanism = "posted_price_adaptive",
                                       adapt_eta = eta, adapt_target = 0.5)
  expect_false(isTRUE(all.equal(run(0)$mean_unit_cost, run(0.2)$mean_unit_cost)))
})

test_that("every winner pays the posted level", {
  env <- node_run_env("tree", "high", 90L, "uniform", "off")
  tasks <- tibble::tibble(task_id = sprintf("t%02d", 1:20), agent_id = 1:20,
                          deadline = 1000, value_base = runif(20, 1, 2),
                          recipe = rep(c("l1", "l3"), 10))
  lvl <- 1.3 * unname(posted_price_anchor_per_leaf(env, env$anc)[tasks$recipe])
  a <- posted_price_allocate(tasks, env, rep(10, 20), lvl, order = "arrival")
  expect_equal(a$payment, lvl[match(a$task_id, tasks$task_id)])
})

test_that("the arm is tuned over its step and target and reported in the tuned table", {
  g <- node_tuning_grid(c(1L, 2L))
  ad <- g[g$mechanism == "posted_price_adaptive", ]
  expect_setequal(unique(ad$adapt_eta), c(0.05, 0.1, 0.2, 0.4))
  expect_setequal(unique(ad$adapt_target),
                  c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.25, 1.5))
  # Thirty-six (step, target) pairs in each of the 48 cell x seed x level rows.
  expect_equal(nrow(ad), 36L * 12L * 2L * 2L)
  expect_true(all(which(g$mechanism == "posted_price_adaptive") >
                    max(which(g$mechanism != "posted_price_adaptive"))))
  # Not in the frontier grid: it has no single level.
  expect_false("posted_price_adaptive" %in% node_exp6_mechanism_grid(10L)$mechanism)

  tuning <- tidyr::expand_grid(
    graph_type = "tree", load_level = "high", architecture = "naive",
    tidyr::expand_grid(mechanism = "posted_price_adaptive", p_post_k = 1,
                       reserve_markup = 1, adapt_eta = c(0.05, 0.1, 0.2),
                       adapt_target = c(0.8, 0.9, 1.0)),
    seed = 1:2) %>%
    dplyr::mutate(welfare = -abs(adapt_eta - 0.1) - abs(adapt_target - 0.9))
  eg <- node_eval_grid(tuning, 11:12)
  r  <- eg[eg$mechanism == "posted_price_adaptive", ]
  expect_equal(unique(r$adapt_eta), 0.1)
  expect_equal(unique(r$adapt_target), 0.9)
  expect_true(all(c("adapt_eta", "adapt_target") %in% names(eg)))
  expect_false(anyNA(eg$adapt_eta))

  tuned <- node_tuned_table(eg %>% dplyr::mutate(
    welfare = 1, tokens_admitted = 1, median_latency = 1,
    welfare_over_optimum = 1, alloc_ratio_true = 1))
  expect_true("posted_price_adaptive" %in% tuned$mechanism)
  expect_equal(tuned$adapt_eta[tuned$mechanism == "posted_price_adaptive"], 0.1)
})

test_that("the mechanism statistics name the arm", {
  src <- paste(deparse(stat_exp6), collapse = "\n")
  expect_true(grepl("posted_price_adaptive", src, fixed = TRUE))
})


# ---- the widened grid and its diagnostics ----------------------------------

test_that("the widened grid keeps the nine pairs it had, byte for byte", {
  g  <- node_tuning_grid(c(1L, 2L))
  ad <- g[g$mechanism == "posted_price_adaptive", ]
  old <- tidyr::expand_grid(adapt_eta = c(0.05, 0.1, 0.2),
                            adapt_target = c(0.9, 1.0, 1.1))
  pairs <- dplyr::distinct(ad, adapt_eta, adapt_target)
  expect_equal(nrow(dplyr::semi_join(old, pairs, by = c("adapt_eta", "adapt_target"))), 9L)
  expect_type(ad$adapt_eta, "double")
  expect_type(ad$adapt_target, "double")
  # the other arms carry the neutral pair, as before
  expect_true(all(g$adapt_eta[g$mechanism != "posted_price_adaptive"] == 0))
  expect_true(all(g$adapt_target[g$mechanism != "posted_price_adaptive"] == 1))
})

test_that("the boundary flag reads the edge of the (step, target) grid", {
  b <- function(eta, target) node_knob_at_boundary(
    "posted_price_adaptive", 1, 1, adapt_eta = eta, adapt_target = target)
  expect_false(b(0.1, 1.0))
  expect_false(b(0.2, 0.7))
  expect_true(b(0.05, 1.0))
  expect_true(b(0.4, 1.0))
  expect_true(b(0.1, 0.5))
  expect_true(b(0.1, 1.5))
  # the other arms are read as before
  expect_true(node_knob_at_boundary("posted_price_fcfs", 4, 1))
  expect_false(node_knob_at_boundary("market", 1, 1.5))
})

adaptive_tuning <- function(peak_eta, peak_target, slope = 1, seeds = 1:4) {
  tidyr::expand_grid(
    graph_type = "tree", load_level = "high", architecture = "naive",
    mechanism = "posted_price_adaptive", p_post_k = 1, reserve_markup = 1,
    adapt_eta = node_adaptive_etas(), adapt_target = node_adaptive_targets(),
    seed = seeds) %>%
    dplyr::mutate(welfare = 10 - slope * (abs(log(adapt_eta / peak_eta)) +
                                          abs(adapt_target - peak_target)) +
                    seed / 1000)
}

tuned_from <- function(tuning) {
  eg <- node_eval_grid(tuning, 11:12)
  node_tuned_table(eg %>% dplyr::mutate(
    welfare = 1, tokens_admitted = 1, median_latency = 1,
    welfare_over_optimum = 1, alloc_ratio_true = 1), tuning)
}

test_that("the tuned table flags an adaptive optimum on the grid's edge", {
  edge <- tuned_from(adaptive_tuning(0.4, 1.0))
  r <- edge[edge$mechanism == "posted_price_adaptive", ]
  expect_equal(r$adapt_eta, 0.4)
  expect_true(r$knob_at_boundary)
  inner <- tuned_from(adaptive_tuning(0.1, 0.9))
  r <- inner[inner$mechanism == "posted_price_adaptive", ]
  expect_equal(c(r$adapt_eta, r$adapt_target), c(0.1, 0.9))
  expect_false(r$knob_at_boundary)
})

test_that("the tuned table reads flatness against each knob's inward neighbour", {
  # A sharp peak: every neighbour is worse on every seed by the same margin.
  peaked <- tuned_from(adaptive_tuning(0.1, 0.9, slope = 1))
  expect_false(peaked$knob_flat[peaked$mechanism == "posted_price_adaptive"])
  # A flat surface with seed noise: the neighbours are not separable.
  set.seed(3)
  flat <- adaptive_tuning(0.1, 0.9, slope = 0)
  flat$welfare <- 10 + stats::rnorm(nrow(flat), sd = 0.5)
  fl <- tuned_from(flat)
  expect_true(fl$knob_flat[fl$mechanism == "posted_price_adaptive"])
})
