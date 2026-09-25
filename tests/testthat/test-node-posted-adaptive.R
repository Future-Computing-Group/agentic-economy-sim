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

test_that("the offered load is read at the most loaded node", {
  env <- node_run_env("tree", "high", 90L, "uniform", "off")
  tasks <- tibble::tibble(task_id = sprintf("t%02d", 1:30), agent_id = 1L,
                          deadline = 1000, value_base = 1.5, recipe = "l2")
  rho <- node_offered_ratio(tasks, env)
  Ctok <- node_token_capacity(env)
  # thirty tokens at l2 load l2 (50), e1 (50) and d (100)
  expect_equal(rho, 30 / min(Ctok[c("l2", "e1", "d")]))
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
  expect_setequal(unique(ad$adapt_eta), c(0.05, 0.1, 0.2))
  expect_setequal(unique(ad$adapt_target), c(0.8, 0.9, 1.0))
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
