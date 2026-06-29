# Tests for per-tier price formation:
#
# integrator_clear() must compute and expose per-tier prices via an internal
# tâtonnement on per-tier excess demand, NOT broadcast a single scalar slice
# price across the tier dimension. These tests require a $tier_prices field
# carrying a genuine per-tier price vector.

# ----- Fixtures --------------------------------------------------------------

make_test_env <- function(load_level = "medium", N = 20L,
                          graph_type = "tree") {
  graph <- build_dependency_graph(graph_type)
  init_environment(graph, load_level = load_level, n_agents = N,
                   graph_type = graph_type)
}

make_test_integrator <- function() {
  integrator_init(beta = 0.8, efficiency_factor = 0.85,
                  eta = 0.15)
}

make_test_tasks <- function(n_tasks = 40L, seed = 1L) {
  set.seed(seed)
  tibble::tibble(
    task_id    = sprintf("t%03d", seq_len(n_tasks)),
    agent_id   = sample.int(5, n_tasks, replace = TRUE),
    deadline   = sample(c(100L, 150L, 200L), n_tasks, replace = TRUE),
    value_base = runif(n_tasks, 1, 2)
  )
}

# scalar util_hat in [0,1]; success_model expects scalar a, b.
mock_util_hat       <- 0.5
mock_success_model  <- function() init_success_model()


# ----- Test 1: contract — integrator_clear returns $tier_prices ---------------

test_that("integrator_clear returns a $tier_prices field", {
  env   <- make_test_env()
  ing   <- make_test_integrator()
  tasks <- make_test_tasks()
  blb   <- base_latency_for_bids(env)

  res <- integrator_clear(tasks, env, mock_util_hat, blb,
                          mock_success_model(), ing, iters = 5L)

  expect_true("tier_prices" %in% names(res),
              info = "integrator_clear must expose a per-tier price vector")
  expect_s3_class(res$tier_prices, "data.frame")  # tibble of (tier, price)
  expect_true(all(c("tier", "price") %in% names(res$tier_prices)))
  expect_equal(nrow(res$tier_prices), nrow(tier_capacities(env)))
  expect_true(all(res$tier_prices$price >= 0))
  expect_true(all(!is.na(res$tier_prices$price)))
})


# ----- Test 2: per-tier tâtonnement responds to load ------------------------

test_that("tier_prices vary with input load (not constant-by-construction)", {
  env <- make_test_env(load_level = "medium", N = 20L)
  blb <- base_latency_for_bids(env)

  # Low: tasks well below slice capacity → per-tier excess negative → prices
  # converge to the zero floor.
  # High: tasks well ABOVE slice capacity → per-tier excess strongly
  # positive → prices climb. Broadcasting a single scalar slice price would
  # yield identical price vectors regardless of demand contrast.
  res_low  <- integrator_clear(make_test_tasks(n_tasks = 4L,   seed = 1L),
                               env, mock_util_hat, blb,
                               mock_success_model(),
                               make_test_integrator(),
                               iters = 30L)
  res_high <- integrator_clear(make_test_tasks(n_tasks = 600L, seed = 2L),
                               env, mock_util_hat, blb,
                               mock_success_model(),
                               make_test_integrator(),
                               iters = 30L)

  diff_max <- max(abs(res_low$tier_prices$price - res_high$tier_prices$price))
  expect_gt(
    diff_max, 1e-2,
    label = "tier_prices(high) vs tier_prices(low) must differ — broadcast detector"
  )

  # Directional sanity: high-demand tier prices should exceed low-demand.
  expect_true(
    all(res_high$tier_prices$price >= res_low$tier_prices$price - 1e-6),
    info = "high-demand tier_prices must dominate low-demand tier_prices"
  )
})


# ----- Test 3: cross-tier variation under non-uniform demand ------------------

test_that("tier_prices vary across tiers when capacities & demand differ", {
  # Tree topology: device demand_weight = 2, edge = 2, cloud = 2; tier
  # capacities = (200, 300, 500) so per-tier utilisation differs by design.
  env  <- make_test_env(load_level = "high", N = 50L)
  blb  <- base_latency_for_bids(env)

  res <- integrator_clear(make_test_tasks(n_tasks = 300L, seed = 3L),
                          env, mock_util_hat, blb,
                          mock_success_model(),
                          make_test_integrator(),
                          iters = 30L)

  # With non-uniform per-tier capacity utilisation, the tâtonnement should
  # produce distinct tier prices. Broadcast code yielded an identical vector.
  expect_gt(
    stats::sd(res$tier_prices$price), 1e-4,
    label = "tier_prices must show cross-tier variation under non-uniform demand"
  )
})


# ----- Test 4: smoke regression — integrator_clear end-to-end -----------------

test_that("integrator_clear retains its existing return contract (smoke)", {
  env   <- make_test_env()
  ing   <- make_test_integrator()
  tasks <- make_test_tasks()
  blb   <- base_latency_for_bids(env)

  expect_no_error({
    res <- integrator_clear(tasks, env, mock_util_hat, blb,
                            mock_success_model(), ing, iters = 5L)
  })
  expect_true(all(c("allocation", "integrator", "unit_cost",
                    "slice_capacity") %in% names(res)))
})


# ----- Test 5: tier_prices PERSIST across calls -----------------------------
# Regression: per-round tier prices must persist across calls. Re-initializing
# tier prices on each call produces a deterministic per-round attractor that
# erases round-to-round variance.

test_that("integrator tier_prices persist across calls (cross-round variance)", {
  env <- make_test_env(load_level = "high", N = 60L)
  blb <- base_latency_for_bids(env)
  ing <- make_test_integrator()
  sm  <- mock_success_model()

  # Successive calls with stochastically varying task sets. With tier-price
  # persistence, integrator$tier_prices carries forward, so the recorded price
  # history shows ≥3 distinct values per tier in the saturated regime; without
  # persistence the prices collapse to a per-round attractor.
  set.seed(42)
  histories <- list()
  for (k in seq_len(8L)) {
    tasks <- make_test_tasks(n_tasks = 200L + k * 10L, seed = 100L + k)
    res   <- integrator_clear(tasks, env, mock_util_hat, blb, sm, ing,
                              iters = 10L)
    ing   <- res$integrator
    histories[[k]] <- res$tier_prices
  }

  # Per-tier price sequences across the 8 calls. The binding tier (the
  # one whose capacity slack matches efficiency-scaled demand) varies; the
  # non-binding tiers saturate at the price floor.
  tiers <- unique(histories[[1]]$tier)
  varied <- vapply(tiers, function(tr) {
    seq_t <- vapply(histories,
                    function(df) df$price[df$tier == tr][1],
                    numeric(1))
    length(unique(round(seq_t, 4)))
  }, integer(1))

  # At least one tier must show round-to-round variation (persistence active).
  expect_gt(max(varied), 1L,
            label = "at least one tier's price must vary across calls (persistence)")
  # Strong form: the binding tier should take ≥3 distinct values across 8
  # saturated calls — direct regression that tier prices persist across calls.
  expect_gte(max(varied), 3L,
             label = "binding tier should take ≥3 distinct values across 8 saturated calls")
})


# ----- Test 6: price floor is 0.01 (not 0), per P2A reference design ----------

test_that("per-tier prices respect a 0.01 floor (no log-floor pathology)", {
  env <- make_test_env(load_level = "medium", N = 5L)  # very slack — drives prices down
  blb <- base_latency_for_bids(env)
  ing <- make_test_integrator()
  sm  <- mock_success_model()

  # 12 calls with tiny task counts → strongly negative per-tier excess →
  # prices drift down. With floor 0, they'd reach 0 (log-floor pathology in σ);
  # with floor 0.01, they stop at 0.01.
  for (k in seq_len(12L)) {
    tasks <- make_test_tasks(n_tasks = 2L, seed = 200L + k)
    res   <- integrator_clear(tasks, env, mock_util_hat, blb, sm, ing,
                              iters = 15L)
    ing   <- res$integrator
  }

  final_prices <- res$tier_prices$price
  expect_true(all(final_prices >= 0.01 - 1e-9),
              info = "tier_prices must respect 0.01 floor (P2A reference)")
})
