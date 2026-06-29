# Unit tests for the agent-facing price-stability metric and the EMA-price
# recording. The headline metric is the scale-free
# coefficient of variation (CV = sd/mean) of the per-task cost the agent pays,
# undefined where there is no sustained price, recorded as the integrator's
# EMA-smoothed slice price (what the agent actually faces), not the raw clearing
# price. These tests pin that contract.

test_that("agent_price_volatility is the coefficient of variation (sd/mean)", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(agent_price_volatility(x, mean_floor = 0),
               stats::sd(x) / mean(x), tolerance = 1e-9)
})

test_that("CV is scale-invariant (fixes the bundle-sum vs slice-price scale confound)", {
  x <- c(0.40, 0.50, 0.60, 0.50, 0.45)
  expect_equal(agent_price_volatility(x),
               agent_price_volatility(10 * x), tolerance = 1e-9)
})

test_that("undefined (NA) where no sustained price exists (mean below floor)", {
  uncontended <- c(0.001, 0, 0.002, 0, 0.001)   # agent pays ~nothing
  expect_true(is.na(agent_price_volatility(uncontended, mean_floor = 0.01)))
})

test_that("NA for fewer than 3 finite observations", {
  expect_true(is.na(agent_price_volatility(c(0.5, 0.6))))
  expect_true(is.na(agent_price_volatility(c(0.5, NA, NaN))))
})

test_that("a steadier price series scores lower volatility than a noisier one at equal mean", {
  steady   <- rep(0.5, 5)                  # CV = 0
  volatile <- c(0.2, 0.8, 0.3, 0.7, 0.5)   # same mean 0.5, positive sd
  expect_lt(agent_price_volatility(steady, mean_floor = 0),
            agent_price_volatility(volatile, mean_floor = 0))
})

test_that("integrator_clear records the EMA-smoothed slice price as unit_cost (not raw sp)", {
  env   <- init_environment(build_dependency_graph("sp"), load_level = "high",
                            n_agents = 20L, graph_type = "sp")
  integ <- integrator_init(beta = 0.8, efficiency_factor = 0.75, eta = 0.15)
  blb   <- base_latency_for_bids(env)
  set.seed(11)
  tasks <- tibble::tibble(
    task_id    = sprintf("t%03d", seq_len(40L)),
    agent_id   = sample.int(5, 40L, replace = TRUE),
    deadline   = sample(c(100L, 150L, 200L), 40L, replace = TRUE),
    value_base = runif(40L, 1, 2)
  )
  ic <- integrator_clear(tasks, env, 0.5, blb, init_success_model(), integ, iters = 5L)
  # The recorded agent-facing price must be the posted EMA slice price.
  expect_equal(ic$unit_cost, ic$integrator$slice_price, tolerance = 1e-12)
})
