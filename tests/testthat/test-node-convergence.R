# test-node-convergence.R
# ---------------------------------------------------------------------------
# Whether the price process finds a clearing vector, and in how many steps.
#
# The residual excess demand at the pipeline's iteration budget cannot answer
# that on its own: it says how far a truncated walk got, and zero over-demand
# is necessary for a clearing price vector, not sufficient. A node with slack
# capacity whose price sits above its marginal cost is an overshoot, not an
# equilibrium, and only complementary slackness separates the two.
#
# This block runs the market arm at four budgets with the state carried across
# rounds as the pipeline carries it, and checks both conditions per round. It
# adds a driver and targets of its own: the arms already built must not move.
# ---------------------------------------------------------------------------

conv_cap <- c(d = 100, e1 = 50)


test_that("the equilibrium check reads clearing and complementary slackness", {
  ok <- function(demand, prices) {
    node_market_equilibrium(demand, conv_cap, prices, reserve = 0.04)
  }
  # Every node exactly at capacity: the price vector clears the round.
  expect_true(ok(c(d = 100, e1 = 50), c(d = 0.5, e1 = 0.3)))
  # A node with capacity to spare, priced at marginal cost: nobody wants the
  # rest at what it costs, which is an equilibrium too.
  expect_true(ok(c(d = 100, e1 = 20), c(d = 0.5, e1 = 0.04)))
  # The same slack node priced above marginal cost: the walk overshot and
  # rationed demand a lower price would have served.
  expect_false(ok(c(d = 100, e1 = 20), c(d = 0.5, e1 = 0.30)))
  # Demand above capacity is not an equilibrium at any price.
  expect_false(ok(c(d = 101, e1 = 50), c(d = 0.5, e1 = 0.3)))

  # The tolerance is a numerical one, not a margin.
  expect_true(ok(c(d = 100 + 1e-10, e1 = 50), c(d = 0.5, e1 = 0.3)))
  expect_false(ok(c(d = 100 + 1e-6, e1 = 50), c(d = 0.5, e1 = 0.3)))
})

test_that("the round's demand is the demand backing the terminal prices", {
  env   <- node_run_env("tree", "high", 90L, "uniform", "off")
  tasks <- node_round_tasks(env, init_agents(90L), t = 1L, seed = 1L,
                            deadlines = c(500L, 750L, 1000L))
  res <- clear_multitier_market(tasks, env, util_hat = 0.1,
                                base_latency = base_latency_for_bids(env),
                                market_state = init_market_state(env),
                                lambda_l_default = node_lambda_l())
  dem <- node_round_demand(tasks, env, res$surplus)
  cap <- tier_capacities(env)
  # The kernel's own residual is that demand against capacity, so the two
  # readings of the same round agree.
  expect_equal(sum(pmax(unname(dem[cap$tier]) - cap$capacity, 0)) / sum(cap$capacity),
               res$clearing$resid_excess)
  expect_equal(node_round_demand(tasks[0, ], env, numeric(0)),
               setNames(rep(0, nrow(cap)), cap$tier))
})


# ---- the driver ------------------------------------------------------------

test_that("the iteration budget reaches the market and is recorded", {
  short <- node_convergence_run("tree", seed = 1L, n_rounds = 2L, iters = 15L)
  long  <- node_convergence_run("tree", seed = 1L, n_rounds = 2L, iters = 200L)

  expect_equal(short$iters, rep(15L, 2))
  expect_equal(long$iters, rep(200L, 2))
  expect_equal(short$round, 1:2)
  # A longer walk admits a different set. It does NOT leave a smaller residual
  # round by round: the walk overshoots and comes back, and the price it
  # carries into the next round is itself a function of the budget, so the
  # runs diverge. That is the measurement, so the test pins only that the
  # budget reaches the market and that the run is a function of it.
  expect_false(isTRUE(all.equal(long$admitted, short$admitted)))
  expect_equal(node_convergence_run("tree", seed = 1L, n_rounds = 2L,
                                    iters = 15L), short)
})

test_that("the market state is carried across rounds as the driver carries it", {
  # The load-bearing equivalence: this block is the pipeline's market arm with
  # the iteration budget exposed, not a second market with its own dynamics.
  rows <- node_convergence_run("tree", seed = 1L, n_rounds = 10L, iters = 15L)
  ref  <- node_run_single("tree", "high", N = 90L, seed = 1L, n_rounds = 10L,
                          mechanism = "market", lambda_l_default = node_lambda_l())
  expect_equal(mean(rows$admitted), ref$tokens_admitted)
  expect_equal(mean(post_burn_in(rows$resid_excess)), ref$resid_excess)
})

test_that("a round that fails the check keeps the demand profile it failed on", {
  rows <- node_convergence_run("entangled", seed = 1L, n_rounds = 3L,
                               iters = 1000L, save_profile = TRUE)
  expect_true("demand_profile" %in% names(rows))
  saved <- !vapply(rows$demand_profile, is.null, logical(1))
  expect_equal(saved, !rows$equilibrium_ok)
  if (any(saved)) {
    prof <- rows$demand_profile[saved][[1]]
    expect_true(all(c("tier", "demand", "capacity", "price") %in% names(prof)))
  }
  # Off that switch nothing is kept, whatever the round did.
  bare <- node_convergence_run("entangled", seed = 1L, n_rounds = 2L, iters = 1000L)
  expect_true(all(vapply(bare$demand_profile, is.null, logical(1))))
})


# ---- the grid, the summary and the pipeline --------------------------------

test_that("the sweep runs three instances over four budgets", {
  g <- node_convergence_grid(n_seeds = 10L)
  expect_setequal(g$iters, c(15L, 50L, 200L, 1000L))
  expect_setequal(g$graph_type, c("tree", "sp", "entangled"))
  expect_equal(nrow(g), 3L * 10L * 4L)
  # The profiles are kept where non-existence would appear: the crossing
  # instance, at the budget beyond which nothing more will converge.
  expect_true(all(g$save_profile ==
                    (g$graph_type == "entangled" & g$iters == 1000L)))
})

test_that("the summary reports the converged fraction per instance and budget", {
  rows <- tidyr::expand_grid(graph_type = c("tree", "entangled"),
                             iters = c(15L, 200L), round = 1:4) %>%
    dplyr::mutate(seed = 1L, resid_excess = ifelse(iters == 15L, 0.1, 0),
                  admitted = 50,
                  equilibrium_ok = iters == 200L | round > 3)
  s <- node_convergence_summary(rows)
  expect_equal(nrow(s), 4L)
  expect_equal(s$converged_fraction[s$iters == 15L], c(0.25, 0.25))
  expect_equal(s$converged_fraction[s$iters == 200L], c(1, 1))
  expect_equal(s$n_rounds, rep(4L, 4))
})

test_that("the pipeline carries the convergence block as its own targets", {
  src <- paste(readLines(here::here("_targets.R")), collapse = "\n")
  for (nm in c("node_exp6_convergence_grid", "node_exp6_convergence",
               "node_exp6_convergence_summary")) {
    expect_true(grepl(paste0("tar_target\\(\\s*", nm, "[,\\s]"), src), info = nm)
  }
})
