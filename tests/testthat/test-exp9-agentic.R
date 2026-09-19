# Tests for Exp.9 — the REAL agentic workload.
# build_dependency_graph("agentic") encodes the measured DAG + demand profile
# of a real multi-step LLM tool-use agent (provenance: agentic/). The framework's
# predictions (polymatroidal SP structure -> stable + DSIC) must run + hold on it.
#
# The environment now consumes the WHOLE measurement, not half of it: per-tier
# base latencies are the profile's measured stage latencies, and the deadlines
# and the value-decay rate are rescaled to the critical path those latencies
# imply. The two behavioural tests below are therefore run at that environment's
# own constants, and their thresholds are re-derived against it.

.agentic_env_args <- list(deadlines = agentic_deadlines(),
                          lambda_l_default = agentic_lambda_l())

test_that("the agentic graph is series-parallel with the measured demand profile", {
  g <- build_dependency_graph("agentic")
  expect_setequal(g$nodes$node, c("plan", "tool0", "tool1", "aggregate"))
  # SP structure: plan fans out to two tools, which converge on aggregate.
  expect_true(all(c("plan","tool0") %in% g$edges$from))
  expect_true(all(g$edges$to[g$edges$from %in% c("tool0","tool1")] == "aggregate"))
  # The demand weights are READ from the measured profile rather than typed in
  # here or in the graph, so the two cannot drift apart: a regenerated profile
  # either reaches the simulator or fails this test.
  tiers <- jsonlite::fromJSON(here::here("agentic", "agentic_profile.json"))$tiers
  measured <- vapply(c("device", "edge", "cloud"),
                     function(tr) tiers[[tr]]$demand_weight, numeric(1))
  dw <- setNames(g$demand_weights$demand_weight, g$demand_weights$tier)
  expect_equal(dw[c("device", "edge", "cloud")], measured)
  # A task's two tool calls both land on the edge tier, so edge demand per task
  # is their sum and outweighs the single planning call on the device tier.
  expect_true(dw["edge"] > dw["device"])
  expect_true(dw["cloud"] > dw["edge"])      # aggregate heavier than either tool
})

test_that("the agentic environment carries the measured base latencies", {
  # Read from the profile, not typed in: a regenerated profile must fail this
  # test until the environment is re-derived from it.
  tiers <- jsonlite::fromJSON(here::here("agentic", "agentic_profile.json"))$tiers
  measured <- vapply(c("device", "edge", "cloud"),
                     function(tr) tiers[[tr]]$mean_latency_ms, numeric(1))
  env <- init_environment(build_dependency_graph("agentic"), "high",
                          n_agents = 10L, graph_type = "agentic")
  got <- setNames(env$base_latency$base_ms, env$base_latency$tier)
  expect_equal(got[c("device", "edge", "cloud")], measured)
  # Every other environment keeps the nominal 5 / 15 / 50.
  sp <- init_environment(build_dependency_graph("sp"), "high",
                         n_agents = 10L, graph_type = "sp")
  expect_equal(sp$base_latency$base_ms, c(5, 15, 50))
})

test_that("the agentic deadline rule rounds the multipliers times the critical path", {
  # deadlines = round_to_100(c(1.25, 1.5, 1.75) * D_bar), D_bar the zero-queue
  # critical path at the measured latencies. The expectation is computed from
  # the profile rather than typed as three integers: the profile is a
  # recording, so a re-run moves D_bar and a literal would pin the recording
  # instead of the rule it is read through.
  d_bar <- critical_path_ms(build_dependency_graph("agentic"),
                            agentic_base_latency())
  expect_equal(agentic_deadlines(),
               as.integer(round(c(1.25, 1.5, 1.75) * d_bar / 100) * 100))
  # D_bar pinned to the measurement, not to a rounded literal: the agentic DAG
  # runs its two edge tools in parallel, so its critical path visits exactly one
  # tier of each kind and is the sum of the three measured stage latencies.
  # A rounded literal at relative tolerance would tolerate a drift of
  # several ms in the measurement this is read against.
  expect_equal(critical_path_ms(build_dependency_graph("agentic"),
                                agentic_base_latency()),
               sum(agentic_base_latency()))
})

test_that("the agentic value-decay rate holds the value surviving the critical path", {
  # Same rescaling rule as the deadlines, same invariant: the share of a task's
  # value that survives its own environment's zero-queue critical path. Without
  # it the nominal 0.005 per ms leaves a millionth of the value of a task
  # whose pipeline runs for seconds, and the agentic market clears nothing
  # at all.
  d_agentic <- critical_path_ms(build_dependency_graph("agentic"),
                                agentic_base_latency())
  d_nominal <- critical_path_ms(build_dependency_graph("sp"),
                                c(device = 5, edge = 15, cloud = 50))
  expect_equal(exp(-agentic_lambda_l() * d_agentic),
               exp(-0.005 * d_nominal), tolerance = 1e-9)
  expect_lt(exp(-0.005 * d_agentic), 1e-6)   # what the nominal rate would leave
})

test_that("the agentic drop rate responds to load", {
  # Liveness check on the re-derived environment, not a performance claim: under
  # the old deadline set the drop rate was dominated by capacity rationing and
  # could not distinguish a deadline miss from a task never admitted. Measured
  # on 3 seeds, naive arm: 0.17-0.19 at medium against 0.49-0.53 at high.
  args <- c(list("naive", "agentic", N = 200L, seed = 1L, n_rounds = 40L),
            .agentic_env_args)
  med  <- do.call(exp4_run_single, c(args, list(load_level = "medium")))
  high <- do.call(exp4_run_single, c(args, list(load_level = "high")))
  expect_gt(high$drop_rate, med$drop_rate + 0.1)
  # Both cells actually trade, so the comparison is between live markets.
  expect_gt(med$clearing_fraction, 0.3)
  expect_gt(high$clearing_fraction, 0.3)
})

test_that("the integrator reduces price volatility on the real agentic workload (contended regime)", {
  # Price stability is a CONTENDED-regime property: the integrator absorbs
  # volatility only where the naive multi-tier market is itself volatile. The
  # measured agentic workload is light (Sigma_demand 4.84, against 12 for
  # entangled), so it contends only at scale; at N = 200 (high load) the
  # bottleneck tier's offered load is 1.81, the naive market is genuinely
  # volatile and the integrator absorbs it. Measured on 3 seeds at the
  # environment's own constants: naive 0.1907 to 0.2495, hybrid 0.1451 to 0.1457.
  nv <- do.call(exp4_run_single,
                c(list("naive", "agentic", "high", N = 200L, seed = 1L,
                       n_rounds = 40L), .agentic_env_args))
  hy <- do.call(exp4_run_single,
                c(list("hybrid", "agentic", "high", N = 200L, seed = 1L,
                       n_rounds = 40L), .agentic_env_args))
  expect_true(is.finite(nv$mean_price_volatility))
  expect_true(is.finite(hy$mean_price_volatility))
  expect_gt(nv$mean_price_volatility, 0.15)                       # naive genuinely volatile here
  expect_lt(hy$mean_price_volatility, nv$mean_price_volatility)   # integrator absorbs it
})

test_that("DSIC holds on the real agentic workload (exp7a, binding regime)", {
  # Agentic demand is lighter than sp, so a smaller cap is needed to bind.
  # Mean VCG payment on the re-derived environment, 3 seeds: 2.55 to 2.74.
  r <- exp7a_run_single("agentic", "high", N = 8L, seed = 2L, cap = 12,
                        shades = c(0.5, 0.7, 0.9, 1.0, 1.1, 1.3), n_rounds = 15L,
                        deadlines = agentic_deadlines(),
                        lambda_l_default = agentic_lambda_l())
  expect_gt(r$mean_payment, 1e-6)          # binding -> non-vacuous
  reg <- vapply(c(0.5,0.7,0.9,1.0,1.1,1.3),
                function(a) r[[sprintf("regret_%g", a)]], numeric(1))
  expect_equal(reg[4], 0, tolerance = 1e-9)        # truthful = 0
  expect_true(all(reg >= -1e-6))                   # truthful is best response
})
