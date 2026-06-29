# Tests for Exp.9 — the REAL agentic workload.
# build_dependency_graph("agentic") encodes the measured DAG + demand profile
# of a real multi-step LLM tool-use agent (provenance: agentic/). The framework's
# predictions (polymatroidal SP structure -> stable + DSIC) must run + hold on it.

test_that("the agentic graph is series-parallel with the measured demand profile", {
  g <- build_dependency_graph("agentic")
  expect_setequal(g$nodes$node, c("plan", "tool0", "tool1", "aggregate"))
  # SP structure: plan fans out to two tools, which converge on aggregate.
  expect_true(all(c("plan","tool0") %in% g$edges$from))
  expect_true(all(g$edges$to[g$edges$from %in% c("tool0","tool1")] == "aggregate"))
  # Measured demand weights (cloud/aggregate heaviest), device/edge/cloud.
  dw <- setNames(g$demand_weights$demand_weight, g$demand_weights$tier)
  expect_equal(unname(dw["cloud"]), 2.25, tolerance = 1e-9)
  expect_true(dw["cloud"] > dw["edge"])      # aggregate heavier than tools
})

test_that("the integrator reduces price volatility on the real agentic workload (contended regime)", {
  # Price stability is a CONTENDED-regime property: the integrator absorbs
  # volatility only where the naive multi-tier market is itself volatile. The
  # measured agentic workload is light (Sigma_demand 4.36), so it contends only
  # at scale; at N = 200 (high load) the naive market is genuinely volatile and
  # the integrator absorbs it. In slack regimes neither market is volatile and
  # the reduction is vacuous, so we test where there is volatility to reduce.
  nv <- exp4_run_single("naive",  "agentic", "high", N = 200L, seed = 1L, n_rounds = 40L)
  hy <- exp4_run_single("hybrid", "agentic", "high", N = 200L, seed = 1L, n_rounds = 40L)
  expect_true(is.finite(nv$mean_price_volatility))
  expect_true(is.finite(hy$mean_price_volatility))
  expect_gt(nv$mean_price_volatility, 0.05)                       # naive genuinely volatile here
  expect_lt(hy$mean_price_volatility, nv$mean_price_volatility)   # integrator absorbs it
})

test_that("DSIC holds on the real agentic workload (exp7a, binding regime)", {
  # Agentic demand is lighter than sp, so a smaller cap is needed to bind.
  r <- exp7a_run_single("agentic", "high", N = 8L, seed = 2L, cap = 12,
                        shades = c(0.5, 0.7, 0.9, 1.0, 1.1, 1.3), n_rounds = 15L)
  expect_gt(r$mean_payment, 1e-6)          # binding -> non-vacuous
  reg <- vapply(c(0.5,0.7,0.9,1.0,1.1,1.3),
                function(a) r[[sprintf("regret_%g", a)]], numeric(1))
  expect_equal(reg[4], 0, tolerance = 1e-9)        # truthful = 0
  expect_true(all(reg >= -1e-6))                   # truthful is best response
})
