# test-welfare-under-caps.R
# ---------------------------------------------------------------------------
# The congestion penalty in compute_welfare() charges each resource a demand of
# (served count) x (the environment's MIX-AVERAGE per-task weight). That is the
# advertised quantity, not the realised one, and on a node-indexed environment
# it is charged to nodes the served tasks never touched. Under a coordinate
# governance cap the charge lands on a leaf whose capacity is zero, the
# utilisation divides to Inf, and welfare -- and efficiency with it -- goes to
# -Inf on every round of the arm.
#
# execute_allocation() already takes the realised per-node demand of the
# admitted mix (R/sim_helpers.R) for exactly this reason; the welfare penalty is
# the site that was not carried over.
# ---------------------------------------------------------------------------

cap_env <- function(a = "tree", mix = "skewed") {
  node_env(a, "high", node_agents()[[a]], mix)
}

# A served round that touches only the leaves a locality cap leaves open.
served_on <- function(env, leaves, n = 40L, seed = 1L) {
  set.seed(seed)
  tibble(task_id = sprintf("t%02d", seq_len(n)), agent_id = rep_len(1:8, n),
         deadline = 1000, value_base = runif(n, 1, 2),
         recipe = rep_len(leaves, n), latency = 100, success = TRUE)
}


test_that("welfare stays finite when a governance cap closes a leaf", {
  env    <- cap_env()
  capped <- apply_leaf_caps(env, c(l3 = 0, l4 = 0))   # the locality determinant
  served <- served_on(capped, c("l1", "l2"))

  expect_equal(capped$capacities$capacity[capped$capacities$tier == "l3"], 0)
  w <- compute_welfare(served, capped, prices_df = NULL, cong_cost = TRUE)
  expect_true(is.finite(w))
  expect_gt(w, 0)
})

test_that("the congestion penalty charges a node its realised demand, not the nominal mix", {
  # No served task touches l3 or l4, so closing them cannot change what the
  # round congests: welfare under the cap must equal welfare without it.
  env    <- cap_env()
  served <- served_on(env, c("l1", "l2"))

  expect_equal(compute_welfare(served, apply_leaf_caps(env, c(l3 = 0, l4 = 0)),
                               prices_df = NULL, cong_cost = TRUE),
               compute_welfare(served, env, prices_df = NULL, cong_cost = TRUE))
})
