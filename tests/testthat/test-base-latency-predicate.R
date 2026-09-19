# Tests for base_latency_for_bids() — the bid-time base-latency estimate.
#
# The function used to test env$per_tier for a column named `latency`. per_tier
# joins base_latency, whose column is `base_ms`, so the predicate was never true
# and every experiment on every topology bid against the hard-coded constant 50.
# The estimate is now the zero-queue critical path of the environment's DAG:
# critical_path_ms(env$graph, base_ms by tier). These tests pin that it is
# topology-aware, that it reads the column that exists, and that a per_tier
# without base_ms fails loudly instead of silently falling back.

.blb_env <- function(gt, load = "medium", N = 20L) {
  init_environment(build_dependency_graph(gt), load_level = load,
                   n_agents = N, graph_type = gt)
}

# Zero-queue critical paths, hand-computed from each topology's longest tier
# sequence. The four default environments are at the nominal base latencies
# (device 5, edge 15, cloud 50):
#   linear     in -> pre -> edge_inf -> cloud_inf -> post   5+5+15+50+50 = 125
#   tree       in1 -> pre1 -> edge_inf -> cloud_inf -> post 5+15+15+50+50 = 135
#   sp         in1 -> pre1 -> edge_inf1 -> cloud_inf1 -> post   ditto     = 135
#   entangled  in2 -> pre2 -> edge_inf2 -> feature -> cloud_inf -> post
#                                                      5+5+15+15+50+50    = 140
# The agentic environment is at the MEASURED per-stage latencies, so its path
# plan -> tool0 -> aggregate is one device plus one edge plus one cloud stage of
# the real workload. Summed here from the profile rather than typed in, so a
# regenerated profile fails this test until the environment is re-derived.
.agentic_dbar <- local({
  tiers <- jsonlite::fromJSON(here::here("agentic", "agentic_profile.json"))$tiers
  sum(vapply(c("device", "edge", "cloud"),
             function(tr) tiers[[tr]]$mean_latency_ms, numeric(1)))
})

.expected_zero_queue <- c(linear = 125, tree = 135, sp = 135,
                          entangled = 140, agentic = .agentic_dbar)


test_that("base_latency_for_bids returns the zero-queue critical path, not 50", {
  env <- .blb_env("sp")
  expect_equal(base_latency_for_bids(env), 135)
  expect_false(isTRUE(all.equal(base_latency_for_bids(env), 50)))
})

test_that("base_latency_for_bids is topology-aware, not one constant", {
  got <- vapply(names(.expected_zero_queue),
                function(gt) base_latency_for_bids(.blb_env(gt)), numeric(1))
  expect_equal(got, .expected_zero_queue)
  # A constant — 50, or any per-tier mean, which is 23.33 for every topology
  # here — cannot separate these topologies. The path length must.
  expect_gt(length(unique(got)), 1)
})

test_that("base_latency_for_bids reads base_ms even when a latency column exists", {
  # The exact dead branch: per_tier carrying BOTH the column the old predicate
  # looked for and the one that holds the data. The old code returned
  # mean(latency); the estimate must come from base_ms via the DAG.
  env <- .blb_env("sp")
  env$per_tier$latency <- c(999, 999, 999)
  expect_equal(base_latency_for_bids(env), 135)
})

test_that("base_latency_for_bids tracks the per-tier base latencies it is given", {
  env <- .blb_env("sp")
  env$per_tier$base_ms <- c(cloud = 3, device = 1, edge = 2)[env$per_tier$tier]
  expect_equal(base_latency_for_bids(env), 11)   # 1 + 2 + 2 + 3 + 3 on sp
})

test_that("base_latency_for_bids fails loudly if per_tier has no base_ms", {
  # Regression guard for the dead-predicate class itself: a per_tier whose
  # latency column is renamed must error, not silently fall back to a constant.
  env <- .blb_env("sp")
  env$per_tier <- dplyr::rename(env$per_tier, latency = base_ms)
  expect_error(base_latency_for_bids(env), "base_ms")
})

test_that("base_latency_for_bids falls back to 50 without a per_tier", {
  expect_equal(base_latency_for_bids(list()), 50)
  expect_equal(base_latency_for_bids(list(graph = build_dependency_graph("sp"))), 50)
})
