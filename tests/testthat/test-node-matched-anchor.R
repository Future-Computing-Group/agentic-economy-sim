# test-node-matched-anchor.R
# ---------------------------------------------------------------------------
# The posted price's dose, and reading the cross-instance ordering at one.
#
# The anchor is the leaf's own path at the per-node reserve, so an instance
# whose leaves pass through five nodes is dosed five thirds of one whose leaves
# pass through three, on an identical value distribution. Any ordering of
# instances under the posted price is then partly an ordering of doses. The
# matched level posts the reference instance's anchor everywhere, so the
# ordering can be read at one dose, and the dose itself is a reported column.
# ---------------------------------------------------------------------------

anchor_run <- function(a, mechanism = "posted_price", k = 2, rounds = 5L) {
  node_run_single(a, "high", N = node_agents()[[a]], seed = 1L,
                  n_rounds = rounds, mechanism = mechanism, p_post_k = k,
                  lambda_l_default = node_lambda_l())
}


test_that("the dose a task faces at marginal cost is a column of every row", {
  rows <- dplyr::bind_rows(lapply(c("tree", "sp", "entangled"), anchor_run))
  expect_equal(rows$posted_anchor_k1, c(0.24, 0.40, 0.24))

  # The same quantity the instance table reports as the path reserve cost:
  # one definition, not two that drift.
  d <- node_instance_diagnostics(mixes = "uniform")
  expect_equal(rows$posted_anchor_k1,
               d$leaf_reserve_cost[match(rows$graph_type, d$graph_type)])
  # It is the level at marginal cost, whatever markup the arm ran at.
  expect_equal(anchor_run("sp", k = 4)$posted_anchor_k1, 0.40)
})

test_that("on the reference instance the matched level is the posted level", {
  own <- anchor_run("tree", "posted_price")
  mat <- anchor_run("tree", "posted_price_matched")
  expect_equal(dplyr::select(mat, -mechanism), dplyr::select(own, -mechanism),
               tolerance = 1e-12)
})

test_that("the matched level posts the reference dose on every instance", {
  own <- anchor_run("sp", "posted_price")
  mat <- anchor_run("sp", "posted_price_matched")

  # Five ancestors against the reference's three: its own price is five thirds
  # of the matched one, which is the whole of the dose gap.
  expect_equal(mat$mean_unit_cost, 2 * 0.24)
  expect_equal(own$mean_unit_cost, 2 * 0.40)
  expect_gt(mat$tokens_admitted, own$tokens_admitted)

  # The instance, its arrivals and both references are untouched by the dose.
  expect_equal(mat$ceiling_zero_queue, own$ceiling_zero_queue)
  expect_equal(mat$optimum_ex_post, own$optimum_ex_post)
})

test_that("the matched level runs at the markups the ordering is reported at", {
  g <- node_exp6_mechanism_grid(n_seeds = 10L)
  expect_setequal(unique(g$p_post_k[g$mechanism == "posted_price_matched"]),
                  c(1, 2, 4))
  # Three markups, three instances, two loads, two architectures, two
  # congestion levels.
  expect_equal(sum(g$mechanism == "posted_price_matched"),
               3L * 3L * 2L * 2L * 2L * 10L)
  expect_false("posted_price_matched" %in% exp6_mechanism_grid(n_seeds = 2L)$mechanism)
})
