# Is the advertised region the true one, or strictly inside it.
#
# The contracted interface advertises a region the market clears against;
# delivery is against the true instance. Their relation is structural: the
# leaf-level linear programme max w.x over each region, on a fixed grid of
# weights, agrees on every weight where the two regions have the same
# projection onto the leaves, and falls short somewhere where the advertised
# one lies strictly inside.

region_envs <- function(g, iface) list(
  adv  = node_run_env(g, "high", node_agents()[[g]], "uniform", iface),
  true = node_run_env(g, "high", node_agents()[[g]], "uniform", "off"))

test_that("the contracted crossing and tree instances advertise an inner region", {
  for (g in c("tree", "entangled")) {
    e <- region_envs(g, "inner")
    r <- node_region_relation(e$adv, e$true)
    expect_equal(r$relation, "inner", info = g)
    expect_equal(r$max_gap, 0.5, info = g)
    expect_equal(unname(r$weight), c(1, 0, 1, 0), info = g)
  }
})

test_that("the contracted series-parallel instance advertises its exact region", {
  e <- region_envs("sp", "inner")
  r <- node_region_relation(e$adv, e$true)
  expect_equal(r$relation, "exact")
  expect_equal(r$max_gap, 0)
})

test_that("an uncontracted arm advertises the true region", {
  for (g in c("tree", "sp", "entangled")) {
    e <- region_envs(g, "off")
    expect_equal(node_region_relation(e$adv, e$true)$relation, "exact", info = g)
  }
})

test_that("the relation is drawn without moving the run's random stream", {
  e <- region_envs("tree", "inner")
  set.seed(9); before <- .Random.seed
  node_region_relation(e$adv, e$true)
  expect_identical(.Random.seed, before)
})

test_that("the driver records the relation once per run", {
  r <- node_run_single("tree", "high", N = 90L, seed = 1L, n_rounds = 4L,
                       architecture = "hybrid_ema")
  expect_equal(r$region_relation, "inner")
  expect_equal(r$region_max_gap, 0.5)
  n <- node_run_single("tree", "high", N = 90L, seed = 1L, n_rounds = 4L)
  expect_equal(n$region_relation, "exact")
  expect_equal(n$region_max_gap, 0)
})

test_that("an over-committing interface is not reported as exact", {
  # The maxflow scalar advertises more than the true region carries in its
  # worst mix, so the advertised optimum exceeds the true one somewhere.
  e <- region_envs("entangled", "maxflow")
  r <- node_region_relation(e$adv, e$true)
  expect_true(r$relation %in% c("outer", "crossing"))
  expect_lt(r$min_gap, 0)
})
