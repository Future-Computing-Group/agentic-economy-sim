# test-node-determinacy.R
# ---------------------------------------------------------------------------
# Are the prices a property of the round, or of where the walk started?
#
# On a gross-substitutes economy the equilibrium prices form a lattice and an
# ascending process from the floor reaches its minimum element, so the round's
# own tasks and capacities decide the terminal vector and the starting point
# does not. Where that hypothesis fails the terminal vector can depend on the
# start, and then a price is not a property of the market at all.
#
# One round is cleared from five starts and the spread across them is the
# response. The tests pin the instrument, not the outcome: a fixture whose
# starts land on two different vectors must show a non-zero spread, and one
# whose starts agree must show zero.
# ---------------------------------------------------------------------------

det_prices <- function(...) {
  tibble(tier = c("d", "e1", "l1"), price = c(...))
}


test_that("the spread across starts is the widest disagreement at any node", {
  same <- list(a = det_prices(0.40, 0.04, 0.04), b = det_prices(0.40, 0.04, 0.04))
  expect_equal(node_price_spread(same, reserve = 0.04), 0)

  # Two starts landing on two vectors: the response is the widest gap at any
  # node, in units of the floor the prices are anchored on.
  two <- list(a = det_prices(0.40, 0.04, 0.04), b = det_prices(0.40, 0.12, 0.04))
  expect_equal(node_price_spread(two, reserve = 0.04), 2)

  # Three starts, and the widest gap is between the extremes.
  three <- c(two, list(c = det_prices(0.44, 0.04, 0.04)))
  expect_equal(node_price_spread(three, reserve = 0.04), 2)
  expect_equal(node_price_spread(list(a = det_prices(1, 1, 1)), reserve = 0.04), 0)
})

test_that("admitted sets are compared as sets", {
  expect_true(node_sets_agree(list(c("a", "b"), c("b", "a"))))
  expect_false(node_sets_agree(list(c("a", "b"), c("a", "c"))))
  expect_true(node_sets_agree(list(character(0), character(0))))
  expect_false(node_sets_agree(list(c("a"), character(0))))
})

test_that("the five starts are the floor, twice it, the carried vector and two draws", {
  env    <- node_run_env("tree", "high", 90L, "uniform", "off")
  carried <- init_market_state(env)$prices
  starts <- node_price_starts(env, carried, seed = 3L)

  expect_equal(names(starts), c("floor", "twice_floor", "carried",
                                "random_1", "random_2"))
  expect_true(all(starts$floor$price == env$reserve_price))
  expect_true(all(starts$twice_floor$price == 2 * env$reserve_price))
  expect_identical(starts$carried, carried)
  # The draws live in the range the process itself occupies: a start at the
  # price cap could not be walked back from inside any iteration budget,
  # since the step down per iteration is bounded by the step size.
  for (nm in c("random_1", "random_2")) {
    expect_true(all(starts[[nm]]$price >= env$reserve_price))
    expect_true(all(starts[[nm]]$price <= 20 * env$reserve_price))
  }
  expect_false(isTRUE(all.equal(starts$random_1$price, starts$random_2$price)))
  # The draws are a function of the seed, so a branch is reproducible.
  expect_equal(node_price_starts(env, carried, seed = 3L), starts)
  expect_false(isTRUE(all.equal(
    node_price_starts(env, carried, seed = 4L)$random_1$price,
    starts$random_1$price)))
})


# ---- the driver ------------------------------------------------------------

test_that("each round is cleared from every start and the spread is reported", {
  rows <- node_determinacy_run("tree", seed = 1L, n_rounds = 3L, iters = 200L)

  expect_equal(rows$round, 1:3)
  expect_equal(unique(rows$n_starts), 5L)
  expect_true(all(rows$price_spread >= 0))
  expect_true(all(rows$admitted_max >= rows$admitted_min))
  expect_true(all(rows$welfare_max >= rows$welfare_min))
  # Where every start lands on one vector, the sets agree and the welfare
  # spread closes with it.
  agreed <- rows$price_spread == 0
  expect_true(all(rows$sets_agree[agreed]))
  expect_true(all(rows$welfare_max[agreed] == rows$welfare_min[agreed]))

  # The terminal vectors are kept, one per start, for the theory pass.
  expect_equal(nrow(rows$terminal_prices[[1]]), 5L * nrow(tier_capacities(
    node_run_env("tree", "high", 90L, "uniform", "off"))))
  expect_setequal(rows$terminal_prices[[1]]$start,
                  c("floor", "twice_floor", "carried", "random_1", "random_2"))
})

test_that("the round's demand stream is the pipeline's own", {
  # The block re-clears the pipeline's rounds; it does not invent a demand
  # process of its own, so the tasks it prices are the tasks that round had.
  rows <- node_determinacy_run("tree", seed = 1L, n_rounds = 4L, iters = 15L)
  ref  <- node_convergence_run("tree", seed = 1L, n_rounds = 4L, iters = 15L)
  expect_equal(rows$n_offered, ref$n_offered)
  # The carried start is the pipeline's own clearing, so it admits what the
  # pipeline's market arm admitted in that round.
  expect_equal(rows$admitted_carried, ref$admitted)
})

test_that("the sweep runs three instances over the pipeline's seeds", {
  g <- node_determinacy_grid(n_seeds = 10L)
  expect_setequal(g$graph_type, c("tree", "sp", "entangled"))
  # Three instances, two price processes, ten seeds.
  expect_equal(nrow(g), 3L * 2L * 10L)
})

test_that("the pipeline carries the determinacy block as its own targets", {
  src <- paste(readLines(here::here("_targets.R")), collapse = "\n")
  for (nm in c("node_exp6_determinacy_grid", "node_exp6_determinacy",
               "node_exp6_determinacy_summary")) {
    expect_true(grepl(paste0("tar_target\\(\\s*", nm, "[,\\s]"), src), info = nm)
  }
})
