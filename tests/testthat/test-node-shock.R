# test-node-shock.R
# ---------------------------------------------------------------------------
# What the market does after something breaks.
#
# Two shocks: half the binding edge node's capacity for fifty rounds, and a
# burst of arrivals at twice the rate for ten. The responses are how long the
# round takes to settle again, how far the price overshoots on the way, and
# what the run lost against the same seed with no shock at all.
#
# The contracted arm advertises a scalar it derived before the shock and does
# not re-derive it, so its slice transmits the loss to delivery rather than to
# admission; that difference between the arms is the measurement.
# ---------------------------------------------------------------------------

shock_env <- function(a = "tree") node_run_env(a, "high", node_agents()[[a]], "uniform", "off")


test_that("a node's capacity is scaled in both places the environment keeps it", {
  env <- shock_env()
  half <- node_scale_node_capacity(env, "e1", 0.5)

  cap  <- function(e, n) e$capacities$capacity[e$capacities$tier == n]
  tier <- function(e, n) e$per_tier$capacity[e$per_tier$tier == n]
  expect_equal(cap(half, "e1"), 0.5 * cap(env, "e1"))
  expect_equal(tier(half, "e1"), 0.5 * tier(env, "e1"))
  # Admission packs against one copy and execution queues against the other,
  # so a shock that moved only one would bind in one place and not the other.
  expect_equal(cap(half, "e1"), tier(half, "e1"))
  expect_equal(cap(half, "e2"), cap(env, "e2"))
  expect_equal(node_scale_node_capacity(env, "e1", 1), env)
})

test_that("the binding edge node is the edge node its own demand fills first", {
  for (a in c("tree", "sp", "entangled")) {
    env  <- shock_env(a)
    node <- node_binding_edge_node(env)
    expect_true(node %in% env$capacities$tier, info = a)
    expect_equal(unname(env$spec$nodes$phys[env$spec$nodes$node == node]),
                 "edge", info = a)
  }
  # On the tree the leaves under the first edge node carry half the arrivals
  # against its own capacity, which is what the instance table reports as its
  # binding pair with the root.
  expect_equal(node_binding_edge_node(shock_env("tree")), "e1")
})


# ---- the runs --------------------------------------------------------------

shock_rows <- function(...) {
  node_shock_run("tree", seed = 1L, n_rounds = 12L, shock_start = 5L,
                 capacity_end = 9L, burst_end = 7L, ...)
}

test_that("the capacity shock binds while it lasts and lifts when it ends", {
  rows <- shock_rows(shock = "capacity")

  expect_equal(rows$round, 1:12)
  expect_equal(rows$in_shock, rows$round >= 5 & rows$round < 9)
  expect_true(all(c("admitted", "welfare", "welfare_control", "resid_excess",
                    "equilibrium_ok", "unit_cost") %in% names(rows)))
  # Half the binding node is half the tokens it can carry, so the arm admits
  # less while the shock is on than the same seed does without it.
  expect_lt(mean(rows$admitted[rows$in_shock]),
            mean(rows$admitted_control[rows$in_shock]))
  # Before the shock the two runs are the same run.
  expect_equal(rows$admitted[rows$round < 5], rows$admitted_control[rows$round < 5])
  expect_equal(rows$welfare[rows$round < 5], rows$welfare_control[rows$round < 5])
})

test_that("the burst doubles the arrivals and only inside its window", {
  rows <- shock_rows(shock = "burst")

  expect_equal(rows$in_shock, rows$round >= 5 & rows$round < 7)
  expect_gt(mean(rows$n_offered[rows$in_shock]),
            1.5 * mean(rows$n_offered[!rows$in_shock]))
  # Outside the window the shocked run offers what the control offers.
  expect_equal(rows$n_offered[rows$round < 5],
               rows$n_offered_control[rows$round < 5])
})

test_that("the contracted arm runs the shock through its own advertised region", {
  rows <- node_shock_run("tree", architecture = "hybrid_noema", shock = "capacity",
                         seed = 1L, n_rounds = 8L, shock_start = 4L,
                         capacity_end = 7L)
  expect_equal(unique(rows$architecture), "hybrid_noema")
  expect_equal(nrow(rows), 8L)
  expect_true(all(is.finite(rows$welfare)))
})


# ---- the responses ---------------------------------------------------------

test_that("re-settling is the first round that stays settled for five", {
  base <- tibble(graph_type = "tree", architecture = "naive", shock = "capacity",
                 seed = 1L, shock_start = 5L, round = 1:20,
                 in_shock = round >= 5 & round < 10,
                 admitted = 50, admitted_control = 50, n_offered = 100,
                 n_offered_control = 100, unit_cost = 1, welfare = 10,
                 welfare_control = 12, resid_excess = 0)
  # A single settled round inside a run of unsettled ones is not a recovery.
  flick <- base %>% mutate(equilibrium_ok = round %in% c(11, 14:20))
  s <- node_shock_summary(flick)
  expect_equal(s$rounds_to_resettle, 14 - 5)
  # Nothing that stays settled: the response is missing, not optimistic.
  never <- base %>% mutate(equilibrium_ok = round %% 2 == 0)
  expect_true(is.na(node_shock_summary(never)$rounds_to_resettle))
  # The welfare lost is measured against the same seed with no shock, from
  # the round the shock lands on.
  expect_equal(s$welfare_lost, sum(base$welfare_control[base$round >= 5] -
                                     base$welfare[base$round >= 5]))
  expect_equal(s$n_rounds, 20L)
})

test_that("the overshoot is the peak price after the shock over what it settles at", {
  rows <- tibble(graph_type = "tree", architecture = "naive", shock = "capacity",
                 seed = 1L, shock_start = 5L, round = 1:20,
                 in_shock = round >= 5 & round < 10,
                 admitted = 50, admitted_control = 50, n_offered = 100,
                 n_offered_control = 100, welfare = 10, welfare_control = 10,
                 resid_excess = 0, equilibrium_ok = TRUE,
                 unit_cost = ifelse(round == 6, 3, 1))
  s <- node_shock_summary(rows, settled_tail = 5L)
  expect_equal(s$price_overshoot, 3 / 1)
})

test_that("the sweep crosses the instances, the arms and the two shocks", {
  g <- node_shock_grid(n_seeds = 10L)
  expect_setequal(g$graph_type, c("tree", "sp", "entangled"))
  expect_setequal(g$architecture, c("naive", "hybrid_noema"))
  expect_setequal(g$shock, c("capacity", "burst"))
  expect_equal(nrow(g), 3L * 2L * 2L * 10L)
})

test_that("the pipeline carries the shock block as its own targets", {
  src <- paste(readLines(here::here("_targets.R")), collapse = "\n")
  for (nm in c("node_exp6_shock_grid", "node_exp6_shock",
               "node_exp6_shock_summary")) {
    expect_true(grepl(paste0("tar_target\\(\\s*", nm, "[,\\s]"), src), info = nm)
  }
})
