# Tests for the per-task recipe model: task_recipes, the recipe-aware packing
# kernel, and the recipe-aware tatonnement.
#
# The load-bearing assertion is the identity guard: an environment with no
# recipes, and an environment whose recipes are all the mix-average bundle, must
# both reproduce a clearing recorded from the single-bundle code path exactly.
# Everything from Exp.1 to Exp.10 runs through that path, so anything less than
# exact reproduction is a silent restatement of published numbers.

rec_env <- function(cap = 30, recipes = NULL) {
  env <- init_environment(build_dependency_graph("sp"), "high",
                          n_agents = 8L, graph_type = "sp")
  env$capacities <- dplyr::mutate(env$capacities, capacity = cap)
  env$recipes    <- recipes
  env
}

rec_tasks <- function(n = 12L, seed = 11L, recipe = NULL) {
  set.seed(seed)
  tk <- tibble::tibble(
    task_id    = sprintf("t%03d", seq_len(n)),
    agent_id   = sample.int(8L, n, replace = TRUE),
    deadline   = sample(c(500L, 750L, 1000L), n, replace = TRUE),
    value_base = runif(n, 1, 2)
  )
  if (!is.null(recipe)) tk$recipe <- recipe
  tk
}

# ---- task_recipes -----------------------------------------------------------

test_that("task_recipes broadcasts the environment bundle when no recipes are set", {
  env <- rec_env()
  tk  <- rec_tasks(5L)
  A   <- task_recipes(tk, env)
  bundle <- task_bundle(env)

  expect_equal(dim(A), c(5L, nrow(bundle)))
  expect_equal(colnames(A), bundle$tier)
  for (i in seq_len(5L)) expect_identical(unname(A[i, ]), bundle$demand)
})

test_that("task_recipes reads the per-task recipe column", {
  env <- rec_env(recipes = list(A = c(device = 2, edge = 1, cloud = 1.5),
                                B = c(device = 1, edge = 2, cloud = 1.5)))
  tk  <- rec_tasks(4L, recipe = c("A", "B", "A", "B"))
  A   <- task_recipes(tk, env)

  expect_equal(colnames(A), task_bundle(env)$tier)
  expect_equal(unname(A[1, ]), c(2, 1, 1.5))
  expect_equal(unname(A[2, ]), c(1, 2, 1.5))
  expect_equal(unname(colSums(A)), c(6, 6, 6))
})

test_that("task_recipes reorders a recipe to the bundle's tier order", {
  # The column-sum assertion above is order-invariant, so it passes with the
  # reordering removed. This one does not: the recipe is written cloud first.
  env <- rec_env(recipes = list(A = c(cloud = 1.5, device = 2, edge = 1)))
  A   <- task_recipes(rec_tasks(1L, recipe = "A"), env)
  expect_equal(colnames(A), c("device", "edge", "cloud"))
  expect_equal(unname(A[1, ]), c(2, 1, 1.5))
})

# ---- the packing kernel -----------------------------------------------------

test_that("the packing kernel is unchanged by an explicit homogeneous recipe", {
  env  <- rec_env(cap = 30)
  bund <- task_bundle(env)
  homog <- list(homog = setNames(bund$demand, bund$tier))

  tk  <- rec_tasks(10L)
  ev  <- task_expected_value(tk, 0.5, base_latency_for_bids(env), init_success_model())
  tk_h <- tk; tk_h$recipe <- "homog"

  expect_identical(.greedy_pack_by(ev, tk, env),
                   .greedy_pack_by(ev, tk_h, rec_env(cap = 30, recipes = homog)))
})

test_that("the packing kernel respects per-task recipes", {
  # One resource. Recipes, not values, decide what fits: the top-value task
  # alone exhausts capacity under its own recipe, so the broadcast packer and
  # the recipe-aware packer choose different sets.
  env <- rec_env(cap = 10)
  env$demand_weights <- tibble::tibble(tier = c("device", "edge", "cloud"),
                                       demand_weight = c(1, 1, 1))
  env$capacities <- tibble::tibble(tier = c("device", "edge", "cloud"),
                                   capacity = c(10, 10, 10))
  env$recipes <- list(hog   = c(device = 10, edge = 10, cloud = 10),
                      light = c(device = 5,  edge = 5,  cloud = 5))
  tk <- rec_tasks(3L, recipe = c("hog", "light", "light"))
  chosen <- .greedy_pack_by(c(6, 4, 4), tk, env)
  expect_equal(sort(chosen), 1L)      # value-greedy takes the hog and stops

  tk2 <- tk; tk2$recipe <- c("light", "light", "light")
  expect_equal(sort(.greedy_pack_by(c(6, 4, 4), tk2, env)), c(1L, 2L))
})

test_that("pack_tasks_greedy still honours max_tasks", {
  env <- rec_env(cap = 300)
  tk  <- rec_tasks(10L)
  expect_equal(nrow(pack_tasks_greedy(tk, rep(1, 10), env, max_tasks = 3L)), 3L)
})

# ---- the identity guard -----------------------------------------------------

test_that("homogeneous recipes reproduce the recorded single-bundle clearing", {
  fix <- readRDS(test_path("fixtures", "clear-multitier-homogeneous.rds"))
  # The fixture was recorded at the price step in force at the time; the test
  # pins the recipe refactor, not the step, so the step is passed explicitly.
  eta_recorded <- 0.25

  clear_cell <- function(cell, recipes, recipe_col) {
    env <- if (cell == "saturated") rec_env(cap = 20) else
      init_environment(build_dependency_graph("sp"), "high",
                       n_agents = 8L, graph_type = "sp")
    env$recipes <- recipes
    n  <- if (cell == "saturated") 12L else 40L
    tk <- rec_tasks(n, seed = 11L, recipe = recipe_col)
    clear_multitier_market(tk, env, util_hat = 0.0,
                           base_latency = base_latency_for_bids(env),
                           market_state = init_market_state(env),
                           eta = eta_recorded)
  }

  for (cell in c("saturated", "slack")) {
    # (a) the refactor itself is neutral: no recipes at all.
    expect_identical(clear_cell(cell, NULL, NULL), fix[[cell]])
    # (b) the recipe path collapses: every task carries the mix-average bundle.
    # The allocation gains the recipe column -- it has to, since execution
    # charges the admitted mix -- so the comparison is on the columns the
    # fixture recorded, and every recorded value must still be identical.
    env  <- rec_env()
    bund <- task_bundle(env)
    homog <- list(homog = setNames(bund$demand, bund$tier))
    got <- clear_cell(cell, homog, "homog")
    expect_identical(got$allocation[names(fix[[cell]]$allocation)],
                     fix[[cell]]$allocation)
    got$allocation <- fix[[cell]]$allocation
    expect_identical(got, fix[[cell]])
  }
})

# ---- the tatonnement aggregates per-task recipes ----------------------------

test_that("the tatonnement prices aggregate per-task recipe demand", {
  # One tatonnement step, closed form. Twelve tasks, all with positive surplus
  # at the starting price, capacity 12 on every tier, a price step of 0.25
  # passed explicitly so the arithmetic does not depend on the default:
  #
  #   p_r' = p_r + eta * (total_demand_r - C_r) / C_r
  #
  # Under a recipe of (1, 3, 1) the edge total is 36 and the others are 12, so
  # edge moves by 0.25 * 24 / 12 = 0.5 and the others do not move at all. A
  # tatonnement reading one bundle per environment cannot produce that: it
  # spreads the same total over the tiers in the environment's fixed
  # proportions and moves all three.
  flat_env <- function(recipe) {
    env <- init_environment(build_dependency_graph("tree"), "medium",
                            n_agents = 8L, graph_type = "tree")
    env$demand_weights <- tibble::tibble(tier = c("device", "edge", "cloud"),
                                         demand_weight = c(5 / 3, 5 / 3, 5 / 3))
    env$capacities <- tibble::tibble(tier = c("device", "edge", "cloud"),
                                     capacity = c(12, 12, 12))
    env$reserve_price <- 0
    env$recipes <- list(r = recipe)
    env
  }
  step_once <- function(recipe) {
    env <- flat_env(recipe)
    ms  <- init_market_state(env)
    ms$prices <- dplyr::mutate(ms$prices, price = 0.01)
    tk  <- rec_tasks(12L, recipe = "r")
    res <- clear_multitier_market(tk, env, util_hat = 0.0,
                                  base_latency = base_latency_for_bids(env),
                                  market_state = ms, iters = 1L, eta = 0.25)
    expect_equal(res$clearing$n_alloc, 0L)   # capacity binds; only the prices matter here
    setNames(res$clearing$prices$price, res$clearing$prices$tier)
  }

  expect_equal(step_once(c(device = 1, edge = 3, cloud = 1)),
               c(device = 0.01, edge = 0.51, cloud = 0.01))
  expect_equal(step_once(c(device = 1, edge = 1, cloud = 3)),
               c(device = 0.01, edge = 0.01, cloud = 0.51))
})
