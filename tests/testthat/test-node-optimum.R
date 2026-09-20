# test-node-optimum.R
# ---------------------------------------------------------------------------
# The two references a mechanism's realised welfare is read against.
#
#   ceiling_zero_queue: the exact leaf-block optimum on the round's true values
#     at zero queue. No admission policy can reach it, because admitting the
#     tasks it counts is what creates the queue that discounts them.
#   optimum_ex_post: the best realised welfare over admission sets of the form
#     "the top m tasks by true value", each scored through the execution model
#     itself. This one is attainable, by a planner who knows the round.
#
# Both are computed off the market on the TRUE instance, so neither moves with
# the arm's own congestion state, and neither draws from the random number
# generator: the arm's stream has to stay where it was.
# ---------------------------------------------------------------------------

.opt_env <- function(scale = 0.05) scale_capacities(node_env("tree", "high", 10L), scale)

.opt_tasks <- function() {
  tibble::tibble(task_id = c("t1", "t2", "t3"), agent_id = 1:3,
                 deadline = 500, value_base = c(3, 2, 1), recipe = "l1")
}


# ---- the deterministic execution path --------------------------------------

test_that("the execution model runs without a draw when its noise is switched off", {
  env <- .opt_env()
  alloc <- .opt_tasks()
  set.seed(11L)
  before <- .Random.seed

  quiet <- execute_allocation(alloc, env, latency_noise_cv = 0)
  # The load-bearing property: scoring a reference set inside the round loop
  # must not move the arm's own stream.
  expect_identical(.Random.seed, before)
  expect_identical(quiet$latency, execute_allocation(alloc, env,
                                                     latency_noise_cv = 0)$latency)
  # Every task of one round takes the round's critical path, undispersed.
  expect_equal(length(unique(quiet$latency)), 1L)
  expect_gt(quiet$latency[1], 70)

  # The default path still draws, and draws the same spread it always did.
  set.seed(11L)
  noisy <- execute_allocation(alloc, env)
  set.seed(11L)
  expect_identical(noisy$latency,
                   rnorm(3, mean = quiet$latency, sd = 0.1 * quiet$latency))
})


# ---- the two references ----------------------------------------------------

test_that("the ex-post optimum is the best subset when value order decides it", {
  # Three tasks at one leaf: every subset of a given size loads the nodes
  # identically, so the best subset of each size IS the top m by value and the
  # scan over m sees the whole lattice.
  env   <- .opt_env()
  tasks <- .opt_tasks()
  v     <- node_true_value(tasks, base_latency_per_leaf(env), 0.005)
  score <- function(a) compute_welfare(execute_allocation(a, env,
                                                          latency_noise_cv = 0),
                                       env, NULL, lambda_l_default = 0.005)

  subsets <- unlist(lapply(0:3, function(k) utils::combn(3, k, simplify = FALSE)),
                    recursive = FALSE)
  exhaustive <- max(vapply(subsets, function(i) score(tasks[i, ]), numeric(1)))

  opt <- node_ex_post_optimum(tasks, v, score)
  expect_equal(opt[["value"]], exhaustive)
  # The capacities are cut to a twentieth here, so the third task's queue
  # costs more than its value and the planner's best move is to leave it out.
  expect_equal(opt[["m"]], 2)
  expect_equal(opt[["value"]], score(tasks[1:2, ]))
  expect_gt(opt[["value"]], score(tasks))

  # The zero-queue ceiling counts the same values with no queue at all, so it
  # is above what any admission set realises.
  ceiling <- node_zero_queue_ceiling(env, tasks, v)
  expect_gt(ceiling, opt[["value"]])
  expect_lte(opt[["value"]], ceiling)
})

test_that("the true value is the zero-queue value of the task and nothing else", {
  env   <- .opt_env()
  tasks <- .opt_tasks()
  base  <- base_latency_per_leaf(env)
  expect_equal(node_true_value(tasks, base, 0.005),
               tasks$value_base * exp(-0.005 * base[["l1"]]))
  # A task whose own path already misses its deadline keeps only its salvage.
  late <- dplyr::mutate(tasks, deadline = 10)
  expect_equal(node_true_value(late, base, 0.005, salvage = 0.25),
               0.25 * tasks$value_base * exp(-0.005 * base[["l1"]]))
})

test_that("the scan over admission sizes stays inside its budget", {
  # A round of a hundred arrivals must not cost a hundred scorings: the coarse
  # scan is capped and the refinement walks only the gap it stepped over.
  tasks <- tibble::tibble(task_id = paste0("t", 1:100), agent_id = 1:100,
                          deadline = 500, value_base = 100:1, recipe = "l1")
  seen <- 0L
  score <- function(a) { seen <<- seen + 1L; -abs(nrow(a) - 37) }
  opt <- node_ex_post_optimum(tasks, tasks$value_base, score)
  expect_equal(opt[["m"]], 37)
  expect_lte(seen, 35L)
})


# ---- what the driver reports -----------------------------------------------

test_that("the driver reports both references and the arm's distance to them", {
  r <- node_run_single("tree", "high", N = 90L, seed = 1L, n_rounds = 6L,
                       lambda_l_default = node_lambda_l())
  expect_true(all(c("ceiling_zero_queue", "optimum_ex_post", "optimum_ex_post_m",
                    "welfare_over_optimum", "welfare_over_ceiling") %in% names(r)))

  expect_gt(r$ceiling_zero_queue, r$optimum_ex_post)
  expect_gt(r$optimum_ex_post_m, 0)
  # The market clears more than the planner would admit and pays for it.
  expect_lt(r$welfare_over_optimum, 1)
  expect_lt(r$welfare_over_ceiling, r$welfare_over_optimum)
  # Both ratios are taken on one numerator, the post-burn-in mean welfare, so
  # their quotient is the quotient of the two references.
  expect_equal(r$welfare_over_optimum / r$welfare_over_ceiling,
               r$ceiling_zero_queue / r$optimum_ex_post)
})

test_that("an arm that admits nothing sits at zero against both references", {
  r <- node_run_single("tree", "high", N = 90L, seed = 1L, n_rounds = 6L,
                       mechanism = "posted_price", p_post_k = 4,
                       lambda_l_default = node_lambda_l())
  expect_equal(r$tokens_admitted, 0)
  expect_equal(r$welfare_over_optimum, 0)
  expect_gt(r$optimum_ex_post, 0)
})
