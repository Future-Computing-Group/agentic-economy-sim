# The arrival-order posted price.
#
# A posted price in deployment screens participation and then serves whoever
# turns up, in the order they turn up. The value-ranked arm screens the same
# way and then chooses among the participants by expected value, which is the
# operator doing the allocation the price was supposed to do. The two arms are
# the same mechanism under two service disciplines, so they part company
# exactly where capacity binds: while the node capacities carry every
# participant, both admit the screened set.

fcfs_env <- function(scale = 1) {
  scale_capacities(node_run_env("tree", "high", 90L, "uniform", "off"), scale)
}

# One recipe for every task, so "arrival order" has an unambiguous reading:
# equal demand means the packer never skips a task and takes the rows it can
# afford in the order the key gives them.
fcfs_tasks <- function(env, n = 60L, seed = 5L) {
  set.seed(seed)
  tibble(task_id = sprintf("t%03d", seq_len(n)), agent_id = rep_len(1:9, n),
         deadline = 1000, value_base = runif(n, 1, 2),
         recipe = rownames(env$anc)[[1]])
}

fcfs_ev <- function(tasks) tasks$value_base


test_that("the two posted arms admit the same set while capacity is slack", {
  env   <- fcfs_env()
  tasks <- fcfs_tasks(env)
  ev    <- fcfs_ev(tasks)
  p     <- stats::median(ev)

  by_value   <- posted_price_allocate(tasks, env, ev, p)
  by_arrival <- posted_price_allocate(tasks, env, ev, p, order = "arrival")

  expect_setequal(by_arrival$task_id, tasks$task_id[ev > p])
  expect_setequal(by_arrival$task_id, by_value$task_id)
})

test_that("the two posted arms differ where capacity binds", {
  env   <- fcfs_env(scale = 0.05)
  tasks <- fcfs_tasks(env)
  ev    <- fcfs_ev(tasks)
  p     <- min(ev) / 2                    # everyone participates; capacity alone rations

  by_value   <- posted_price_allocate(tasks, env, ev, p)
  by_arrival <- posted_price_allocate(tasks, env, ev, p, order = "arrival")

  expect_lt(nrow(by_arrival), nrow(tasks))          # the fixture really binds
  expect_equal(nrow(by_arrival), nrow(by_value))    # the same budget, spent differently
  expect_false(setequal(by_arrival$task_id, by_value$task_id))
  # Arrival order is the row order of the screened tasks, so the admitted set
  # is their prefix; the value-ranked arm takes the most valuable instead.
  expect_equal(by_arrival$task_id, head(tasks$task_id, nrow(by_arrival)))
  expect_gt(sum(ev[match(by_value$task_id, tasks$task_id)]),
            sum(ev[match(by_arrival$task_id, tasks$task_id)]))
})

test_that("the screen is the same screen and every winner pays the posted price", {
  env   <- fcfs_env(scale = 0.05)
  tasks <- fcfs_tasks(env)
  ev    <- fcfs_ev(tasks)
  p     <- stats::quantile(ev, 0.4, names = FALSE)

  alloc <- posted_price_allocate(tasks, env, ev, p, order = "arrival")
  expect_gt(nrow(alloc), 0)
  expect_true(all(ev[match(alloc$task_id, tasks$task_id)] > p))
  expect_true(all(alloc$payment == p))
})

test_that("the value-ranked arm is untouched by the ordering argument", {
  env   <- fcfs_env(scale = 0.05)
  tasks <- fcfs_tasks(env)
  ev    <- fcfs_ev(tasks)
  p     <- stats::median(ev)
  expect_identical(posted_price_allocate(tasks, env, ev, p),
                   posted_price_allocate(tasks, env, ev, p, order = "value"))
})


# ---- the arm inside the driver ---------------------------------------------

test_that("the driver runs the arrival-order arm at the same anchor", {
  one <- function(m) node_run_single("tree", "high", N = 90L, seed = 1L,
                                     n_rounds = 8L, mechanism = m, p_post_k = 2)
  fcfs   <- one("posted_price_fcfs")
  posted <- one("posted_price")

  expect_equal(fcfs$mechanism, "posted_price_fcfs")
  # The same dose: the level and the anchor are the mechanism's, the service
  # discipline is not.
  expect_equal(fcfs$mean_unit_cost, posted$mean_unit_cost)
  expect_equal(fcfs$p_post_k, 2)
  # The references are off the market and do not move with the discipline.
  expect_equal(fcfs$optimum_ex_post, posted$optimum_ex_post)
  expect_equal(fcfs$ceiling_zero_queue, posted$ceiling_zero_queue)
  expect_true(is.finite(fcfs$welfare))
  # The discipline is a real treatment: the two arms do not admit one set.
  expect_false(isTRUE(all.equal(fcfs$welfare, posted$welfare)))
})


# ---- the grids and the tuned table -----------------------------------------

test_that("the tuning grid carries the new arm at the posted levels, appended", {
  g <- node_tuning_grid(c(1L, 2L))
  expect_setequal(unique(g$p_post_k[g$mechanism == "posted_price_fcfs"]),
                  node_tuning_posted_levels())
  expect_setequal(unique(g$reserve_markup[g$mechanism == "posted_price_fcfs"]), 1)
  # Appended, never inserted: every row the block already ran keeps its index
  # and its content.
  old <- (length(node_tuning_posted_levels()) +
            2 * length(node_reserve_markups())) * 12 * 2 * 2
  expect_false("posted_price_fcfs" %in% g$mechanism[seq_len(old)])
  expect_setequal(unique(g$mechanism[(old + 1L):nrow(g)]), "posted_price_fcfs")
})

test_that("the new arm is tuned and reported like the arm it comparates", {
  # The same fixture shape the tuned block's own tests use, with the new arm
  # peaking at a level of its own.
  knobs <- dplyr::bind_rows(
    tidyr::expand_grid(mechanism = c("posted_price", "posted_price_fcfs"),
                       p_post_k = node_tuning_posted_levels(),
                       reserve_markup = 1),
    tidyr::expand_grid(mechanism = "market", p_post_k = 1,
                       reserve_markup = node_reserve_markups()))
  tuning <- tidyr::expand_grid(knobs, graph_type = "tree", load_level = "high",
                               architecture = "naive", seed = 1:2) %>%
    dplyr::mutate(welfare = dplyr::case_when(
      mechanism == "posted_price"      ~ -abs(p_post_k - 1.5),
      mechanism == "posted_price_fcfs" ~ -abs(p_post_k - 2),
      TRUE                             ~ -abs(reserve_markup - 1.25)) + seed / 100)

  eval_grid <- node_eval_grid(tuning, c(11L, 12L))
  expect_setequal(unique(eval_grid$p_post_k[
    eval_grid$mechanism == "posted_price_fcfs"]), 2)

  eval_raw <- eval_grid %>%
    dplyr::mutate(welfare = 10 + seed, tokens_admitted = 50,
                  median_latency = 200, welfare_over_optimum = 0.4,
                  alloc_ratio_true = 0.6)
  tuned <- node_tuned_table(eval_raw, tuning)
  expect_true("posted_price_fcfs" %in% tuned$mechanism)
  row <- dplyr::filter(tuned, mechanism == "posted_price_fcfs")
  # Its knob is its own, and it is read against its own grid rather than
  # falling through to the reserve markup of an arm that has none.
  expect_equal(row$p_post_k, 2)
  expect_false(row$knob_at_boundary)
  expect_false(is.na(row$knob_flat))
})

test_that("a knob on the edge of the posted grid is flagged for the new arm too", {
  edge <- tibble(mechanism = "posted_price_fcfs",
                 p_post_k = c(min(node_tuning_posted_levels()),
                              max(node_tuning_posted_levels()), 1.5),
                 reserve_markup = 1)
  expect_equal(node_knob_at_boundary(edge$mechanism, edge$p_post_k,
                                     edge$reserve_markup),
               c(TRUE, TRUE, FALSE))
})

test_that("the mechanism grid runs the new arm at every posted level, appended", {
  g <- node_exp6_mechanism_grid(n_seeds = 10L)
  expect_setequal(unique(g$p_post_k[g$mechanism == "posted_price_fcfs"]),
                  node_posted_levels())
  # Nine levels, three instances, two loads, two architectures, ten seeds,
  # two congestion levels.
  expect_equal(sum(g$mechanism == "posted_price_fcfs"),
               9L * 3L * 2L * 2L * 10L * 2L)
  # Appended after every row the block already ran.
  old <- nrow(g) - sum(g$mechanism == "posted_price_fcfs")
  expect_equal(old, 4800L)
  expect_false("posted_price_fcfs" %in% g$mechanism[seq_len(old)])
})

test_that("the mechanism statistics keep each arrival-order level its own arm", {
  # Pooling nine posted levels under one label would test a mixture of nine
  # prices against the other arms and report it as one.
  raw <- tidyr::expand_grid(
    mechanism = "posted_price_fcfs", p_post_k = c(1, 2), graph_type = "tree",
    load_level = "high", architecture = "naive", seed = 1:4) %>%
    dplyr::bind_rows(tidyr::expand_grid(
      mechanism = "greedy_ev", p_post_k = 1, graph_type = "tree",
      load_level = "high", architecture = "naive", seed = 1:4)) %>%
    dplyr::mutate(median_latency = seed + p_post_k, drop_rate = seed / 10,
                  welfare = seed * p_post_k, mean_price_volatility = 0,
                  efficiency = 0.5)
  s <- stat_exp6(raw)
  groups <- unique(s$per_topo_load[[1]]$ci$mechanism)
  expect_true(all(c("posted_price_fcfs_k1", "posted_price_fcfs_k2") %in% groups))
})


# ---- arrival order drawn per round -----------------------------------------

test_that("the arrival order is a seeded permutation of the round", {
  p <- node_arrival_order(12L, seed = 3L, t = 5L)
  expect_setequal(p, seq_len(12L))
  expect_identical(p, node_arrival_order(12L, seed = 3L, t = 5L))
  expect_false(identical(p, node_arrival_order(12L, seed = 3L, t = 6L)))
  expect_false(identical(p, node_arrival_order(12L, seed = 4L, t = 5L)))
  # Pinned: the order a fixed seed and round draws.
  expect_identical(p, c(12L, 1L, 3L, 8L, 10L, 6L, 7L, 2L, 5L, 4L, 11L, 9L))
  expect_length(node_arrival_order(0L, seed = 1L, t = 1L), 0L)
})

test_that("drawing the arrival order leaves the round's own stream untouched", {
  set.seed(42); before <- .Random.seed
  node_arrival_order(30L, seed = 1L, t = 1L)
  expect_identical(.Random.seed, before)
})

test_that("a given arrival order admits its prefix when capacity binds", {
  env   <- fcfs_env(scale = 0.05)
  tasks <- fcfs_tasks(env)
  ev    <- fcfs_ev(tasks)
  p     <- min(ev) / 2
  pos   <- node_arrival_order(nrow(tasks), seed = 1L, t = 1L)
  alloc <- posted_price_allocate(tasks, env, ev, p, order = "arrival",
                                 arrival = pos)
  expect_lt(nrow(alloc), nrow(tasks))
  # The tasks that arrived first, in the order they arrived.
  expect_equal(alloc$task_id,
               head(tasks$task_id[order(pos)], nrow(alloc)))
})
