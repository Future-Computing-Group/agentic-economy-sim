# Tests for the Kubernetes-style rank (deployed practice: kube-scheduler's
# priority class, then least-requested, then arrival order). The point of the
# baseline is that the deployed scheduler is value-blind and deadline-blind, so
# the first two tests pin that blindness and the next three are the collapse
# pre-checks: a new mechanism has to be a new mechanism, not a monotone
# transform of one already in the ablation.

k8_env <- function(graph_type = "sp", load = "high", N = 8L) {
  init_environment(build_dependency_graph(graph_type), load_level = load,
                   n_agents = N, graph_type = graph_type)
}

k8_env_saturated <- function(cap = 30) {
  env <- k8_env()
  env$capacities <- dplyr::mutate(env$capacities, capacity = cap)
  env
}

k8_tasks <- function(n = 40L, n_agents = 8L, seed = 1L) {
  set.seed(seed)
  tibble::tibble(
    task_id    = sprintf("t%03d", seq_len(n)),
    agent_id   = sample.int(n_agents, n, replace = TRUE),
    deadline   = sample(c(500L, 750L, 1000L), n, replace = TRUE),
    value_base = runif(n, 1, 2)
  )
}

k8_edf_score <- function(tasks) (max(tasks$deadline) + 1) - tasks$deadline

k8_ev <- function(tasks, env, util_hat = 0.5) {
  task_expected_value(tasks, util_hat, base_latency_for_bids(env),
                      init_success_model())
}


# ---- what the deployed scheduler cannot see ---------------------------------

test_that("k8s_rank_score is deadline-independent", {
  env    <- k8_env()
  tasks  <- k8_tasks()
  permuted <- tasks
  permuted$deadline <- rev(tasks$deadline)
  expect_identical(k8s_rank_score(permuted, env), k8s_rank_score(tasks, env))
})

test_that("k8s_rank_score is value-independent", {
  env    <- k8_env()
  tasks  <- k8_tasks()
  permuted <- tasks
  permuted$value_base <- rev(tasks$value_base)
  expect_identical(k8s_rank_score(permuted, env), k8s_rank_score(tasks, env))
})


# ---- collapse pre-checks: the new arm is not an existing arm -----------------

test_that("k8s_rank_score is not a monotone transform of the EDF score", {
  env   <- k8_env()
  tasks <- k8_tasks()
  rho   <- suppressWarnings(stats::cor(k8s_rank_score(tasks, env),
                                       k8_edf_score(tasks), method = "spearman"))
  expect_gt(rho, -1)
  expect_lt(rho, 1)
})

test_that("k8s_rank_score is not a monotone transform of the expected value", {
  env   <- k8_env()
  tasks <- k8_tasks()
  rho   <- suppressWarnings(stats::cor(k8s_rank_score(tasks, env),
                                       k8_ev(tasks, env), method = "spearman"))
  expect_gt(rho, -1)
  expect_lt(rho, 1)
})

test_that("the k8s allocation is not the random allocation", {
  env   <- k8_env_saturated()
  tasks <- k8_tasks()
  set.seed(1)
  a_k8  <- pack_tasks_greedy(tasks, k8s_rank_score(tasks, env), env)$task_id
  a_rnd <- pack_tasks_greedy(tasks, runif(nrow(tasks), 0.01, 1), env)$task_id
  jaccard <- length(intersect(a_k8, a_rnd)) / length(union(a_k8, a_rnd))
  expect_gt(length(a_k8), 0)
  expect_lt(jaccard, 1)
})


# ---- what the rank is made of ------------------------------------------------

test_that("the least-requested term is constant within a round under identical bundles", {
  # Kubernetes scores a node by its free capacity share after placing the pod.
  # Every task here demands the same bundle against one aggregate capacity per
  # tier, so the term contributes nothing to the ordering; it is kept because it
  # stops being constant as soon as tasks carry different recipes, which the
  # negative control below shows.
  env   <- k8_env()
  tasks <- k8_tasks()
  term  <- least_requested_term(tasks, env)
  expect_length(term, nrow(tasks))
  expect_equal(stats::var(term), 0)

  env$recipes      <- list(light = c(device = 1, edge = 1, cloud = 1),
                           heavy = c(device = 4, edge = 4, cloud = 4))
  tasks$recipe     <- rep(c("light", "heavy"), length.out = nrow(tasks))
  expect_gt(stats::var(least_requested_term(tasks, env)), 0)
})

test_that("priority classes are stable per agent across rounds", {
  env <- k8_env()
  classes <- function(tasks) {
    s <- k8s_rank_score(tasks, env)          # class is the only term above 1000
    vapply(split(s %/% 1000, tasks$agent_id), function(x) unique(x), numeric(1))
  }
  c1 <- classes(k8_tasks(seed = 1))
  c2 <- classes(k8_tasks(seed = 2))
  common <- intersect(names(c1), names(c2))   # not every agent submits every round
  expect_gt(length(common), 1)
  expect_equal(c1[common], c2[common])
  expect_equal(unname(c1), priority_class(as.integer(names(c1))))
  expect_lte(length(unique(c1)), 3)          # three classes, as a cluster grants
})


# ---- the arm inside Exp.6 ---------------------------------------------------

test_that("the k8s arm schedules without posting a price", {
  r <- exp6_run_single("k8s", "naive", "sp", "high",
                       N = 20L, seed = 1L, n_rounds = 10L)
  expect_equal(r$mechanism, "k8s")
  expect_gt(r$welfare, 0)
  # A rank scheduler posts nothing, so both price columns are absent exactly as
  # they are for the random, EDF and value-greedy arms.
  expect_true(is.na(r$mean_price_volatility))
  # NA, not NaN: mean() of an all-missing series returns NaN, which passes every
  # na.rm path but reads as a computed number in the statistics dump.
  expect_identical(r$mean_unit_cost, NA_real_)
})
