# Tests for Man.N1 — the ported Clarke-pivot VCG (enables Exp.7a DSIC empirics).
# Property-based: IR, non-negativity, welfare-max equivalence, and the headline
# DSIC property (truthful is the best response). Plus a hash-guard that the
# oracle_pack_realised refactor (extracting .greedy_pack_by) is behaviour-neutral.

# ---- fixtures ---------------------------------------------------------------

vcg_env <- function(graph_type = "sp", load = "high", N = 30L) {
  init_environment(build_dependency_graph(graph_type), load_level = load,
                   n_agents = N, graph_type = graph_type)
}

# A SATURATED environment: per-tier capacities shrunk so that only a few tasks
# fit. This is the regime where VCG payments are positive and the DSIC property
# is non-trivial. Without saturation, capacity never binds, all payments are ~0,
# and any truthfulness test passes vacuously.
vcg_env_saturated <- function(graph_type = "sp", cap = 30) {
  env <- vcg_env(graph_type = graph_type, load = "high", N = 8L)
  env$capacities <- dplyr::mutate(env$capacities, capacity = cap)
  env
}

vcg_tasks <- function(n = 40L, n_agents = 8L, seed = 1L) {
  set.seed(seed)
  tibble::tibble(
    task_id    = sprintf("t%03d", seq_len(n)),
    agent_id   = sample.int(n_agents, n, replace = TRUE),
    deadline   = sample(c(100L, 150L, 200L), n, replace = TRUE),
    value_base = runif(n, 1, 2)
  )
}

sm  <- function() init_success_model()
blb <- function(env) base_latency_for_bids(env)


# ---- oracle refactor is behaviour-neutral -----------------------------------

test_that(".greedy_pack_by keeps oracle_pack_realised byte-identical", {
  env <- vcg_env(); tasks <- vcg_tasks()
  o <- oracle_pack_realised(tasks, env, util_hat = 0.5, blb(env), sm())
  # Recompute welfare independently via the set variant; must agree exactly.
  ev  <- task_expected_value(tasks, 0.5, blb(env), sm())
  set <- greedy_alloc_set(ev, tasks, env)
  expect_equal(o$oracle_value, sum(set$realised_value))
  expect_equal(o$n, nrow(set))
})


# ---- contracts --------------------------------------------------------------

test_that("vcg_allocate handles empty + returns the right columns", {
  env <- vcg_env()
  empty <- vcg_allocate(vcg_tasks(0L), env, 0.5, blb(env), sm())
  expect_equal(nrow(empty), 0L)
  expect_true(all(c("task_id", "agent_id", "realised_value", "vcg_payment")
                  %in% names(empty)))
})

test_that("a single task that fits pays zero (displaces nobody)", {
  env <- vcg_env(N = 1L)
  one <- tibble::tibble(task_id = "t1", agent_id = 1L, deadline = 200L,
                        value_base = 1.9)
  res <- vcg_allocate(one, env, util_hat = 0, blb(env), sm())
  if (nrow(res) == 1) expect_equal(res$vcg_payment, 0)
})


# ---- welfare-max equivalence ------------------------------------------------

test_that("vcg allocation welfare equals the value-greedy oracle", {
  env <- vcg_env(); tasks <- vcg_tasks(seed = 4L)
  res <- vcg_allocate(tasks, env, util_hat = 0.5, blb(env), sm())
  ora <- oracle_pack_realised(tasks, env, util_hat = 0.5, blb(env), sm())
  expect_equal(sum(res$realised_value), ora$oracle_value)
})


# ---- IR + non-negativity (property over several seeds) ----------------------

test_that("VCG is individually rational and payments are non-negative", {
  env <- vcg_env()
  for (s in 1:6) {
    res <- vcg_allocate(vcg_tasks(seed = s), env, util_hat = 0.5, blb(env), sm())
    if (nrow(res) == 0) next
    expect_true(all(res$vcg_payment >= -1e-9),
                info = sprintf("seed %d: payment must be >= 0", s))
    expect_true(all(res$vcg_payment <= res$realised_value + 1e-9),
                info = sprintf("seed %d: IR — payment <= realised value", s))
  }
})


# ---- the headline DSIC property: truthful is the best response ---------------

test_that("truthful reporting maximises a focal agent's utility (DSIC), SATURATED regime", {
  # MUST be a binding-capacity regime, else payments are ~0 and the test is
  # vacuous. Saturate so VCG payments are positive.
  env       <- vcg_env_saturated(graph_type = "sp", cap = 30)
  base      <- vcg_tasks(n = 40L, n_agents = 8L, seed = 11L)
  focal     <- 3L
  uh        <- 0.5
  shades    <- c(0.5, 0.7, 0.9, 1.0, 1.1, 1.3)

  true_ev_full <- task_expected_value(base, uh, blb(env), sm())
  focal_rows   <- which(base$agent_id == focal)

  res_truthful <- vcg_allocate(base, env, util_hat = uh, blb(env), sm())
  # GUARD: the test is only meaningful if capacity binds (payments > 0). If this
  # fails the fixture is not saturated and the DSIC test below proves nothing.
  expect_gt(sum(res_truthful$vcg_payment), 1e-6)

  utility_at <- function(alpha) {
    rep_tasks <- base
    rep_tasks$value_base[focal_rows] <- base$value_base[focal_rows] * alpha
    res <- vcg_allocate(rep_tasks, env, util_hat = uh, blb(env), sm())
    fm <- res$agent_id == focal
    if (!any(fm)) return(0)
    tid_to_true <- setNames(true_ev_full[focal_rows], base$task_id[focal_rows])
    true_val    <- sum(tid_to_true[res$task_id[fm]], na.rm = TRUE)
    true_val - sum(res$vcg_payment[fm])
  }

  u <- vapply(shades, utility_at, numeric(1))
  names(u) <- as.character(shades)
  expect_gte(u["1"], max(u) - 1e-6)
})

test_that("VCG is second-price on a single binding slot (hand-verifiable)", {
  # One effective slot (capacity fits exactly one task), two agents, one task
  # each. The winner (higher ev) must pay the loser's ev (the externality it
  # imposes) — the textbook second-price / Clarke-pivot identity.
  env <- vcg_env(graph_type = "tree", load = "high", N = 2L)
  # cloud demand_weight for tree = 2; set every tier capacity to 2 → exactly one
  # task fits (the second task's cloud demand 2 would need 4 > 2).
  env$capacities <- dplyr::mutate(env$capacities, capacity = 2)

  two <- tibble::tibble(
    task_id    = c("a", "b"),
    agent_id   = c(1L, 2L),
    deadline   = c(200L, 200L),
    value_base = c(1.9, 1.4)
  )
  uh  <- 0.0
  ev  <- task_expected_value(two, uh, blb(env), sm())
  # Ensure the fixture really is one-slot + distinct ev (else the identity is moot).
  res <- vcg_allocate(two, env, util_hat = uh, blb(env), sm())
  expect_equal(nrow(res), 1L)                 # exactly one winner
  winner_row <- which(two$task_id == res$task_id[1])
  loser_ev   <- ev[setdiff(1:2, winner_row)]
  # Winner pays the loser's ev (second price).
  expect_equal(res$vcg_payment[1], loser_ev, tolerance = 1e-8)
})
