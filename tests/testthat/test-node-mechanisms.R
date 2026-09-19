# test-node-mechanisms.R
# ---------------------------------------------------------------------------
# The six allocation mechanisms over the node-level region, and the two that
# are deployed practice rather than argument: a static posted price and a
# Kubernetes-style rank.
#
# Both needed rebuilding for a substrate where tasks differ in which resources
# they touch. A posted price anchored on one mix-average bundle prices a task
# that touches three nodes the same as one that touches five; a least-requested
# score averaged over every column of the recipe matrix gives the nodes a task
# does not touch a free share of one each.
# ---------------------------------------------------------------------------

mech_env <- function(a = "entangled", mix = "uniform") {
  node_run_env(a, "high", node_agents()[[a]], mix, "off")
}

mech_tasks <- function(env, n = 24L, seed = 1L) {
  set.seed(seed)
  tibble(task_id = sprintf("t%02d", seq_len(n)),
         agent_id = rep_len(1:6, n), deadline = 1000,
         value_base = runif(n, 1, 2),
         recipe = rep_len(rownames(env$anc), n))
}


# ---- the posted price, one level per leaf ---------------------------------

test_that("the posted price is one level per leaf at its own path reserve cost", {
  env <- mech_env("entangled")
  anc <- env$anc
  p   <- posted_price_anchor_per_leaf(env, anc, k = 1)

  expect_equal(names(p), rownames(anc))
  # A leaf's price is the token weight times its ancestor count times the
  # per-node reserve, so the crossing leaf, which passes through a fourth
  # node, costs more than the leaves that do not.
  expect_equal(unname(p[["l1"]]), 2 * 3 * env$reserve_price)
  expect_equal(unname(p[["l2"]]), 2 * 4 * env$reserve_price)
  expect_gt(p[["l2"]], p[["l1"]])
  expect_equal(posted_price_anchor_per_leaf(env, anc, k = 4), 4 * p)

  # Every leaf of the parallel arm has five ancestors, so its levels are flat
  # and higher: the unmatched control, reported rather than engineered away.
  sp <- mech_env("sp")
  expect_equal(unname(posted_price_anchor_per_leaf(sp, sp$anc, 1)),
               rep(2 * 5 * sp$reserve_price, 4))
})

test_that("every winner pays the price of the leaf it was admitted at", {
  env   <- mech_env("entangled")
  tasks <- mech_tasks(env)
  ev    <- task_expected_value(tasks, 0.5, base_latency_for_bids(env),
                               init_success_model())
  p_leaf <- posted_price_anchor_per_leaf(env, env$anc, k = 1)
  p_task <- unname(p_leaf[as.character(tasks$recipe)])

  alloc <- posted_price_allocate(tasks, env, ev, p_task)
  expect_gt(nrow(alloc), 0)
  expect_equal(alloc$payment, unname(p_leaf[as.character(alloc$recipe)]))
})


# ---- the Kubernetes rank ---------------------------------------------------

test_that("the least-requested term averages over the nodes a task touches", {
  env   <- mech_env("entangled")
  tasks <- mech_tasks(env, n = 4L)
  tasks$recipe <- c("l1", "l2", "l3", "l4")

  term <- least_requested_term(tasks, env)
  A    <- task_recipes(tasks, env)
  cap  <- tier_capacities(env)
  C    <- cap$capacity[match(colnames(A), cap$tier)]
  for (i in seq_len(nrow(A))) {
    touched <- A[i, ] > 0
    expect_equal(term[i], mean(1 - A[i, touched] / C[touched]))
  }
  # The untouched nodes contributed a free share of one each under the old
  # average, which collapsed the spread by the number of columns they filled.
  expect_gt(diff(range(term)), 0)
})

test_that("the k8s ordering on an identical-bundle environment is unchanged", {
  # Under identical bundles every task touches every tier, so the term is a
  # constant, it normalises to zero, and a constant shift changes no ordering.
  env <- init_environment(build_dependency_graph("sp"), "high", 8L, "sp")
  tasks <- tibble(task_id = sprintf("t%02d", 1:20), agent_id = rep(1:5, 4),
                  deadline = 1000, value_base = 1)

  term <- least_requested_term(tasks, env)
  expect_equal(diff(range(term)), 0)
  score <- k8s_rank_score(tasks, env)
  expect_equal(order(score, decreasing = TRUE),
               order(1000 * priority_class(tasks$agent_id) +
                       (1 - seq_len(20) / 21), decreasing = TRUE))
})

test_that("on a node environment the term is normalised and orders within a class", {
  env   <- mech_env("entangled")
  tasks <- mech_tasks(env, n = 12L)
  tasks$agent_id <- 3L                       # one priority class throughout

  score <- k8s_rank_score(tasks, env)
  term  <- least_requested_term(tasks, env)
  norm  <- (term - min(term)) / diff(range(term))
  expect_equal(range(norm), c(0, 1))
  # The rescaled term spans ten against an arrival tiebreak spanning one, so
  # it orders within the class instead of being swamped by arrival order.
  expect_equal(order(score, decreasing = TRUE),
               order(10 * norm + (1 - seq_len(12) / 13), decreasing = TRUE))
  expect_gt(diff(range(10 * norm)), diff(range(1 - seq_len(12) / 13)))
})


# ---- the six arms through the driver ---------------------------------------

test_that("every mechanism runs on the node region and is scored against the exact reference", {
  run <- function(m, k = 1) node_run_single(
    "entangled", "high", N = 90L, seed = 1L, n_rounds = 12L,
    mechanism = m, p_post_k = k)

  out <- lapply(c("random", "edf", "greedy_ev", "market", "posted_price", "k8s"),
                run)
  names(out) <- c("random", "edf", "greedy_ev", "market", "posted_price", "k8s")
  for (nm in names(out)) {
    expect_equal(out[[nm]]$mechanism, nm, info = nm)
    expect_true(is.finite(out[[nm]]$arm_exact_ratio), info = nm)
    expect_gt(out[[nm]]$tokens_admitted, 0)
  }
  # Value-greedy IS the packing rule the reference is measured against, so it
  # sits at the top; the value-blind arms leave welfare on the table.
  expect_gt(out$greedy_ev$arm_exact_ratio, out$random$arm_exact_ratio)
  expect_gt(out$greedy_ev$arm_exact_ratio, out$k8s$arm_exact_ratio)
  expect_gt(out$greedy_ev$arm_exact_ratio, out$edf$arm_exact_ratio)
  # A markup rations: the same arm at four times marginal cost admits less.
  expect_lt(run("posted_price", 4)$tokens_admitted,
            out$posted_price$tokens_admitted)
})


# ---- the incentive arms ----------------------------------------------------

test_that("the node incentive arms certify the region before they price it", {
  r <- exp7a_run_single("tree", "high", N = 8L, seed = 1L, n_rounds = 3L,
                        substrate = "node", cap_scale = 0.1,
                        shades = c(0.9, 1.0, 1.1))
  expect_equal(r$certificate, "certified")
  expect_true(r$binding)                     # capacity binds: not a vacuous cell
  expect_equal(r$n_negative_externality, 0)
  # DSIC where the allocation rule is the argmax: truth is the best response.
  expect_gte(r$regret_0.9, -1e-9)
  expect_gte(r$regret_1.1, -1e-9)
  expect_equal(unname(r$regret_1), 0, tolerance = 1e-9)
})

test_that("the uncertified converse arm reports a gain rather than a regret", {
  cert <- exp7b_run_single("sp", "high", N = 8L, seed = 1L, n_rounds = 3L,
                           substrate = "node", cap_scale = 0.1)
  expect_equal(cert$certificate, "certified")
  expect_lte(cert$br_gain_max, 1e-9)
  expect_equal(cert$n_negative_externality, 0)

  conv <- exp7b_run_single("entangled", "high", N = 8L, seed = 1L, n_rounds = 3L,
                           substrate = "node", cap_scale = 0.1)
  expect_equal(conv$certificate, "uncertified")
  # The exact packer enters as the round's REFERENCE and never as its
  # allocator, and only where the round fits the enumeration cap.
  expect_gte(conv$exact_fit_fraction, 0)
  expect_lte(conv$exact_fit_fraction, 1)
  expect_true(is.finite(conv$br_gain_max))
})

test_that("the node incentive arms still refuse an uncertified environment", {
  # The lift is for one certified case and for nothing else: an environment
  # that never went through the certifier is refused exactly as before.
  env <- node_run_env("tree", "high", 8L, "uniform", "off")
  expect_error(exp7_require_identical_bundles(env), "heterogeneous-recipe")
  expect_error(vcg_allocate(mech_tasks(env, 6L), env, 0.5,
                            base_latency_for_bids(env), init_success_model()),
               "no incentive claim is available")
})
