# test-crossing-sweep.R
# ---------------------------------------------------------------------------
# The crossing sweep: structural predicates, the generator's coverage of the
# strata, and the two instruments read per round.
#
# Every instance this simulator ships is laminar or laminar-plus-one-set, so
# its packing matrix is totally unimodular and its relaxation is integral in
# every round whatever the values are. The sweep exists to leave that region:
# it generates leaf-block families whose crossing graph has an odd cycle and
# whose blocks admit no interval order, where integrality can fail, and it
# measures where it does.
#
# The predicates are checked on families whose verdicts are known by hand: the
# three-leaf triangle (the smallest family that is not a union of two laminar
# families) and three pairwise-crossing intervals (an odd crossing cycle whose
# blocks are still intervals of one leaf order, hence still integral).
# ---------------------------------------------------------------------------


# ---- families whose verdicts are known by hand -----------------------------

triangle_family <- function() {
  sweep_family(
    leaves = c("a", "b", "c"),
    internal_blocks = list(ab = c("a", "b"), bc = c("b", "c"), ac = c("a", "c")),
    capacity = c(ab = 1, bc = 1, ac = 1, a = 1, b = 1, c = 1))
}

interval_triple_family <- function() {
  leaves <- paste0("x", 1:5)
  sweep_family(
    leaves = leaves,
    internal_blocks = list(i1 = leaves[1:3], i2 = leaves[2:4], i3 = leaves[3:5]),
    capacity = setNames(rep(1, 8), c("i1", "i2", "i3", leaves)))
}

test_that("the three-leaf triangle is the family the theory calls hardest", {
  f <- triangle_family()
  expect_equal(f$crossing_count, 3L)
  expect_false(f$crossing_graph_bipartite)
  expect_false(f$interval_order)
  expect_false(f$tu_verdict)
  expect_equal(f$stratum, "oddcycle_nonInterval")

  # ... and its packing is the one that separates the relaxation: one task per
  # leaf at unit values, every pair sharing a node, so the integer optimum
  # takes one task and the relaxation takes a half of each.
  A <- f$anc[c("a", "b", "c"), c("ab", "bc", "ac"), drop = FALSE]
  g <- node_lp_ip_gap(c(1, 1, 1), A, f$capacity[c("ab", "bc", "ac")])
  expect_equal(g$gap, 0.5)
  expect_false(g$integral)
})

test_that("three pairwise-crossing intervals cross without leaving integrality", {
  f <- interval_triple_family()
  expect_equal(f$crossing_count, 3L)
  expect_false(f$crossing_graph_bipartite)
  expect_true(f$interval_order)
  expect_true(f$tu_verdict)
  expect_equal(f$stratum, "oddcycle_interval")

  # An interval matrix is totally unimodular, so the relaxation is integral at
  # every value vector, not merely at the one the test happens to draw.
  set.seed(4L)
  A <- f$anc[, names(f$blocks), drop = FALSE]
  for (i in 1:10) {
    g <- node_lp_ip_gap(runif(5, 1, 2), A, f$capacity[colnames(A)])
    expect_true(g$integral)
  }
})

test_that("the shipped instances land where the theory puts them", {
  fs <- sweep_substrate_families("small")
  expect_equal(fs$T$stratum, "laminar")
  expect_equal(fs$X$stratum, "one_crossing")
  expect_equal(fs$S$stratum, "laminar")
  expect_equal(fs$X$crossing_count, 1L)
  expect_true(all(vapply(fs, function(f) f$crossing_graph_bipartite, logical(1))))
  expect_true(all(vapply(fs, function(f) isTRUE(f$tu_verdict), logical(1))))
})


# ---- the demand the sweep draws --------------------------------------------

test_that("the sweep's skewed mix is the substrate's at the substrate's width", {
  leaves <- paste0("l", 1:4)
  expect_equal(sweep_leaf_shares("skewed", leaves),
               node_leaf_shares("skewed", leaves))
  expect_equal(sweep_leaf_shares("uniform", leaves),
               node_leaf_shares("uniform", leaves))
  # ... and it keeps its shape at every width the generator draws.
  for (n in 3:8) {
    s <- sweep_leaf_shares("skewed", paste0("l", seq_len(n)))
    expect_equal(sum(s), 1)
    expect_equal(unname(s[1]), 0.55)
    expect_equal(unname(s[2]), 0.05)
  }
})

test_that("the onset prediction is the substrate's law in matrix coordinates", {
  spec <- leaf_instance_specs("scale")$X
  anc  <- ancestor_matrix(spec)
  cap  <- token_capacity(spec)
  f <- sweep_family(
    leaves = rownames(anc),
    internal_blocks = setNames(
      lapply(setdiff(colnames(anc), rownames(anc)),
             function(v) rownames(anc)[anc[, v] > 0]),
      setdiff(colnames(anc), rownames(anc))),
    capacity = cap)

  for (mix in c("uniform", "skewed")) {
    got  <- sweep_onset_prediction(f, sweep_leaf_shares(mix, rownames(anc)),
                                   n_agents = 90L, lambda = 1.5)
    want <- node_onset_law(spec, mix, "high", 90L, 20L)
    expect_equal(got$binding_node, want$binding_node)
    expect_equal(got$p_cross, want$p_cross)
  }
})


# ---- the greedy the sweep runs is the market's own -------------------------

test_that("the matrix-level greedy admits what the market's packer admits", {
  env    <- node_run_env("tree", "high", 90L, "uniform", "off")
  agents <- init_agents(90L)
  tasks  <- node_round_tasks(env, agents, 1L, 7L, c(500L, 750L, 1000L))
  v_true <- node_true_value(tasks, base_latency_per_leaf(env), node_lambda_l())

  cap <- tier_capacities(env)
  A   <- task_recipes(tasks, env)[, cap$tier, drop = FALSE]

  want <- pack_tasks_greedy(tasks, v_true, env)$task_id
  got  <- tasks$task_id[sweep_greedy_pack(v_true, A, cap$capacity)]

  expect_gt(length(want), 0)
  expect_setequal(got, want)
})


# ---- the generator ---------------------------------------------------------

test_that("the generator reaches every stratum and carries the hand-built ones", {
  inst <- sweep_instances(n_per_stratum = 2L, seed = 1L, max_draws = 800L)
  counts <- table(inst$stratum)
  for (s in c("laminar", "one_crossing", "bilaminar_multi",
              "oddcycle_interval", "oddcycle_nonInterval")) {
    expect_gte(as.integer(counts[[s]]), 2L)
  }
  expect_true(all(c("triangle", "substrate_T", "substrate_X", "substrate_S")
                  %in% inst$instance))
  expect_equal(inst$stratum[inst$instance == "triangle"], "oddcycle_nonInterval")
  expect_equal(inst$stratum[inst$instance == "substrate_X"], "one_crossing")
})


# ---- a round, end to end ---------------------------------------------------

test_that("two rounds on the triangle report both instruments", {
  rows <- sweep_run(triangle_family(), instance = "triangle",
                    stratum = "oddcycle_nonInterval", leaf_mix = "uniform",
                    seeds = 1L, n_rounds = 2L)
  expect_equal(nrow(rows), 2L)
  expect_true(all(c("gap", "integral", "relative_gap", "exactness",
                    "binding_node", "p_cross_binding", "binding_crossed")
                  %in% names(rows)))
  expect_true(all(rows$gap >= -1e-9))
  expect_true(all(rows$exactness <= 1 + 1e-9))
  # Unit capacities against a hundred-odd arrivals: the triangle's relaxation
  # is fractional and its greedy is inexact, which is why it is in the sweep.
  expect_true(any(!rows$integral))
})

test_that("a laminar family keeps its guarantees over a run", {
  fs   <- sweep_substrate_families("small")
  rows <- sweep_run(fs$T, instance = "substrate_T", stratum = "laminar",
                    leaf_mix = "skewed", seeds = 1:2, n_rounds = 3L)
  expect_equal(nrow(rows), 6L)
  expect_true(all(rows$integral))
  expect_true(all(rows$relative_gap < 1e-9))
})
