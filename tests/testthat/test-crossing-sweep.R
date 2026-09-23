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
  # The SCALE specs, which are the ones the pipeline runs; the small ones are
  # this suite's own six-node fixture and a substrate row drawn from them would
  # describe the fixture rather than the study's instances.
  fs <- sweep_substrate_families("scale")
  expect_equal(fs$T$stratum, "laminar")
  expect_equal(fs$X$stratum, "one_crossing")
  expect_equal(fs$S$stratum, "laminar")
  expect_equal(fs$X$crossing_count, 1L)
  # The one crossing is the interface X is named for: one edge exports l1 and
  # l2, the other l2 and l3, and neither block contains the other.
  expect_setequal(fs$X$blocks$e1, c("l1", "l2"))
  expect_setequal(fs$X$blocks$e2, c("l2", "l3"))
  expect_true(all(vapply(fs, function(f) f$crossing_graph_bipartite, logical(1))))
  expect_true(all(vapply(fs, function(f) f$interval_order, logical(1))))
  expect_true(all(vapply(fs, function(f) isTRUE(f$tu_verdict), logical(1))))

  # ... at the token capacities those specs carry, not at a rule of the
  # sweep's own: the substrate rows are the shipped instances or they are
  # nothing.
  specs <- leaf_instance_specs("scale")
  for (nm in names(fs)) {
    want <- token_capacity(specs[[nm]])
    expect_equal(fs[[nm]]$capacity[names(want)], want, info = nm)
  }
  expect_equal(unname(token_capacity(specs$X)[c("d", "e1", "e2", "e3",
                                                "l1", "l2", "l3", "l4")]),
               c(100, 50, 50, 50, 75, 50, 75, 50))
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
  expect_true(all(c("triangle", "triangle_capacity", "substrate_T",
                    "substrate_X", "substrate_S", "npubsub_domains")
                  %in% inst$instance))
  expect_equal(inst$stratum[inst$instance == "triangle"], "oddcycle_nonInterval")
  expect_equal(inst$stratum[inst$instance == "substrate_X"], "one_crossing")
  # The substrate rows are the instances the pipeline runs: four leaves, four
  # internal nodes, not the suite's six-node fixture.
  expect_equal(inst$n_leaves[inst$instance == "substrate_X"], 4L)
  expect_equal(inst$n_internal[inst$instance == "substrate_X"], 4L)
})

test_that("every instance of the sweep gets a total-unimodularity verdict", {
  # The enumeration's size limit sits above the widest family the generator can
  # draw, so no instance of the sweep is reported as unknown.
  inst <- sweep_instances(20L)
  expect_false(any(is.na(inst$tu_verdict)))
  # Five strata at the quota plus the hand-built families that take a place in
  # one, and the series-parallel instance appended outside the quota.
  expect_equal(nrow(inst), 101L)
  expect_equal(sum(inst$instance != "substrate_SP"), 100L)
  # Laminar and one-crossing families are unions of at most two laminar
  # families, whose incidence matrices are totally unimodular; the enumeration
  # is checked against that rather than trusted on its own.
  expect_true(all(inst$tu_verdict[inst$stratum %in% c("laminar",
                                                      "one_crossing")]))
})

test_that("a round with a positive gap only happens where the verdict is FALSE", {
  # A strictly fractional relaxation is a submatrix with a determinant outside
  # 0 and plus or minus 1, so a gap anywhere on an instance and a TRUE verdict
  # on the same instance cannot both be right.
  inst <- sweep_instances(n_per_stratum = 3L, seed = 1L, max_draws = 2000L)
  rows <- bind_rows(lapply(seq_len(nrow(inst)), function(i)
    sweep_run(inst$family[[i]], inst$instance[i], inst$stratum[i], "uniform",
              seeds = 1:2, n_rounds = 3L)))

  gapped <- unique(rows$instance[!rows$integral])
  expect_gt(length(gapped), 0L)
  expect_false(any(inst$tu_verdict[inst$instance %in% gapped]))
})


# ---- a round, end to end ---------------------------------------------------

test_that("two rounds on the triangle report both instruments", {
  rows <- sweep_run(triangle_family(), instance = "triangle",
                    stratum = "oddcycle_nonInterval", leaf_mix = "uniform",
                    seeds = 1L, n_rounds = 2L)
  expect_equal(nrow(rows), 2L)
  expect_true(all(c("gap", "integral", "relative_gap", "exactness",
                    "binding_node", "p_cross_binding", "binding_crossed",
                    "n_admit_greedy", "n_admit_optimum")
                  %in% names(rows)))
  expect_true(all(rows$gap >= -1e-9))
  expect_true(all(rows$exactness <= 1 + 1e-9))
  # Unit capacities against a hundred-odd arrivals: the triangle's relaxation
  # is fractional and its greedy is inexact, which is why it is in the sweep.
  expect_true(any(!rows$integral))
  # One task a node at unit capacities, and the greedy takes the same one.
  expect_equal(rows$n_admit_optimum, c(1L, 1L))
  expect_equal(rows$n_admit_greedy, c(1L, 1L))
})

test_that("the admitted counts are the programs' own, not read off their values", {
  # Values are drawn per task, so a count inferred from a value would be wrong
  # by construction. The optimum's count comes off its 0/1 solution vector and
  # its value is that solution's objective.
  f    <- sweep_substrate_families("scale")$X
  rows <- sweep_run(f, "substrate_X", "one_crossing", "uniform",
                    seeds = 1L, n_rounds = 3L)
  expect_true(all(rows$n_admit_optimum > 0L))
  expect_true(all(rows$n_admit_greedy <= rows$n_admit_optimum))
  expect_true(all(rows$n_admit_optimum <= rows$n_positive))
  # A laminar-plus-one-set family is integral, and its greedy is not thereby
  # optimal, so the two counts are two measurements.
  expect_true(all(rows$integral))
})

test_that("a laminar family keeps its guarantees over a run", {
  fs   <- sweep_substrate_families("scale")
  rows <- sweep_run(fs$T, instance = "substrate_T", stratum = "laminar",
                    leaf_mix = "skewed", seeds = 1:2, n_rounds = 3L)
  expect_equal(nrow(rows), 6L)
  expect_true(all(rows$integral))
  expect_true(all(rows$relative_gap < 1e-9))
})

test_that("the triangle is in the sweep at both capacity rules", {
  # The same three crossing blocks at two tightnesses. Neither is totally
  # unimodular, so the theory certifies nothing about either; what separates
  # them is the capacity rule, and the pair is what keeps the unit-capacity
  # row from being read as the behaviour of the structure.
  cap <- sweep_triangle_capacity_family()
  expect_equal(cap$stratum, "oddcycle_nonInterval")
  expect_equal(cap$crossing_count, 3L)
  expect_false(cap$tu_verdict)
  # The generated instances' own rule: a node at 0.6 of the demand expected
  # under its block, which at three leaves and the high-load rate is 54.
  expect_equal(unname(cap$capacity[["ab"]]), 54)

  for (mix in c("uniform", "skewed")) {
    unit <- sweep_run(sweep_triangle_family(), "triangle",
                      "oddcycle_nonInterval", mix, seeds = 1:3, n_rounds = 5L)
    big  <- sweep_run(cap, "triangle_capacity", "oddcycle_nonInterval", mix,
                      seeds = 1:3, n_rounds = 5L)
    expect_true(all(!unit$integral), info = mix)
    expect_true(all(big$integral), info = mix)
    expect_lt(max(big$gap), 1e-9)
  }
})


# ---- the named instance ----------------------------------------------------

test_that("the deployed pipeline templates translate to a laminar family", {
  f <- sweep_npubsub_family()
  expect_equal(f$leaves, c("cqi_chain", "anomaly_sp", "ran_entangled"))
  expect_equal(f$internal,
               c("du", "cu", "near_rt_ric", "non_rt_ric", "smo"))
  # Three of the five domains carry a stage of all three templates and two
  # carry a stage of the chain alone, so every pair of blocks is nested or
  # equal: the family is laminar, and a clearing price exists for every round
  # and every value vector on it.
  expect_equal(f$crossing_count, 0L)
  expect_true(f$crossing_graph_bipartite)
  expect_true(f$interval_order)
  expect_true(f$tu_verdict)
  expect_equal(f$stratum, "laminar")

  # The same fraction rule as the generated instances: an internal node at 0.6
  # of the demand expected under its block, a leaf at 1.5 of its own.
  expect_equal(unname(f$capacity[["du"]]), 81)
  expect_equal(unname(f$capacity[["non_rt_ric"]]), 27)
  expect_equal(unname(f$capacity[["cqi_chain"]]), 68)

  rows <- sweep_run(f, instance = "npubsub_domains", stratum = f$stratum,
                    leaf_mix = "uniform", seeds = 1L, n_rounds = 2L)
  expect_true(all(rows$integral))
})


# ---- the grid and the two summaries ----------------------------------------

test_that("the grid is one branch per instance and mix, seeds looped inside", {
  inst <- sweep_instances(n_per_stratum = 2L, seed = 1L, max_draws = 800L)
  grid <- sweep_grid(inst)
  expect_equal(nrow(grid), 2L * nrow(inst))
  expect_setequal(unique(grid$leaf_mix), c("uniform", "skewed"))
  expect_true("family" %in% names(grid))
  expect_true(inherits(grid$family[[1]], "list"))
  # The branch count is what keeps the block off a per-seed grid: ten seeds a
  # branch would put it in the thousands.
  expect_lt(nrow(grid), 1000L)
})

test_that("the summaries report the strata, the named rows and the predicates", {
  fs <- sweep_substrate_families("scale")
  inst <- tibble(instance = c("substrate_T", "triangle", "gen_001", "gen_002"),
                 n_leaves = c(4L, 3L, 4L, 3L), n_internal = c(4L, 3L, 4L, 3L),
                 crossing_count = c(0L, 3L, 0L, 3L),
                 crossing_graph_bipartite = c(TRUE, FALSE, TRUE, FALSE),
                 interval_order = c(TRUE, FALSE, TRUE, FALSE),
                 tu_verdict = c(TRUE, FALSE, TRUE, FALSE),
                 stratum = c("laminar", "oddcycle_nonInterval",
                             "laminar", "oddcycle_nonInterval"))
  one <- function(f, nm, st) sweep_run(f, nm, st, "uniform", seeds = 1L,
                                       n_rounds = 3L)
  rows <- bind_rows(
    one(fs$T, "substrate_T", "laminar"),
    one(sweep_triangle_family(), "triangle", "oddcycle_nonInterval"),
    one(fs$S, "gen_001", "laminar"),
    one(sweep_triangle_capacity_family(), "gen_002", "oddcycle_nonInterval"))

  s <- sweep_summary(rows)
  expect_setequal(s$label, c("laminar", "oddcycle_nonInterval",
                             "substrate_T", "triangle"))
  expect_true(all(c("n_instances", "n_rounds", "fraction_positive_gap",
                    "mean_relative_gap", "max_relative_gap", "mean_exactness",
                    "worst_exactness", "onset_error", "mean_admitted",
                    "displaced_over_admitted") %in% names(s)))
  expect_equal(names(s)[(ncol(s) - 1L):ncol(s)],
               c("mean_admitted", "displaced_over_admitted"))
  expect_equal(s$fraction_positive_gap[s$label == "laminar"], 0)
  expect_gt(s$fraction_positive_gap[s$label == "triangle"], 0)

  # The hand-built rows are out of the stratum rows, so a stratum's numbers are
  # the generated families' and the unit-capacity triangle cannot carry its own
  # stratum's mean.
  expect_equal(s$n_instances[s$label == "laminar"], 1L)
  expect_equal(s$n_instances[s$label == "oddcycle_nonInterval"], 1L)
  expect_equal(s$fraction_positive_gap[s$label == "oddcycle_nonInterval"], 0)
  # ... and naming nothing puts them back, so the exclusion is one switch.
  s_in <- sweep_summary(rows, named_excluded = character(0))
  expect_equal(s_in$n_instances[s_in$label == "oddcycle_nonInterval"], 2L)
  expect_gt(s_in$fraction_positive_gap[s_in$label == "oddcycle_nonInterval"], 0)

  b <- sweep_by_instance(rows, inst)
  expect_equal(nrow(b), 4L)
  expect_true(all(c("crossing_count", "tu_verdict", "mean_exactness",
                    "worst_exactness", "onset_error") %in% names(b)))
  expect_equal(names(b)[(ncol(b) - 1L):ncol(b)],
               c("mean_admitted", "displaced_over_admitted"))
  expect_equal(b$crossing_count[b$instance == "triangle"], 3L)
  expect_equal(b$mean_exactness[b$instance == "substrate_T"], 1)
  # One task a round at unit capacities, and no displacement in it.
  expect_equal(b$mean_admitted[b$instance == "triangle"], 1)
  expect_equal(b$displaced_over_admitted[b$instance == "triangle"], 0)
  expect_gt(b$mean_admitted[b$instance == "gen_002"], 1)
})


# ---- the two-terminal series-parallel recogniser ---------------------------

test_that("the recogniser reduces a two-terminal chain to a single edge", {
  # One service node between the virtual source and the virtual terminal: the
  # series reductions alone take it to the source-terminal edge.
  expect_true(sweep_series_parallel(tibble(from = "d", to = "l1")))
})

test_that("the recogniser separates the shipped instances by their shape", {
  specs <- leaf_instance_specs("scale")
  # A rooted tree closed by a virtual terminal is series-parallel: each leaf
  # reduces in series onto its parent and the siblings then reduce in parallel.
  expect_true(sweep_series_parallel(specs$T$edges))
  # X adds the crossing arc, so two internal nodes and two leaves sit on a
  # four-cycle that neither reduction touches.
  expect_false(sweep_series_parallel(specs$X$edges))
  # The fan is a complete bipartite core between the edge nodes and the
  # leaves: no vertex of degree two, no parallel pair, nothing reduces.
  expect_false(sweep_series_parallel(specs$S$edges))
})


# ---- the two-terminal series-parallel instance -----------------------------

test_that("the series-parallel instance is series-parallel and not a tree", {
  arcs <- sweep_sp_arcs()
  expect_true(sweep_series_parallel(arcs))
  # Not a tree: the two parallel service nodes rejoin at one node, which is
  # the reachability a rooted tree cannot express.
  expect_equal(sum(arcs$to == "m"), 2L)
})

test_that("the series-parallel instance is laminar at the substrate's width", {
  f <- sweep_sp_family()
  expect_equal(f$n_leaves, 4L)
  expect_equal(f$n_internal, 4L)
  expect_equal(length(f$blocks), 8L)      # eight nodes, four of them leaves
  expect_equal(f$crossing_count, 0L)
  expect_true(f$crossing_graph_bipartite)
  expect_true(f$interval_order)
  expect_true(f$tu_verdict)
  expect_equal(f$stratum, "laminar")

  # The blocks the arcs give: the source node reaches every leaf, the parallel
  # pair and their join reach the two leaves behind the join.
  expect_setequal(f$blocks$d, c("l1", "l2", "l3", "l4"))
  expect_setequal(f$blocks$e1, c("l1", "l2"))
  expect_setequal(f$blocks$e2, c("l1", "l2"))
  expect_setequal(f$blocks$m, c("l1", "l2"))

  # The generated instances' own rule, as every hand-built row uses: an
  # internal node at 0.6 of the demand expected under its block, a leaf at 1.5
  # of its own.
  expect_equal(unname(f$capacity[["d"]]), 81)
  expect_equal(unname(f$capacity[["m"]]), 40)
  expect_equal(unname(f$capacity[["l1"]]), 51)

  # Laminar, so the relaxation is integral and the greedy is exact in every
  # round whatever the values are.
  rows <- sweep_run(f, instance = "substrate_SP", stratum = f$stratum,
                    leaf_mix = "uniform", seeds = 1:2, n_rounds = 3L)
  expect_true(all(rows$integral))
  expect_equal(max(rows$relative_gap), 0)
  expect_equal(min(rows$exactness), 1)
})

test_that("the series-parallel instance is a named row appended to the sweep", {
  inst <- sweep_instances(n_per_stratum = 2L, seed = 1L, max_draws = 800L)
  expect_true("substrate_SP" %in% inst$instance)
  expect_equal(inst$stratum[inst$instance == "substrate_SP"], "laminar")
  # Appended after the generated families, so neither the generator's draw
  # sequence nor the names it gives shift: the first generated family is still
  # the sixth instance of the table.
  expect_equal(inst$instance[7], "gen_006")
  expect_equal(inst$instance[nrow(inst)], "substrate_SP")
  expect_equal(inst$n_leaves[inst$instance == "substrate_SP"], 4L)
  expect_equal(inst$crossing_count[inst$instance == "substrate_SP"], 0L)
  expect_true(inst$tu_verdict[inst$instance == "substrate_SP"])

  # ... and it is reported on its own row rather than inside a stratum mean,
  # like every other hand-built family.
  expect_true("substrate_SP" %in% eval(formals(sweep_summary)$named))
})
