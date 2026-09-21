# test-node-existence.R
# ---------------------------------------------------------------------------
# Whether a clearing price vector exists for the round at all.
#
# The convergence block asks whether the price process FINDS an equilibrium.
# This one asks the prior question: under linear anonymous node prices and
# single-bundle unit demand, a task wants every node on its path or none of
# them, so the round's packing is an integer program and a price vector
# supports it exactly when that program's linear relaxation has an integral
# optimum. A strictly fractional relaxation is a round no anonymous linear
# price can clear, whatever walk is run at it.
#
# The certificate is therefore two solves of one matrix, and the reported
# quantity is their difference. Nothing about the market arm enters it: the
# question is about the demand the round offers, not about what an arm did
# with it.
# ---------------------------------------------------------------------------


# ---- the pure certificate --------------------------------------------------

test_that("a laminar packing has an integral relaxation", {
  # Two leaves under one root: the constraint family is a chain plus disjoint
  # blocks, and the relaxation of such a packing is integral.
  A <- rbind(c(1, 1, 0), c(1, 1, 0), c(1, 0, 1), c(1, 0, 1))
  colnames(A) <- c("d", "e1", "e2")
  cap <- c(d = 3, e1 = 2, e2 = 2)

  g <- node_lp_ip_gap(c(3, 2, 2, 1), A, cap)
  expect_equal(g$lp_value, 7)
  expect_equal(g$ip_value, 7)
  expect_equal(g$gap, 0)
  expect_true(g$integral)
})

test_that("a crossing packing separates the relaxation from the optimum", {
  # Three unit-capacity nodes and three tasks, each task taking a different
  # pair: every pair of tasks shares a node, so one task is the optimum, while
  # a half of each is feasible for the relaxation and worth one and a half.
  A <- rbind(c(1, 1, 0), c(0, 1, 1), c(1, 0, 1))
  colnames(A) <- c("a", "b", "c")
  cap <- c(a = 1, b = 1, c = 1)

  g <- node_lp_ip_gap(c(1, 1, 1), A, cap)
  expect_equal(g$lp_value, 1.5)
  expect_equal(g$ip_value, 1)
  expect_equal(g$gap, 0.5)
  expect_false(g$integral)
})

test_that("the binary program returns the allocation behind its value", {
  # How many tasks the optimum served is not recoverable from its value: the
  # values are drawn per task. The solution vector is returned so the count is
  # read off the allocation, and it is the allocation the value came from.
  A <- rbind(c(1, 1, 0), c(0, 1, 1), c(1, 0, 1), c(1, 0, 0))
  colnames(A) <- c("a", "b", "c")
  cap <- c(a = 2, b = 1, c = 1)
  v   <- c(5, 4, 3, 2)

  g <- node_lp_ip_gap(v, A, cap)
  expect_length(g$solution, length(v))
  expect_true(all(g$solution %in% c(0, 1)))
  expect_equal(sum(v * g$solution), g$ip_value)
  # ... and the allocation it reports is feasible on the same capacities.
  expect_true(all(as.numeric(crossprod(A, g$solution)) <= cap + 1e-9))

  # An empty round admits nobody, and says so with an empty vector rather than
  # with a length the caller would have to guess at.
  empty <- node_lp_ip_gap(numeric(0),
                          matrix(numeric(0), nrow = 0, ncol = 3,
                                 dimnames = list(NULL, c("a", "b", "c"))), cap)
  expect_length(empty$solution, 0L)
})

test_that("an empty round is trivially integral", {
  g <- node_lp_ip_gap(numeric(0), matrix(numeric(0), nrow = 0, ncol = 2,
                                         dimnames = list(NULL, c("a", "b"))),
                      c(a = 1, b = 1))
  expect_equal(g$lp_value, 0)
  expect_equal(g$ip_value, 0)
  expect_equal(g$gap, 0)
  expect_true(g$integral)
})


# ---- the environment-facing wrapper ---------------------------------------

test_that("the certificate prices the bundle at the reserve and drops the rest", {
  env   <- node_run_env("tree", "high", 90L, "uniform", "off")
  tasks <- node_round_tasks(env, init_agents(90L), t = 1L, seed = 1L,
                            deadlines = c(500L, 750L, 1000L))
  v <- node_true_value(tasks, base_latency_per_leaf(env), node_lambda_l())

  # A reserve above every task's own per-token value leaves nothing to pack.
  A    <- task_recipes(tasks, env)
  high <- 2 * max(v / rowSums(A))
  none <- node_existence_certificate(tasks, env, v, high)
  expect_equal(none$n_tasks, nrow(tasks))
  expect_equal(none$n_positive, 0L)
  expect_equal(none$lp_value, 0)
  expect_equal(none$ip_value, 0)
  expect_true(none$integral)

  # At a reserve in the middle of that distribution the count is exactly the
  # tasks whose value covers their own bundle at the reserve.
  mid <- stats::median(v / rowSums(A))
  some <- node_existence_certificate(tasks, env, v, mid)
  expect_equal(some$n_positive, sum(v - mid * rowSums(A) > 0))
  expect_lt(some$n_positive, nrow(tasks))
  expect_gt(some$lp_value, 0)

  # The profile is kept only where the round has a gap to explain.
  expect_true(all(vapply(some$profile, is.null, logical(1))) == some$integral)
})

test_that("the profile of a fractional round carries its tasks", {
  # A hand-built region whose packing is fractional by construction: three
  # unit nodes, three recipes, each taking a pair. The wrapper's bookkeeping
  # is checked where the answer is known rather than on a generated round.
  env <- list(
    capacities     = tibble::tibble(tier = c("a", "b", "c"), capacity = 1),
    demand_weights = tibble::tibble(tier = c("a", "b", "c"), demand_weight = 1),
    recipes = list(ab = c(a = 1, b = 1, c = 0), bc = c(a = 0, b = 1, c = 1),
                   ac = c(a = 1, b = 0, c = 1)))
  tasks <- tibble::tibble(task_id = 1:3, recipe = c("ab", "bc", "ac"))

  cert <- node_existence_certificate(tasks, env, c(3, 3, 3), reserve = 0.5)
  expect_equal(cert$n_tasks, 3L)
  expect_equal(cert$n_positive, 3L)     # each bundle costs 1 at the reserve
  expect_equal(cert$lp_value, 3)
  expect_equal(cert$ip_value, 2)
  expect_equal(cert$gap, 1)
  expect_false(cert$integral)

  prof <- cert$profile[[1]]
  expect_equal(nrow(prof), cert$n_positive)
  expect_true(all(c("adjusted_value", "leaf") %in% names(prof)))
  expect_equal(prof$adjusted_value, rep(2, 3))
  expect_equal(prof$leaf, c("ab", "bc", "ac"))

  # A reserve the bundles cannot cover leaves nothing to pack, on the same
  # region and the same values.
  none <- node_existence_certificate(tasks, env, c(3, 3, 3), reserve = 2)
  expect_equal(none$n_positive, 0L)
  expect_equal(none$lp_value, 0)
  expect_true(none$integral)
  expect_null(none$profile[[1]])
})


# ---- the driver, the grid and the summary ---------------------------------

test_that("every instance of the study clears at every round it is run on", {
  # The structural guarantee, on the pipeline's own instances and generator.
  # Each instance's constraint family is a union of laminar families whose
  # incidence matrix is totally unimodular, so a fractional round on any of
  # the three would be a defect and not a finding.
  for (gt in c("tree", "sp", "entangled")) {
    rows <- dplyr::bind_rows(lapply(1:3, function(s)
      node_existence_run(gt, seed = s, n_rounds = 5L)))

    expect_equal(nrow(rows), 15L, info = gt)
    expect_true(all(rows$integral), info = gt)
    expect_equal(rows$gap, rep(0, 15), info = gt)
    expect_true(all(rows$n_positive <= rows$n_tasks), info = gt)
    expect_equal(rows$round, rep(1:5, 3), info = gt)
    expect_equal(rows$seed, rep(1:3, each = 5), info = gt)
  }
})

test_that("the run is a function of its seed and carries no market", {
  a <- node_existence_run("tree", seed = 2L, n_rounds = 2L)
  expect_equal(node_existence_run("tree", seed = 2L, n_rounds = 2L), a)
  # The offered demand is what is certified, so the rows are the generator's
  # and no allocation column travels with them.
  expect_false(any(c("admitted", "welfare") %in% names(a)))
})

test_that("the grid sweeps three instances at both mixes", {
  g <- node_existence_grid(n_seeds = 10L)
  expect_setequal(g$graph_type, c("tree", "sp", "entangled"))
  expect_setequal(g$leaf_mix, c("uniform", "skewed"))
  expect_equal(nrow(g), 3L * 2L * 10L)
})

test_that("the summary reports the integral fraction per instance and mix", {
  rows <- tidyr::expand_grid(graph_type = c("tree", "entangled"),
                             leaf_mix = "uniform", round = 1:4) %>%
    dplyr::mutate(seed = 1L,
                  integral = graph_type == "tree" | round > 2,
                  ip_value = 10,
                  gap = ifelse(integral, 0, 1),
                  lp_value = ip_value + gap)
  s <- node_existence_summary(rows)

  expect_equal(nrow(s), 2L)
  expect_equal(s$n_rounds, rep(4L, 2))
  expect_equal(s$fraction_integral[s$graph_type == "tree"], 1)
  expect_equal(s$fraction_integral[s$graph_type == "entangled"], 0.5)
  expect_equal(s$mean_gap[s$graph_type == "entangled"], 0.5)
  expect_equal(s$max_gap[s$graph_type == "entangled"], 1)
  expect_equal(s$mean_relative_gap[s$graph_type == "entangled"], 0.05)
})

test_that("the pipeline carries the existence block as its own targets", {
  src <- paste(readLines(here::here("_targets.R")), collapse = "\n")
  for (nm in c("node_exp6_existence_grid", "node_exp6_existence",
               "node_exp6_existence_summary")) {
    expect_true(grepl(paste0("tar_target\\(\\s*", nm, "[,\\s]"), src), info = nm)
  }
})
