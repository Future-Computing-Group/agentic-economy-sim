# Tests for Exp.7b — the non-uniform JOINT-misreport arm. Exp.7a sweeps one
# scalar shade per agent; this arm sweeps a named finite set that contains
# per-task (joint) deviations and reports the WORST CASE over it.
#
# Design constraints: the arm runs only on the identical-bundle environment
# (where value-greedy is the exact welfare argmax and vcg_allocate is therefore
# DSIC), it must run where capacity BINDS, and its statistic must be able to
# fire — a detector that can never report a positive gain certifies nothing.

# ---- fixtures ---------------------------------------------------------------

# A deliberately NON-EXACT packer: same capacity feasibility, reversed order, so
# the allocation is not the welfare argmax and the mechanism is not DSIC. Used
# only to show the statistic is live; it never enters R/.
anti_greedy_pack_by <- function(rank_vec, tasks_all, env) {
  if (nrow(tasks_all) == 0) return(integer(0))
  cap       <- tier_capacities(env)
  bundle    <- task_bundle(env)
  order_idx <- order(rank_vec, decreasing = FALSE)   # the mutation
  remaining <- setNames(cap$capacity, cap$tier)
  chosen <- integer(0)
  for (i in order_idx) {
    if (!is.finite(rank_vec[i]) || rank_vec[i] <= 0) next
    if (all(remaining[bundle$tier] >= bundle$demand)) {
      remaining[bundle$tier] <- remaining[bundle$tier] - bundle$demand
      chosen <- c(chosen, i)
    }
  }
  chosen
}

# An environment carrying per-task recipes: what Exp.7b must refuse.
heterogeneous_env <- function() {
  env <- init_environment(build_dependency_graph("tree"), "high",
                          n_agents = 4L, graph_type = "tree")
  env$recipes <- tibble::tibble(task_id = c("t1", "t2"),
                                tier = c("edge", "cloud"), demand = c(2, 5))
  env
}

# ---- schema and sign convention --------------------------------------------

test_that("exp7b_run_single returns the documented schema and no regret_* column", {
  r <- exp7b_run_single(graph_type = "tree", load_level = "high", N = 8L,
                        seed = 1L, cap = 30, n_rounds = 4L)
  expect_s3_class(r, "tbl_df")
  expect_equal(nrow(r), 1L)
  expect_true(all(c("graph_type", "load_level", "N", "seed", "cap",
                    "mean_payment", "binding", "br_gain_max", "br_gain_mean",
                    "br_gain_member_argmax") %in% names(r)))
  # The best-response-gain sign (a profitable deviation is positive) lives in
  # br_gain_*; exp7a's opposite-signed regret_* convention must never appear in
  # the same table.
  expect_false(any(grepl("^regret_", names(r))))
  expect_s3_class(r$br_gain_member_argmax, "factor")
})

# ---- the DSIC measurement ---------------------------------------------------

test_that("no joint misreport beats truthful under exact greedy VCG (br_gain_max <= 0)", {
  # Both at the production cap of 30. Tree needs the longer horizon to saturate
  # at that cap; SP binds within a few rounds.
  for (gt in c("tree", "sp")) {
    for (s in 1:3) {
      r <- exp7b_run_single(graph_type = gt, load_level = "high", N = 8L,
                            seed = s, cap = 30,
                            n_rounds = if (gt == "tree") 12L else 6L)
      expect_true(r$binding,
                  info = sprintf("%s seed %d: capacity must bind or the result is vacuous", gt, s))
      expect_gt(r$mean_payment, 1e-6)
      expect_lte(r$br_gain_max, 1e-9)
      expect_lte(r$br_gain_mean, 1e-9)
      # Ties, and maxima inside the numerical noise floor, resolve to truthful.
      expect_equal(as.character(r$br_gain_member_argmax), "truthful")
    }
  }
})

# ---- the statistic is live --------------------------------------------------

test_that("a non-exact allocator makes the worst-case statistic fire", {
  local_global_stub(".greedy_pack_by", anti_greedy_pack_by)
  r <- exp7b_run_single(graph_type = "sp", load_level = "high", N = 8L,
                        seed = 1L, cap = 30, n_rounds = 6L)
  expect_gt(r$br_gain_max, 0)
  expect_false(as.character(r$br_gain_member_argmax) == "truthful")

  # And the arm can name a NON-UNIFORM member: the deviation Exp.7a's single
  # scalar shade cannot express.
  r2 <- exp7b_run_single(graph_type = "tree", load_level = "high", N = 8L,
                         seed = 1L, cap = 30, n_rounds = 6L)
  expect_gt(r2$br_gain_max, 0)
  expect_false(grepl("^(truthful|uniform_)",
                     as.character(r2$br_gain_member_argmax)))
})

test_that("br_gain_max is the max over rounds, not their mean", {
  # Round 1 is cleared by the non-exact packer, so a deviation pays there; every
  # later round is cleared exactly, so its gain is zero. A worst case must read
  # the round-1 gain whatever the horizon; a mean would divide it by n_rounds.
  round_no <- 0L
  real_bind <- bind_tasks
  real_pack <- .greedy_pack_by
  local_global_stub("bind_tasks", function(...) {
    round_no <<- round_no + 1L
    real_bind(...)
  })
  local_global_stub(".greedy_pack_by", function(rank_vec, tasks_all, env) {
    if (round_no == 1L) anti_greedy_pack_by(rank_vec, tasks_all, env)
    else real_pack(rank_vec, tasks_all, env)
  })

  round_no <- 0L
  r1 <- exp7b_run_single(graph_type = "sp", load_level = "high", N = 8L,
                         seed = 1L, cap = 30, n_rounds = 1L)
  round_no <- 0L
  r3 <- exp7b_run_single(graph_type = "sp", load_level = "high", N = 8L,
                         seed = 1L, cap = 30, n_rounds = 3L)

  expect_gt(r1$br_gain_max, 0)                 # the fixture bites in round 1
  expect_equal(r3$br_gain_max, r1$br_gain_max) # max over rounds, not their mean
  expect_equal(as.character(r3$br_gain_member_argmax),
               as.character(r1$br_gain_member_argmax))
})

test_that("br_gain_reduce takes the max over the strategy set, and labels it", {
  # The reduction the whole arm performs: the worst case over S is a MAX, and
  # the tolerance floor moves the LABEL only, never the reported magnitude.
  gains <- c(truthful = 0, uniform_0.5 = -0.2, independent_HL = 0.3,
             drop_highest = -1)
  red <- br_gain_reduce(gains, tol = 1e-9)
  expect_equal(red$max, 0.3)
  expect_equal(red$member, "independent_HL")

  floored <- br_gain_reduce(c(truthful = 0, uniform_1.1 = 5e-10), tol = 1e-9)
  # Exactly, not within testthat's default numeric tolerance: a floor applied
  # to the magnitude would return 0, and 0 is "equal" to 5e-10 under it.
  expect_identical(floored$max, 5e-10)     # magnitude raw
  expect_equal(floored$member, "truthful")
})

test_that("br_gain_mean is the mean over agent-rounds of the per-agent worst case", {
  # A synthetic market: every task is allocated and nothing is paid, except on
  # one member evaluation of one agent-round, which is credited 0.5 in total.
  # That agent-round's worst case is exactly 0.5 and the other one's is 0, so
  # the mean over the two agent-rounds is 0.25. A mean over every (agent,
  # round, member) triple would divide the same 0.5 by the size of S instead.
  call_no <- 0L
  local_global_stub("vcg_allocate", function(tasks_all, env, ...) {
    call_no <<- call_no + 1L
    n <- nrow(tasks_all)
    tibble::tibble(
      task_id        = as.character(tasks_all$task_id),
      agent_id       = as.integer(tasks_all$agent_id),
      realised_value = 0,
      # Call 1 clears round 1 truthfully; call 3 is the second member of S.
      vcg_payment    = rep(if (call_no == 3L) -0.5 / n else 0, n))
  })

  # One agent, two rounds, both non-empty: exactly two agent-rounds.
  r <- exp7b_run_single(graph_type = "tree", load_level = "high", N = 1L,
                        seed = 3L, cap = 30, n_rounds = 2L)
  expect_equal(r$br_gain_max, 0.5)
  expect_equal(r$br_gain_mean, 0.25)
  expect_equal(as.character(r$br_gain_member_argmax), "uniform_0.5")
})

test_that("a strictly negative worst case is reported, not floored at zero", {
  # The per-round maximum must not be floored at 0. The floor is inert only
  # while truthful is in S with a gain of exactly 0; hand the agents a strategy
  # set in which every member strictly loses (withhold everything: the agent is
  # allocated nothing and forgoes the value it would have won) and the worst
  # case must come back negative rather than as a silent 0.
  local_global_stub("misreport_strategy_set", function(ev) {
    S <- list(withhold_all = rep(0, length(ev)))
    attr(S, "independent_fallback") <- FALSE
    S
  })
  r <- exp7b_run_single(graph_type = "tree", load_level = "high", N = 4L,
                        seed = 1L, cap = 30, n_rounds = 2L)
  expect_lt(r$br_gain_max, 0)
  expect_lt(r$br_gain_mean, 0)
})

# ---- the identical-bundle contract -----------------------------------------

test_that("a heterogeneous-recipe environment is refused", {
  expect_error(exp7_require_identical_bundles(heterogeneous_env()),
               "recipe", ignore.case = TRUE)
  env <- init_environment(build_dependency_graph("tree"), "high",
                          n_agents = 4L, graph_type = "tree")
  expect_error(
    exp7_require_identical_bundles(env, tibble::tibble(task_id = "t1", recipe = "a")),
    "recipe", ignore.case = TRUE)
  # Per-task bundles instead of one environment-wide bundle, whether or not the
  # per-task rows happen to name distinct tiers.
  env2 <- env
  env2$demand_weights <- tibble::tibble(task_id = c("t1", "t2"),
                                        tier = c("edge", "edge"),
                                        demand_weight = c(2, 5))
  expect_error(exp7_require_identical_bundles(env2), "one row per tier")
  env3 <- env
  env3$demand_weights <- tibble::tibble(task_id = c("t1", "t2"),
                                        tier = c("edge", "cloud"),
                                        demand_weight = c(2, 5))
  expect_error(exp7_require_identical_bundles(env3), "one row per tier")
  expect_true(exp7_require_identical_bundles(env))
})

test_that("both Exp.7 arms refuse a heterogeneous environment at entry", {
  het <- heterogeneous_env()   # built before the stub, else it recurses
  local_global_stub("init_environment", function(...) het)
  expect_error(exp7b_run_single(graph_type = "tree", load_level = "high",
                                N = 4L, seed = 1L, cap = 30, n_rounds = 1L),
               "recipe", ignore.case = TRUE)
  expect_error(exp7a_run_single(graph_type = "tree", load_level = "high",
                                N = 4L, seed = 1L, cap = 30, n_rounds = 1L),
               "recipe", ignore.case = TRUE)
})

# ---- aggregation ------------------------------------------------------------

test_that("exp7b_aggregate takes the max of the worst case across seeds", {
  fake <- list(
    tibble::tibble(graph_type = "tree", mean_payment = 1, mean_welfare = 10,
                   binding = TRUE, br_gain_max = 0, br_gain_mean = -0.5,
                   br_gain_member_argmax = factor("truthful"),
                   independent_fallback_rate = 0),
    tibble::tibble(graph_type = "tree", mean_payment = 3, mean_welfare = 20,
                   binding = TRUE, br_gain_max = 0.25, br_gain_mean = -0.1,
                   br_gain_member_argmax = factor("independent_LH"),
                   independent_fallback_rate = 0.5)
  )
  agg <- exp7b_aggregate(fake)
  expect_equal(nrow(agg), 1L)
  expect_equal(agg$br_gain_max, 0.25)          # max, not mean
  expect_equal(agg$br_gain_mean, -0.3)         # mean of the secondary
  expect_equal(agg$mean_payment, 2)
  expect_match(agg$argmax_members, "independent_LH=1")
})

# ---- statistics hook and pipeline wiring ------------------------------------

test_that("the Exp.7b stats hook survives a degenerate (all-zero) worst case", {
  raw <- tidyr::expand_grid(graph_type = c("tree", "sp", "agentic"),
                            seed = 1:5) %>%
    dplyr::mutate(br_gain_max  = 0,
                  br_gain_mean = -seed / 10,
                  mean_payment = 1 + seed / 10)
  st <- stat_exp7b(raw)
  kw <- st$by_topology$kruskal
  expect_true(all(c("br_gain_max", "br_gain_mean", "mean_payment") %in% kw$metric))
  expect_true(all(kw$n == 15L))       # every statistic carries its sample size
  expect_true(all(kw$tripwire_ok))
  rep <- make_stats_report(list(exp7b = st))
  expect_gte(nrow(rep), 3L)
  expect_true(all(rep$experiment == "exp7b"))
})

test_that("both Exp.7 arms are wired over the same cells in the pipeline", {
  src <- paste(readLines(here::here("_targets.R")), collapse = "\n")
  for (tgt in c("exp7b_results_raw", "exp7b_summary_table", "stats_exp7b")) {
    expect_true(grepl(tgt, src, fixed = TRUE), info = tgt)
  }
  # Same grid, same saturating caps as the exp7a arm.
  expect_equal(lengths(regmatches(src, gregexpr("map\\(exp7_param_grid\\)", src)))[1], 2L)
  expect_equal(lengths(regmatches(
    src, gregexpr('ifelse\\(exp7_param_grid\\$graph_type == "agentic", 12, 30\\)', src)))[1], 2L)
})
