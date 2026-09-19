# Tests for Exp.11 -- heterogeneous per-task recipes over one fixed DAG.
#
# The three arms share a DAG, an agent count, a load level and a seed, and their
# aggregate per-tier demand is matched to the last task by construction, so the
# only thing that varies is whether tasks arrive in one recipe or two. What is
# read off is a set of numbers, not a threshold: price dispersion, the
# greedy-versus-exact welfare gap, the drop rate and the deliverable share of
# what was admitted.

# ---- the matching itself ----------------------------------------------------

test_that("the arms carry matched aggregate per-tier demand", {
  cat_ <- exp11_recipe_catalogue()
  # A and B are not scalar multiples of one another: that is what makes the
  # catalogue heterogeneous rather than a rescaling.
  expect_false(isTRUE(all.equal(cat_$A / cat_$B, rep(cat_$A[[1]] / cat_$B[[1]], 3),
                                check.attributes = FALSE)))
  # Their 50/50 mix-average IS the homogeneous arm's recipe, exactly.
  expect_equal((cat_$A + cat_$B) / 2, exp11_mix_average())

  # ... and on an even task count the realised aggregates agree to the last task.
  n   <- 12L
  env <- exp11_env(cap = 9, N = 8L, hetero = TRUE)
  tk  <- tibble::tibble(task_id = sprintf("t%02d", seq_len(n)),
                        recipe  = exp11_assign_recipes(n, offset = 0L))
  expect_equal(unname(colSums(task_recipes(tk, env))),
               unname(n * exp11_mix_average()), tolerance = 1e-9)
})

test_that("recipe assignment is parity-stable across rounds", {
  # An odd round must leave the next round starting on the other type, or the
  # long-run aggregate match is only stochastic.
  expect_equal(exp11_assign_recipes(3L, offset = 0L), c("A", "B", "A"))
  expect_equal(exp11_assign_recipes(3L, offset = 1L), c("B", "A", "B"))
  expect_equal(exp11_assign_recipes(0L, offset = 1L), character(0))

  # Two odd rounds in a row balance out, which is what carrying the offset buys.
  off <- 0L
  labs <- character(0)
  for (n in c(3L, 3L)) {
    labs <- c(labs, exp11_assign_recipes(n, offset = off))
    off  <- (off + n) %% 2L
  }
  expect_equal(sum(labs == "A"), sum(labs == "B"))
})

test_that("the aggregate match is exact on an even-parity branch", {
  # The arms match on the realised offered demand of the generated-and-truncated
  # task stream. Odd-sized rounds are what can break it, and the carried parity
  # cancels them pairwise, so a branch holding an EVEN number of odd-sized
  # rounds matches to the last bit. Seed 4 over 20 rounds holds ten.
  het <- exp11_run_single("hetero_naive", cap = 9, seed = 4L, n_rounds = 20L)
  hom <- exp11_run_single("homogeneous",  cap = 9, seed = 4L, n_rounds = 20L)
  cols <- c("agg_demand_device", "agg_demand_edge", "agg_demand_cloud")
  expect_identical(unlist(het[cols]), unlist(hom[cols]))
})

test_that("the aggregate match is within half a task on an odd-parity branch", {
  # An odd number of odd-sized rounds leaves one task unpaired, so the residual
  # is bounded by half a task spread over the branch -- and is not zero, which
  # is the half the shipped even-count test could not see. Seed 1 over 20 rounds
  # holds fifteen odd-sized rounds.
  n_rounds <- 20L
  het <- exp11_run_single("hetero_naive", cap = 9, seed = 1L, n_rounds = n_rounds)
  hom <- exp11_run_single("homogeneous",  cap = 9, seed = 1L, n_rounds = n_rounds)
  cols <- c("agg_demand_device", "agg_demand_edge", "agg_demand_cloud")
  resid <- max(abs(unlist(het[cols]) - unlist(hom[cols])))
  # The bound is half a task spread over the branch, not a bit-exact figure:
  # the diagnostic is a mean over rounds, so it carries rounding of its own.
  expect_lte(resid, 0.5 / n_rounds + 1e-12)
  expect_gt(resid, 0)
  # Cloud matches exactly whatever the parity: both recipes carry cloud = 1.5.
  expect_identical(het$agg_demand_cloud, hom$agg_demand_cloud)
})

test_that("the parity offset is carried across the round boundary", {
  # Pinning the offset to zero inside the loop starts every odd-sized round on
  # type A, so the leftovers accumulate instead of cancelling: over a branch
  # with fifteen odd-sized rounds that is a device-edge gap of 0.75 against the
  # 0.05 a carried parity leaves. One unpaired task moves device up by
  # 0.5 / n_rounds and edge down by the same, so the gap is bounded by twice it.
  n_rounds <- 20L
  r <- exp11_run_single("hetero_naive", cap = 9, seed = 1L, n_rounds = n_rounds)
  expect_lte(abs(r$agg_demand_device - r$agg_demand_edge), 1 / n_rounds + 1e-12)
})

# ---- the runner -------------------------------------------------------------

test_that("every arm returns the reported columns on one cell", {
  cols <- c("arm", "cap", "N", "seed", "price_cv", "welfare_ratio",
            "greedy_exact_ratio", "admitted_exact_ratio",
            "drop_rate", "served_among_admitted",
            "binding_fraction", "truncation_rate",
            "agg_demand_device", "agg_demand_edge", "agg_demand_cloud")
  for (arm in exp11_arms()) {
    res <- exp11_run_single(arm, cap = 9, seed = 1L, n_rounds = 12L)
    expect_equal(nrow(res), 1L)
    expect_true(all(cols %in% names(res)), info = arm)
    expect_equal(res$arm, arm)
  }
})

test_that("capacity binds in the configured cells", {
  # The saturation guard that makes every other number non-vacuous.
  for (arm in exp11_arms()) {
    res <- exp11_run_single(arm, cap = 9, seed = 2L, n_rounds = 20L)
    expect_gt(res$binding_fraction, 0, label = paste0(arm, " binding_fraction"))
  }
})

test_that("value-greedy IS the argmax under identical recipes", {
  # The design's own bug-detector: with one bundle per task, value-greedy is the
  # exact welfare maximiser, so the ratio must sit at 1.0 up to tie-breaking. A
  # departure there is a bug, not a result. This is a statement about the
  # PACKING RULE, so its numerator has to be the value-greedy set on the full
  # instance -- not the admitted set, whose size is also set by the
  # tatonnement's positive-surplus filter at the cleared prices.
  for (arm in c("homogeneous", "homog_encapsulated")) {
    res <- exp11_run_single(arm, cap = 9, seed = 3L, n_rounds = 20L)
    expect_equal(res$greedy_exact_ratio, 1, tolerance = 1e-9, info = arm)
  }
  # Under two recipes the rule is a knapsack heuristic and falls short, but only
  # by a little: the gap is a packing gap, not a rationing gap.
  het <- exp11_run_single("hetero_naive", cap = 9, seed = 3L, n_rounds = 20L)
  expect_lt(het$greedy_exact_ratio, 1)
  expect_gt(het$greedy_exact_ratio, 0.9)
})

test_that("the admitted set is reported separately from the packing rule", {
  # admitted_exact_ratio carries the rationing the ratio above deliberately
  # excludes, so it is far below it on every arm; a per-resource arm's admitted
  # set is capacity-feasible and so still bounded by the argmax.
  for (arm in c("homogeneous", "hetero_naive")) {
    res <- exp11_run_single(arm, cap = 9, seed = 3L, n_rounds = 20L)
    expect_lte(res$admitted_exact_ratio, 1 + 1e-9)
    expect_lt(res$admitted_exact_ratio, res$greedy_exact_ratio)
  }
})

test_that("truncation stays under the design's own trip at the operating point", {
  # 0 <= rate <= 1 is vacuously true of a mean of indicators. The design's trip
  # is 5 per cent: above it the agent count drops rather than the cap rising.
  # The lower bound matters too -- a counter pinned at zero would pass any
  # upper bound, and the cap does fire at this operating point.
  res <- exp11_run_single("hetero_naive", cap = 9, seed = 1L, n_rounds = 100L)
  expect_gt(res$truncation_rate, 0)
  expect_lt(res$truncation_rate, 0.05)
})

test_that("the enumeration cap cannot be widened from the runner", {
  # exp11_run_single passes its own max_tasks as the packer's max_n, so without
  # this the contract is whatever the caller asks for rather than n = 14.
  expect_error(
    exp11_run_single("homogeneous", cap = 9, seed = 1L, n_rounds = 2L,
                     max_tasks = 20L),
    "max_tasks"
  )
  expect_no_error(exp11_run_single("homogeneous", cap = 9, seed = 1L,
                                   n_rounds = 2L, max_tasks = 14L))
})

test_that("exp11_aggregate collapses seeds within an arm and cap", {
  rows <- unlist(lapply(c(9, 12), function(cp)
    lapply(1:2, function(s) exp11_run_single("hetero_naive", cap = cp,
                                             seed = s, n_rounds = 10L))),
    recursive = FALSE)
  agg <- exp11_aggregate(rows)
  # Two capacities in, two rows out: dropping cap from the grouping would
  # average across contention levels and survive a single-cap test.
  expect_equal(nrow(agg), 2L)
  expect_equal(sort(agg$cap), c(9, 12))
  at9 <- agg$drop_rate[agg$cap == 9]
  expect_equal(at9, mean(vapply(rows[1:2], \(r) r$drop_rate, numeric(1))))
})

# ---- no DSIC claim on this arm ---------------------------------------------

test_that("the VCG mechanism refuses a heterogeneous-recipe environment", {
  env <- exp11_env(cap = 9, N = 8L, hetero = TRUE)
  tk  <- tibble::tibble(task_id = "t1", agent_id = 1L, deadline = 750,
                        value_base = 1.5, recipe = "A")
  expect_error(
    vcg_allocate(tk, env, 0.5, base_latency_for_bids(env), init_success_model()),
    "identical-bundle"
  )
  # The identical-bundle environment it IS proven on still runs.
  env_h <- exp11_env(cap = 9, N = 8L, hetero = FALSE)
  expect_no_error(vcg_allocate(tk[setdiff(names(tk), "recipe")], env_h, 0.5,
                               base_latency_for_bids(env_h), init_success_model()))
})

# ---- the over-commitment sweep ---------------------------------------------

test_that("measured over-commitment tracks the closed form on the sweep", {
  sw <- exp11_overcommitment_sweep()
  expect_equal(sw$lambda_2, c(1, 1.25, 1.5, 2, 4))
  expect_equal(sw$rho_measured, sw$rho_predicted, tolerance = 1e-12)

  # The exact-interface control: nothing is over-committed and nothing truncates.
  ctl <- sw[sw$lambda_2 == 1, ]
  expect_equal(ctl$rho_measured, 1)
  expect_equal(ctl$unserved_fraction, 0)

  # At lambda_2 = 2 half of the heavy-slice commitments cannot be served.
  expect_equal(sw$heavy_unserved_fraction[sw$lambda_2 == 2], 0.5)
  # Over-commitment is monotone in the size mismatch.
  expect_true(all(diff(sw$rho_measured) > 0))
})

test_that("the advertised interface and the deliverable set are separate", {
  # The whole measurement is the difference between them, so a design that
  # clears against one region and checks feasibility against the same region
  # cannot see the effect at all.
  sw <- exp11_overcommitment_sweep(lambda_2 = 2, kappa = 12)
  expect_lte(sw$x1, 12)                       # advertised: x1 <= kappa
  expect_lte(sw$x2, 12 / 2)                   # advertised: x2 <= kappa / lambda_2
  expect_lte(sw$x1 + sw$x2, 12)               # advertised: x1 + x2 <= kappa
  expect_gt(sw$x1 + 2 * sw$x2, 12)            # deliverable: violated
})

# ---- the inner-exposure control --------------------------------------------

test_that("the exposed catalogue rank is not submodular", {
  # f({s2,s3}) - f({s3}) = 0 < 1 = f({s1,s2,s3}) - f({s1,s3}), with f the
  # deliverable rank computed by the exact packer rather than asserted.
  A <- rbind(c(1, 0), c(0, 1), c(1, 1))
  f <- function(S) exact_pack_by_value(rep(1, length(S)), A[S, , drop = FALSE], c(1, 1))$value
  expect_equal(f(c(2L, 3L)) - f(3L), 0)
  expect_equal(f(1:3) - f(c(1L, 3L)), 1)
})

test_that("inner exposure is deliverable and its price is measurable", {
  ctl <- exp11_inner_exposure_control()
  raw   <- ctl[ctl$regime == "raw_catalogue", ]
  inner <- ctl[ctl$regime == "inner_exposure", ]

  expect_gt(raw$overcommitment, 1)            # the raw catalogue over-commits
  expect_equal(inner$overcommitment, 1)       # the inner box never does
  expect_equal(inner$forgone, raw$deliverable_optimum - inner$advertised)
  expect_gt(inner$forgone, 0)                 # the safe interface has a price
})

# ---- execution charges the admitted mix ------------------------------------

test_that("execution charges the admitted mix, not the count times the mean", {
  # The deliverable set has to be represented separately from the advertised
  # interface. The integrator advertises a slice capacity computed from the MEAN
  # recipe; four type-A plus two type-B tasks demand (10, 8, 9) where six
  # mean-recipe tasks demand (9, 9, 9). If execution charges the mean, the
  # over-admission the encapsulated arm commits is invisible and the drop rate
  # measures nothing.
  env <- exp11_env(cap = 12, N = 8L, hetero = TRUE)
  alloc <- tibble::tibble(
    task_id    = sprintf("t%d", 1:6),
    agent_id   = 1L,
    deadline   = 750,
    value_base = 1.5,
    recipe     = c("A", "A", "A", "A", "B", "B")
  )
  expect_equal(unname(colSums(task_recipes(alloc, env))), c(10, 8, 9))

  balanced <- alloc
  balanced$recipe <- c("A", "B", "A", "B", "A", "B")
  expect_equal(unname(colSums(task_recipes(balanced, env))), c(9, 9, 9))

  edge_heavy <- alloc
  edge_heavy$recipe <- c("B", "B", "B", "B", "A", "A")
  expect_equal(unname(colSums(task_recipes(edge_heavy, env))), c(8, 10, 9))

  lat <- function(a) { set.seed(1L); mean(execute_allocation(a, env)$latency) }

  # The direction is not the same for the two skews, and that is a property of
  # the DAG rather than noise: the tree's critical path visits device once and
  # edge twice, so loading edge lengthens it and loading device by the same
  # amount shortens it. What the assertion pins is that execution responds to
  # WHICH tasks were admitted at all.
  expect_gt(lat(edge_heavy), lat(balanced))
  expect_lt(lat(alloc),      lat(balanced))
})

test_that("the encapsulated arms are not identical once recipes differ", {
  het <- exp11_run_single("hetero_encapsulated", cap = 12, seed = 5L, n_rounds = 20L)
  hom <- exp11_run_single("homog_encapsulated",  cap = 12, seed = 5L, n_rounds = 20L)
  expect_false(isTRUE(all.equal(het$served_among_admitted, hom$served_among_admitted)))
})

# ---- the figure -------------------------------------------------------------

test_that("exp11_prepare bands the reported metrics and the figure renders", {
  raw <- tidyr::expand_grid(arm = exp11_arms(), cap = c(6, 9, 12), seed = 1:2) |>
    dplyr::mutate(price_cv = seq_len(24) / 24, greedy_exact_ratio = 0.99,
                  admitted_exact_ratio = 0.6, welfare_ratio = 0.8,
                  drop_rate = 0.5, served_among_admitted = 0.7)
  pre <- exp11_prepare(raw)

  expect_equal(nrow(pre), length(exp11_arms()) * 3L)
  expect_true(all(c("price_cv_mean", "price_cv_lo", "price_cv_hi",
                    "served_among_admitted_mean") %in% names(pre)))
  expect_equal(levels(pre$arm), exp11_arms())
  # A one-seed cell has no spread, so the band is NA rather than a zero-width
  # interval asserted as certainty.
  expect_true(all(is.na(exp11_prepare(dplyr::filter(raw, seed == 1L))$price_cv_lo)))

  expect_s3_class(make_exp11_tufte(raw), "ggplot")
})

# ---- the statistics stratify like their siblings ---------------------------

test_that("stat_exp11 stratifies by capacity", {
  # Capacity moves every metric hard (the drop rate runs 95 to 40 per cent
  # across the swept range), so pooling it treats cells that are not
  # exchangeable as replicates and can hide an arm effect inside cap variance.
  # Every sibling group_splits its secondary factor first; this one does too.
  set.seed(9L)
  raw <- tidyr::expand_grid(arm = exp11_arms(), cap = c(9, 12), seed = 1:3) |>
    dplyr::mutate(price_cv = runif(24), greedy_exact_ratio = runif(24),
                  admitted_exact_ratio = runif(24), welfare_ratio = runif(24),
                  drop_rate = runif(24))
  st <- stat_exp11(raw)

  expect_named(st, c("per_cap", "interaction"))
  expect_equal(length(st$per_cap), 2L)
  expect_equal(names(st$per_cap), c("9", "12"))
  expect_true(all(vapply(st$per_cap, \(x) "kruskal" %in% names(x), logical(1))))
  # Each stratum's test sees that capacity alone: 4 arms x 3 seeds.
  expect_true(all(st$per_cap[["9"]]$kruskal$n == 12L))
  expect_true(all(st$per_cap[["12"]]$kruskal$n == 12L))
  # ... and it still contributes to the single machine-written statistics dump.
  expect_gt(nrow(make_stats_report(list(exp11 = st))), 0L)
})
