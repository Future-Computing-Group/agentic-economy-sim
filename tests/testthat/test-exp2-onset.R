# Tests for the Exp.2 price-dispersion onset.
#
# Exp.2 scales the agent population per topology. Price dispersion is not a
# smooth function of N: below the point where the bottleneck tier starts to
# contend, every round clears at the reserve, so the dispersion column is
# identically zero and its Spearman correlation with N says nothing about where
# the transition is. The onset is therefore reported as its own statistic --
# the smallest population whose mean dispersion leaves zero -- and translated
# into the bottleneck offered load rho at that population, so the arms are
# comparable on load rather than on agent count.
#
# The sweep carries the same three synthetic topologies at two load levels and
# the measured agentic DAG, whose demand weights differ from all three. Whether
# the onset is a population or a load is decided by whether one rho covers all
# of them, so the onset is keyed on (graph_type, load_level), not on topology
# alone, and the grid has to reach each arm's onset at both loads.

# ---- fixtures ---------------------------------------------------------------

# An exp2-shaped per-seed frame with a planted onset per topology and load: sp
# at N = 20 on three seeds of four at medium and at N = 10 on all four at high,
# tree at N = 30 at medium and at N = 20 on half the seeds at high, entangled
# never at either load.
onset_frame <- function() {
  tidyr::expand_grid(
    graph_type = c("tree", "sp", "entangled"),
    load_level = c("medium", "high"),
    N          = c(10L, 20L, 30L),
    seed       = 1:4
  ) %>%
    dplyr::mutate(
      mean_price_volatility = dplyr::case_when(
        graph_type == "sp" & load_level == "medium" & N == 20L & seed <= 3 ~ 0.01,
        graph_type == "sp" & load_level == "medium" & N == 30L             ~ 0.05,
        graph_type == "sp" & load_level == "high"                          ~ 0.04,
        graph_type == "tree" & load_level == "medium" & N == 30L           ~ 0.02,
        graph_type == "tree" & load_level == "high" & N == 20L & seed <= 2 ~ 0.03,
        graph_type == "tree" & load_level == "high" & N == 30L             ~ 0.03,
        TRUE                                                               ~ 0
      ),
      median_latency = N + seed,
      drop_rate      = (N + seed) / 1000,
      utilisation    = (N + seed) / 200,
      welfare        = N * 10 + seed
    )
}

# Walk _targets.R for one target's command expression without building the
# pipeline: sourcing it would stand up a crew controller and the whole target
# list as a side effect.
target_command <- function(name) {
  found <- NULL
  descend <- function(e) {
    if (!is.call(e)) return(invisible(NULL))
    if (identical(e[[1]], as.name("tar_target")) && length(e) >= 3L &&
        identical(e[[2]], as.name(name))) {
      found <<- e[[3]]
    }
    for (i in seq_along(e)) {
      arg <- tryCatch(e[[i]], error = function(err) NULL)
      if (is.call(arg)) descend(arg)
    }
  }
  for (expr in parse(here::here("_targets.R"))) descend(expr)
  found
}

grid_of <- function(name) {
  eval(target_command(name),
       list2env(targets_constants("n_seeds"), parent = globalenv()))
}


# ---- the onset statistic ----------------------------------------------------

test_that("stat_exp2 reports the price-dispersion onset per topology and load", {
  onset <- suppressWarnings(stat_exp2(onset_frame()))$onset

  expect_s3_class(onset, "data.frame")
  expect_equal(nrow(onset), 6L)
  expect_setequal(paste(onset$graph_type, onset$load_level, sep = "_"),
                  c("tree_medium", "tree_high", "sp_medium", "sp_high",
                    "entangled_medium", "entangled_high"))
  expect_true(all(c("N_onset", "seeds_nonzero_at_onset", "rho_onset",
                    "N_onset_all_seeds", "rho_onset_all_seeds") %in%
                    names(onset)))

  row <- function(gt, ll) onset[onset$graph_type == gt & onset$load_level == ll, ]

  # The smallest N whose mean dispersion across seeds is above zero, per load.
  expect_equal(row("sp", "medium")$N_onset, 20L)
  expect_equal(row("sp", "high")$N_onset, 10L)
  expect_equal(row("tree", "medium")$N_onset, 30L)
  expect_equal(row("tree", "high")$N_onset, 20L)
  # No N in the grid leaves zero: the onset is outside the grid, not at its top.
  expect_true(is.na(row("entangled", "medium")$N_onset))
  expect_true(is.na(row("entangled", "high")$N_onset))

  # The share of seeds already dispersing at the onset, which separates an
  # onset the whole ensemble crosses from one a single seed carries.
  expect_equal(row("sp", "medium")$seeds_nonzero_at_onset, 0.75)
  expect_equal(row("tree", "high")$seeds_nonzero_at_onset, 0.5)
  expect_equal(row("tree", "medium")$seeds_nonzero_at_onset, 1)
  expect_true(is.na(row("entangled", "high")$seeds_nonzero_at_onset))
})

test_that("the all-seeds onset is the smallest N where no seed is still at zero", {
  onset <- suppressWarnings(stat_exp2(onset_frame()))$onset
  row <- function(gt, ll) onset[onset$graph_type == gt & onset$load_level == ll, ]

  # sp at medium crosses on three seeds of four at N = 20 and on all four at
  # N = 30, so the two onsets differ and the pair brackets the transition.
  expect_equal(row("sp", "medium")$N_onset, 20L)
  expect_equal(row("sp", "medium")$N_onset_all_seeds, 30L)
  expect_equal(row("sp", "medium")$rho_onset_all_seeds,
               rho_bottleneck("sp", 30L, "medium"))

  # Where the whole ensemble crosses at once the two coincide.
  expect_equal(row("sp", "high")$N_onset_all_seeds, 10L)
  expect_equal(row("tree", "high")$N_onset_all_seeds, 30L)
  expect_true(is.na(row("entangled", "medium")$N_onset_all_seeds))
  expect_true(is.na(row("entangled", "medium")$rho_onset_all_seeds))
})

test_that("the onset's rho is the calibration criterion's rho at that N and load", {
  onset <- suppressWarnings(stat_exp2(onset_frame()))$onset
  row <- function(gt, ll) onset[onset$graph_type == gt & onset$load_level == ll, ]

  # Same definition the operating point is calibrated on, evaluated at the
  # onset population and the load level the run was measured at -- not a
  # second copy of the formula.
  expect_equal(row("sp", "medium")$rho_onset, rho_bottleneck("sp", 20L, "medium"))
  expect_equal(row("sp", "high")$rho_onset, rho_bottleneck("sp", 10L, "high"))
  expect_equal(row("tree", "medium")$rho_onset, rho_bottleneck("tree", 30L, "medium"))
  expect_equal(row("tree", "high")$rho_onset, rho_bottleneck("tree", 20L, "high"))
  expect_true(is.na(row("entangled", "medium")$rho_onset))

  # The high-load rho is 1.5x the medium rho at the same N: a row that read its
  # load off the wrong group would be finite and positive but 1.5x wrong.
  expect_equal(rho_bottleneck("tree", 20L, "high"),
               1.5 * rho_bottleneck("tree", 20L, "medium"))
  expect_gt(row("sp", "medium")$rho_onset, 0)
})

test_that("the onset reaches the flat statistics dump keyed by topology and load", {
  rep <- suppressWarnings(make_stats_report(list(exp2 = stat_exp2(onset_frame()))))

  expect_setequal(rep$cell,
                  c("tree_medium", "tree_high", "sp_medium", "sp_high",
                    "entangled_medium", "entangled_high"))
  expect_setequal(unique(rep$metric),
                  c("exp2_onset_N", "exp2_onset_rho",
                    "exp2_onset_seeds_nonzero", "exp2_onset_N_all_seeds",
                    "exp2_onset_rho_all_seeds"))
  expect_equal(nrow(rep), 6L * 5L)
  # The CSV's columns are unchanged: the onset rows are more rows, not a
  # second shape.
  expect_equal(names(rep),
               c("experiment", "cell", "metric", "group_var", "statistic",
                 "df", "n", "p_value", "tripwire_ok"))

  sp_n <- rep[rep$cell == "sp_medium" & rep$metric == "exp2_onset_N", ]
  expect_equal(sp_n$statistic, 20)
  expect_equal(sp_n$n, 4L)          # seeds contributing at the onset
})


# ---- the measured agentic arm -----------------------------------------------

test_that("rho_bottleneck is on the edge tier of the measured agentic topology", {
  # Measured per-task demand weights against capacities 200/300/500. The busiest
  # tier is the edge tier, which carries both tool calls of every task, not the
  # cloud tier the heaviest single stage sits on.
  w <- agentic_demand_weights()
  capacity <- c(device = 200, edge = 300, cloud = 500)
  expect_equal(names(which.max(w / capacity)), "edge")
  expect_equal(rho_bottleneck("agentic", 100L, "medium"),
               max(w / capacity) * 1.0 * 100)
  expect_equal(rho_bottleneck("agentic", 100L, "high"),
               1.5 * rho_bottleneck("agentic", 100L, "medium"))
})

test_that("exp2_run_single runs the measured agentic environment", {
  res <- exp2_run_single(N = 5L, load_level = "medium", graph_type = "agentic",
                         seed = 1L, n_rounds = 2L,
                         deadlines = agentic_deadlines(),
                         lambda_l_default = agentic_lambda_l())

  expect_equal(nrow(res), 1L)
  expect_equal(res$graph_type, "agentic")
  expect_equal(res$N, 5L)
  # The measured environment's own base latencies, not the nominal 5/15/50: a
  # run that fell back to the nominal ones would land near 135 ms.
  expect_gt(res$median_latency, 3000)
})

test_that("the onset statistic carries the agentic arm through", {
  frame <- onset_frame() %>%
    dplyr::bind_rows(
      tidyr::expand_grid(graph_type = "agentic", load_level = "medium",
                         N = c(10L, 20L, 30L), seed = 1:4) %>%
        dplyr::mutate(
          mean_price_volatility = ifelse(N >= 20L, 0.02, 0),
          median_latency = N + seed,
          drop_rate      = (N + seed) / 1000,
          utilisation    = (N + seed) / 200,
          welfare        = N * 10 + seed
        )
    )
  onset <- suppressWarnings(stat_exp2(frame))$onset
  row <- onset[onset$graph_type == "agentic", ]

  expect_equal(nrow(row), 1L)
  expect_equal(row$N_onset, 20L)
  expect_equal(row$rho_onset, rho_bottleneck("agentic", 20L, "medium"))
})


# ---- the grids reach each arm's onset ---------------------------------------

test_that("the per-tier Exp.2 grid spans N = 10 to 120 at medium and at high load", {
  # The per-tier arm's own grid. The node sweep refines it to twenty-eight
  # points and is pinned in test-node-sweep.R; this one is unchanged, so the
  # branches already computed under it stay valid.
  grid <- grid_of("exp2_param_grid")

  expect_equal(sort(unique(grid$N)), seq(10, 120, by = 10))
  expect_setequal(unique(grid$graph_type), c("tree", "sp", "entangled"))
  expect_setequal(unique(grid$load_level), c("medium", "high"))
  expect_equal(nrow(grid), 3L * 12L * 10L * 2L)

  # The medium block is unchanged and still comes first, row for row, so its
  # already-computed branches stay valid.
  expect_equal(
    grid[seq_len(360L), ],
    tidyr::expand_grid(
      graph_type = c("tree", "sp", "entangled"),
      N          = seq(10, 120, by = 10),
      seed       = seq_len(10L),
      load_level = "medium"
    )
  )
})

test_that("the agentic grid brackets the load at which the synthetic arms price", {
  grid <- grid_of("exp2b_param_grid")

  expect_equal(sort(unique(grid$N)), seq(40, 200, by = 20))
  expect_equal(nrow(grid), 9L * 10L)

  # The synthetic onsets sit at rho 0.70 to 0.80. The agentic grid has to reach
  # both sides of that band and land a point inside it: one step is
  # 20 * 1.81 / 300 = 0.121 in rho, and N = 120 sits at 0.72.
  rho <- vapply(sort(unique(grid$N)),
                \(n) rho_bottleneck("agentic", n, "medium"), numeric(1))
  expect_lt(min(rho), 0.70)
  expect_gt(max(rho), 0.80)
  expect_true(any(rho > 0.70 & rho < 0.80))
})


# ---- the Spearman output is untouched ---------------------------------------

test_that("stat_exp2 still returns the per-topology Spearman correlations", {
  res <- suppressWarnings(stat_exp2(onset_frame()))

  expect_setequal(names(res$correlations), c("tree", "sp", "entangled"))
  for (d in res$correlations) {
    expect_equal(names(d), c("metric", "rho", "p_value"))
    expect_equal(d$metric, c("median_latency", "drop_rate", "utilisation",
                             "mean_price_volatility", "welfare"))
  }
})
