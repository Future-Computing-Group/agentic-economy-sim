# Tests for the Exp.2 price-dispersion onset.
#
# Exp.2 scales the agent population per topology. Price dispersion is not a
# smooth function of N: below the point where the bottleneck tier starts to
# contend, every round clears at the reserve, so the dispersion column is
# identically zero and its Spearman correlation with N says nothing about where
# the transition is. The onset is therefore reported as its own statistic --
# the smallest population whose mean dispersion leaves zero -- and translated
# into the bottleneck offered load rho at that population, so the three
# topologies are comparable on load rather than on agent count.
#
# The grid has to reach far enough for every topology's onset to be inside it:
# tree is the slackest of the three and prices away from the reserve only well
# past the population where sp and entangled already have.

# ---- fixtures ---------------------------------------------------------------

# An exp2-shaped per-seed frame with a planted onset per topology: sp at N = 20
# on three seeds of four, tree at N = 30 on all four, entangled never.
onset_frame <- function() {
  tidyr::expand_grid(
    graph_type = c("tree", "sp", "entangled"),
    N          = c(10L, 20L, 30L),
    seed       = 1:4
  ) %>%
    dplyr::mutate(
      load_level            = "medium",
      mean_price_volatility = dplyr::case_when(
        graph_type == "sp"   & N == 20L & seed <= 3 ~ 0.01,
        graph_type == "sp"   & N == 30L             ~ 0.05,
        graph_type == "tree" & N == 30L             ~ 0.02,
        TRUE                                        ~ 0
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


# ---- the onset statistic ----------------------------------------------------

test_that("stat_exp2 reports the price-dispersion onset per topology", {
  onset <- suppressWarnings(stat_exp2(onset_frame()))$onset

  expect_s3_class(onset, "data.frame")
  expect_setequal(onset$graph_type, c("tree", "sp", "entangled"))
  expect_equal(nrow(onset), 3L)
  expect_true(all(c("N_onset", "seeds_nonzero_at_onset", "rho_onset") %in%
                    names(onset)))

  row <- function(gt) onset[onset$graph_type == gt, ]

  # The smallest N whose mean dispersion across seeds is above zero.
  expect_equal(row("sp")$N_onset, 20L)
  expect_equal(row("tree")$N_onset, 30L)
  # No N in the grid leaves zero: the onset is outside the grid, not at its top.
  expect_true(is.na(row("entangled")$N_onset))

  # The share of seeds already dispersing at the onset, which separates an
  # onset the whole ensemble crosses from one a single seed carries.
  expect_equal(row("sp")$seeds_nonzero_at_onset, 0.75)
  expect_equal(row("tree")$seeds_nonzero_at_onset, 1)
  expect_true(is.na(row("entangled")$seeds_nonzero_at_onset))
})

test_that("the onset's rho is the calibration criterion's rho at that N", {
  onset <- suppressWarnings(stat_exp2(onset_frame()))$onset
  row <- function(gt) onset[onset$graph_type == gt, ]

  # Same definition the operating point is calibrated on, evaluated at the
  # onset population and the load level the run was measured at -- not a
  # second copy of the formula.
  expect_equal(row("sp")$rho_onset, rho_bottleneck("sp", 20L, "medium"))
  expect_equal(row("tree")$rho_onset, rho_bottleneck("tree", 30L, "medium"))
  expect_true(is.na(row("entangled")$rho_onset))

  # A wrong topology or a wrong N would still be finite and positive, so the
  # equalities above are the contract; these only rule out a degenerate zero.
  expect_gt(row("sp")$rho_onset, 0)
  expect_gt(row("tree")$rho_onset, 0)
})

test_that("the onset reaches the flat statistics dump with its sample size", {
  rep <- suppressWarnings(make_stats_report(list(exp2 = stat_exp2(onset_frame()))))

  expect_setequal(rep$cell, c("tree", "sp", "entangled"))
  expect_setequal(unique(rep$metric),
                  c("exp2_onset_N", "exp2_onset_rho",
                    "exp2_onset_seeds_nonzero"))
  expect_equal(nrow(rep), 3L * 3L)
  # The CSV's columns are unchanged: the onset rows are more rows, not a
  # second shape.
  expect_equal(names(rep),
               c("experiment", "cell", "metric", "group_var", "statistic",
                 "df", "n", "p_value", "tripwire_ok"))

  sp_n <- rep[rep$cell == "sp" & rep$metric == "exp2_onset_N", ]
  expect_equal(sp_n$statistic, 20)
  expect_equal(sp_n$n, 4L)          # seeds contributing at the onset
})


# ---- the grid reaches the tree's onset --------------------------------------

test_that("the Exp.2 grid spans N = 10 to 120 in steps of 10", {
  grid <- eval(target_command("exp2_param_grid"),
               list2env(targets_constants("n_seeds"), parent = globalenv()))

  expect_equal(sort(unique(grid$N)), seq(10, 120, by = 10))
  expect_setequal(unique(grid$graph_type), c("tree", "sp", "entangled"))
  expect_equal(unique(grid$load_level), "medium")
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
