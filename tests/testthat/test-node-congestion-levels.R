# test-node-congestion-levels.R
# ---------------------------------------------------------------------------
# The mechanism block at both congestion levels.
#
# The queue term the arms are compared under is a parameter, and one of its
# settings has an outside measurement behind it: the level at which the
# simulator's latency response to offered load matches what the emulated
# testbed produced on real inference. A comparison reported at one level and
# a sensitivity reported at the other are two different papers, so the
# frontier, the tuned table and the mixed arm run at both.
# ---------------------------------------------------------------------------

test_that("the congestion levels are the sweep's own baseline and calibrated rows", {
  lv <- node_congestion_levels()
  expect_setequal(lv$congestion, c("baseline", "calibrated"))
  expect_setequal(names(lv), c("congestion", "exec_clamp", "queue_coef"))

  # One source for the calibrated setting: the sweep's row and this factor's
  # level are the same numbers, not two copies that drift.
  s <- node_sensitivity_settings()
  for (nm in c("baseline", "calibrated")) {
    expect_equal(lv$exec_clamp[lv$congestion == nm],
                 s$exec_clamp[s$setting == nm], info = nm)
    expect_equal(lv$queue_coef[lv$congestion == nm],
                 s$queue_coef[s$setting == nm], info = nm)
  }
  # The baseline level is the driver's own default, so its rows are the rows
  # the block already ran.
  expect_equal(lv$exec_clamp[lv$congestion == "baseline"], 0.99)
  expect_equal(lv$queue_coef[lv$congestion == "baseline"], 2)
})

test_that("every grid of the mechanism block carries both levels", {
  grids <- list(mechanism = node_exp6_mechanism_grid(n_seeds = 10L),
                tuning    = node_tuning_grid(c(1L, 2L)))
  for (nm in names(grids)) {
    g <- grids[[nm]]
    expect_setequal(g$congestion, c("baseline", "calibrated"))
    expect_true(all(c("exec_clamp", "queue_coef") %in% names(g)), info = nm)
    # The level decides the two parameters, so no row carries a setting its
    # level does not name.
    expect_equal(g$exec_clamp, ifelse(g$congestion == "baseline", 0.99, 0.95),
                 info = nm)
    expect_equal(g$queue_coef, ifelse(g$congestion == "baseline", 2, 0.75),
                 info = nm)
  }
})

test_that("the baseline half of the mechanism grid is the grid as it was", {
  # The rows the block already ran must still be there, at the driver's own
  # defaults, so the baseline numbers reproduce.
  g <- node_exp6_mechanism_grid(n_seeds = 10L)
  base <- g[g$congestion == "baseline",
            c("mechanism", "p_post_k", "graph_type", "load_level",
              "architecture", "seed")]
  expect_equal(nrow(base), 2400L)
  expect_equal(nrow(g), 2L * 2400L)
  expect_equal(sum(g$congestion == "calibrated"), 2400L)
  expect_equal(nrow(dplyr::distinct(base)), nrow(base))
})

test_that("the knob is tuned inside its own congestion level", {
  # A knob chosen at one queue term and reported at another is a knob chosen
  # on the wrong economy.
  knobs <- tidyr::expand_grid(
    graph_type = "tree", load_level = "high", architecture = "naive",
    congestion = c("baseline", "calibrated"),
    mechanism = "posted_price", p_post_k = c(1, 2), reserve_markup = 1,
    seed = 1:2)
  knobs$welfare <- ifelse(knobs$congestion == "baseline",
                          knobs$p_post_k, 3 - knobs$p_post_k)

  eg <- node_eval_grid(knobs, seeds = 11:12)
  expect_setequal(eg$congestion, c("baseline", "calibrated"))
  posted <- eg[eg$mechanism == "posted_price", ]
  expect_equal(posted$p_post_k[posted$congestion == "baseline"][1], 2)
  expect_equal(posted$p_post_k[posted$congestion == "calibrated"][1], 1)
  # The arms with nothing to tune are evaluated at both levels too, and every
  # row carries the parameters its level names.
  expect_setequal(eg$mechanism[eg$congestion == "calibrated"],
                  c("posted_price", "greedy_ev", "k8s"))
  expect_equal(eg$queue_coef, ifelse(eg$congestion == "baseline", 2, 0.75))
})

test_that("the tuned table and the frontier are keyed by the congestion level", {
  raw <- tidyr::expand_grid(
    graph_type = "tree", load_level = "high", architecture = "naive",
    congestion = c("baseline", "calibrated"),
    mechanism = c("market", "posted_price"), seed = 1:2) %>%
    dplyr::mutate(p_post_k = 1, reserve_markup = 1, tokens_admitted = 50,
                  median_latency = ifelse(congestion == "baseline", 300, 100),
                  welfare = ifelse(congestion == "baseline", 10, 40),
                  welfare_over_optimum = 0.5, alloc_ratio_true = 0.9,
                  n_eval_seeds = 2L)

  tuned <- node_tuned_table(raw)
  expect_true("congestion" %in% names(tuned))
  expect_equal(nrow(tuned), 4L)
  expect_setequal(tuned$welfare, c(10, 40))

  front <- node_frontier_table(raw)
  expect_true("congestion" %in% names(front))
  expect_equal(nrow(front), 4L)
})

test_that("the pipeline passes the level's queue term to every branched target", {
  src <- paste(readLines(here::here("_targets.R")), collapse = " ")
  for (g in c("node_exp6_param_grid", "node_exp6_tuning_grid",
              "node_exp6_eval_grid")) {
    expect_true(grepl(paste0("exec_clamp\\s*=\\s*", g, "\\$exec_clamp"), src),
                info = g)
    expect_true(grepl(paste0("queue_coef\\s*=\\s*", g, "\\$queue_coef"), src),
                info = g)
  }
})
