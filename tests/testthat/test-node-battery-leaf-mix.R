# test-node-battery-leaf-mix.R
# ---------------------------------------------------------------------------
# The battery ran at one leaf mix. The uniform mix spreads the arrivals evenly
# over the leaves, so no node is fed much harder than its own capacity, and
# every question the battery asks -- does the walk converge, is the price a
# property of the round, what does one moved report do, how does the market
# come back from a shock -- is asked in the easiest instance the substrate has.
#
# The skewed mix is added beside it. Only the bidirectional arm runs there:
# the ascending rule's rows answer an existence question stated for that rule
# on the instance it was stated on, and doubling them would answer a different
# question under the same name.
#
# The uniform rows keep their exact definitions and the skewed rows are bound
# below them, so the grids read as an extension of what they were.
# ---------------------------------------------------------------------------

uniform_rows <- function(g) dplyr::filter(g, leaf_mix == "uniform")
skewed_rows  <- function(g) dplyr::filter(g, leaf_mix == "skewed")

test_that("the convergence grid adds the skewed mix for the bidirectional arm", {
  g <- node_convergence_grid(n_seeds = 10L)

  expect_setequal(unique(g$leaf_mix), c("uniform", "skewed"))
  # 3 instances x 2 arms x 10 seeds x 4 budgets, and the skewed half of it.
  expect_equal(nrow(uniform_rows(g)), 3L * 2L * 10L * 4L)
  expect_equal(nrow(skewed_rows(g)), 3L * 1L * 10L * 4L)
  expect_equal(nrow(g), 240L + 120L)
  expect_setequal(unique(skewed_rows(g)$mechanism), "market")
  # The uniform rows come first and in the order they had.
  expect_equal(g$leaf_mix, rep(c("uniform", "skewed"), c(240L, 120L)))
})

test_that("the determinacy grid adds the skewed mix for the bidirectional arm", {
  g <- node_determinacy_grid(n_seeds = 10L)
  expect_equal(nrow(uniform_rows(g)), 3L * 2L * 10L)
  expect_equal(nrow(skewed_rows(g)), 3L * 1L * 10L)
  expect_setequal(unique(skewed_rows(g)$mechanism), "market")
  expect_equal(g$leaf_mix, rep(c("uniform", "skewed"), c(60L, 30L)))
})

test_that("the report-stability grid runs both mixes", {
  # No mechanism column: the block clears the bidirectional market and nothing
  # else, so both mixes run on every row.
  g <- node_report_stability_grid(n_seeds = 10L)
  expect_equal(nrow(uniform_rows(g)), 3L * 10L)
  expect_equal(nrow(skewed_rows(g)), 3L * 10L)
  expect_equal(g$leaf_mix, rep(c("uniform", "skewed"), c(30L, 30L)))
})

test_that("the shock grid adds the skewed mix for the bidirectional arm", {
  g <- node_shock_grid(n_seeds = 10L)
  expect_equal(nrow(uniform_rows(g)), 3L * 2L * 2L * 2L * 10L)
  expect_equal(nrow(skewed_rows(g)), 3L * 2L * 2L * 1L * 10L)
  expect_setequal(unique(skewed_rows(g)$mechanism), "market")
  expect_equal(g$leaf_mix, rep(c("uniform", "skewed"), c(240L, 120L)))
})

test_that("the ascending arm is never paired with the skewed mix", {
  for (g in list(node_convergence_grid(3L), node_determinacy_grid(3L),
                 node_shock_grid(3L))) {
    expect_equal(sum(g$mechanism == "market_asc" & g$leaf_mix == "skewed"), 0L)
  }
})

test_that("a row says which mix it ran", {
  # The mix travels in the run's own output, so a summary can never attribute a
  # round to a mix it was not drawn under.
  cv <- node_convergence_run("tree", seed = 1L, n_rounds = 2L, iters = 15L,
                             leaf_mix = "skewed")
  expect_setequal(cv$leaf_mix, "skewed")
  dt <- node_determinacy_run("tree", seed = 1L, n_rounds = 2L, iters = 15L,
                             leaf_mix = "skewed")
  expect_setequal(dt$leaf_mix, "skewed")
  rs <- node_report_stability_run("tree", seed = 1L, n_rounds = 1L,
                                  n_tasks = 2L, steps = 0, leaf_mix = "skewed")
  expect_setequal(rs$leaf_mix, "skewed")
  sh <- node_shock_run("tree", architecture = "naive", shock = "burst",
                       seed = 1L, n_rounds = 4L, shock_start = 2L,
                       burst_end = 3L, leaf_mix = "skewed")
  expect_setequal(sh$leaf_mix, "skewed")
})

test_that("the summaries keep the two mixes apart", {
  base <- tidyr::expand_grid(leaf_mix = c("uniform", "skewed"), round = 1:4)

  cv <- base %>% dplyr::mutate(graph_type = "tree", mechanism = "market",
                               iters = 15L, equilibrium_ok = leaf_mix == "uniform",
                               resid_excess = 0.1, admitted = 50)
  s <- node_convergence_summary(cv)
  expect_equal(nrow(s), 2L)
  expect_equal(s$converged_fraction[s$leaf_mix == "uniform"], 1)
  expect_equal(s$converged_fraction[s$leaf_mix == "skewed"], 0)

  dt <- base %>% dplyr::mutate(graph_type = "tree", mechanism = "market",
                               price_spread = ifelse(leaf_mix == "uniform", 0, 1),
                               sets_agree = TRUE, welfare_max = 2, welfare_min = 1)
  s <- node_determinacy_summary(dt)
  expect_equal(nrow(s), 2L)
  expect_equal(s$determinate[s$leaf_mix == "uniform"], 1)
  expect_equal(s$determinate[s$leaf_mix == "skewed"], 0)

  rs <- base %>%
    dplyr::mutate(graph_type = "tree", step = 0.01, seed = 1L, task_id = "t",
                  n_admitted = 50, own_changed = TRUE, welfare_delta = 0.5,
                  hamming = ifelse(leaf_mix == "uniform", 2, 6),
                  hamming_fixed = 2, own_changed_fixed = TRUE,
                  welfare_delta_fixed = 0.5)
  s <- node_report_stability_summary(rs)
  expect_equal(nrow(s), 2L)
  expect_equal(s$beyond_exchange[s$leaf_mix == "uniform"], 0)
  expect_equal(s$beyond_exchange[s$leaf_mix == "skewed"], 1)

  sh <- base %>%
    dplyr::mutate(graph_type = "tree", architecture = "naive", shock = "burst",
                  mechanism = "market", seed = 1L, shock_start = 1L,
                  equilibrium_ok = TRUE, resid_excess = 0, unit_cost = 1,
                  welfare = 1, welfare_control = ifelse(leaf_mix == "uniform", 1, 2),
                  admitted = 10, admitted_control = 10)
  s <- node_shock_summary(sh, hold = 1L, settled_tail = 2L)
  expect_equal(nrow(s), 2L)
  expect_equal(s$welfare_lost[s$leaf_mix == "uniform"], 0)
  expect_equal(s$welfare_lost[s$leaf_mix == "skewed"], 4)
})

test_that("the pipeline runs each battery branch at its row's mix", {
  src <- paste(readLines(here::here("_targets.R")), collapse = "\n")
  for (nm in c("node_exp6_convergence_grid", "node_exp6_determinacy_grid",
               "node_exp6_report_stability_grid", "node_exp6_shock_grid")) {
    expect_true(grepl(paste0("leaf_mix\\s*=\\s*", nm, "\\$leaf_mix"), src),
                info = nm)
  }
})
