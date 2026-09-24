# The joint-misreport block at two operating points.
#
# The converse on the crossing instance was run at an operating point small
# enough to enumerate (8 agents, a tenth of the capacity). The evaluation
# point every other block reports is run beside it, for all three instances,
# so the gain can be read at both.

test_that("the grid keeps the enumerable rows first and adds the evaluation point", {
  g <- node_exp7b_grid(1:10)
  old <- g[seq_len(30L), ]
  expect_true(all(old$operating_point == "enumerable"))
  expect_equal(old$graph_type, rep(c("tree", "sp", "entangled"), each = 10L))
  expect_equal(old$seed, rep(1:10, 3L))
  # The rows as the block ran them: entangled at 8 agents and a tenth of the
  # capacity, the laminar instances at their evaluation populations.
  expect_equal(unique(old$N[old$graph_type == "entangled"]), 8L)
  expect_equal(unique(old$cap_scale[old$graph_type == "entangled"]), 0.1)
  expect_equal(unique(old$N[old$graph_type == "tree"]), node_agents()[["tree"]])
  new <- g[-seq_len(30L), ]
  expect_true(all(new$operating_point == "evaluation"))
  expect_equal(nrow(new), 30L)
  expect_equal(unname(new$N), unname(node_agents()[new$graph_type]))
  expect_true(all(new$cap_scale == 1))
})

test_that("the summary reports both points side by side", {
  rows <- tidyr::expand_grid(graph_type = c("tree", "entangled"),
                             operating_point = c("enumerable", "evaluation"),
                             seed = 1:3) %>%
    dplyr::mutate(N = 8L, cap_scale = 0.1, br_gain_mean = seed / 10,
                  br_gain_max = seed, certificate = graph_type == "tree")
  s <- node_exp7b_summary(rows)
  expect_equal(nrow(s), 4L)
  expect_true(all(c("graph_type", "operating_point", "N", "cap_scale",
                    "n_seeds", "gain_mean", "gain_max") %in% names(s)))
  expect_equal(unique(s$gain_mean), 0.2)
  expect_equal(unique(s$gain_max), 3)
  expect_equal(unique(s$n_seeds), 3L)
})

test_that("the pipeline runs the block over its own grid", {
  src <- paste(readLines(here::here("_targets.R")), collapse = "\n")
  expect_true(grepl("node_exp7b_param_grid, node_exp7b_grid(seq_len(n_seeds))",
                    src, fixed = TRUE))
  expect_true(grepl("node_exp7b_summary(bind_rows(node_exp7b_results_raw))",
                    src, fixed = TRUE))
})
