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
  new <- g[31:60, ]
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
                  br_gain_max = seed,
                  certificate = ifelse(graph_type == "tree", "certified",
                                       "uncertified"),
                  greedy_exact_incidence = ifelse(graph_type == "tree", 0, 0.2))
  s <- node_exp7b_summary(rows)
  expect_equal(nrow(s), 4L)
  expect_true(all(c("graph_type", "operating_point", "N", "cap_scale",
                    "n_seeds", "gain_mean", "gain_max", "certificate",
                    "certificate_ok", "exactness_incidence") %in% names(s)))
  # The verdict is the certifier's string, read as such.
  expect_equal(s$certificate_ok[s$graph_type == "tree"], c(TRUE, TRUE))
  expect_equal(s$certificate_ok[s$graph_type == "entangled"], c(FALSE, FALSE))
  expect_equal(unique(s$certificate[s$graph_type == "entangled"]), "uncertified")
  expect_equal(s$exactness_incidence[s$graph_type == "entangled"], c(0.2, 0.2))
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


# ---- the contention sweep ---------------------------------------------------

test_that("the sweep crosses agents with capacity, after the rows already run", {
  g <- node_exp7b_grid(1:10)
  g <- g[g$mechanism == "vcg", ]
  expect_equal(g$operating_point[1:60], rep(c("enumerable", "evaluation"), each = 30L))
  sw <- g[g$operating_point == "sweep", ]
  expect_true(all(which(g$operating_point == "sweep") > 60L))
  expect_setequal(unique(sw$N), c(8L, 15L, 30L, 45L, 90L))
  expect_setequal(unique(sw$cap_scale), c(0.1, 0.25, 0.5, 1.0))
  # No sweep row repeats a point already in the grid.
  key <- function(d) paste(d$graph_type, d$N, d$cap_scale, d$seed)
  expect_length(intersect(key(sw), key(g[1:60, ])), 0L)
  expect_false(any(duplicated(key(sw))))
  # Five populations by four capacities by three instances by ten seeds, less
  # the points the first sixty rows already run: T at (90, 1), S at (45, 1),
  # X at (8, 0.1) and (90, 1).
  expect_equal(nrow(sw), 5L * 4L * 3L * 10L - 4L * 10L)
})

test_that("the block reports the greedy shortfall incidence at every size", {
  r <- exp7b_run_single(graph_type = "entangled", load_level = "high", N = 8L,
                        seed = 1L, substrate = "node", cap_scale = 0.1,
                        lambda_l_default = node_lambda_l(), n_rounds = 4L)
  expect_true("greedy_exact_incidence" %in% names(r))
  expect_true(is.finite(r$greedy_exact_incidence))
  expect_gte(r$greedy_exact_incidence, 0)
  expect_lte(r$greedy_exact_incidence, 1)
})
