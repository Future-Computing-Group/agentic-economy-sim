# A crossed design's statistics name every factor: the per-cell summaries are
# cut on the cell variables the caller names, and the interaction model carries
# every factor of the design, so an architecture-by-governance experiment does
# not report an interaction table that omits architecture.

test_that("node_stat_factor crosses the named factors in cells and in the model", {
  set.seed(1)
  raw <- tidyr::expand_grid(architecture = c("naive", "hybrid_ema"),
                            policy = c("none", "locality"),
                            graph_type = c("tree", "sp", "entangled"),
                            load_level = c("medium", "high"),
                            seed = 1:3)
  raw$welfare <- stats::runif(nrow(raw), 10, 50) +
    ifelse(raw$architecture == "hybrid_ema", 20, 0)
  raw$drop_rate <- stats::runif(nrow(raw))
  st <- node_stat_factor(raw, "policy",
                         cell_vars = c("architecture", "graph_type", "load_level"),
                         interaction_vars = c("architecture", "policy"))
  expect_true("hybrid_ema_tree_high" %in% names(st$per_topo_load))
  expect_true(any(grepl("architecture", st$interaction$term)))
  expect_true(any(grepl("architecture:policy", st$interaction$term)))
})

test_that("the default call keeps the single-factor shape", {
  set.seed(2)
  raw <- tidyr::expand_grid(policy = c("none", "locality"),
                            graph_type = c("tree", "sp"),
                            load_level = c("medium", "high"), seed = 1:3)
  raw$welfare <- stats::runif(nrow(raw), 10, 50); raw$drop_rate <- stats::runif(nrow(raw))
  st <- node_stat_factor(raw, "policy")
  expect_setequal(names(st$per_topo_load),
                  c("tree_medium", "tree_high", "sp_medium", "sp_high"))
})
