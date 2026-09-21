# test-exp6-congestion-cells.R
# ---------------------------------------------------------------------------
# The mechanism block runs every arm at two congestion levels, so the level is
# a factor of the design and not a nuisance dimension to pool over. A summary
# that groups without it reports the mean of the two queue terms as though it
# were a setting the simulator was ever run at, and a test that pools them
# compares arms across two different economies inside one cell.
#
# Both the aggregated table and the statistics therefore carry the level.
# ---------------------------------------------------------------------------

cong_frame <- function(seeds = 1:3) {
  raw <- tidyr::expand_grid(
    mechanism    = c("market", "posted_price"),
    graph_type   = c("tree", "entangled"),
    load_level   = c("medium", "high"),
    architecture = c("naive", "hybrid"),
    congestion   = c("baseline", "calibrated"),
    seed         = seeds)
  set.seed(4)
  raw %>%
    dplyr::mutate(
      p_post_k = 1,
      # The level moves welfare hard, which is exactly why pooling it hides
      # the comparison the block exists to make.
      welfare = 10 + 20 * (congestion == "calibrated") +
        stats::runif(dplyr::n()),
      median_latency = 300 - 100 * (congestion == "calibrated") +
        stats::runif(dplyr::n()),
      drop_rate = stats::runif(dplyr::n()),
      mean_price_volatility = stats::runif(dplyr::n()),
      efficiency = stats::runif(dplyr::n()))
}


# ---- the aggregated table --------------------------------------------------

test_that("the summary keeps the two congestion levels apart", {
  raw <- cong_frame()
  by  <- c("mechanism", "p_post_k", "architecture", "graph_type",
           "load_level", "congestion")

  s <- node_aggregate(raw, by)
  # One row per arm per level, and each row is that level's own welfare.
  expect_equal(nrow(s), 2L * 2L * 2L * 2L * 2L)
  expect_equal(sum(s$congestion == "calibrated"), nrow(s) / 2L)
  expect_true(all(s$welfare[s$congestion == "baseline"] < 11))
  expect_true(all(s$welfare[s$congestion == "calibrated"] > 30))

  # Without the level the table reports the mean of the two, which is a queue
  # term no cell was ever run at.
  pooled <- node_aggregate(raw, setdiff(by, "congestion"))
  expect_equal(nrow(pooled), nrow(s) / 2L)
  expect_true(all(pooled$welfare > 19 & pooled$welfare < 22))
})

test_that("the pipeline groups the summary table by the congestion level", {
  src  <- paste(readLines(here::here("_targets.R")), collapse = " ")
  call <- regmatches(src, regexpr("node_exp6_summary_table,.*?\\)\\)\\)", src,
                                  perl = TRUE))
  expect_length(call, 1L)
  expect_true(grepl("congestion", call))
})


# ---- the statistics --------------------------------------------------------

test_that("stat_exp6 cells and per-architecture splits carry the level", {
  res <- stat_exp6(cong_frame())

  nms <- names(res$per_topo_load)
  expect_setequal(nms, c("tree_medium_naive_baseline",
                         "tree_medium_naive_calibrated",
                         "tree_medium_hybrid_baseline",
                         "tree_medium_hybrid_calibrated",
                         "tree_high_naive_baseline",
                         "tree_high_naive_calibrated",
                         "tree_high_hybrid_baseline",
                         "tree_high_hybrid_calibrated",
                         "entangled_medium_naive_baseline",
                         "entangled_medium_naive_calibrated",
                         "entangled_medium_hybrid_baseline",
                         "entangled_medium_hybrid_calibrated",
                         "entangled_high_naive_baseline",
                         "entangled_high_naive_calibrated",
                         "entangled_high_hybrid_baseline",
                         "entangled_high_hybrid_calibrated"))
  # 2 mechanisms x 3 seeds per cell, not 2 x 2 levels x 3 seeds.
  for (cell in res$per_topo_load) expect_true(all(cell$kruskal$n == 6L))

  expect_setequal(names(res$per_architecture),
                  c("naive_baseline", "naive_calibrated",
                    "hybrid_baseline", "hybrid_calibrated"))
  for (cell in res$per_architecture) expect_true(all(cell$kruskal$n == 24L))

  # The level is a factor of the interaction model too.
  expect_true(any(grepl("congestion", res$interaction$term)))
})


# ---- the machine-written report --------------------------------------------

test_that("the measured tables label their rows with the congestion level", {
  df <- tidyr::expand_grid(graph_type = "tree", load_level = "high",
                           architecture = "naive",
                           congestion = c("baseline", "calibrated"),
                           mechanism = c("market", "posted_price")) %>%
    dplyr::mutate(welfare = seq_len(dplyr::n()), n_seeds = 10L)

  out <- node_stats_rows(df, "welfare", "mechanism",
                         cell_vars = c("graph_type", "load_level",
                                       "architecture", "congestion"),
                         n_col = "n_seeds")
  expect_setequal(names(out), c("tree_high_naive_baseline_market",
                                "tree_high_naive_baseline_posted_price",
                                "tree_high_naive_calibrated_market",
                                "tree_high_naive_calibrated_posted_price"))
  # One row per key, so each number is attributable to the level it was run at.
  for (cell in out) expect_equal(nrow(cell$statistics), 1L)

  # Pooled, the two levels land under one key and neither can be read off it.
  pooled <- node_stats_rows(df, "welfare", "mechanism",
                            cell_vars = c("graph_type", "load_level",
                                          "architecture"),
                            n_col = "n_seeds")
  expect_equal(nrow(pooled[["tree_high_naive_market"]]$statistics), 2L)
})

test_that("the pipeline labels the frontier and tuned rows by the level", {
  src <- paste(readLines(here::here("_targets.R")), collapse = " ")
  for (nm in c("exp6_frontier", "exp6_tuned")) {
    call <- regmatches(src, regexpr(paste0(nm, "\\s*=\\s*node_stats_rows.*?n_col"),
                                    src, perl = TRUE))
    expect_length(call, 1L)
    expect_true(grepl("congestion", call), info = nm)
  }
})


test_that("a frame with no congestion column analyses exactly as it did", {
  res <- stat_exp6(dplyr::select(cong_frame(), -congestion))

  # No level to separate, so the cell label does not grow one, and the model
  # does not gain a factor with a single value.
  expect_setequal(names(res$per_topo_load),
                  c("tree_medium_naive", "tree_medium_hybrid",
                    "tree_high_naive", "tree_high_hybrid",
                    "entangled_medium_naive", "entangled_medium_hybrid",
                    "entangled_high_naive", "entangled_high_hybrid"))
  expect_setequal(names(res$per_architecture), c("naive", "hybrid"))
  expect_false(any(grepl("congestion", res$interaction$term)))
})
