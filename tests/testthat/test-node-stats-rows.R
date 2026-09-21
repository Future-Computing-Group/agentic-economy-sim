# test-node-stats-rows.R
# ---------------------------------------------------------------------------
# The measured tables in the shape the machine-written report harvests.
#
# The report is the transcription source the supplement's numbers come from,
# and it collects frames named `statistics` under a cell. The frontier, the
# tuned comparison and the sensitivity sweep are measured tables rather than
# tests, so they carry a statistic, an n and no p-value, exactly as the onset
# law and the regret standard errors already do.
# ---------------------------------------------------------------------------

stats_rows_fixture <- function() {
  tibble(graph_type = c("tree", "tree", "sp", "sp"),
         load_level = "high", architecture = "naive",
         mechanism = c("market", "posted_price", "market", "posted_price"),
         welfare = c(10, 20, 30, 40), tokens_admitted = c(90, 60, 45, 30),
         n_seeds = 10L)
}


test_that("a measured table becomes one cell per row and one entry per metric", {
  out <- node_stats_rows(stats_rows_fixture(),
                         metrics = c("welfare", "tokens_admitted"),
                         group_var = "mechanism",
                         cell_vars = c("graph_type", "load_level", "architecture"),
                         n_col = "n_seeds")

  # The group the number belongs to travels in the cell label, which is what
  # the report keeps; the group_var column names the factor it varies over.
  expect_setequal(names(out), c("tree_high_naive_market",
                                "tree_high_naive_posted_price",
                                "sp_high_naive_market",
                                "sp_high_naive_posted_price"))
  one <- out[["tree_high_naive_market"]]$statistics
  expect_equal(one$metric, c("welfare", "tokens_admitted"))
  expect_equal(one$statistic, c(10, 90))
  expect_equal(one$group_var, rep("mechanism", 2))
  expect_equal(one$n, rep(10L, 2))
  # A measured table carries no test, so there is no p-value and no tripwire.
  expect_true(all(is.na(one$p_value)))
  expect_true(all(is.na(one$tripwire_ok)))

  # A metric the table does not carry is skipped rather than invented.
  expect_equal(nrow(node_stats_rows(stats_rows_fixture(), c("welfare", "nope"),
                                    "mechanism",
                                    c("graph_type"))[[1]]$statistics), 1L)
})

test_that("the report harvests the three measured tables under their own names", {
  rep <- make_stats_report(list(
    exp6_frontier = node_stats_rows(
      dplyr::mutate(stats_rows_fixture(),
                    welfare_vs_posted_at_matched_volume = c(1.4, NA, 1.2, NA)),
      "welfare_vs_posted_at_matched_volume", "mechanism",
      c("graph_type", "load_level", "architecture"), "n_seeds"),
    exp6_tuned = node_stats_rows(stats_rows_fixture(), "welfare", "mechanism",
                                 c("graph_type", "load_level", "architecture"),
                                 "n_seeds"),
    exp6_sensitivity = node_stats_rows(
      dplyr::mutate(stats_rows_fixture(), setting = "baseline"),
      "welfare", "mechanism",
      c("setting", "graph_type", "architecture"), "n_seeds")))

  expect_setequal(unique(rep$experiment),
                  c("exp6_frontier", "exp6_tuned", "exp6_sensitivity"))
  expect_true(any(grepl("market", rep$cell)))
  expect_equal(rep$statistic[rep$experiment == "exp6_tuned" &
                               rep$cell == "tree_high_naive_market"], 10)
  expect_setequal(names(rep),
                  c("experiment", "cell", "metric", "group_var", "statistic",
                    "df", "n", "p_value", "tripwire_ok"))
})

test_that("the pipeline's node report carries the three measured tables", {
  src <- paste(readLines(here::here("_targets.R")), collapse = " ")
  expect_true(grepl("exp6_frontier\\s*=\\s*node_stats_rows", src))
  expect_true(grepl("exp6_tuned\\s*=\\s*node_stats_rows", src))
  expect_true(grepl("exp6_sensitivity\\s*=\\s*node_stats_rows", src))
})


# ---- the frontier's rows are keyed by the posted level too ------------------
#
# The frontier is a curve: its posted rows differ from one another in nothing
# but the level they were run at. A key built without the level therefore
# holds every level of the family at once, and the transcription source can
# say neither which level a number came from nor which of them a ratio was
# read against.

frontier_rows_fixture <- function() {
  tidyr::expand_grid(graph_type = "tree", load_level = "high",
                     architecture = "naive", congestion = "calibrated",
                     mechanism = "posted_price", p_post_k = c(1, 2)) %>%
    dplyr::mutate(welfare = c(10, 20), n_seeds = 10L)
}

test_that("the posted level in the key gives every frontier row one of its own", {
  cv <- c("graph_type", "load_level", "architecture", "congestion")

  pooled <- node_stats_rows(frontier_rows_fixture(), "welfare", "mechanism",
                            cv, "n_seeds")
  expect_length(pooled, 1L)
  expect_equal(nrow(pooled[[1]]$statistics), 2L)

  out <- node_stats_rows(frontier_rows_fixture(), "welfare", "mechanism",
                         c(cv, "p_post_k"), "n_seeds")
  expect_setequal(names(out), c("tree_high_naive_calibrated_1_posted_price",
                                "tree_high_naive_calibrated_2_posted_price"))
  # No two rows of the report share a (cell, metric): one key, one number.
  rep <- make_stats_report(list(exp6_frontier = out))
  expect_equal(anyDuplicated(rep[, c("cell", "metric")]), 0L)
})

test_that("the pipeline keys the frontier rows by the posted level", {
  src <- paste(readLines(here::here("_targets.R")), collapse = " ")
  # The argument itself, not the call around it: the tuned call REPORTS the
  # chosen level as a metric, which is a different thing from keying on it.
  cell_vars <- function(nm) {
    call <- regmatches(src, regexpr(
      paste0(nm, "\\s*=\\s*node_stats_rows.*?n_col"), src, perl = TRUE))
    regmatches(call, regexpr("cell_vars\\s*=\\s*c\\([^)]*\\)", call,
                             perl = TRUE))
  }

  expect_true(grepl("p_post_k", cell_vars("exp6_frontier")))
  # The tuned table reports one row per mechanism per cell, so its key is
  # already unique and the chosen level stays a column rather than a label.
  expect_false(grepl("p_post_k", cell_vars("exp6_tuned")))
})
