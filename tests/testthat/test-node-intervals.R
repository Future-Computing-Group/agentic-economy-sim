# The released node-level intervals.
#
# Every bracketed node-level number is a Student-t 95 per cent interval over
# the evaluation seeds. The released target emits them, one row per block,
# cell and response, with the n behind each, so a bracket can be read off a
# file the pipeline writes rather than recomputed by hand.

test_that("the interval rows carry mean, t bounds and n for every present metric", {
  df <- tibble::tibble(graph_type = rep(c("tree", "sp"), each = 4),
                       seed = rep(1:4, 2),
                       welfare = c(1, 2, 3, 4, 5, 5, 5, 5),
                       stranded_demand = NA_real_)
  r <- node_interval_rows(df, "graph_type", block = "toy")
  expect_setequal(names(r), c("block", "cell_vars", "cell", "metric", "mean",
                              "lo", "hi", "n"))
  # A metric the frame lacks is not invented; one that is all NA says n = 0.
  expect_setequal(unique(r$metric), c("welfare", "stranded_demand"))
  w <- r[r$metric == "welfare" & r$cell == "tree", ]
  ci <- stats::t.test(1:4)$conf.int
  expect_equal(c(w$mean, w$lo, w$hi, w$n), c(2.5, ci[[1]], ci[[2]], 4))
  # No spread, no interval: the point is reported and the bounds are NA.
  flat <- r[r$metric == "welfare" & r$cell == "sp", ]
  expect_equal(flat$mean, 5)
  expect_true(is.na(flat$lo) && is.na(flat$hi))
  expect_equal(r$n[r$metric == "stranded_demand"], c(0L, 0L))
  expect_equal(unique(r$block), "toy")
})

test_that("a near-constant response is a point, not an error", {
  # A structural column that is constant up to rounding has a spread t.test
  # refuses to take an interval over; the mean is reported with no bounds.
  df <- tibble::tibble(cell = "a", seed = 1:10,
                       greedy_exact_ratio = c(1, rep(1 - 2e-16, 9)))
  r <- node_interval_rows(df, "cell", block = "toy")
  expect_equal(r$mean, mean(df$greedy_exact_ratio))
  expect_true(is.na(r$lo) && is.na(r$hi))
  expect_equal(r$n, 10L)
})

test_that("the block responses join the contrasted metrics", {
  m <- node_interval_metrics()
  expect_true(all(node_metrics() %in% m))
  expect_true(all(c("greedy_exact_incidence", "greedy_exact_worst",
                    "stranded_demand", "overcommitment") %in% m))
})

test_that("six printed manuscript brackets are the emitted t intervals", {
  # The per-seed rows of the six cells, taken from the pipeline store.
  fx <- readRDS(test_path("fixtures", "node-interval-cells.rds"))
  e1 <- node_interval_rows(fx$exp1, c("graph_type", "load_level"), block = "exp1")
  e4 <- node_interval_rows(fx$exp4, c("architecture", "graph_type", "load_level"),
                           block = "exp4")
  at <- function(r, cell, metric, digits) {
    x <- r[r$cell == cell & r$metric == metric, ]
    expect_equal(x$n, 10L)
    round(c(x$mean, x$lo, x$hi), digits)
  }
  expect_equal(at(e4, "naive_tree_high", "median_latency", 1), c(311.8, 307.8, 315.8))
  expect_equal(at(e4, "hybrid_ema_tree_high", "median_latency", 1), c(99.1, 97.2, 101.0))
  expect_equal(at(e1, "tree_high", "drop_rate", 4), c(0.3197, 0.3157, 0.3238))
  expect_equal(at(e1, "entangled_high", "drop_rate", 4), c(0.3482, 0.3419, 0.3544))
  expect_equal(at(e1, "sp_high", "drop_rate", 4), c(0.4300, 0.4215, 0.4386))
  expect_equal(at(e1, "tree_high", "welfare", 2), c(15.73, 15.17, 16.30))
})

test_that("the three node statistics targets name the node metric list", {
  src <- paste(readLines(here::here("_targets.R")), collapse = "\n")
  for (f in c("stat_exp1", "stat_exp4", "stat_exp6")) {
    n <- sub("stat_exp", "", f)
    expect_true(grepl(paste0("node_stats_exp", n, ", ", f, "\\(bind_rows\\(node_exp",
                             n, "_results_raw\\),\\s*metrics = node_metrics\\(\\)\\)"),
                      src, perl = TRUE), info = f)
  }
  expect_true(grepl("tar_target\\(\\s*node_intervals[,\\s]", src))
  expect_true(grepl("results/node-intervals.csv", src, fixed = TRUE))
})

test_that("the per-tier statistics are unchanged by the metrics argument", {
  set.seed(1)
  df <- tidyr::expand_grid(graph_type = c("tree", "sp"),
                           load_level = c("medium", "high"),
                           architecture = c("naive", "hybrid"),
                           seed = 1:6) %>%
    dplyr::mutate(median_latency = runif(dplyr::n()), drop_rate = runif(dplyr::n()),
                  utilisation = runif(dplyr::n()),
                  mean_price_volatility = runif(dplyr::n()),
                  welfare = runif(dplyr::n()), efficiency = runif(dplyr::n()))
  set.seed(2); a <- stat_exp1(df)
  set.seed(2); b <- stat_exp1(df, metrics = c("median_latency", "drop_rate",
                                              "utilisation", "mean_price_volatility",
                                              "welfare", "efficiency"))
  expect_identical(a, b)
  set.seed(2); a <- stat_exp4(df)
  set.seed(2); b <- stat_exp4(df, metrics = c("median_latency", "drop_rate", "welfare",
                                              "mean_price_volatility", "efficiency"))
  expect_identical(a, b)
})
