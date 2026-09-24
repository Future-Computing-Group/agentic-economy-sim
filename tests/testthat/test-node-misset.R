# A posted price that could not be set for the cell it runs in.
#
# Analysis of existing runs, no new ones: the tuned comparison picks each
# posted level inside the exact cell it is evaluated in. A level that must be
# committed before the operating condition is known is read here two ways,
# from the tuning runs: the level tuned at the OTHER load, and one level per
# instance pooled over the architectures. Each is then read off the frontier
# grid at that level in this cell, and contrasted seed by seed with the
# market arm the frontier ran on the same seeds.

misset_fixture <- function() {
  lv <- c(1, 2, 4)
  tuning <- tidyr::expand_grid(
    graph_type = "tree", load_level = c("medium", "high"),
    architecture = c("naive", "hybrid_noema"), congestion = "calibrated",
    mechanism = c("posted_price_fcfs", "market"), p_post_k = lv,
    reserve_markup = c(1, 2), seed = 1:2) %>%
    dplyr::filter((mechanism == "market" & p_post_k == 1) |
                    (mechanism != "market" & reserve_markup == 1)) %>%
    # fcfs peaks at 1 at medium and at 4 at high; naive and hybrid agree
    # except at high, where hybrid peaks at 2. The market peaks at markup 2.
    dplyr::mutate(welfare = dplyr::case_when(
      mechanism == "market" ~ -abs(reserve_markup - 2),
      load_level == "medium" ~ -abs(p_post_k - 1),
      architecture == "naive" ~ -abs(p_post_k - 4),
      TRUE ~ -abs(p_post_k - 2) * 0.1))
  frontier <- tidyr::expand_grid(
    graph_type = "tree", load_level = c("medium", "high"),
    architecture = c("naive", "hybrid_noema"), congestion = "calibrated",
    mechanism = c("posted_price_fcfs", "market"), p_post_k = lv,
    seed = 1:10) %>%
    dplyr::filter(mechanism != "market" | p_post_k == 1) %>%
    dplyr::mutate(welfare = ifelse(mechanism == "market", 10 + seed / 10,
                                   p_post_k + seed / 100))
  list(tuning = tuning, frontier = frontier)
}

test_that("the other-load variant runs the level the other load was tuned to", {
  f <- misset_fixture()
  m <- node_misset_posted(f$tuning, f$frontier, arms = "posted_price_fcfs")
  r <- m[m$variant == "other_load" & m$architecture == "naive", ]
  expect_equal(r$level_used[r$load_level == "medium"], 4)
  expect_equal(r$level_used[r$load_level == "high"], 1)
  expect_equal(r$level_tuned_here[r$load_level == "medium"], 1)
})

test_that("the pooled variant runs one level per instance over the architectures", {
  f <- misset_fixture()
  m <- node_misset_posted(f$tuning, f$frontier, arms = "posted_price_fcfs")
  r <- m[m$variant == "pooled_architecture" & m$load_level == "high", ]
  # naive peaks at 4 (loss 0 there, 0.2 for hybrid); hybrid peaks at 2 but
  # its losses are ten times smaller, so the pooled level is 4 on both.
  expect_equal(unique(r$level_used), 4)
})

test_that("the contrast is paired by seed against the frontier's market arm", {
  f <- misset_fixture()
  m <- node_misset_posted(f$tuning, f$frontier, arms = "posted_price_fcfs")
  r <- m[m$variant == "other_load" & m$architecture == "naive" &
           m$load_level == "medium", ]
  d <- (4 + (1:10) / 100) - (10 + (1:10) / 10)
  ci <- stats::t.test(d)$conf.int
  expect_equal(r$n, 10L)
  expect_equal(r$diff_mean, mean(d))
  expect_equal(c(r$diff_lo, r$diff_hi), as.numeric(ci))
  # The tuned market's markup in this cell is 2, so the frontier's market arm
  # (markup 1) is not the tuned one, and the row says so.
  expect_equal(r$market_tuned_markup, 2)
  expect_false(r$market_is_tuned)
})

test_that("a level the frontier grid never ran is reported missing, not read", {
  f <- misset_fixture()
  f$frontier <- f$frontier[!(f$frontier$mechanism == "posted_price_fcfs" &
                               f$frontier$p_post_k == 4), ]
  m <- node_misset_posted(f$tuning, f$frontier, arms = "posted_price_fcfs")
  r <- m[m$variant == "other_load" & m$architecture == "naive" &
           m$load_level == "medium", ]
  expect_false(r$level_on_frontier)
  expect_true(is.na(r$diff_mean))
})

test_that("the pipeline carries the mis-set analysis as its own target", {
  src <- paste(readLines(here::here("_targets.R")), collapse = "\n")
  expect_true(grepl("tar_target\\(node_exp6_misset_posted,", src))
})

test_that("the contrast against the tuned market is read on its held-out seeds", {
  f <- misset_fixture()
  eval_raw <- tidyr::expand_grid(
    graph_type = "tree", load_level = c("medium", "high"),
    architecture = c("naive", "hybrid_noema"), congestion = "calibrated",
    mechanism = "market", seed = 11:30) %>%
    dplyr::mutate(welfare = 12 + seed / 100)
  m <- node_misset_posted(f$tuning, f$frontier, arms = "posted_price_fcfs",
                          eval_raw = eval_raw)
  r <- m[m$variant == "other_load" & m$architecture == "naive" &
           m$load_level == "medium", ]
  x <- 4 + (1:10) / 100; y <- 12 + (11:30) / 100
  expect_equal(r$n_tuned_market, 20L)
  expect_equal(r$diff_tuned_mean, mean(x) - mean(y))
  expect_equal(c(r$diff_tuned_lo, r$diff_tuned_hi),
               as.numeric(stats::t.test(x, y)$conf.int))
})
