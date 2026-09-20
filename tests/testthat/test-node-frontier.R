# test-node-frontier.R
# ---------------------------------------------------------------------------
# The posted-price family as a curve rather than as three arms.
#
# A posted level is a rationing dial: it moves admitted volume, and realised
# welfare moves with the congestion that volume creates. Comparing one level
# against a discovered price therefore compares two volumes as much as two
# mechanisms. The frontier puts every arm in the same (volume, latency,
# welfare) plane and reads the posted curve at each other arm's own realised
# volume and realised latency, paired inside a seed so the comparison never
# crosses two arrival streams.
# ---------------------------------------------------------------------------

test_that("the node mechanism grid refines the posted levels and leaves the per-tier grid alone", {
  g <- node_exp6_mechanism_grid(n_seeds = 10L)

  expect_setequal(unique(g$p_post_k[g$mechanism == "posted_price"]),
                  c(1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 3, 4))
  # The level travels with the posted arm alone: crossing it with the others
  # would run each of them nine times under a price it never posts.
  expect_setequal(unique(g$p_post_k[g$mechanism != "posted_price"]), 1)
  expect_equal(sum(g$mechanism == "k8s"), 3L * 2L * 2L * 10L)
  # The three levels the existing rows were run at are still in the grid, so
  # those rows reproduce.
  expect_true(all(c(1, 2, 4) %in% g$p_post_k))

  # The per-tier grid is a different experiment and keeps its own three levels.
  expect_setequal(
    unique(exp6_mechanism_grid(n_seeds = 10L)$p_post_k[
      exp6_mechanism_grid(n_seeds = 10L)$mechanism == "posted_price"]),
    c(1, 2, 4))
})


# ---- the frontier ----------------------------------------------------------

# One cell, two seeds, a posted curve of three levels and one other arm whose
# volume and latency both fall between two posted levels. Every number below is
# hand-computed from these rows.
.frontier_fixture <- function() {
  posted <- tidyr::expand_grid(seed = 1:2, p_post_k = c(1, 2, 4)) %>%
    dplyr::mutate(
      mechanism       = "posted_price",
      tokens_admitted = c(100, 80, 40)[match(p_post_k, c(1, 2, 4))],
      median_latency  = c(400, 200, 100)[match(p_post_k, c(1, 2, 4))],
      welfare         = c(10, 30, 20)[match(p_post_k, c(1, 2, 4))] + seed,
      arm_exact_ratio = 0.9)
  market <- tibble::tibble(
    seed = 1:2, p_post_k = 1, mechanism = "market",
    tokens_admitted = 90, median_latency = 300,
    welfare = c(25, 27), arm_exact_ratio = 0.95)
  dplyr::bind_rows(posted, market) %>%
    dplyr::mutate(graph_type = "tree", load_level = "high",
                  architecture = "naive")
}

test_that("the frontier reads the posted curve at each arm's own volume and latency", {
  f <- node_frontier_table(.frontier_fixture())
  m <- f[f$mechanism == "market", ]

  # At 90 tokens the posted curve interpolates between (100, 10 + s) and
  # (80, 30 + s), so the matched-volume reference is 20 + s and the ratio is
  # 25/21 on seed 1 and 27/22 on seed 2.
  ratios_v <- c(25 / 21, 27 / 22)
  expect_equal(m$welfare_vs_posted_at_matched_volume, mean(ratios_v))
  ci_v <- as.numeric(stats::t.test(ratios_v)$conf.int)
  expect_equal(c(m$welfare_vs_posted_at_matched_volume_lo,
                 m$welfare_vs_posted_at_matched_volume_hi), ci_v)

  # At 300 ms it interpolates between (400, 10 + s) and (200, 30 + s), so the
  # matched-latency reference is 20 + s as well.
  expect_equal(m$welfare_vs_posted_at_matched_latency, mean(ratios_v))

  # The posted family stays in the table as its own points, one per level,
  # with no matched column of its own: it IS the reference.
  p <- f[f$mechanism == "posted_price", ]
  expect_equal(nrow(p), 3L)
  expect_equal(p$tokens_admitted[order(p$p_post_k)], c(100, 80, 40))
  expect_equal(p$welfare[order(p$p_post_k)], c(11.5, 31.5, 21.5))
  expect_true(all(is.na(p$welfare_vs_posted_at_matched_volume)))
  expect_equal(unique(f$n_seeds), 2L)
  expect_setequal(f$alloc_ratio, c(0.9, 0.95))
})

test_that("an arm outside the posted family's measured range gets no matched reference", {
  # Extrapolating a concave curve past its last measured level would invent the
  # very number the frontier exists to measure.
  fix <- .frontier_fixture()
  fix$tokens_admitted[fix$mechanism == "market"] <- 120
  f <- node_frontier_table(fix)
  expect_true(is.na(f$welfare_vs_posted_at_matched_volume[f$mechanism == "market"]))
})

test_that("the frontier keeps the cells apart", {
  fix <- dplyr::bind_rows(.frontier_fixture(),
                          dplyr::mutate(.frontier_fixture(), load_level = "medium",
                                        welfare = welfare / 2))
  f <- node_frontier_table(fix)
  expect_setequal(f$load_level, c("high", "medium"))
  # Halving both the arm and its reference leaves the matched ratio alone.
  expect_equal(f$welfare_vs_posted_at_matched_volume[f$mechanism == "market" &
                                                       f$load_level == "medium"],
               f$welfare_vs_posted_at_matched_volume[f$mechanism == "market" &
                                                       f$load_level == "high"])
})

test_that("the pipeline branches the node mechanism block over the refined grid", {
  # The grid lives inside a tar_target call, which the constants reader cannot
  # reach, so the block is read off the pipeline source.
  src <- readLines(here::here("_targets.R"))
  expect_true(any(grepl("node_exp6_param_grid, node_exp6_mechanism_grid", src)))
  expect_true(any(grepl("tar_target(node_exp6_frontier,", src, fixed = TRUE)))
})
