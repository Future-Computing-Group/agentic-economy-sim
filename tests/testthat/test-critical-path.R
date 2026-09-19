# Tests for critical_path_ms() — the DAG zero-queue/longest-path helper
# extracted from execute_allocation()'s inline traversal.
#
# The extraction is behaviour-neutral by contract, so the tests are (a) an
# independent oracle for the path value, (b) an end-to-end identity guard on a
# fixed cell, and (c) hand-computed paths that pin what the path is a function
# of: the TIER SEQUENCE along the longest path, not the hop count.

# ---- fixtures ---------------------------------------------------------------

# Independent oracle: exhaustive source-to-sink path enumeration. Deliberately
# NOT the topological-order algorithm under test — a copy of that would only
# prove the copy was faithful. The DAGs here have at most a handful of paths.
.longest_path_bruteforce <- function(graph, tier_latency) {
  lat  <- setNames(as.numeric(tier_latency[graph$nodes$tier]), graph$nodes$node)
  succ <- split(graph$edges$to, graph$edges$from)
  from_node <- function(v) {
    kids <- succ[[v]]
    if (is.null(kids)) return(lat[[v]])
    lat[[v]] + max(vapply(kids, from_node, numeric(1)))
  }
  max(vapply(graph$nodes$node, from_node, numeric(1)))
}

.mk_graph <- function(nodes, tiers, from, to) {
  list(
    nodes = tibble::tibble(node = nodes, tier = tiers),
    edges = tibble::tibble(from = from, to = to)
  )
}

.base_ms <- c(device = 5, edge = 15, cloud = 50)


# ---- the extracted helper matches an independent longest-path oracle --------

test_that("critical_path_ms matches a brute-force longest path on every topology", {
  for (gt in c("linear", "tree", "sp", "entangled", "agentic")) {
    g <- build_dependency_graph(gt)
    expect_equal(critical_path_ms(g, .base_ms),
                 .longest_path_bruteforce(g, .base_ms),
                 info = sprintf("%s at base latencies", gt))
    # A second, non-monotone latency vector: pins that the path is re-selected
    # per latency vector rather than fixed by the topology alone.
    alt <- c(device = 100, edge = 7, cloud = 1)
    expect_equal(critical_path_ms(g, alt),
                 .longest_path_bruteforce(g, alt),
                 info = sprintf("%s at alternative latencies", gt))
  }
})


# ---- hand-computed paths ----------------------------------------------------

test_that("critical_path_ms on a two-node DAG is the sum of its two tiers", {
  g <- .mk_graph(c("a", "b"), c("device", "cloud"), "a", "b")
  expect_equal(critical_path_ms(g, .base_ms), 55)     # 5 + 50
})

test_that("critical_path_ms on a three-node DAG takes the longer branch", {
  chain <- .mk_graph(c("a", "b", "c"), c("device", "edge", "cloud"),
                     c("a", "b"), c("b", "c"))
  expect_equal(critical_path_ms(chain, .base_ms), 70)  # 5 + 15 + 50

  fork <- .mk_graph(c("a", "b", "c"), c("device", "edge", "cloud"),
                    c("a", "a"), c("b", "c"))
  expect_equal(critical_path_ms(fork, .base_ms), 55)   # max(5 + 15, 5 + 50)
})

test_that("critical_path_ms uses the tier sequence, not the hop count", {
  # Two chains of equal length (four hops) whose tier sequences differ.
  heavy_tail <- .mk_graph(c("a", "b", "c", "d"),
                          c("device", "edge", "cloud", "cloud"),
                          c("a", "b", "c"), c("b", "c", "d"))
  light_head <- .mk_graph(c("a", "b", "c", "d"),
                          c("device", "device", "edge", "cloud"),
                          c("a", "b", "c"), c("b", "c", "d"))
  expect_equal(critical_path_ms(heavy_tail, .base_ms), 120)  # 5 + 15 + 50 + 50
  expect_equal(critical_path_ms(light_head, .base_ms), 75)   # 5 + 5 + 15 + 50
  expect_false(isTRUE(all.equal(critical_path_ms(heavy_tail, .base_ms),
                                critical_path_ms(light_head, .base_ms))))
})


# ---- end-to-end identity guard ---------------------------------------------

test_that("extracting critical_path_ms leaves exp4_run_single bit-identical", {
  # Hash guard for the extraction: the fixture was recorded from the inline
  # traversal before the refactor, on this exact cell and seed.
  #
  # The bid-time base latency was the constant 50 when the fixture was
  # recorded, because base_latency_for_bids' predicate was dead (see
  # test-base-latency-predicate.R). Fixing that predicate moves prices and the
  # metrics that depend on them, which is a deliberate behaviour change and NOT
  # a licence to re-record this fixture: the guard exists to show that the
  # EXTRACTION moved nothing. So it is held at the old constant here, and the
  # new behaviour is asserted in its own file. Re-recording this fixture would
  # destroy the only evidence that the refactor was neutral.
  # (testthat::local_mocked_bindings needs a package namespace; this repo is a
  # project-style codebase sourced into the global environment.)
  rlang::local_bindings(base_latency_for_bids = function(env) 50,
                        .env = globalenv())
  expected <- readRDS(test_path("fixtures", "exp4-naive-sp-medium-n20-seed1.rds"))
  actual   <- exp4_run_single("naive", "sp", "medium",
                              N = 20L, seed = 1L, n_rounds = 20L)
  # Compared on the columns the fixture recorded. Summary columns added since
  # are further statistics of the same runs and cannot move the recorded ones;
  # re-recording the fixture to widen it would destroy the pre-refactor
  # evidence this guard exists to carry.
  expect_identical(actual[names(expected)], expected)
})
