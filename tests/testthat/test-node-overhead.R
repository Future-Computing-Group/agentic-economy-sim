# The encapsulation overhead on the node substrate.
#
# The per-tier driver charges an additive protocol-translation latency on the
# integrator's path and nothing on the uncontracted one. The node driver takes
# the same knob with the same semantics: the overhead is paid by tasks that
# cross the integrator interface, so it reaches the contracted architectures
# and is identically zero for the uncontracted ones. The references are off
# the market and do not pay it.

test_that("the overhead argument leaves a default run byte-identical", {
  a <- node_run_single("tree", "high", N = 90L, seed = 3L, n_rounds = 8L)
  b <- node_run_single("tree", "high", N = 90L, seed = 3L, n_rounds = 8L,
                       enc_overhead_ms = 0)
  expect_identical(a, b)
})

test_that("the overhead raises the contracted arm's latency", {
  lat <- function(d) node_run_single("tree", "high", N = 90L, seed = 3L,
                                     n_rounds = 8L, architecture = "hybrid_ema",
                                     enc_overhead_ms = d)$median_latency
  l0 <- lat(0); l50 <- lat(50)
  expect_gt(l50, l0)
})

test_that("the overhead never reaches the uncontracted arm", {
  run <- function(d) node_run_single("tree", "high", N = 90L, seed = 3L,
                                     n_rounds = 8L, architecture = "naive",
                                     enc_overhead_ms = d)
  expect_identical(run(0), run(50))
})

test_that("the overhead grid runs the uncontracted arm at zero only", {
  g <- node_overhead_grid(1:2)
  expect_setequal(unique(g$architecture),
                  c("naive", "hybrid_noema", "hybrid_ema"))
  expect_equal(unique(g$enc_overhead_ms[g$architecture == "naive"]), 0)
  expect_setequal(g$enc_overhead_ms[g$architecture == "hybrid_ema"],
                  c(0, 25, 50))
  expect_setequal(unique(g$graph_type), c("tree", "sp", "entangled"))
  expect_equal(unique(g$load_level), "high")
  # naive at one level plus two contracted arms at three, over three
  # instances, two seeds and both congestion levels.
  expect_equal(nrow(g), 7L * 3L * 2L * 2L)
  expect_equal(nrow(node_overhead_grid(seq_len(10))), 420L)
})

test_that("the overhead grid carries both congestion levels, baseline first", {
  g <- node_overhead_grid(1:2)
  lv <- node_congestion_levels()
  expect_equal(unique(g$congestion), c("baseline", "calibrated"))
  half <- nrow(g) / 2
  expect_true(all(g$congestion[seq_len(half)] == "baseline"))
  # The level decides the two parameters, exactly as in the mechanism grid.
  m <- match(g$congestion, lv$congestion)
  expect_equal(g$exec_clamp, lv$exec_clamp[m])
  expect_equal(g$queue_coef, lv$queue_coef[m])
  # The baseline block is the steep level, named explicitly now that the
  # driver's default is the calibrated one.
  expect_true(all(g$exec_clamp[seq_len(half)] == 0.99))
  expect_true(all(g$queue_coef[seq_len(half)] == 2))
})

test_that("a calibrated row run with its level's parameters is the default run", {
  a <- node_run_single("tree", "high", N = 90L, seed = 3L, n_rounds = 8L,
                       architecture = "hybrid_ema", enc_overhead_ms = 25)
  lv <- node_congestion_levels()
  b <- node_run_single("tree", "high", N = 90L, seed = 3L, n_rounds = 8L,
                       architecture = "hybrid_ema", enc_overhead_ms = 25,
                       exec_clamp = lv$exec_clamp[lv$congestion == "calibrated"],
                       queue_coef = lv$queue_coef[lv$congestion == "calibrated"])
  expect_identical(a, b)
})

test_that("the summary pairs each arm with naive at its own congestion level", {
  seeds <- 1:4
  cell <- function(arch, d, lat, cong) tibble::tibble(
    graph_type = "tree", load_level = "high", congestion = cong,
    architecture = arch, enc_overhead_ms = d, seed = seeds,
    median_latency = lat + seeds, welfare = 1, tokens_admitted = 1)
  raw <- dplyr::bind_rows(
    cell("naive", 0, 300, "baseline"), cell("hybrid_ema", 0, 280, "baseline"),
    cell("naive", 0, 120, "calibrated"), cell("hybrid_ema", 0, 100, "calibrated"),
    cell("hybrid_ema", 50, 130, "calibrated"))
  s <- node_overhead_summary(raw)
  expect_setequal(unique(s$congestion), c("baseline", "calibrated"))
  arm <- dplyr::filter(s, architecture == "hybrid_ema") %>%
    dplyr::arrange(congestion, enc_overhead_ms)
  # Against naive at the same level: 20 at baseline; 20 then -10 calibrated.
  expect_equal(arm$lead_median_latency, c(20, 20, -10))
  expect_equal(arm$latency_lead_zero_at[arm$congestion == "calibrated"],
               c(100 / 3, 100 / 3))
})

test_that("the overhead summary carries per-cell means and paired leads", {
  # A synthetic frame whose leads are known: the contracted arm is 30 ms
  # faster at zero overhead and loses 1 ms of that per millisecond charged,
  # so its latency lead reaches zero at 30.
  seeds <- 1:6
  cell <- function(arch, d, lat) tibble::tibble(
    graph_type = "tree", load_level = "high", architecture = arch,
    enc_overhead_ms = d, seed = seeds,
    median_latency = lat + seeds, welfare = 10 + seeds,
    tokens_admitted = 40 + seeds)
  raw <- dplyr::bind_rows(
    cell("naive", 0, 300),
    cell("hybrid_ema", 0, 270), cell("hybrid_ema", 25, 295),
    cell("hybrid_ema", 50, 320))
  s <- node_overhead_summary(raw)

  expect_true(all(c("architecture", "enc_overhead_ms", "median_latency",
                    "welfare", "tokens_admitted", "n_seeds",
                    "lead_median_latency", "lead_median_latency_lo",
                    "lead_median_latency_hi", "lead_welfare",
                    "lead_tokens_admitted",
                    "latency_lead_zero_at") %in% names(s)))
  expect_equal(nrow(s), 4L)
  # The uncontracted arm is the reference and leads nothing.
  base <- dplyr::filter(s, architecture == "naive")
  expect_true(is.na(base$lead_median_latency))
  arm <- dplyr::filter(s, architecture == "hybrid_ema") %>%
    dplyr::arrange(enc_overhead_ms)
  # A lead is positive where the contracted arm is better: lower latency,
  # higher welfare and more tokens.
  expect_equal(arm$lead_median_latency, c(30, 5, -20))
  expect_equal(arm$lead_welfare, c(0, 0, 0))
  expect_equal(unique(arm$latency_lead_zero_at), 30)
  expect_equal(unique(arm$n_seeds), length(seeds))
})

test_that("the summary reports no crossing the levels did not bracket", {
  seeds <- 1:4
  cell <- function(arch, d, lat) tibble::tibble(
    graph_type = "tree", load_level = "high", architecture = arch,
    enc_overhead_ms = d, seed = seeds,
    median_latency = lat + seeds, welfare = 1, tokens_admitted = 1)
  raw <- dplyr::bind_rows(
    cell("naive", 0, 300),
    cell("hybrid_noema", 0, 100), cell("hybrid_noema", 50, 150))
  s <- node_overhead_summary(raw)
  expect_true(all(is.na(dplyr::filter(s, architecture == "hybrid_noema")$latency_lead_zero_at)))
})


# ---- the overhead is paid on the exported path only --------------------------
#
# The protocol translation is paid by tasks that cross the integrator's
# interface, which are the tasks whose leaf the contracted cluster exports
# (L_J), and the agents see it at bid time as they see every other latency.

test_that("the exported leaf set is the cluster's leaves", {
  expect_equal(node_exported_leaves(node_instance("tree"), node_cluster("tree")),
               c("l1", "l2", "l3"))
  expect_equal(node_exported_leaves(node_instance("entangled"),
                                    node_cluster("entangled")),
               c("l1", "l2", "l3"))
  expect_equal(node_exported_leaves(node_instance("sp"), node_cluster("sp")),
               c("l1", "l2", "l3", "l4"))
})

ov_alloc <- function() tibble::tibble(
  task_id = c("in", "out"), agent_id = 1:2, deadline = 1000,
  value_base = 1.5, recipe = c("l1", "l4"))

test_that("execution charges the overhead only on a leaf the cluster exports", {
  env <- node_run_env("tree", "high", 90L, "uniform", "off")
  a0  <- execute_allocation(ov_alloc(), env, latency_noise_cv = 0)
  a50 <- execute_allocation(ov_alloc(), env, latency_noise_cv = 0,
                            enc_overhead_ms = 50, enc_leaves = c("l1", "l2", "l3"))
  expect_equal(a50$latency - a0$latency, c(50, 0))
  # Without an exported set the per-tier behaviour is kept: every path pays.
  ab <- execute_allocation(ov_alloc(), env, latency_noise_cv = 0,
                           enc_overhead_ms = 50)
  expect_equal(ab$latency - a0$latency, c(50, 50))
})

test_that("the bid sees the overhead on an exported leaf and nowhere else", {
  env <- node_run_env("tree", "high", 90L, "uniform", "inner")
  b0  <- node_bid_inputs(env, ov_alloc(), NULL)
  env$enc_overhead_leaf <- setNames(c(50, 50, 50, 0), c("l1", "l2", "l3", "l4"))
  b50 <- node_bid_inputs(env, ov_alloc(), NULL)
  expect_equal(b50$base_latency - b0$base_latency, c(50, 0))
})

test_that("agents respond to the overhead, so the admitted volume moves", {
  one <- function(d) node_run_single("tree", "high", N = 90L, seed = 3L,
                                     n_rounds = 8L, architecture = "hybrid_ema",
                                     enc_overhead_ms = d)
  expect_false(isTRUE(all.equal(one(0)$tokens_admitted, one(50)$tokens_admitted)))
})
