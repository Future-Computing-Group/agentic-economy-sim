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
  # instances and two seeds.
  expect_equal(nrow(g), 7L * 3L * 2L)
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
