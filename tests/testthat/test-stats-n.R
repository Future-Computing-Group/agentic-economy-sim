# Tests for the statistics-plumbing contract: every statistic travels with the
# sample size it was computed on, and the Kruskal-Wallis cells are the cells the
# manuscript describes.
#
# The defect these pin: `stat_exp6` split the per-seed rows by topology and load
# only, so each mechanism test ran on 4 mechanisms x 2 architectures x 10 seeds
# = 80 observations while the text described a 40-observation design. Neither
# object carried an N, so the two were never comparable.

# ---- fixtures ---------------------------------------------------------------

# A well-behaved three-group frame: 5 observations per group, no missing values.
kw_frame <- function(n_per = 5L) {
  tibble::tibble(
    grp = rep(c("a", "b", "c"), each = n_per),
    val = c(seq_len(n_per), seq_len(n_per) + 10, seq_len(n_per) + 20) * 1.0
  )
}

# The same frame with non-finite values and a missing group label injected, so
# the count the test consumed differs from nrow().
kw_frame_dirty <- function() {
  d <- kw_frame()
  d$val[1] <- NA_real_
  d$val[2] <- Inf
  d$grp[nrow(d)] <- NA_character_
  d
}

# The arms of the mechanism ablation: the four original ones, and the same four
# once the deployed-practice arms land, where the posted price is three arms
# rather than one because it is posted at three markups.
exp6_arms <- function() {
  tibble::tibble(mechanism = c("market", "greedy_ev", "edf", "random"), p_post_k = 1)
}

exp6_deployed_arms <- function() {
  dplyr::bind_rows(
    exp6_arms(),
    tibble::tibble(mechanism = "k8s", p_post_k = 1),
    tibble::tibble(mechanism = "posted_price", p_post_k = c(1, 2, 4))
  )
}

# An exp6-shaped per-seed frame: arms x 2 architectures x 10 seeds in a single
# topology x load cell, which is 40 observations per architecture on the four
# original arms and 80 once the deployed-practice arms are in.
exp6_frame <- function(n_seeds = 10L, arms = exp6_arms()) {
  grid <- tidyr::expand_grid(
    arms,
    architecture = c("naive", "hybrid"),
    graph_type   = "entangled",
    load_level   = "high",
    seed         = seq_len(n_seeds)
  )
  set.seed(7L)
  dplyr::mutate(
    grid,
    median_latency        = runif(dplyr::n(), 10, 100),
    drop_rate             = runif(dplyr::n()),
    welfare               = runif(dplyr::n()) + match(mechanism, arms$mechanism),
    mean_price_volatility = runif(dplyr::n()),
    efficiency            = runif(dplyr::n())
  )
}


# ---- n travels with every statistic -----------------------------------------

test_that("kruskal_test returns n equal to the number of observations used", {
  res <- kruskal_test(kw_frame(), "grp", "val")

  expect_true("n" %in% names(res))
  expect_equal(res$n, 15L)
})

test_that("kruskal_test's n counts the filtered observations, not nrow()", {
  d   <- kw_frame_dirty()
  res <- kruskal_test(d, "grp", "val")

  # 15 rows in, three dropped (NA value, Inf value, NA group).
  expect_equal(nrow(d), 15L)
  expect_equal(res$n, 12L)
})

test_that("kruskal_test returns n on the degenerate early-return path", {
  one_group <- tibble::tibble(grp = rep("a", 5L), val = as.numeric(1:5))
  too_few   <- tibble::tibble(grp = c("a", "b"), val = c(1, 2))

  r1 <- kruskal_test(one_group, "grp", "val")
  r2 <- kruskal_test(too_few, "grp", "val")

  expect_true("n" %in% names(r1))
  expect_true("n" %in% names(r2))
  expect_equal(r1$n, 5L)
  expect_equal(r2$n, 2L)
  # bind_rows over a mix of paths must not produce a ragged frame.
  expect_equal(nrow(dplyr::bind_rows(r1, r2, kruskal_test(kw_frame(), "grp", "val"))), 3L)
})


# ---- the tripwire -----------------------------------------------------------

test_that("the tripwire passes on a real fixture and fails on an impossible H", {
  ok <- kruskal_test(kw_frame(), "grp", "val")
  expect_true("tripwire_ok" %in% names(ok))
  expect_true(ok$tripwire_ok)

  # Hand-constructed violation: H equal to n exceeds the n - 1 ceiling.
  expect_false(kruskal_tripwire_ok(H = 15, n = 15L))
  expect_true(kruskal_tripwire_ok(H = 13.9, n = 15L))
  # The degenerate path has no statistic to check.
  expect_true(kruskal_tripwire_ok(H = NA_real_, n = 2L))
})

test_that("an impossible H reaches the emitted column and raises a warning", {
  # No real sample can produce H > n - 1, so the only way to exercise the wire
  # between the check and the emitted row is to hand kruskal_test an impossible
  # statistic. Asserting the helper in isolation leaves that wire untested: the
  # column could be hardcoded TRUE, or the warning deleted, and stay green.
  impossible <- function(...) {
    list(statistic = c(`Kruskal-Wallis chi-squared` = 99), parameter = c(df = 2),
         p.value = 0.1)
  }

  res <- with_mocked_bindings(
    kruskal_test(kw_frame(), "grp", "val"),
    kruskal.test = impossible, .package = "stats"
  ) %>% suppressWarnings()

  expect_equal(res$H, 99)
  expect_equal(res$n, 15L)
  expect_false(res$tripwire_ok)

  expect_warning(
    with_mocked_bindings(
      kruskal_test(kw_frame(), "grp", "val"),
      kruskal.test = impossible, .package = "stats"
    ),
    "tripwire"
  )
})


# ---- exp6 cells are the cells the manuscript describes ----------------------

test_that("stat_exp6 splits the per-topology-by-load cells by architecture", {
  res <- stat_exp6(exp6_frame())
  nms <- names(res$per_topo_load)

  expect_true(all(grepl("naive|hybrid", nms)))
  expect_setequal(nms, c("entangled_high_naive", "entangled_high_hybrid"))

  for (cell in res$per_topo_load) {
    # 4 mechanisms x 10 seeds, not 4 x 2 architectures x 10 seeds.
    expect_true(all(cell$kruskal$n == 40L))
  }

  # A store written before the markup column analyses unchanged: no markup
  # column means no posted-price arm to separate.
  stale <- stat_exp6(dplyr::select(exp6_frame(), -p_post_k))
  expect_equal(nrow(stale$per_topo_load[["entangled_high_naive"]]$ci), 4L)
})

test_that("stat_exp6 tests the posted-price markups as separate arms", {
  res  <- stat_exp6(exp6_frame(arms = exp6_deployed_arms()))
  cell <- res$per_topo_load[["entangled_high_naive"]]

  # 8 arms x 10 seeds, in a cell where every arm contributes a finite value for
  # every metric, which is what this synthetic frame is. On the real grid an arm
  # that admits nothing has no latency to report, kruskal_test drops the group,
  # and the emitted n falls below 80 for that metric: what a quoted statistic
  # travels with is its own emitted n, never a flat cell size.
  expect_true(all(cell$kruskal$n == 80L))
  expect_equal(nrow(cell$ci), 8L)
})


# ---- the flat dump ----------------------------------------------------------

test_that("stats_report has one row per test with the required columns", {
  s6  <- stat_exp6(exp6_frame())
  rep <- make_stats_report(list(exp6 = s6))

  expect_true(all(c("experiment", "cell", "metric", "statistic",
                    "df", "n", "p_value") %in% names(rep)))
  # 2 architecture cells + 2 collapsed per-architecture cells, 5 metrics each.
  expect_equal(nrow(rep), 4L * 5L)
  expect_true(all(!is.na(rep$n)))
  expect_equal(sum(rep$n[rep$cell == "per_topo_load/entangled_high_naive"]), 5L * 40L)
})

test_that("stats_report warns when a listed experiment contributes no rows", {
  # stat_exp2 reports Spearman correlations, not Kruskal-Wallis tests, so it
  # contributes nothing to the dump. A listed experiment must never vanish from
  # the transcription source in silence.
  no_kruskal <- list(correlations = list(
    tree = tibble::tibble(metric = "welfare", rho = 0.99, p_value = 1e-6)
  ))

  expect_warning(make_stats_report(list(exp2 = no_kruskal)),
                 "contributed no rows")
})


# ---- pinned value from the pipeline store -----------------------------------

test_that("the entangled/high naive mechanism cell reproduces H = 27.53 on n = 40", {
  skip_if_not(requireNamespace("targets", quietly = TRUE), "targets not installed")
  store <- here::here("_targets")
  skip_if_not(dir.exists(store), "targets store not present")

  raw <- tryCatch(
    dplyr::bind_rows(targets::tar_read(exp6_results_raw, store = store)),
    error = function(e) NULL
  )
  skip_if(is.null(raw), "exp6_results_raw not in the store")

  # The pinned value belongs to the four-mechanism design, so it is computed on
  # exactly those four arms. Their per-seed rows are unchanged by the arms added
  # beside them, so filtering keeps the pin alive across the rebuild rather than
  # letting it go dormant the moment the store holds eight arms.
  originals <- c("random", "edf", "greedy_ev", "market")
  skip_if(!all(originals %in% raw$mechanism), "store lacks the four original arms")
  raw <- dplyr::filter(raw, mechanism %in% originals)

  res  <- stat_exp6(raw)
  cell <- res$per_topo_load[["entangled_high_naive"]]
  kw   <- cell$kruskal[cell$kruskal$metric == "welfare", ]

  expect_equal(kw$n, 40L)
  # Pinned on the store built at unit efficiency, the common price step and the
  # per-topology operating point; the value the supplement quotes.
  expect_equal(round(kw$H, 2), 27.53)
})
