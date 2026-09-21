# test-exp6-interaction-balance.R
# ---------------------------------------------------------------------------
# The mechanism block's design is not complete: the mixed pricing rule needs a
# slice to post a price for, so it runs on the contracted architecture alone.
# A crossed model fitted on the whole frame therefore has empty cells, its
# mechanism-by-architecture columns are aliased, and the interaction comes back
# missing -- the one table that says whether the ordering of the arms depends
# on the architecture.
#
# The model is fitted instead on the arms every cell of the crossed factors
# ran, and the arms that could not enter it are reported beside it rather than
# dropped silently.
# ---------------------------------------------------------------------------

# One arm absent from one architecture, everything else crossed.
.exp6_frame <- function(seeds = 1:3) {
  full <- tidyr::expand_grid(
    mechanism    = c("market", "greedy_ev"),
    graph_type   = c("tree", "sp"),
    load_level   = c("medium", "high"),
    architecture = c("naive", "hybrid"),
    seed         = seeds)
  partial <- tidyr::expand_grid(
    mechanism    = "market_posted_slice",
    graph_type   = c("tree", "sp"),
    load_level   = c("medium", "high"),
    architecture = "hybrid",
    seed         = seeds)
  set.seed(11)
  dplyr::bind_rows(full, partial) %>%
    dplyr::mutate(
      p_post_k              = 1,
      congestion            = "calibrated",
      welfare               = 10 + 5 * (architecture == "hybrid") +
        3 * (mechanism == "market") + stats::runif(dplyr::n()),
      median_latency        = stats::runif(dplyr::n(), 100, 300),
      drop_rate             = stats::runif(dplyr::n()),
      mean_price_volatility = stats::runif(dplyr::n()),
      efficiency            = stats::runif(dplyr::n()))
}

test_that("the whole frame is what makes the crossed model unfittable", {
  # The defect the balanced fit exists for: with the slice arm in, the model
  # matrix is rank-deficient and no interaction table comes back at all.
  raw   <- .exp6_frame()
  terms <- c("mechanism", "graph_type", "load_level", "architecture")
  a <- suppressMessages(art_anova(
    raw %>% dplyr::mutate(across(all_of(terms), factor)),
    stats::reformulate(paste(terms, collapse = " * "), response = "welfare")))
  expect_null(a)
})

test_that("the interaction is fitted on the arms every cell ran", {
  st <- suppressMessages(stat_exp6(.exp6_frame()))

  expect_false(is.null(st$interaction))
  expect_true(any(grepl("mechanism:architecture", st$interaction$term)))
  # The arm that could not enter the model is named, not silently absent.
  expect_equal(st$interaction_dropped, "market_posted_slice")
  # and the per-cell summaries still carry it: the balance is the model's
  # requirement, not the block's.
  expect_true(any(vapply(st$per_architecture, function(d)
    "market_posted_slice" %in% d$ci$mechanism, logical(1))))
})

test_that("a complete design drops nothing", {
  raw <- .exp6_frame() %>%
    dplyr::filter(mechanism != "market_posted_slice")
  st  <- suppressMessages(stat_exp6(raw))

  expect_false(is.null(st$interaction))
  expect_length(st$interaction_dropped, 0L)
})
