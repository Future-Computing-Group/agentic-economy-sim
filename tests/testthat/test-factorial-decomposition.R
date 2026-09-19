# The two-factor decomposition behind the architecture x smoothing factorial.
#
# The decomposition is a reuse: compute_synergy already computes this shape for
# Exp.5 (two binary factors, two marginals, a joint gain and the interaction
# between them) on per-seed rows with bootstrap CIs. It is generalised to the
# factorial's factors rather than copied, so the first test here is the
# behaviour guard on its original call.

# ---- the generalisation is behaviour-neutral --------------------------------

# The pre-generalisation body, kept as an independent oracle. Pinning the
# numbers instead would pin this machine's bootstrap draws; running both
# implementations under one seed pins the behaviour.
.synergy_reference <- function(raw_df, metric) {
  raw_df %>%
    group_by(graph_type, load_level, seed) %>%
    summarise(
      val_nn = mean(.data[[metric]][architecture == "naive"  & policy == "none"],   na.rm = TRUE),
      val_hn = mean(.data[[metric]][architecture == "hybrid" & policy == "none"],   na.rm = TRUE),
      val_ns = mean(.data[[metric]][architecture == "naive"  & policy == "strict"], na.rm = TRUE),
      val_hs = mean(.data[[metric]][architecture == "hybrid" & policy == "strict"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      marginal_arch = val_hn - val_nn,
      marginal_gov  = val_ns - val_nn,
      joint_gain    = val_hs - val_nn,
      synergy       = joint_gain - (marginal_arch + marginal_gov)
    ) %>%
    group_by(graph_type, load_level) %>%
    summarise(
      synergy_mean = mean(synergy, na.rm = TRUE),
      synergy_lo   = bootstrap_ci(synergy)$lo,
      synergy_hi   = bootstrap_ci(synergy)$hi,
      super_additive = synergy_mean > 0,
      .groups = "drop"
    )
}

.exp5_fixture <- function() {
  tidyr::expand_grid(graph_type = c("sp", "entangled"), load_level = "high",
                     architecture = c("naive", "hybrid"),
                     policy = c("none", "strict"), seed = 1:6) %>%
    mutate(welfare = 10 + 2 * (architecture == "hybrid") +
             3 * (policy == "strict") + 0.1 * seed +
             0.25 * (graph_type == "entangled") * seed +
             # A seed-varying interaction, so the bootstrap runs on a real
             # spread rather than on a constant.
             (architecture == "hybrid" & policy == "strict") *
               (1.5 + 0.3 * ((seed %% 3) - 1)))
}

test_that("compute_synergy is unchanged on its Exp.5 call", {
  fix <- .exp5_fixture()
  set.seed(11); expected <- .synergy_reference(fix, "welfare")
  set.seed(11); actual   <- compute_synergy(fix, "welfare")
  expect_identical(actual, expected)
})


# ---- the four-cell decomposition --------------------------------------------

test_that("the decomposition is additive on a constructed fixture", {
  # One cell, six seeds, every seed carrying the same hand-chosen volatilities:
  #   naive 0.10, naive_ema 0.06, hybrid_noema 0.07, hybrid_ema 0.02
  # so, as reductions against the naive cell,
  #   delta_S = 0.04, delta_E = 0.03, delta_joint = 0.08
  #   interaction = 0.08 - (0.04 + 0.03) = 0.01
  cv <- c(naive = 0.10, naive_ema = 0.06, hybrid_noema = 0.07, hybrid_ema = 0.02)
  fix <- tidyr::expand_grid(graph_type = "sp", load_level = "high", N = 40L,
                            architecture = names(cv), seed = 1:6) %>%
    mutate(mean_price_volatility_tail = unname(cv[architecture]))

  got <- stat_exp4_factorial(fix)$decomposition
  expect_equal(nrow(got), 1L)
  expect_equal(got$delta_S_mean, 0.04)
  expect_equal(got$delta_E_mean, 0.03)
  expect_equal(got$delta_joint_mean, 0.08)
  expect_equal(got$interaction_mean, 0.01)
  # The sample size travels with the estimate, and with each estimate: four
  # contrasts, four counts, because a seed can drop out of one of them.
  expect_equal(got$delta_S_n, 6L)
  expect_equal(got$delta_E_n, 6L)
  expect_equal(got$delta_joint_n, 6L)
  expect_equal(got$interaction_n, 6L)
})

test_that("the decomposition is read off the trimmed volatility", {
  # The headline contrast is between the cells' price dynamics, so it runs on
  # the column that drops the opening transient. Here the two columns disagree
  # by construction: the full-run column would give a decomposition of zero.
  cv  <- c(naive = 0.10, naive_ema = 0.06, hybrid_noema = 0.07, hybrid_ema = 0.02)
  fix <- tidyr::expand_grid(graph_type = "sp", load_level = "high", N = 40L,
                            architecture = names(cv), seed = 1:6) %>%
    mutate(mean_price_volatility_tail = unname(cv[architecture]),
           mean_price_volatility      = 0.5)

  got <- stat_exp4_factorial(fix)$decomposition
  expect_equal(got$delta_S_mean, 0.04)
  expect_equal(got$delta_E_mean, 0.03)
  expect_equal(got$delta_joint_mean, 0.08)
  expect_equal(got$interaction_mean, 0.01)
})


test_that("the decomposition splits the cells the factorial is read by", {
  cv  <- c(naive = 0.10, naive_ema = 0.06, hybrid_noema = 0.07, hybrid_ema = 0.02)
  fix <- tidyr::expand_grid(graph_type = c("sp", "entangled"),
                            load_level = c("medium", "high"), N = c(20L, 40L),
                            architecture = names(cv), seed = 1:4) %>%
    mutate(mean_price_volatility_tail = unname(cv[architecture]) +
             0.01 * (load_level == "high") + 0.001 * seed)

  out <- stat_exp4_factorial(fix)
  expect_equal(nrow(out$decomposition), 8L)   # topology x load x N
  # N is a factor of the interaction model, not a dimension left in its
  # residual: four agent counts pooled into the error term make the test
  # conservative about the very interaction it is there to report.
  expect_true(any(grepl("N", out$interaction$term)))
  expect_true(all(c("delta_S_lo", "delta_S_hi") %in% names(out$decomposition)))
  # Main effects of both factors, each carrying the N it was computed on.
  expect_setequal(out$encapsulation$ci$encapsulation, c("off", "on"))
  expect_setequal(out$ema$ci$ema, c("off", "on"))
  expect_true(all(out$encapsulation$kruskal$n > 0))
})


test_that("the decomposition is written out with the rest of the statistics", {
  # The four numbers that answer the attribution question have to be
  # transcribed from a machine-written file carrying their own sample size,
  # like every other statistic the supplement quotes. The flat dump collects
  # Kruskal-Wallis rows only, so the decomposition needs its own file target.
  src <- readLines(here::here("_targets.R"))
  at  <- grep("stats_exp4_decomposition_file", src)
  expect_gt(length(at), 0L)
  block <- paste(src[min(at):(min(at) + 9L)], collapse = " ")
  expect_match(block, "stats_exp4_factorial$decomposition", fixed = TRUE)
  expect_match(block, "write_csv")
  expect_match(block, 'format = "file"', fixed = TRUE)
})
