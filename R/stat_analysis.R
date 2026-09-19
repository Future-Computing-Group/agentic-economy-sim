# stat_analysis.R
# ---------------------------------------------------------------------------
# Statistical analysis functions for the ablation study.
#
# Provides:
#   - Bootstrap 95% CIs (BCa)
#   - Kruskal-Wallis and pairwise Wilcoxon tests (Holm-corrected)
#   - Cliff's delta effect size
#   - Aligned Rank Transform ANOVA for interaction analysis
#   - LaTeX table formatting for supplementary material
#
# All functions operate on per-seed (raw) result tibbles.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(boot)
})


# ===========================================================================
# Bootstrap confidence intervals
# ===========================================================================

#' Compute BCa bootstrap 95% CI for a metric across seeds.
#'
#' @param x       Numeric vector (one value per seed).
#' @param n_boot  Number of bootstrap resamples (default: 2000).
#' @param conf    Confidence level (default: 0.95).
#' @return A single-row tibble: mean, lo, hi.
bootstrap_ci <- function(x, n_boot = 2000L, conf = 0.95) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n == 0L) return(tibble(mean = NA_real_, lo = NA_real_, hi = NA_real_))
  if (n == 1L) return(tibble(mean = x, lo = x, hi = x))
  if (n == 2L) {
    # BCa fails with n=2; use range
    return(tibble(mean = mean(x), lo = min(x), hi = max(x)))
  }

  stat_fn <- function(d, i) mean(d[i])
  b <- tryCatch(
    boot::boot(x, stat_fn, R = n_boot),
    error = function(e) NULL
  )
  if (is.null(b)) {
    se <- sd(x) / sqrt(n)
    m  <- mean(x)
    return(tibble(mean = m, lo = m - 1.96 * se, hi = m + 1.96 * se))
  }

  ci <- tryCatch(
    boot::boot.ci(b, conf = conf, type = "bca"),
    error = function(e) NULL
  )

  m <- mean(x)
  if (!is.null(ci) && !is.null(ci$bca)) {
    tibble(mean = m, lo = ci$bca[4], hi = ci$bca[5])
  } else {
    # Fallback to percentile
    ci_p <- tryCatch(
      boot::boot.ci(b, conf = conf, type = "perc"),
      error = function(e) NULL
    )
    if (!is.null(ci_p) && !is.null(ci_p$percent)) {
      tibble(mean = m, lo = ci_p$percent[4], hi = ci_p$percent[5])
    } else {
      se <- sd(x) / sqrt(n)
      tibble(mean = m, lo = m - 1.96 * se, hi = m + 1.96 * se)
    }
  }
}


#' Compute bootstrap CIs for all numeric metrics in a grouped data frame.
#'
#' @param raw_df    Per-seed results (one row per seed per condition).
#' @param group_vars Character vector of grouping column names.
#' @param metrics   Character vector of metric column names.
#' @param n_boot    Number of bootstrap resamples.
#' @return A tibble with group columns, and for each metric: mean, lo, hi.
bootstrap_ci_grouped <- function(raw_df, group_vars, metrics, n_boot = 2000L) {
  raw_df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      across(
        all_of(metrics),
        list(
          mean = \(x) bootstrap_ci(x, n_boot = n_boot)$mean,
          lo   = \(x) bootstrap_ci(x, n_boot = n_boot)$lo,
          hi   = \(x) bootstrap_ci(x, n_boot = n_boot)$hi
        ),
        .names = "{.col}_{.fn}"
      ),
      n_seeds = n(),
      .groups = "drop"
    )
}


# ===========================================================================
# Hypothesis tests
# ===========================================================================

#' Range check on a Kruskal-Wallis statistic: H cannot exceed n - 1.
#'
#' A tripwire only. It catches a statistic that is arithmetically impossible for
#' the sample it claims to summarise, i.e. a broken test. It does NOT catch a
#' mislabelled N: the check is computed from the data the test consumed, so a
#' statistic computed on 80 observations and reported in the text as N = 40
#' passes it (64.33 <= 79). The guard against that class is the `n` column,
#' which makes the code's sample size and the text's sample size one object.
#'
#' @param H Kruskal-Wallis statistic (NA on the degenerate path).
#' @param n Number of observations the test consumed.
#' @return TRUE if the statistic is in range or absent.
kruskal_tripwire_ok <- function(H, n) {
  isTRUE(is.na(H)) || isTRUE(H <= n - 1)
}


#' Kruskal-Wallis test for a metric across groups.
#'
#' @param raw_df    Per-seed results.
#' @param group_var Name of the grouping column (string).
#' @param metric    Name of the metric column (string).
#' @return A single-row tibble: metric, group_var, H, df, n, p_value,
#'   tripwire_ok. `n` is the number of observations the test consumed, after
#'   dropping non-finite values and missing group labels, and is present on
#'   every path including the degenerate early return.
kruskal_test <- function(raw_df, group_var, metric) {
  vals   <- raw_df[[metric]]
  groups <- raw_df[[group_var]]
  valid  <- is.finite(vals) & !is.na(groups)
  vals   <- vals[valid]
  groups <- factor(groups[valid])
  n      <- length(vals)

  if (nlevels(groups) < 2 || n < 3) {
    return(tibble(metric = metric, group_var = group_var,
                  H = NA_real_, df = NA_integer_, n = n,
                  p_value = NA_real_, tripwire_ok = TRUE))
  }

  kt <- kruskal.test(vals ~ groups)
  H  <- unname(kt$statistic)
  ok <- kruskal_tripwire_ok(H, n)
  if (!ok) {
    warning(sprintf("Kruskal-Wallis tripwire: H = %.4f exceeds n - 1 = %d for %s by %s",
                    H, n - 1L, metric, group_var), call. = FALSE)
  }

  tibble(
    metric      = metric,
    group_var   = group_var,
    H           = H,
    df          = unname(kt$parameter),
    n           = n,
    p_value     = kt$p.value,
    tripwire_ok = ok
  )
}


#' Pairwise Wilcoxon rank-sum tests with Holm correction.
#'
#' @param raw_df    Per-seed results.
#' @param group_var Name of the grouping column.
#' @param metric    Name of the metric column.
#' @return A tibble: metric, group1, group2, W, p_raw, p_adj, significant.
pairwise_wilcox <- function(raw_df, group_var, metric) {
  vals   <- raw_df[[metric]]
  groups <- raw_df[[group_var]]
  valid  <- is.finite(vals) & !is.na(groups)
  vals   <- vals[valid]
  groups <- factor(groups[valid])
  lvls   <- levels(groups)

  if (length(lvls) < 2) {
    return(tibble(metric = character(), group1 = character(),
                  group2 = character(), W = numeric(),
                  p_raw = numeric(), p_adj = numeric(),
                  significant = logical()))
  }

  pairs <- combn(lvls, 2, simplify = FALSE)
  results <- lapply(pairs, function(pair) {
    x <- vals[groups == pair[1]]
    y <- vals[groups == pair[2]]
    if (length(x) < 2 || length(y) < 2) {
      return(tibble(metric = metric, group1 = pair[1], group2 = pair[2],
                    W = NA_real_, p_raw = NA_real_))
    }
    wt <- wilcox.test(x, y, exact = FALSE)
    tibble(metric = metric, group1 = pair[1], group2 = pair[2],
           W = wt$statistic, p_raw = wt$p.value)
  })

  out <- bind_rows(results)
  out$p_adj <- p.adjust(out$p_raw, method = "holm")
  out$significant <- out$p_adj < 0.05
  out
}


# ===========================================================================
# Effect sizes
# ===========================================================================

#' Cliff's delta (non-parametric effect size).
#'
#' @param x Numeric vector (group 1).
#' @param y Numeric vector (group 2).
#' @return A single-row tibble: delta, magnitude.
cliff_delta <- function(x, y) {
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  if (length(x) == 0 || length(y) == 0) {
    return(tibble(delta = NA_real_, magnitude = NA_character_))
  }

  n_x <- length(x)
  n_y <- length(y)
  # Count dominance pairs
  count <- 0
  for (xi in x) {
    count <- count + sum(xi > y) - sum(xi < y)
  }
  d <- count / (n_x * n_y)

  mag <- case_when(
    abs(d) < 0.147 ~ "negligible",
    abs(d) < 0.33  ~ "small",
    abs(d) < 0.474 ~ "medium",
    TRUE            ~ "large"
  )

  tibble(delta = d, magnitude = mag)
}


#' Pairwise Cliff's delta for all group pairs.
#'
#' @param raw_df    Per-seed results.
#' @param group_var Name of the grouping column.
#' @param metric    Name of the metric column.
#' @return A tibble: metric, group1, group2, delta, magnitude.
pairwise_cliff_delta <- function(raw_df, group_var, metric) {
  vals   <- raw_df[[metric]]
  groups <- raw_df[[group_var]]
  valid  <- is.finite(vals) & !is.na(groups)
  vals   <- vals[valid]
  groups <- factor(groups[valid])
  lvls   <- levels(groups)

  if (length(lvls) < 2) {
    return(tibble(metric = character(), group1 = character(),
                  group2 = character(), delta = numeric(),
                  magnitude = character()))
  }

  pairs <- combn(lvls, 2, simplify = FALSE)
  results <- lapply(pairs, function(pair) {
    x <- vals[groups == pair[1]]
    y <- vals[groups == pair[2]]
    cd <- cliff_delta(x, y)
    tibble(metric = metric, group1 = pair[1], group2 = pair[2],
           delta = cd$delta, magnitude = cd$magnitude)
  })

  bind_rows(results)
}


# ===========================================================================
# Full statistical summary for one experiment
# ===========================================================================

#' Run the complete statistical analysis suite for a single factor.
#'
#' Combines bootstrap CIs, Kruskal-Wallis, pairwise Wilcoxon, and
#' Cliff's delta into a single summary list.
#'
#' @param raw_df     Per-seed results.
#' @param group_var  Primary factor column name.
#' @param metrics    Character vector of metric names to analyse.
#' @param n_boot     Bootstrap resamples.
#' @return A list with components: ci, kruskal, pairwise, effect_size.
stat_summary_single_factor <- function(raw_df, group_var, metrics,
                                       n_boot = 2000L) {
  ci_df <- bootstrap_ci_grouped(raw_df, group_var, metrics, n_boot = n_boot)

  kw_list <- lapply(metrics, function(m) kruskal_test(raw_df, group_var, m))
  kw_df   <- bind_rows(kw_list)

  pw_list <- lapply(metrics, function(m) pairwise_wilcox(raw_df, group_var, m))
  pw_df   <- bind_rows(pw_list)

  cd_list <- lapply(metrics, function(m) pairwise_cliff_delta(raw_df, group_var, m))
  cd_df   <- bind_rows(cd_list)

  list(ci = ci_df, kruskal = kw_df, pairwise = pw_df, effect_size = cd_df)
}


# ===========================================================================
# Interaction analysis (ART ANOVA)
# ===========================================================================

#' Run Aligned Rank Transform ANOVA for interaction effects.
#'
#' Requires the ARTool package. Falls back gracefully if not installed.
#'
#' @param raw_df   Per-seed results.
#' @param formula  Formula for the ART model (e.g., welfare ~ topology * load).
#' @return A tibble of ANOVA results, or NULL if ARTool is unavailable.
art_anova <- function(raw_df, formula) {
  if (!requireNamespace("ARTool", quietly = TRUE)) {
    message("ARTool package not installed; skipping ART ANOVA.")
    return(NULL)
  }

  m <- tryCatch(
    ARTool::art(formula, data = raw_df),
    error = function(e) {
      message("ART model failed: ", e$message)
      NULL
    }
  )
  if (is.null(m)) return(NULL)

  a <- tryCatch(
    anova(m),
    error = function(e) {
      message("ART anova failed: ", e$message)
      NULL
    }
  )
  if (is.null(a)) return(NULL)

  as_tibble(a, rownames = "term")
}


# ===========================================================================
# Experiment-specific statistical summaries
# ===========================================================================

#' Statistical summary for Experiment 1 (topology x load).
#'
#' @param raw_df Per-seed results from exp1_results_raw.
#' @return A list with stat summaries per load level, plus interaction ART.
stat_exp1 <- function(raw_df) {
  metrics <- c("median_latency", "drop_rate", "utilisation",
               "mean_price_volatility", "welfare", "efficiency")

  # Per load level: topology effect
  by_load <- raw_df %>%
    group_by(load_level) %>%
    group_split() %>%
    setNames(., sapply(., function(d) d$load_level[1]))

  per_load <- lapply(by_load, function(d) {
    stat_summary_single_factor(d, "graph_type", metrics)
  })

  # Interaction: topology x load
  interaction <- art_anova(
    raw_df %>% mutate(graph_type = factor(graph_type),
                      load_level = factor(load_level)),
    welfare ~ graph_type * load_level
  )

  list(per_load = per_load, interaction = interaction)
}


#' Statistical summary for Experiment 2 (scaling).
#'
#' @param raw_df Per-seed results from exp2_results_raw.
#' @return A list with Spearman correlations per topology.
stat_exp2 <- function(raw_df) {
  metrics <- c("median_latency", "drop_rate", "utilisation",
               "mean_price_volatility", "welfare")

  by_topo <- raw_df %>%
    group_by(graph_type) %>%
    group_split() %>%
    setNames(., sapply(., function(d) d$graph_type[1]))

  correlations <- lapply(by_topo, function(d) {
    lapply(metrics, function(m) {
      ct <- cor.test(d$N, d[[m]], method = "spearman", exact = FALSE)
      tibble(metric = m, rho = ct$estimate, p_value = ct$p.value)
    }) %>% bind_rows()
  })

  list(correlations = correlations)
}


#' Statistical summary for Experiment 3 (governance).
#'
#' @param raw_df Per-seed results from exp3_results_raw.
#' @return A list with stat summaries per topology x load, plus interaction.
stat_exp3 <- function(raw_df) {
  metrics <- c("median_latency", "drop_rate", "welfare", "coverage",
               "price_volatility_general")

  by_tl <- raw_df %>%
    group_by(graph_type, load_level) %>%
    group_split() %>%
    setNames(., sapply(., function(d) paste(d$graph_type[1], d$load_level[1], sep = "_")))

  per_tl <- lapply(by_tl, function(d) {
    stat_summary_single_factor(d, "policy", metrics)
  })

  interaction <- art_anova(
    raw_df %>% mutate(policy = factor(policy),
                      graph_type = factor(graph_type),
                      load_level = factor(load_level)),
    welfare ~ policy * graph_type * load_level
  )

  list(per_topo_load = per_tl, interaction = interaction)
}


#' Statistical summary for Experiment 4 (architecture ablation).
#'
#' @param raw_df Per-seed results from exp4_results_raw.
#' @return A list with stat summaries per topology x load, plus interaction.
stat_exp4 <- function(raw_df) {
  metrics <- c("median_latency", "drop_rate", "welfare",
               "mean_price_volatility", "efficiency")

  by_tl <- raw_df %>%
    group_by(graph_type, load_level) %>%
    group_split() %>%
    setNames(., sapply(., function(d) paste(d$graph_type[1], d$load_level[1], sep = "_")))

  per_tl <- lapply(by_tl, function(d) {
    stat_summary_single_factor(d, "architecture", metrics)
  })

  interaction <- art_anova(
    raw_df %>% mutate(architecture = factor(architecture),
                      graph_type = factor(graph_type),
                      load_level = factor(load_level)),
    welfare ~ architecture * graph_type * load_level
  )

  list(per_topo_load = per_tl, interaction = interaction)
}


#' Statistical summary for the Exp.4 architecture x smoothing factorial.
#'
#' The decomposition of the agent-facing price dispersion into the two crossed
#' factors, taken on the burn-in-trimmed column so that the cells are contrasted
#' on their price dynamics rather than on their initial prices, per (topology,
#' load, N) cell, reported as reductions against the
#' naive cell: delta_E is encapsulation with smoothing off, delta_S is smoothing
#' with the allocation held fixed, delta_joint is the total, and interaction is
#' delta_joint - (delta_E + delta_S). Whatever split these show is the answer to
#' the attribution question; no ordering is asserted in advance.
#'
#' @param raw_df  Per-seed results from exp4_results_raw.
#' @param metric  Metric the decomposition is taken on.
#' @param metrics Metrics the two main-effect summaries are taken on.
#' @return A list: decomposition, the two main effects, and the ART interaction.
stat_exp4_factorial <- function(raw_df,
                                metric = "mean_price_volatility_tail",
                                metrics = c("median_latency", "drop_rate",
                                            "welfare", "mean_price_volatility",
                                            "mean_price_volatility_tail",
                                            "efficiency")) {
  # Named rather than left to a pronoun error three frames down: rows recorded
  # before the trimmed metric existed do not carry the column this reads.
  stopifnot("raw_df carries no column of that name" = metric %in% names(raw_df))
  cells <- raw_df %>%
    filter(architecture %in% c("naive", "naive_ema", "hybrid_noema", "hybrid_ema")) %>%
    mutate(
      encapsulation = if_else(architecture %in% c("hybrid_noema", "hybrid_ema"), "on", "off"),
      ema           = if_else(architecture %in% c("naive_ema", "hybrid_ema"), "on", "off")
    )
  metrics <- intersect(metrics, names(cells))

  decomposition <- factor_contrasts(
      cells, metric,
      f1 = "encapsulation", f1_levels = c("off", "on"),
      f2 = "ema",           f2_levels = c("off", "on"),
      cell_vars = c("graph_type", "load_level", "N")
    ) %>%
    # Sign: the contrasts are gains, the paper reads reductions in dispersion,
    # so each is negated once, here, rather than at every reporting site.
    transmute(graph_type, load_level, N,
              delta_E     = -marginal_f1, delta_S = -marginal_f2,
              delta_joint = -joint_gain,  interaction = -synergy) %>%
    group_by(graph_type, load_level, N) %>%
    summarise(
      # The sample size travels with the estimate, and with each estimate: a
      # seed that drops out of one contrast need not drop out of the others.
      across(c(delta_S, delta_E, delta_joint, interaction),
             list(mean = \(x) mean(x, na.rm = TRUE),
                  lo   = \(x) bootstrap_ci(x)$lo,
                  hi   = \(x) bootstrap_ci(x)$hi,
                  n    = \(x) sum(is.finite(x)))),
      .groups = "drop"
    )

  list(
    decomposition = decomposition,
    encapsulation = stat_summary_single_factor(cells, "encapsulation", metrics),
    ema           = stat_summary_single_factor(cells, "ema", metrics),
    # N is a factor of the model rather than a dimension pooled into its
    # residual: four agent counts left in the error term make the test
    # conservative about the interaction it exists to report.
    interaction   = art_anova(
      cells %>% mutate(encapsulation = factor(encapsulation),
                       ema = factor(ema),
                       graph_type = factor(graph_type),
                       load_level = factor(load_level),
                       N = factor(N)),
      stats::reformulate(c("encapsulation * ema * graph_type * load_level * N"),
                         response = metric)
    )
  )
}


#' The headline tail-dispersion statistic: slice-and-EMA against naive.
#'
#' A RATIO of tail CVs, not the difference `stat_exp4_factorial` decomposes, and
#' it is defined only where the denominator is. Two exclusions follow, and both
#' are reported rather than applied silently:
#'
#'   - Cells where any arm's seed-mean tail CV is zero are dropped. A zero there
#'     means prices never left the reserve floor in that arm, so the cell
#'     measures the floor, not the smoothing; the count that survives is the
#'     cell count the manuscript quotes.
#'   - Within a surviving cell, a seed whose naive tail CV is zero has no ratio
#'     at all. It leaves that cell's mean and is counted in `n_excluded`.
#'
#' The interval is a PAIRED-seed bootstrap: one resample of the seed vector is
#' applied to every cell at once, so the seed-level correlation between cells is
#' carried rather than averaged away, and the median is recomputed from the
#' resampled cell means. Percentile, not BCa: the statistic is a median over
#' nine cells of a ratio, where the acceleration term is estimated on the ten
#' seeds the resample is already exhausting.
#'
#' The envelopes (e) are taken over EVERY cell, floored ones included: they are
#' what bounds the claim on the cells the median cannot speak for.
#'
#' @param raw_df Per-seed results from exp4_results_raw.
#' @param B      Bootstrap resamples.
#' @param seed   Seed for the resampling.
#' @return A long tibble, one row per reported quantity: quantity, graph_type,
#'   load_level, N, value, lo, hi, n, n_excluded. `median_reduction` carries the
#'   interval and the cell count in `n`; the envelope rows carry min in `lo` and
#'   max in `hi` over all cells.
stat_exp4_volatility_reduction <- function(raw_df, B = 10000L, seed = 1L) {
  arms   <- c("naive", "naive_ema", "hybrid_noema", "hybrid_ema")
  metric <- "mean_price_volatility_tail"
  stopifnot("raw_df carries no trimmed volatility column" = metric %in% names(raw_df))
  set.seed(seed)

  wide <- raw_df %>%
    filter(architecture %in% arms) %>%
    select(all_of(c("graph_type", "load_level", "N", "seed", "architecture")),
           tail_cv = all_of(metric)) %>%
    pivot_wider(names_from = "architecture", values_from = "tail_cv") %>%
    arrange(.data$graph_type, .data$load_level, .data$N, .data$seed)
  stopifnot("raw_df is missing a factorial arm" = all(arms %in% names(wide)))

  cell_mean <- wide %>%
    group_by(.data$graph_type, .data$load_level, .data$N) %>%
    summarise(across(all_of(arms), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

  live <- cell_mean %>%
    filter(if_all(all_of(arms), \(x) is.finite(x) & x > 0)) %>%
    mutate(cell = paste(.data$graph_type, .data$load_level, .data$N, sep = "/"))

  # One column per surviving cell, one row per seed: the ratio, or NA where the
  # denominator is zero. A matrix, because the bootstrap resamples its ROWS.
  ratios <- wide %>%
    semi_join(live, by = c("graph_type", "load_level", "N")) %>%
    mutate(cell = paste(.data$graph_type, .data$load_level, .data$N, sep = "/"),
           reduction = if_else(.data$naive > 0, 1 - .data$hybrid_ema / .data$naive,
                               NA_real_)) %>%
    select(all_of(c("cell", "seed", "reduction"))) %>%
    pivot_wider(names_from = "cell", values_from = "reduction") %>%
    arrange(.data$seed)
  mat <- as.matrix(ratios[, live$cell, drop = FALSE])

  cell_value <- colMeans(mat, na.rm = TRUE)
  med        <- median(cell_value, na.rm = TRUE)

  boot_med <- vapply(seq_len(B), function(i) {
    idx <- sample.int(nrow(mat), nrow(mat), replace = TRUE)
    median(colMeans(mat[idx, , drop = FALSE], na.rm = TRUE), na.rm = TRUE)
  }, numeric(1))
  ci <- unname(quantile(boot_med, c(0.025, 0.975), na.rm = TRUE))

  floored <- cell_mean$hybrid_ema[!(cell_mean$naive > 0)]

  row <- function(quantity, value = NA_real_, lo = NA_real_, hi = NA_real_,
                  n = NA_integer_, n_excluded = NA_integer_) {
    tibble(quantity = quantity, graph_type = NA_character_,
           load_level = NA_character_, N = NA_integer_,
           value = value, lo = lo, hi = hi,
           n = as.integer(n), n_excluded = as.integer(n_excluded))
  }

  bind_rows(
    tibble(quantity   = "cell_reduction",
           graph_type = live$graph_type, load_level = live$load_level,
           N          = as.integer(live$N),
           value      = unname(cell_value),
           lo = NA_real_, hi = NA_real_,
           n          = as.integer(colSums(is.finite(mat))),
           n_excluded = as.integer(colSums(!is.finite(mat)))),
    row("median_reduction", value = med, lo = ci[1], hi = ci[2],
        n = nrow(live)),
    row("bootstrap_resamples", value = B),
    row("naive_tail_cv_envelope", lo = min(cell_mean$naive),
        hi = max(cell_mean$naive), n = nrow(cell_mean)),
    row("hybrid_ema_tail_cv_envelope", lo = min(cell_mean$hybrid_ema),
        hi = max(cell_mean$hybrid_ema), n = nrow(cell_mean)),
    row("hybrid_ema_tail_cv_max_at_zero_naive",
        value = if (length(floored)) max(floored) else NA_real_,
        n = length(floored))
  )
}

#' Statistical summary for Experiment 5 (hybrid x governance).
#'
#' @param raw_df Per-seed results from exp5_results_raw.
#' @return A list with stat summaries and interaction analysis.
stat_exp5 <- function(raw_df) {
  metrics <- c("median_latency", "drop_rate", "welfare",
               "mean_price_volatility", "coverage", "efficiency")

  # Main effects: architecture and policy
  arch_stats   <- stat_summary_single_factor(raw_df, "architecture", metrics)
  policy_stats <- stat_summary_single_factor(raw_df, "policy", metrics)

  # Per topology: architecture x policy interaction
  by_topo <- raw_df %>%
    group_by(graph_type) %>%
    group_split() %>%
    setNames(., sapply(., function(d) d$graph_type[1]))

  per_topo <- lapply(by_topo, function(d) {
    stat_summary_single_factor(d, "architecture", metrics)
  })

  # Full interaction: architecture x policy x topology x load
  interaction <- art_anova(
    raw_df %>% mutate(architecture = factor(architecture),
                      policy = factor(policy),
                      graph_type = factor(graph_type),
                      load_level = factor(load_level)),
    welfare ~ architecture * policy * graph_type * load_level
  )

  # Synergy test: is hybrid+governance super-additive?
  # Compare (hybrid,strict) welfare to sum of marginal improvements
  synergy <- compute_synergy(raw_df, "welfare")

  list(architecture = arch_stats, policy = policy_stats,
       per_topo = per_topo, interaction = interaction,
       synergy = synergy)
}


#' Per-seed contrasts for two crossed binary factors.
#'
#' Both marginals, the joint gain and the interaction between them, one row per
#' cell per seed. The factor columns and their levels are arguments, so the
#' Exp.5 architecture x governance question and the Exp.4 encapsulation x
#' smoothing factorial read the same implementation.
#'
#' @param raw_df    Per-seed results.
#' @param metric    Name of the metric column.
#' @param f1,f2     Factor column names.
#' @param f1_levels,f2_levels Two-element vectors: the off level, then the on one.
#' @param cell_vars Columns defining a cell; contrasts are taken within a cell.
#' @return A tibble: cell_vars, seed, the four cell values, marginal_f1,
#'   marginal_f2, joint_gain, synergy.
factor_contrasts <- function(raw_df, metric,
                             f1 = "architecture", f1_levels = c("naive", "hybrid"),
                             f2 = "policy",       f2_levels = c("none", "strict"),
                             cell_vars = c("graph_type", "load_level")) {
  raw_df %>%
    group_by(across(all_of(c(cell_vars, "seed")))) %>%
    summarise(
      val_nn = mean(.data[[metric]][.data[[f1]] == f1_levels[1] & .data[[f2]] == f2_levels[1]], na.rm = TRUE),
      val_hn = mean(.data[[metric]][.data[[f1]] == f1_levels[2] & .data[[f2]] == f2_levels[1]], na.rm = TRUE),
      val_ns = mean(.data[[metric]][.data[[f1]] == f1_levels[1] & .data[[f2]] == f2_levels[2]], na.rm = TRUE),
      val_hs = mean(.data[[metric]][.data[[f1]] == f1_levels[2] & .data[[f2]] == f2_levels[2]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      marginal_f1 = val_hn - val_nn,
      marginal_f2 = val_ns - val_nn,
      joint_gain  = val_hs - val_nn,
      synergy     = joint_gain - (marginal_f1 + marginal_f2)
    )
}


#' Test whether hybrid + governance is super-additive on a given metric.
#'
#' Compares: W(hybrid,strict) - W(naive,none) vs
#'           [W(hybrid,none) - W(naive,none)] + [W(naive,strict) - W(naive,none)]
#'
#' @param raw_df Per-seed results from exp5.
#' @param metric Name of the metric column.
#' @param ...    Passed to factor_contrasts; the defaults are the Exp.5 factors.
#' @return A tibble with synergy estimates per topology x load.
compute_synergy <- function(raw_df, metric, ...) {
  factor_contrasts(raw_df, metric, ...) %>%
    group_by(graph_type, load_level) %>%
    summarise(
      synergy_mean = mean(synergy, na.rm = TRUE),
      synergy_lo   = bootstrap_ci(synergy)$lo,
      synergy_hi   = bootstrap_ci(synergy)$hi,
      super_additive = synergy_mean > 0,
      .groups = "drop"
    )
}


#' Statistical summary for Experiment 6 (mechanism ablation).
#'
#' @param raw_df Per-seed results from exp6_results_raw.
#' @return A list with stat summaries per topology x load, plus interaction.
stat_exp6 <- function(raw_df) {
  metrics <- c("median_latency", "drop_rate", "welfare",
               "mean_price_volatility", "efficiency")

  # The posted price at three markups is three arms, not one. The markup travels
  # in the arm label, so each level is its own group; pooling them would test a
  # mixture of three prices against the other mechanisms and report the mixture
  # as a single arm. A frame with no markup column carries no posted-price arm
  # either, so the label reduces to the mechanism name and a store written
  # before those arms still analyses.
  if (!"p_post_k" %in% names(raw_df)) raw_df$p_post_k <- NA_real_
  raw_df <- raw_df %>%
    mutate(mechanism = ifelse(mechanism == "posted_price",
                              paste0(mechanism, "_k", p_post_k), mechanism))

  # Per topology x load x architecture: mechanism effect. Architecture is a
  # cell factor, not a nuisance dimension to pool over: one test per cell runs
  # on mechanisms x seeds observations, which is the design the paper states.
  by_tl <- raw_df %>%
    group_by(graph_type, load_level, architecture) %>%
    group_split() %>%
    setNames(., sapply(., function(d) paste(d$graph_type[1], d$load_level[1],
                                            d$architecture[1], sep = "_")))

  per_tl <- lapply(by_tl, function(d) {
    stat_summary_single_factor(d, "mechanism", metrics)
  })

  # Per architecture: mechanism effect (collapsed across topology and load)
  by_arch <- raw_df %>%
    group_by(architecture) %>%
    group_split() %>%
    setNames(., sapply(., function(d) d$architecture[1]))

  per_arch <- lapply(by_arch, function(d) {
    stat_summary_single_factor(d, "mechanism", metrics)
  })

  # Interaction: mechanism x topology x load x architecture
  interaction <- art_anova(
    raw_df %>% mutate(mechanism = factor(mechanism),
                      architecture = factor(architecture),
                      graph_type = factor(graph_type),
                      load_level = factor(load_level)),
    welfare ~ mechanism * graph_type * load_level * architecture
  )

  list(per_topo_load = per_tl, per_architecture = per_arch,
       interaction = interaction)
}

#' Statistical summary for Experiment 11 (recipe heterogeneity).
#'
#' Stratified by per-tier capacity before the arm comparison, the way every
#' other experiment stratifies its secondary factor. Capacity is the contention
#' knob and it moves every reported metric hard, so pooling it would hand the
#' Kruskal-Wallis on `arm` a frame whose cells are not exchangeable and let cap
#' variance swamp an arm effect.
#'
#' @param raw_df Per-seed results from exp11_results_raw.
#' @return A list with per-capacity stat summaries, plus the arm x cap ART.
stat_exp11 <- function(raw_df) {
  metrics <- c("price_cv", "greedy_exact_ratio", "admitted_exact_ratio",
               "welfare_ratio", "drop_rate")

  by_cap <- raw_df %>%
    group_by(cap) %>%
    group_split() %>%
    setNames(., sapply(., function(d) as.character(d$cap[1])))

  per_cap <- lapply(by_cap, function(d) {
    stat_summary_single_factor(d, "arm", metrics)
  })

  interaction <- art_anova(
    raw_df %>% mutate(arm = factor(arm), cap = factor(cap)),
    welfare_ratio ~ arm * cap
  )

  list(per_cap = per_cap, interaction = interaction)
}


# ===========================================================================
# Flat statistics dump
# ===========================================================================

#' Flatten every Kruskal-Wallis test in the stat_exp* objects into one tibble.
#'
#' The supplement's statistics are transcribed by hand. This is the single
#' source that transcription reads from, so every quoted statistic arrives with
#' the sample size it was computed on and an audit can diff the manuscript
#' against one file.
#'
#' @param stats_list Named list of stat_exp* return values, e.g.
#'   `list(exp1 = stats_exp1, exp6 = stats_exp6)`.
#' @return A tibble: experiment, cell, metric, group_var, statistic, df, n,
#'   p_value, tripwire_ok. One row per Kruskal-Wallis test.
make_stats_report <- function(stats_list) {
  collect <- function(x, path) {
    if (!is.list(x) || is.data.frame(x)) return(NULL)
    nms <- names(x)
    rows <- lapply(seq_along(x), function(i) {
      el <- x[[i]]
      # Two element names are harvested: `kruskal`, and `statistics` for a
      # frame whose statistic is not a Kruskal-Wallis H (Exp.7a's standard
      # errors). The rename happens here, per frame, so the two shapes bind.
      if (isTRUE(nms[i] %in% c("kruskal", "statistics")) && is.data.frame(el)) {
        el %>%
          rename_with(\(x) rep("statistic", length(x)), any_of("H")) %>%
          mutate(cell = paste(path, collapse = "/"), .before = 1)
      } else {
        collect(el, c(path, nms[i]))
      }
    })
    bind_rows(rows)
  }

  out <- bind_rows(lapply(names(stats_list), function(e) {
    df <- collect(stats_list[[e]], character())
    if (is.null(df) || nrow(df) == 0L) {
      # A listed experiment must never vanish from the transcription source in
      # silence: stat_exp2 reports Spearman correlations, not Kruskal-Wallis
      # tests, so it contributes nothing here and the supplement must take its
      # statistics from elsewhere.
      warning(sprintf("stats_report: %s contributed no rows", e), call. = FALSE)
      return(NULL)
    }
    mutate(df, experiment = e, .before = 1)
  }))
  if (nrow(out) == 0L) return(out)

  out %>%
    select(experiment, cell, metric, group_var, statistic, df, n,
           p_value, tripwire_ok)
}


#' Standard errors for the Exp.7a regret grid.
#'
#' The regret the Exp.7a table reports is a mean over seeds; what the caption
#' quotes beside it is that mean's standard error. One row per topology per
#' shade, each carrying the n it was taken on, so the caption is transcribed
#' from the pipeline rather than computed beside it.
#'
#' @param raw_df Per-seed results from exp7_results_raw: one regret_<shade>
#'   column per shade of the sweep.
#' @return A list: by_topology, the readable table, plus one element per
#'   topology in the shape make_stats_report harvests.
stat_exp7 <- function(raw_df) {
  by_topology <- raw_df %>%
    pivot_longer(starts_with("regret_"), names_to = "shade",
                 names_prefix = "regret_", values_to = "regret") %>%
    filter(is.finite(.data$regret)) %>%
    group_by(.data$graph_type, .data$shade) %>%
    summarise(mean_regret = mean(.data$regret),
              se          = sd(.data$regret) / sqrt(dplyr::n()),
              n           = dplyr::n(),
              .groups     = "drop")

  report <- lapply(split(by_topology, by_topology$graph_type), function(d) {
    list(statistics = tibble(
      metric      = paste0("regret_", d$shade, "_se"),
      group_var   = "seed",
      statistic   = d$se,
      df          = NA_integer_,
      n           = as.integer(d$n),
      p_value     = NA_real_,
      # The tripwire is a range check on a Kruskal-Wallis H against its n. A
      # standard error has no such check, and NA says so rather than claiming
      # a check that never ran.
      tripwire_ok = NA))
  })

  c(list(by_topology = by_topology), report)
}


#' Statistical summary for Experiment 7b (worst-case joint misreport).
#'
#' One topology contrast over the worst-case best-response gain, its secondary
#' mean, and the payment level that makes the incentive numbers non-vacuous. On
#' a DSIC mechanism br_gain_max is a constant zero up to numerical noise, so the
#' Kruskal-Wallis statistic is NA there by construction; the CIs and the payment
#' contrast are what carry information.
#'
#' @param raw_df Per-seed results from exp7b_run_single().
#' @return A list with one element, by_topology, in stat_summary_single_factor
#'   shape (ci / kruskal / pairwise / effect_size).
stat_exp7b <- function(raw_df) {
  list(by_topology = stat_summary_single_factor(
    raw_df, "graph_type", c("br_gain_max", "br_gain_mean", "mean_payment")))
}


# ===========================================================================
# LaTeX table generation
# ===========================================================================
