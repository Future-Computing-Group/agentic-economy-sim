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

#' Kruskal-Wallis test for a metric across groups.
#'
#' @param raw_df    Per-seed results.
#' @param group_var Name of the grouping column (string).
#' @param metric    Name of the metric column (string).
#' @return A single-row tibble: metric, group_var, H, df, p_value.
kruskal_test <- function(raw_df, group_var, metric) {
  vals   <- raw_df[[metric]]
  groups <- raw_df[[group_var]]
  valid  <- is.finite(vals) & !is.na(groups)
  vals   <- vals[valid]
  groups <- factor(groups[valid])

  if (nlevels(groups) < 2 || length(vals) < 3) {
    return(tibble(metric = metric, group_var = group_var,
                  H = NA_real_, df = NA_integer_, p_value = NA_real_))
  }

  kt <- kruskal.test(vals ~ groups)
  tibble(
    metric    = metric,
    group_var = group_var,
    H         = kt$statistic,
    df        = kt$parameter,
    p_value   = kt$p.value
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


#' Test whether hybrid + governance is super-additive on a given metric.
#'
#' Compares: W(hybrid,strict) - W(naive,none) vs
#'           [W(hybrid,none) - W(naive,none)] + [W(naive,strict) - W(naive,none)]
#'
#' @param raw_df Per-seed results from exp5.
#' @param metric Name of the metric column.
#' @return A tibble with synergy estimates per topology x load.
compute_synergy <- function(raw_df, metric) {
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


#' Statistical summary for Experiment 6 (mechanism ablation).
#'
#' @param raw_df Per-seed results from exp6_results_raw.
#' @return A list with stat summaries per topology x load, plus interaction.
stat_exp6 <- function(raw_df) {
  metrics <- c("median_latency", "drop_rate", "welfare",
               "mean_price_volatility", "efficiency")

  # Per topology x load: mechanism effect
  by_tl <- raw_df %>%
    group_by(graph_type, load_level) %>%
    group_split() %>%
    setNames(., sapply(., function(d) paste(d$graph_type[1], d$load_level[1], sep = "_")))

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


# ===========================================================================
# LaTeX table generation
# ===========================================================================
