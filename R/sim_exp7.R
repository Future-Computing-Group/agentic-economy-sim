# sim_exp7.R
# ---------------------------------------------------------------------------
# Experiment 7a: Strategic bidding under VCG — the DSIC empirics.
#
# Tests the DSIC property of Proposition 2 directly: it is PROVEN, and here we
# verify it empirically. We run the Clarke-pivot VCG mechanism (vcg_allocate)
# with strategically misreporting agents and show that truthful reporting is the
# empirical best response.
#
# Design constraints:
#   - Run in a SATURATED (binding-capacity) regime, else VCG payments are ~0
#     and the DSIC property is invisible (the result would be vacuous).
#   - task_bundle is per-environment (identical bundles) => value-greedy is
#     welfare-optimal on every topology, so VCG is DSIC on tree/SP AND
#     entangled in this model. We therefore report DSIC on tree/SP and do NOT
#     present entangled as a non-DSIC contrast (it isn't). Price-stability
#     (sigma_p, topology-dependent) is a SEPARATE story (Exp.1/4).
#   - That premise is now enforced rather than documented: vcg_allocate refuses
#     an environment carrying per-task recipes outright, so the incentive arm
#     cannot be pointed at the heterogeneous-recipe environment of Exp.11, where
#     value-greedy is a knapsack heuristic and no incentive claim is available.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
})


#' Run a single Exp.7a configuration (one seed): VCG best-response sweep.
#'
#' Each round, all agents report truthfully and the VCG allocation + payments
#' are computed. Then, for each agent in turn (others held truthful), the agent
#' re-reports its task values scaled by each shading factor; its utility is
#' evaluated at its TRUE value minus the VCG payment it incurs under the
#' misreport. Regret(alpha) = utility(truthful) - utility(alpha), averaged over
#' agents and rounds. DSIC => regret >= 0 for all alpha, with regret = 0 at
#' alpha = 1.
#'
#' @param graph_type DAG topology ("tree" or "sp"; the polymatroidal regimes).
#' @param load_level Load regime ("medium"/"high").
#' @param N          Number of agents.
#' @param seed       Random seed.
#' @param cap        Per-tier capacity override (saturation control). Smaller =
#'                   more binding. Default 30 saturates the default demand model.
#' @param shades     Value-shading factors to sweep (1.0 = truthful).
#' @param n_rounds   Number of rounds.
#' @param deadlines  Task deadlines (ms). The agentic environment needs its own
#'                   set, rescaled to its measured critical path.
#' @param lambda_l_default Per-ms value-decay rate, rescaled with the deadlines.
#' @param util_hat   Exogenous congestion estimate fed to valuations.
#' @return A one-row tibble: config + mean_payment + binding flag + one
#'         regret_<alpha> column per shading factor + mean welfare.
exp7a_run_single <- function(graph_type = c("tree", "sp", "agentic"),
                             load_level = c("high", "medium"),
                             N = 8L, seed = 1L, cap = 30,
                             shades = c(0.5, 0.7, 0.9, 1.0, 1.1, 1.3),
                             n_rounds = 30L, util_hat = 0.5,
                             deadlines = c(500L, 750L, 1000L),
                             lambda_l_default = 0.005) {
  graph_type <- match.arg(graph_type)
  load_level <- match.arg(load_level)
  set.seed(seed)

  graph <- build_dependency_graph(graph_type)
  env   <- init_environment(graph, load_level, n_agents = N,
                            graph_type = graph_type)
  exp7_require_identical_bundles(env)
  # Saturate: shrink per-tier capacity so allocation is competitive.
  env$capacities <- mutate(env$capacities, capacity = cap)

  agents <- init_agents(N)
  blb    <- base_latency_for_bids(env)
  sm     <- init_success_model()

  payment_acc <- 0
  welfare_acc <- 0
  n_round_eff <- 0L
  # Regret accumulator per shade (summed over agent-rounds), and the count.
  regret_sum  <- setNames(numeric(length(shades)), as.character(shades))
  ar_count    <- 0L

  for (t in seq_len(n_rounds)) {
    agent_split <- split(agents, agents$agent_id)
    tasks_list  <- lapply(agent_split, function(a)
      generate_tasks(a, env, round = t, deadlines = deadlines))
    tasks_all <- bind_tasks(tasks_list)
    if (nrow(tasks_all) == 0) next
    if (n_round_eff == 0L) exp7_require_identical_bundles(env, tasks_all)

    # Truthful valuations (exogenous within the round).
    true_ev <- task_expected_value(tasks_all, util_hat, blb, sm,
                                   lambda_l_default = lambda_l_default)

    res_truth <- vcg_allocate(tasks_all, env, util_hat, blb, sm,
                              lambda_l_default = lambda_l_default)
    payment_acc <- payment_acc + sum(res_truth$vcg_payment)
    welfare_acc <- welfare_acc + sum(res_truth$realised_value)
    n_round_eff <- n_round_eff + 1L

    present_agents <- unique(tasks_all$agent_id)

    # Truthful per-agent utility (true value of allocated tasks - payment).
    truth_util <- function(aid) {
      m <- res_truth$agent_id == aid
      if (!any(m)) return(0)
      tid_true <- setNames(true_ev[tasks_all$agent_id == aid],
                           tasks_all$task_id[tasks_all$agent_id == aid])
      sum(tid_true[res_truth$task_id[m]], na.rm = TRUE) - sum(res_truth$vcg_payment[m])
    }

    for (aid in present_agents) {
      rows_a   <- which(tasks_all$agent_id == aid)
      tid_true <- setNames(true_ev[rows_a], tasks_all$task_id[rows_a])
      u_truth  <- truth_util(aid)

      for (si in seq_along(shades)) {
        alpha <- shades[si]
        rep_tasks <- tasks_all
        rep_tasks$value_base[rows_a] <- tasks_all$value_base[rows_a] * alpha
        res <- vcg_allocate(rep_tasks, env, util_hat, blb, sm,
                            lambda_l_default = lambda_l_default)
        m   <- res$agent_id == aid
        u_alpha <- if (!any(m)) 0 else
          sum(tid_true[res$task_id[m]], na.rm = TRUE) - sum(res$vcg_payment[m])
        regret_sum[si] <- regret_sum[si] + (u_truth - u_alpha)
      }
      ar_count <- ar_count + 1L
    }
  }

  mean_payment <- if (n_round_eff > 0) payment_acc / n_round_eff else 0
  mean_welfare <- if (n_round_eff > 0) welfare_acc / n_round_eff else 0
  regret_mean  <- if (ar_count > 0) regret_sum / ar_count else regret_sum

  out <- tibble(
    graph_type   = graph_type,
    load_level   = load_level,
    N            = as.integer(N),
    seed         = seed,
    cap          = cap,
    mean_payment = mean_payment,
    mean_welfare = mean_welfare,
    binding      = mean_payment > 1e-6
  )
  for (si in seq_along(shades)) {
    out[[sprintf("regret_%g", shades[si])]] <- regret_mean[si]
  }
  out
}


#' Aggregate Exp.7 results across Monte Carlo seeds.
#'
#' @param results_list List of single-seed tibbles from exp7a_run_single().
#' @return A tibble with one row per graph_type, means of mean_payment,
#'   mean_welfare, and every regret_* column.
exp7_aggregate <- function(results_list) {
  bind_rows(results_list) %>%
    group_by(graph_type) %>%
    summarise(
      across(
        starts_with("regret_") | c(mean_payment, mean_welfare),
        \(x) mean(x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}


# ===========================================================================
# Experiment 7b: non-uniform JOINT misreports under VCG.
#
# Exp.7a shades every task of an agent by one scalar, so it cannot see a
# deviation in which one task's report moves another task's payment. Exp.7b
# gives the agent a named, finite strategy set that contains such deviations and
# reports the WORST CASE over it.
#
# Sign convention: br_gain = u(best member) - u(truthful), so a profitable
# deviation is POSITIVE and DSIC implies br_gain <= 0. This is the opposite sign
# of exp7a's regret_* columns (regret = u(truthful) - u(alpha) >= 0 under DSIC).
# The two families are deliberately named apart and never mixed in one table.
# ===========================================================================

#' The named, finite joint-misreport strategy set for one agent.
#'
#' Members (k = the agent's task count in the round):
#'   truthful (1), uniform_<alpha> for alpha in {0.5,0.7,0.9,1.1,1.3} (5),
#'   swap_high_low / swap_low_high (2, only when k >= 2, since they need a
#'   distinct highest- and lowest-value task), independent_<pattern> (the
#'   exhaustive 2^k grid over {0.7, 1.3} when k <= 4, else the 2k single-task
#'   deviations), drop_highest and keep_highest_only (2). Members that repeat an
#'   earlier member's multiplier vector are dropped, so the grid's all-low and
#'   all-high corners are reported under their uniform-shade names.
#'
#' @param ev_focal Numeric vector of the focal agent's TRUE expected task values,
#'   in task order. Only its length and its argmax/argmin are used.
#' @return A named list of multiplier vectors of length k, with attribute
#'   `independent_fallback` = TRUE when the exhaustive grid was replaced by the
#'   single-task fallback.
misreport_strategy_set <- function(ev_focal) {
  k <- length(ev_focal)
  stopifnot(k >= 1L)
  one <- rep(1, k)

  S <- list(truthful = one)
  for (a in c(0.5, 0.7, 0.9, 1.1, 1.3)) S[[sprintf("uniform_%g", a)]] <- rep(a, k)

  hi <- which.max(ev_focal)
  lo <- which.min(ev_focal)
  if (k >= 2L) {
    S$swap_high_low <- replace(one, c(hi, lo), c(1.3, 0.7))
    S$swap_low_high <- replace(one, c(hi, lo), c(0.7, 1.3))
  }

  fallback <- k > 4L
  if (!fallback) {
    grid <- expand.grid(rep(list(c(0.7, 1.3)), k), KEEP.OUT.ATTRS = FALSE)
    for (r in seq_len(nrow(grid))) {
      v <- as.numeric(grid[r, ])
      S[[paste0("independent_", paste0(ifelse(v < 1, "L", "H"), collapse = ""))]] <- v
    }
  } else {
    for (i in seq_len(k)) {
      S[[sprintf("independent_t%d_L", i)]] <- replace(one, i, 0.7)
      S[[sprintf("independent_t%d_H", i)]] <- replace(one, i, 1.3)
    }
  }

  S$drop_highest      <- replace(one, hi, 0)
  S$keep_highest_only <- replace(rep(0, k), hi, 1)

  # Members that are the same multiplier vector are the same market clearing.
  # Keep the first name in S order (truthful, uniform, swap, independent,
  # withholding), which is the conservative label for a tie.
  S <- S[!duplicated(vapply(S, paste, character(1), collapse = "/"))]

  attr(S, "independent_fallback") <- fallback
  S
}


#' Require the identical-bundle environment both Exp.7 arms are pinned to.
#'
#' Value-greedy is the exact welfare argmax only while every task carries the
#' same per-tier bundle (see this file's header), and that exactness is what
#' makes vcg_allocate DSIC here. Under per-task recipes .greedy_pack_by is a
#' heuristic, the argmax property is gone, and a positive best-response gain
#' would be a packing artefact reported as a mechanism result. The
#' heterogeneous-recipe experiment therefore lives on its own branch and this
#' guard is the contract between the two.
#'
#' @param env   Environment list from init_environment().
#' @param tasks Optional task tibble to check alongside the environment.
#' @return TRUE invisibly; stops with an informative error otherwise.
exp7_require_identical_bundles <- function(env, tasks = NULL) {
  why <- paste("The Exp.7 arms are pinned to the identical-bundle environment:",
               "value-greedy is the exact welfare argmax only when every task carries",
               "the same per-tier bundle, which is what makes vcg_allocate DSIC here.",
               "Run the heterogeneous-recipe arm in its own experiment.")
  if (!is.null(env$recipes)) {
    stop("exp7: heterogeneous-recipe environment refused (env$recipes is present). ",
         why, call. = FALSE)
  }
  if (!is.null(tasks) && "recipe" %in% names(tasks)) {
    stop("exp7: heterogeneous tasks refused (tasks carry a `recipe` column). ",
         why, call. = FALSE)
  }
  per_task <- !is.null(env$demand_weights) &&
    "task_id" %in% names(env$demand_weights)
  if (per_task || anyDuplicated(task_bundle(env)$tier)) {
    stop("exp7: non-identical task bundles refused (the environment's bundle is ",
         "not one row per tier). ", why, call. = FALSE)
  }
  invisible(TRUE)
}


#' Reduce one agent-round's gains over the strategy set to its worst case.
#'
#' @param gains Named numeric vector of br_gain per member, as returned over
#'   misreport_strategy_set() (the `truthful` member gains exactly 0).
#' @param tol Label-only tolerance: a maximum at or below it is the value
#'   model's floating-point noise, so the member is reported as `truthful`. The
#'   returned magnitude is always raw.
#' @return A list: `max` (the worst case over the set) and `member` (its name).
br_gain_reduce <- function(gains, tol = 1e-9) {
  best <- max(gains)
  list(max    = best,
       member = if (best > tol) names(gains)[which.max(gains)] else "truthful")
}


#' Run a single Exp.7b configuration (one seed): worst-case joint misreport.
#'
#' Each round every agent reports truthfully and the VCG allocation + payments
#' are computed. Then, for each agent in turn (others held truthful), every
#' member of misreport_strategy_set() is applied to that agent's reports: the
#' market clears on the REPORTED values and the agent's utility is scored at its
#' TRUE values minus the payment it incurs. br_gain(member) = u(member) -
#' u(truthful), so a profitable deviation is positive and DSIC implies <= 0.
#'
#' Reductions: `br_gain_max` is the WORST CASE, the largest gain any agent
#' achieves in any round, i.e. the max over rounds of the per-round maximum over
#' agents of the per-agent maximum over the strategy set. It is a max and not a
#' mean because a deviation that pays in one round refutes DSIC whatever the
#' other rounds do, and because the seed-level reduction is a max too.
#' `br_gain_mean` is the same worst case averaged instead of maximised: the mean
#' over agent-rounds of the per-agent maximum over the strategy set. It is the
#' secondary statistic and the continuity link to the exp7a numbers, and it
#' deliberately does NOT average over the members of S, which would measure how
#' bad the punitive members of our own strategy set are rather than how close
#' any deviation comes to paying. `br_gain_member_argmax` names the member
#' attaining `br_gain_max`, ties resolving to `truthful` (gain exactly 0).
#'
#' @inheritParams exp7a_run_single
#' @param tol Numerical noise floor of the value model's floating-point path. It
#'   labels only: a maximum at or below `tol` leaves `br_gain_member_argmax` at
#'   `truthful`, so a gain of 1e-15 is not reported as a profitable joint
#'   deviation. Every reported magnitude is raw.
#' @return A one-row tibble: config + mean_payment + mean_welfare + binding +
#'   br_gain_max + br_gain_mean + br_gain_member_argmax +
#'   independent_fallback_rate (the share of agent-rounds with more than four
#'   tasks, where the independent block falls back to single-task deviations).
exp7b_run_single <- function(graph_type = c("tree", "sp", "agentic"),
                             load_level = c("high", "medium"),
                             N = 8L, seed = 1L, cap = 30,
                             n_rounds = 30L, util_hat = 0.5,
                             deadlines = c(500L, 750L, 1000L),
                             lambda_l_default = 0.005, tol = 1e-9) {
  graph_type <- match.arg(graph_type)
  load_level <- match.arg(load_level)
  set.seed(seed)

  graph <- build_dependency_graph(graph_type)
  env   <- init_environment(graph, load_level, n_agents = N,
                            graph_type = graph_type)
  exp7_require_identical_bundles(env)
  env$capacities <- mutate(env$capacities, capacity = cap)

  agents <- init_agents(N)
  blb    <- base_latency_for_bids(env)
  sm     <- init_success_model()

  payment_acc <- 0
  welfare_acc <- 0
  n_round_eff <- 0L
  round_max   <- numeric(0)     # per-round max over agents
  ar_gain_sum <- 0              # sum of the per-agent-round worst cases
  best_gain   <- -Inf
  best_member <- "truthful"
  fallback_n  <- 0L
  ar_count    <- 0L

  for (t in seq_len(n_rounds)) {
    agent_split <- split(agents, agents$agent_id)
    tasks_list  <- lapply(agent_split, function(a)
      generate_tasks(a, env, round = t, deadlines = deadlines))
    tasks_all <- bind_tasks(tasks_list)
    if (nrow(tasks_all) == 0) next
    if (n_round_eff == 0L) exp7_require_identical_bundles(env, tasks_all)

    true_ev <- task_expected_value(tasks_all, util_hat, blb, sm,
                                   lambda_l_default = lambda_l_default)

    res_truth <- vcg_allocate(tasks_all, env, util_hat, blb, sm,
                              lambda_l_default = lambda_l_default)
    payment_acc <- payment_acc + sum(res_truth$vcg_payment)
    welfare_acc <- welfare_acc + sum(res_truth$realised_value)
    n_round_eff <- n_round_eff + 1L

    round_best <- -Inf   # the truthful member supplies the zero, not a floor
    for (aid in unique(tasks_all$agent_id)) {
      rows_a   <- which(tasks_all$agent_id == aid)
      tid_true <- setNames(true_ev[rows_a], tasks_all$task_id[rows_a])

      m0      <- res_truth$agent_id == aid
      u_truth <- if (!any(m0)) 0 else
        sum(tid_true[res_truth$task_id[m0]], na.rm = TRUE) -
        sum(res_truth$vcg_payment[m0])

      S     <- misreport_strategy_set(true_ev[rows_a])
      gains <- vapply(S, function(mult) {
        rep_tasks <- tasks_all
        rep_tasks$value_base[rows_a] <- tasks_all$value_base[rows_a] * mult
        res <- vcg_allocate(rep_tasks, env, util_hat, blb, sm,
                            lambda_l_default = lambda_l_default)
        m   <- res$agent_id == aid
        u   <- if (!any(m)) 0 else
          sum(tid_true[res$task_id[m]], na.rm = TRUE) - sum(res$vcg_payment[m])
        u - u_truth
      }, numeric(1))

      red <- br_gain_reduce(gains, tol)
      if (red$max > best_gain) {
        best_gain   <- red$max
        best_member <- red$member
      }
      round_best  <- max(round_best, red$max)
      ar_gain_sum <- ar_gain_sum + red$max
      fallback_n  <- fallback_n + attr(S, "independent_fallback")
      ar_count    <- ar_count + 1L
    }
    round_max <- c(round_max, round_best)
  }

  mean_payment <- if (n_round_eff > 0) payment_acc / n_round_eff else 0
  mean_welfare <- if (n_round_eff > 0) welfare_acc / n_round_eff else 0

  tibble(
    graph_type   = graph_type,
    load_level   = load_level,
    N            = as.integer(N),
    seed         = seed,
    cap          = cap,
    mean_payment = mean_payment,
    mean_welfare = mean_welfare,
    binding      = mean_payment > 1e-6,
    br_gain_max  = if (length(round_max) > 0) max(round_max) else 0,
    br_gain_mean = if (ar_count > 0) ar_gain_sum / ar_count else 0,
    br_gain_member_argmax     = factor(best_member),
    independent_fallback_rate = if (ar_count > 0) fallback_n / ar_count else 0
  )
}


#' Aggregate Exp.7b results across Monte Carlo seeds.
#'
#' The worst case is reduced by MAX across seeds (a deviation that pays in one
#' seed is not averaged away); the secondary mean is reduced by mean.
#'
#' @param results_list List of single-seed tibbles from exp7b_run_single().
#' @return One row per graph_type: br_gain_max, br_gain_mean, mean_payment,
#'   mean_welfare, binding_rate, independent_fallback_rate, and
#'   argmax_members, the "member=count" frequency table of the argmax member.
exp7b_aggregate <- function(results_list) {
  bind_rows(results_list) %>%
    group_by(graph_type) %>%
    summarise(
      br_gain_max               = max(br_gain_max, na.rm = TRUE),
      br_gain_mean              = mean(br_gain_mean, na.rm = TRUE),
      mean_payment              = mean(mean_payment, na.rm = TRUE),
      mean_welfare              = mean(mean_welfare, na.rm = TRUE),
      binding_rate              = mean(binding),
      independent_fallback_rate = mean(independent_fallback_rate),
      argmax_members            = {
        tb <- table(as.character(br_gain_member_argmax))
        paste(names(tb), tb, sep = "=", collapse = "; ")
      },
      .groups = "drop"
    )
}
