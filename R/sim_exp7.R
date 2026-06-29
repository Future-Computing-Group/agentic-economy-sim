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
#' @param util_hat   Exogenous congestion estimate fed to valuations.
#' @return A one-row tibble: config + mean_payment + binding flag + one
#'         regret_<alpha> column per shading factor + mean welfare.
exp7a_run_single <- function(graph_type = c("tree", "sp", "agentic"),
                             load_level = c("high", "medium"),
                             N = 8L, seed = 1L, cap = 30,
                             shades = c(0.5, 0.7, 0.9, 1.0, 1.1, 1.3),
                             n_rounds = 30L, util_hat = 0.5) {
  graph_type <- match.arg(graph_type)
  load_level <- match.arg(load_level)
  set.seed(seed)

  graph <- build_dependency_graph(graph_type)
  env   <- init_environment(graph, load_level, n_agents = N,
                            graph_type = graph_type)
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
      generate_tasks(a, env, round = t, deadlines = c(500L, 750L, 1000L)))
    tasks_all <- bind_tasks(tasks_list)
    if (nrow(tasks_all) == 0) next

    # Truthful valuations (exogenous within the round).
    true_ev <- task_expected_value(tasks_all, util_hat, blb, sm)

    res_truth <- vcg_allocate(tasks_all, env, util_hat, blb, sm)
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
        res <- vcg_allocate(rep_tasks, env, util_hat, blb, sm)
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
