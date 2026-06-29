# sim_helpers.R
# ---------------------------------------------------------------------------
# Shared helper functions used by all four experiments.
#
# Contents:
#   - DAG construction (build_dependency_graph)
#   - Environment and agent initialisation
#   - Task generation
#   - Allocation execution (critical-path DAG latency + M/M/1 queueing)
#   - Trust updates (asymmetric reward/penalty)
#   - Per-tier utilisation (offered-load congestion forecast)
#   - Utility functions (bind_tasks, base_latency_for_bids)
#
# Paper reference: Section VII (Simulation Setup) and Table V (Parameters).
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
})

`%||%` <- function(x, y) if (is.null(x)) y else x


# ===========================================================================
# DAG construction
# ===========================================================================

#' Build a service-dependency DAG for a given topology.
#'
#' Each topology defines a set of nodes (with tier assignments), directed
#' edges, and per-tier demand weights that control how much capacity each
#' task consumes on each resource tier.
#'
#' @param graph_type One of "linear", "tree", "sp", "entangled".
#' @return A list with components: nodes (tibble), edges (tibble), demand_weights (tibble).
build_dependency_graph <- function(graph_type) {
  if (graph_type == "linear") {
    # Linear chain: in -> pre -> edge_inf -> cloud_inf -> post
    # Balanced demand across tiers; polymatroidal feasibility region.
    nodes <- tibble(
      node = c("in", "pre", "edge_inf", "cloud_inf", "post"),
      tier = c("device", "device", "edge", "cloud", "cloud")
    )
    edges <- tribble(
      ~from,       ~to,
      "in",        "pre",
      "pre",       "edge_inf",
      "edge_inf",  "cloud_inf",
      "cloud_inf", "post"
    )
    demand_weights <- tibble(
      tier = c("device", "edge", "cloud"),
      demand_weight = c(2, 1, 2)
    )

  } else if (graph_type == "tree") {
    # Tree: two device inputs merge at edge, then linear to cloud.
    # Balanced demand; polymatroidal feasibility region.
    nodes <- tibble(
      node = c("in1", "in2", "pre1", "edge_inf", "cloud_inf", "post"),
      tier = c("device", "device", "edge", "edge", "cloud", "cloud")
    )
    edges <- tribble(
      ~from,       ~to,
      "in1",       "pre1",
      "in2",       "pre1",
      "pre1",      "edge_inf",
      "edge_inf",  "cloud_inf",
      "cloud_inf", "post"
    )
    demand_weights <- tibble(
      tier = c("device", "edge", "cloud"),
      demand_weight = c(2, 2, 2)
    )

  } else if (graph_type == "sp") {
    # Series-parallel: two parallel inference branches merged at post.
    # Cloud-heavy demand (parallel branches create high cloud load).
    nodes <- tibble(
      node = c("in1", "in2", "pre1", "pre2",
               "edge_inf1", "edge_inf2",
               "cloud_inf1", "cloud_inf2", "post"),
      tier = c("device", "device", "edge", "edge",
               "edge", "edge",
               "cloud", "cloud", "cloud")
    )
    edges <- tribble(
      ~from,        ~to,
      "in1",        "pre1",
      "in2",        "pre2",
      "pre1",       "edge_inf1",
      "pre2",       "edge_inf2",
      "edge_inf1",  "cloud_inf1",
      "edge_inf2",  "cloud_inf2",
      "cloud_inf1", "post",
      "cloud_inf2", "post"
    )
    demand_weights <- tibble(
      tier = c("device", "edge", "cloud"),
      demand_weight = c(2, 4, 5)
    )

  } else if (graph_type == "entangled") {
    # Entangled: cross-edges between branches create complementarities.
    # Heavy demand on ALL tiers with asymmetric ratios (device-heavy),
    # making tatonnement price adjustment harder to stabilise.
    nodes <- tibble(
      node = c("in1", "in2", "pre1", "pre2", "pre3",
               "edge_inf1", "edge_inf2", "feature",
               "cloud_inf", "post"),
      tier = c("device", "device", "device", "device", "device",
               "edge", "edge", "edge",
               "cloud", "cloud")
    )
    edges <- tribble(
      ~from,       ~to,
      "in1",       "pre1",
      "in2",       "pre2",
      "in2",       "pre3",
      "pre1",      "edge_inf2",
      "pre2",      "edge_inf2",
      "pre3",      "edge_inf1",
      "edge_inf1", "feature",
      "edge_inf2", "feature",
      "feature",   "cloud_inf",
      "cloud_inf", "post"
    )
    demand_weights <- tibble(
      tier = c("device", "edge", "cloud"),
      demand_weight = c(5, 4, 3)
    )

  } else if (graph_type == "agentic") {
    # REAL agentic workload (Exp.9). Structure and per-tier demand
    # weights are MEASURED from a real multi-step LLM tool-use agent
    # (plan -> 2 parallel tool calls -> aggregate) run on a local model;
    # see agentic/run_agent_workload.py + agentic/agentic_profile.json for
    # provenance and reproduction. The DAG is series-parallel (the canonical
    # tool-using agent pattern); demand weights are the measured mean token
    # counts per tier, normalised (device/edge/cloud = 1.11/1.0/2.25 from
    # mistral-7B: plan light, parallel tools moderate, aggregate heaviest).
    nodes <- tibble(
      node = c("plan", "tool0", "tool1", "aggregate"),
      tier = c("device", "edge", "edge", "cloud")
    )
    edges <- tribble(
      ~from,   ~to,
      "plan",  "tool0",
      "plan",  "tool1",
      "tool0", "aggregate",
      "tool1", "aggregate"
    )
    demand_weights <- tibble(
      tier = c("device", "edge", "cloud"),
      demand_weight = c(1.11, 1.0, 2.25)   # measured; see agentic_profile.json
    )

  } else {
    stop("Unknown graph_type: ", graph_type)
  }

  list(nodes = nodes, edges = edges, demand_weights = demand_weights)
}


# ===========================================================================
# Environment initialisation
# ===========================================================================

#' Initialise the simulation environment.
#'
#' Creates a three-tier (device/edge/cloud) environment with per-tier
#' capacities, base latencies, and topology-aware demand weights.
#'
#' @param graph       DAG object from build_dependency_graph().
#' @param load_level  One of "low", "medium", "high" (maps to lambda = 0.5/1.0/1.5).
#' @param n_agents    Number of agents in the population.
#' @param graph_type  String identifying the DAG topology.
#' @return A list containing the full environment specification.
init_environment <- function(graph, load_level, n_agents, graph_type) {
  # Per-tier resource capacities (units: tasks that can be served per round)
  capacities <- tibble(
    tier     = c("device", "edge", "cloud"),
    capacity = c(200, 300, 500)
  )

  # Base processing latency per tier (ms), before queueing effects
  base_latency <- tibble(
    tier    = c("device", "edge", "cloud"),
    base_ms = c(5, 15, 50)
  )

  # Load factor: Poisson lambda for task arrivals per agent per round
  load_factor <- case_match(
    load_level,
    "low"    ~ 0.5,
    "medium" ~ 1.0,
    "high"   ~ 1.5,
    .default = 1.0
  )

  # Count nodes per tier (used as fallback demand weights)
  nodes_per_tier <- graph$nodes %>%
    count(tier, name = "nodes")

  # Use topology-specific demand_weights if available;
  # fall back to nodes_per_tier for backward compatibility
  demand_weights <- if (!is.null(graph$demand_weights)) {
    graph$demand_weights
  } else {
    nodes_per_tier %>% transmute(tier = tier, demand_weight = as.numeric(nodes))
  }

  # Join capacity and latency info for convenient per-tier lookups
  per_tier <- nodes_per_tier %>%
    left_join(capacities,   by = "tier") %>%
    left_join(base_latency, by = "tier")

  list(
    graph          = graph,
    graph_type     = graph_type,
    capacities     = capacities,
    base_latency   = base_latency,
    nodes_per_tier = nodes_per_tier,
    demand_weights = demand_weights,
    per_tier       = per_tier,
    load_level     = load_level,
    load_factor    = load_factor,
    n_agents       = n_agents,
    # Per-tier reserve / marginal-cost price: real services price > 0 even when
    # idle. Without it the clearing price is a pure congestion shadow price (0
    # when capacity is slack), making the agent-facing price intermittent and
    # price-volatility ill-formed. The clearing price is max(reserve, shadow).
    # Set so a task's per-tier resource bundle at reserve costs a modest fraction
    # (~1/3) of mean task value, leaving surplus for the market to allocate.
    reserve_price  = 0.04
  )
}


# ===========================================================================
# Agent initialisation
# ===========================================================================

#' Create the agent population.
#'
#' Agents are assigned consumer (60%) or provider (40%) roles at random.
#' All agents start with trust = 0.8.
#'
#' @param n_agents Number of agents.
#' @return A tibble with columns: agent_id, role, trust.
init_agents <- function(n_agents) {
  tibble(
    agent_id = seq_len(n_agents),
    role     = sample(c("consumer", "provider"), n_agents,
                      replace = TRUE, prob = c(0.6, 0.4)),
    trust    = 0.8
  )
}


# ===========================================================================
# Task generation
# ===========================================================================

#' Generate tasks for a single agent in a given round.
#'
#' The number of tasks follows a Poisson distribution with lambda equal to
#' the environment's load_factor. Each task has a random deadline and a
#' base value drawn from Uniform[1, 2].
#'
#' @param agent_row  Single-row tibble for this agent.
#' @param env        Environment list from init_environment().
#' @param round      Current simulation round (used for task ID generation).
#' @param deadlines  Integer vector of possible deadlines (ms).
#' @return A tibble of tasks, or NULL if no tasks are generated.
generate_tasks <- function(agent_row, env, round, deadlines = c(500L, 750L, 1000L)) {
  lambda  <- env$load_factor
  n_tasks <- rpois(1, lambda)
  if (n_tasks == 0) return(NULL)

  tibble(
    task_id    = paste0("a", agent_row$agent_id, "_t", round, "_", seq_len(n_tasks)),
    agent_id   = agent_row$agent_id,
    deadline   = sample(deadlines, n_tasks, replace = TRUE),
    value_base = runif(n_tasks, 1, 2)
  )
}


# ===========================================================================
# Allocation execution
# ===========================================================================

#' Execute an allocation: compute realised latency and deadline success.
#'
#' Latency model (execution phase):
#'   1. Compute per-tier utilisation rho = demand / capacity.
#'   2. M/M/1-inspired queueing delay: queue_term = lf * rho/(1-rho) * 2,
#'      capped at 500 ms to avoid divergence near rho = 1.
#'   3. Per-tier latency = base_ms + queue_term.
#'   4. DAG critical-path algorithm (topological order, longest path)
#'      determines the end-to-end latency for the entire pipeline.
#'   5. Per-task latency is drawn from N(critical_path, 0.1 * critical_path)
#'      to model execution-time noise (CV = 10%).
#'
#' If efficiency_factor is provided (hybrid architecture), effective demand
#' per task is reduced by that factor, modelling the integrator's internal
#' scheduling optimisation.
#'
#' @param allocation       Tibble of accepted tasks (task_id, agent_id, deadline, value_base).
#' @param env              Environment list.
#' @param efficiency_factor Optional multiplier < 1 reducing effective demand (hybrid mode).
#' @param enc_overhead_ms   Additive encapsulation/protocol-translation latency
#'                          (ms) charged by the integrator on the hybrid path
#'                          (Exp.12). Added to the critical-path latency
#'                          before the deadline check, so it raises latency,
#'                          deadline misses, and lowers welfare. Default 0.
#' @return A tibble with columns: task_id, agent_id, latency, deadline, success.
execute_allocation <- function(allocation, env, efficiency_factor = NULL,
                               enc_overhead_ms = 0) {
  n_tasks <- nrow(allocation)
  if (n_tasks == 0) {
    return(tibble(
      task_id  = character(),
      agent_id = integer(),
      latency  = numeric(),
      deadline = numeric(),
      success  = logical()
    ))
  }

  graph    <- env$graph
  nodes_df <- graph$nodes
  edges_df <- graph$edges

  # Demand weights: topology-aware per-tier demand per task
  dw <- if (!is.null(env$demand_weights)) {
    env$demand_weights
  } else {
    env$nodes_per_tier %>% transmute(tier = tier, demand_weight = as.numeric(nodes))
  }

  # If hybrid architecture, integrator reduces effective execution-time demand
  eff <- if (!is.null(efficiency_factor)) efficiency_factor else 1.0

  # ---- 1. Per-tier utilisation and queueing latency ----
  per_tier <- env$per_tier %>%
    left_join(dw, by = "tier") %>%
    mutate(
      demand_weight = coalesce(demand_weight, as.numeric(nodes)),
      demand        = demand_weight * n_tasks * eff,
      rho           = pmin(0.99, demand / pmax(capacity, 1)),
      # M/M/1-inspired queueing delay, capped at 500 ms
      queue_term    = env$load_factor * (rho / (1 - rho + 1e-3)) * 2,
      tier_latency  = base_ms + pmin(queue_term, 500)
    )

  L_tier <- per_tier$tier_latency
  names(L_tier) <- per_tier$tier

  # ---- 2. Per-node latency from its tier ----
  nodes_df <- nodes_df %>%
    mutate(node_latency = as.numeric(L_tier[tier]))

  # ---- 3. Critical path on the DAG (longest path via topological sort) ----
  indeg <- edges_df %>%
    count(to, name = "indeg") %>%
    right_join(nodes_df %>% select(node), by = c("to" = "node")) %>%
    transmute(node = to, indeg = replace_na(indeg, 0))

  dist <- nodes_df %>%
    transmute(node, dist = -Inf) %>%
    deframe()

  # Initialise source nodes (zero in-degree)
  zero_indeg_nodes <- indeg %>%
    filter(indeg == 0) %>%
    pull(node)

  for (v in zero_indeg_nodes) {
    dist[v] <- nodes_df$node_latency[nodes_df$node == v]
  }

  # BFS-style topological traversal
  adj   <- split(edges_df$to, edges_df$from)
  queue <- zero_indeg_nodes

  while (length(queue) > 0) {
    u     <- queue[1]
    queue <- queue[-1]
    if (!is.null(adj[[u]])) {
      for (v in adj[[u]]) {
        cand <- dist[u] + nodes_df$node_latency[nodes_df$node == v]
        if (cand > dist[v]) {
          dist[v] <- cand
        }
        indeg$indeg[indeg$node == v] <- indeg$indeg[indeg$node == v] - 1
        if (indeg$indeg[indeg$node == v] == 0) {
          queue <- c(queue, v)
        }
      }
    }
  }

  critical_latency <- max(dist[is.finite(dist)])

  # Integrator encapsulation/protocol-translation overhead (Exp.12):
  # an additive latency charged on the encapsulated (hybrid) path.
  critical_latency <- critical_latency + enc_overhead_ms

  # ---- 4. Per-task latency with Gaussian noise (CV = 10%) ----
  allocation %>%
    mutate(
      latency = rnorm(n(), mean = critical_latency, sd = 0.1 * critical_latency),
      success = latency <= deadline
    )
}


# ===========================================================================
# Trust updates
# ===========================================================================

#' Update agent trust scores based on round outcomes.
#'
#' Asymmetric update: successful agents gain +reward, agents with any
#' failure lose -penalty (failure-dominant). Trust is clipped to [0, 1].
#'
#' @param agents    Tibble of agents with a trust column.
#' @param results_t Tibble of execution results with success column.
#' @param reward    Trust increment for pure-success agents (default: 0.03).
#' @param penalty   Trust decrement for agents with any failure (default: 0.08).
#' @return Updated agents tibble.
update_trust <- function(agents, results_t, reward = 0.03, penalty = 0.08) {
  if (nrow(results_t) == 0) return(agents)

  fail_ids    <- results_t %>% filter(!success) %>% pull(agent_id) %>% unique()
  success_ids <- results_t %>% filter(success)  %>% pull(agent_id) %>% unique()
  pure_success <- setdiff(success_ids, fail_ids)

  agents %>%
    mutate(
      trust = case_when(
        agent_id %in% fail_ids     ~ pmax(0, trust - penalty),
        agent_id %in% pure_success ~ pmin(1, trust + reward),
        TRUE                       ~ trust
      )
    )
}


# ===========================================================================
# Utilisation and stability metrics
# ===========================================================================

#' Compute per-tier utilisation as demand / capacity.
#'
#' @param env               Environment list.
#' @param n_tasks_generated  Number of tasks generated this round (demand proxy).
#' @param capacity_override  Optional tibble overriding env$capacities (used for
#'                           governance capacity splits in Exp3).
#' @return A tibble with columns: tier, util.
compute_utilisation_per_tier <- function(env, n_tasks_generated,
                                         capacity_override = NULL) {
  capacities <- if (!is.null(capacity_override)) capacity_override else env$capacities

  if (n_tasks_generated == 0) {
    return(capacities %>% mutate(util = 0))
  }

  dw <- if (!is.null(env$demand_weights)) {
    env$demand_weights
  } else {
    env$nodes_per_tier %>% transmute(tier = tier, demand_weight = as.numeric(nodes))
  }

  dw %>%
    left_join(capacities, by = "tier") %>%
    mutate(
      demand = demand_weight * n_tasks_generated,
      util   = demand / capacity
    ) %>%
    select(tier, util)
}


# ===========================================================================
# Utility functions (shared across experiments)
# ===========================================================================

#' Bind a list of task tibbles into a single tibble.
#'
#' Handles NULL entries and empty lists gracefully.
#'
#' @param tasks_list List of tibbles (possibly with NULLs).
#' @return A single tibble of all tasks.
bind_tasks <- function(tasks_list) {
  tasks_list <- purrr::compact(tasks_list)
  if (length(tasks_list) == 0L) {
    tibble(
      task_id    = character(),
      agent_id   = integer(),
      deadline   = numeric(),
      value_base = numeric()
    )
  } else {
    bind_rows(tasks_list)
  }
}

#' Compute a reasonable base latency estimate for bid valuation.
#'
#' Uses the mean per-tier latency from the environment if available;
#' otherwise falls back to 50 ms.
#'
#' @param env Environment list.
#' @return Scalar base latency estimate (ms).
base_latency_for_bids <- function(env) {
  if ("per_tier" %in% names(env) &&
      is.data.frame(env$per_tier) &&
      "latency" %in% names(env$per_tier)) {
    return(mean(env$per_tier$latency, na.rm = TRUE))
  }
  50
}
