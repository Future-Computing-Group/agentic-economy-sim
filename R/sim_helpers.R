# sim_helpers.R
# ---------------------------------------------------------------------------
# Shared helper functions used by all four experiments.
#
# Contents:
#   - DAG construction (build_dependency_graph)
#   - Measured agentic profile (agentic_profile_path, agentic_base_latency,
#     agentic_deadlines, agentic_lambda_l)
#   - Environment and agent initialisation
#   - Task generation
#   - Critical path (critical_path_ms: longest path over the DAG)
#   - Allocation execution (critical-path DAG latency + M/M/1 queueing)
#   - Trust updates (asymmetric reward/penalty)
#   - Per-tier utilisation (offered-load congestion forecast, rho_bottleneck)
#   - Utility functions (bind_tasks, base_latency_for_bids)
#
# Paper reference: the evaluation's experimental setting (sec:simulation-setup)
# and the baseline parameter table (tab:sim-baseline-params).
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
# Measured agentic workload profile
# ===========================================================================

#' Path to the measured agentic workload profile.
#'
#' The pipeline tracks this file as a `format = "file"` target, so a regenerated
#' profile invalidates the results it parameterises.
#'
#' @return Absolute path to agentic/agentic_profile.json.
agentic_profile_path <- function() {
  here::here("agentic", "agentic_profile.json")
}

#' Per-tier mean stage latency (ms) of the real agentic workload.
#'
#' Read from agentic/agentic_profile.json rather than typed in, so a regenerated
#' profile cannot silently diverge from the environment it parameterises. The
#' same file supplies the demand weights already used by
#' build_dependency_graph("agentic").
#'
#' @param path Path to the measured profile.
#' @return Named numeric vector (device, edge, cloud) of mean stage latency (ms).
agentic_base_latency <- function(path = agentic_profile_path()) {
  tiers <- jsonlite::fromJSON(path)$tiers
  vapply(c("device", "edge", "cloud"),
         function(tr) tiers[[tr]]$mean_latency_ms, numeric(1))
}

#' Task deadlines for the agentic environment (ms).
#'
#' Rescaled from the environment's own zero-queue critical path rather than
#' inherited from the nominal environments, whose deadlines the real workload
#' misses by roughly a factor of three:
#'
#'   D_bar     = critical_path_ms(agentic graph, measured base latencies)
#'   deadlines = round_to_100(multipliers * D_bar)
#'
#' D_bar is 3350.5 ms, giving 4200, 5000 and 5900 ms. The multipliers are a
#' measurement-design choice, placed against the band realised latency can
#' occupy rather than against any outcome: the queueing term is capped at 500 ms
#' per tier and the agentic critical path visits three tiers, so latency lives in
#' [D_bar, D_bar + 1500] before execution noise of sd = 0.1 * critical path.
#' 4200 ms is inside that band and is missed once queueing bites, 5000 ms sits at
#' its top edge, and 5900 ms is above it by more than two standard deviations. A
#' deadline set entirely above the band would make the drop rate identically zero
#' and the experiment vacuous; one entirely below it would make it identically
#' one.
#'
#' @param multipliers Multiples of the zero-queue critical path.
#' @param path        Path to the measured profile.
#' @return Integer vector of deadlines (ms).
agentic_deadlines <- function(multipliers = c(1.25, 1.5, 1.75),
                              path = agentic_profile_path()) {
  d_bar <- critical_path_ms(build_dependency_graph("agentic"),
                            agentic_base_latency(path))
  as.integer(round(multipliers * d_bar / 100) * 100)
}

#' Value-decay rate for the agentic environment (per ms).
#'
#' lambda_l is the time constant of the task-value model,
#' V = v_base * exp(-lambda_l * latency). The nominal 0.005 per ms belongs to
#' the nominal environments, whose zero-queue critical path is 135 ms: it says
#' that about half a task's value survives its own pipeline. Applied unchanged
#' to a workload whose critical path is 3350.5 ms it leaves 5e-8 of that value,
#' so no task has positive surplus against the reserve price and the market
#' clears nothing at all: measured over 3 seeds at N = 200, clearing fraction
#' 0.000, welfare 0.000 and VCG payments 0.000 on both arms at both loads.
#'
#' It is therefore rescaled by the same rule as the deadlines and to the same
#' invariant: the share of a task's value surviving its own environment's
#' zero-queue critical path is held at the nominal 0.509, so the value model and
#' the deadline set are on one time scale. This is a calibration of the
#' environment to its measured latencies, not a tuning of any outcome.
#'
#' @param lambda_l_nominal Decay rate of the nominal environments (per ms).
#' @param path             Path to the measured profile.
#' @return Scalar decay rate (per ms) for the agentic environment.
agentic_lambda_l <- function(lambda_l_nominal = 0.005,
                             path = agentic_profile_path()) {
  nominal <- base_latency_for_bids(init_environment(build_dependency_graph("sp"),
                                                    "medium", 1L, "sp"))
  lambda_l_nominal * nominal /
    critical_path_ms(build_dependency_graph("agentic"), agentic_base_latency(path))
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

  # Base processing latency per tier (ms), before queueing effects. The agentic
  # environment takes the MEASURED per-stage latencies of the real workload; the
  # nominal environments keep the model's 5 / 15 / 50. Using only the measured
  # demand weights and not the measured latencies is what made the agentic
  # experiment score a real workload against deadlines it misses by a factor of
  # three.
  base_latency <- tibble(
    tier    = c("device", "edge", "cloud"),
    base_ms = if (graph_type == "agentic") {
      unname(agentic_base_latency()[c("device", "edge", "cloud")])
    } else {
      c(5, 15, 50)
    }
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


#' Scale every per-tier capacity an environment carries.
#'
#' The environment carries capacity twice: `capacities`, which admission prices
#' and packs against, and the copy joined into `per_tier`, which execution
#' queues against (rho = demand / capacity, execute_allocation). Scaling one and
#' not the other makes a capacity knob bind at admission and leave execution
#' queueing on the unscaled tiers, so a sweep over it measures a fraction of the
#' parameter it names. Both live here, next to the function that creates them.
#'
#' @param env    Environment list from init_environment().
#' @param factor Multiplier on per-tier capacity.
#' @return The environment, every capacity scaled.
scale_capacities <- function(env, factor) {
  if (factor == 1.0) return(env)
  env$capacities <- dplyr::mutate(env$capacities, capacity = capacity * factor)
  env$per_tier   <- dplyr::mutate(env$per_tier,   capacity = capacity * factor)
  env
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
# Critical path
# ===========================================================================

#' End-to-end latency of a DAG's critical path (longest path).
#'
#' The path is a TIER SEQUENCE: each node contributes the latency of its tier,
#' and the critical path is the source-to-sink sequence maximising that sum.
#' Computed by topological traversal (in-degree table, longest distance).
#'
#' Called with per-tier latencies including queueing (execute_allocation) or
#' with the per-tier base latencies alone, which gives the zero-queue critical
#' path (base_latency_for_bids).
#'
#' @param graph        DAG object from build_dependency_graph() (nodes, edges).
#' @param tier_latency Named numeric vector of per-tier latencies (ms), named
#'                     by tier.
#' @return Scalar critical-path latency (ms).
critical_path_ms <- function(graph, tier_latency) {
  nodes_df <- graph$nodes %>%
    mutate(node_latency = as.numeric(tier_latency[tier]))
  edges_df <- graph$edges

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

  max(dist[is.finite(dist)])
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
#'   4. critical_path_ms() turns those per-tier latencies into the end-to-end
#'      latency for the entire pipeline (longest path over the DAG).
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

  # Demand weights: topology-aware per-tier demand per task
  dw <- if (!is.null(env$demand_weights)) {
    env$demand_weights
  } else {
    env$nodes_per_tier %>% transmute(tier = tier, demand_weight = as.numeric(nodes))
  }

  # If hybrid architecture, integrator reduces effective execution-time demand
  eff <- if (!is.null(efficiency_factor)) efficiency_factor else 1.0

  # Realised per-tier demand of the ADMITTED MIX, when tasks carry recipes. The
  # count times the mean recipe is the ADVERTISED quantity; charging execution
  # that would make the deliverable set a copy of the advertised interface, and
  # an interface that over-commits a tier -- four tasks needing (2,1,1.5) and two
  # needing (1,2,1.5) draw (10,8,9) where six mean tasks draw (9,9,9) -- would
  # queue and miss deadlines exactly as if it had not.
  realised <- if (!is.null(env$recipes) && "recipe" %in% names(allocation)) {
    colSums(task_recipes(allocation, env))
  } else {
    NULL
  }

  # ---- 1. Per-tier utilisation and queueing latency ----
  per_tier <- env$per_tier %>%
    left_join(dw, by = "tier") %>%
    mutate(
      demand_weight = coalesce(demand_weight, as.numeric(nodes)),
      demand        = if (is.null(realised)) demand_weight * n_tasks * eff
                      else unname(realised[tier]) * eff,
      rho           = pmin(0.99, demand / pmax(capacity, 1)),
      # M/M/1-inspired queueing delay, capped at 500 ms
      queue_term    = env$load_factor * (rho / (1 - rho + 1e-3)) * 2,
      tier_latency  = base_ms + pmin(queue_term, 500)
    )

  # ---- 2-3. Critical path on the DAG, at the queued per-tier latencies ----
  critical_latency <- critical_path_ms(
    env$graph, setNames(per_tier$tier_latency, per_tier$tier)
  )

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

#' Bottleneck-tier offered load at a given load level.
#'
#' rho = max_r(w_r / C_r) * lambda * N: the busiest tier's demand per task times
#' the expected number of tasks per round. The operating-point criterion is
#' stated on this quantity and not on the across-tier mean, which cannot
#' separate a contended topology from a collapsed one (entangled at N = 75 has
#' mean utilisation 1.66 and a drop rate of 1.00). Shared by the calibration
#' sweep and the test that pins its outcome, so both read one definition.
#'
#' @param graph_type DAG topology.
#' @param n_agents   Agent population.
#' @param load_level One of "low", "medium", "high".
#' @return Scalar offered load.
rho_bottleneck <- function(graph_type, n_agents, load_level = "high") {
  env <- init_environment(build_dependency_graph(graph_type), load_level,
                          n_agents = n_agents, graph_type = graph_type)
  ratio <- env$demand_weights %>%
    left_join(env$capacities, by = "tier") %>%
    mutate(ratio = demand_weight / capacity) %>%
    pull(ratio)
  max(ratio) * env$load_factor * n_agents
}


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

#' Compute a base latency estimate for bid valuation.
#'
#' The estimate is the ZERO-QUEUE CRITICAL PATH of the environment's DAG: the
#' end-to-end latency a task would see at the per-tier base latencies, before
#' any queueing. That is what a bid-time estimate of end-to-end base latency
#' should be, and it is topology-aware, unlike a per-tier summary statistic.
#'
#' An environment that carries no per_tier (an ad-hoc stub) falls back to 50 ms;
#' a per_tier that carries no base_ms is an error rather than a silent fallback.
#'
#' @param env Environment list.
#' @return Scalar base latency estimate (ms).
base_latency_for_bids <- function(env) {
  if (!is.data.frame(env$per_tier)) return(50)
  stopifnot("env$per_tier carries no base_ms column" =
              "base_ms" %in% names(env$per_tier))
  critical_path_ms(env$graph,
                   setNames(env$per_tier$base_ms, env$per_tier$tier))
}
