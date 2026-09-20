# sim_nodelevel.R
# ---------------------------------------------------------------------------
# The node-level evaluation: one driver carrying the per-round loop for every
# experiment over a market whose resources are the service NODES of a
# dependency graph.
#
# The market kernel is untouched. clear_multitier_market, .greedy_pack_by,
# task_recipes, tier_capacities and compute_welfare all key on whatever labels
# the environment carries, and build_leaf_graph labels them by node. What this
# file adds is the instance at the evaluation's scale, the leaf-share draw that
# matches the demand stream across supply arms, the per-token latency model at
# bid time, and the exact reference the structural instrument is measured
# against.
#
# The six per-tier drivers are untouched and keep their own target names: this
# is a second evaluation over a second substrate, not a replacement of theirs.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
})


# ===========================================================================
# The instances, the leaf mix and the environment
# ===========================================================================

#' The instance an arm name selects.
#'
#' The three level names the pipeline already carries are kept and only what
#' constructs them changes, so every target grid, statistics grouping and plot
#' facet keyed on graph_type survives untouched. The mapping is stated once
#' here and asserted here: `tree` is the rooted tree, `sp` the
#' parallel-in-series instance, `entangled` the crossing one.
#'
#' @param graph_type One of "tree", "sp", "entangled".
#' @return An instance spec from leaf_instance_specs("scale").
node_instance <- function(graph_type = c("tree", "sp", "entangled")) {
  graph_type <- match.arg(graph_type)
  spec <- leaf_instance_specs("scale")[[
    c(tree = "T", sp = "S", entangled = "X")[[graph_type]]]]
  stopifnot(
    "the arm's laminarity verdict is not the instance's" =
      leaf_blocks_laminar(ancestor_matrix(spec)) == (graph_type != "entangled"))
  spec
}

#' The population that puts each arm at the same offered load.
#'
#' rho = lambda N / K_c, with K_c 100, 100 and 50 tokens. Matching it is the
#' improvement that removes the standing confound between a topology and the
#' population it was run at; it does not dissolve between sp and the other two,
#' where it holds constant a function of the treatment.
#'
#' @return Named integer vector, one population per arm.
node_agents <- function() c(tree = 90L, sp = 45L, entangled = 90L)

#' The value-decay rate the node instances are calibrated at.
#'
#' The nominal 0.005 per ms belongs to environments whose zero-queue critical
#' path is 135 ms; every node instance's is 70 ms. Rescaled by the same rule
#' the measured workload is rescaled by, so the share of a task's value
#' surviving its own pipeline is held where it was. Matched across arms, so it
#' confounds nothing.
#'
#' @return Scalar decay rate (per ms).
node_lambda_l <- function() 0.005 * 135 / 70

#' The leaf-share distribution a mix name selects.
#'
#' At uniform shares the over-commitment an interface arm measures on e1 is
#' max(0, 100 * 0.5 - 50) = 0, an exact knife-edge, so such an arm would
#' measure a null that is not a refutation. The skewed mix puts e1's charge at
#' 60 tokens against a capacity of 50 and lifts it above the multinomial
#' fluctuation. Every result row carries its mix and no contrast crosses mixes.
#'
#' @param leaf_mix One of "uniform", "skewed".
#' @param leaves   Character vector of leaves, in the instance's order.
#' @return Named numeric vector of shares, summing to one.
node_leaf_shares <- function(leaf_mix = c("uniform", "skewed"), leaves) {
  leaf_mix <- match.arg(leaf_mix)
  s <- switch(leaf_mix,
              uniform = rep(1 / length(leaves), length(leaves)),
              skewed  = c(0.55, 0.05, 0.20, 0.20))
  stopifnot("the mix needs one share per leaf" = length(s) == length(leaves))
  setNames(s, leaves)
}

#' Mix-average per-node demand weights at a leaf-share vector.
#'
#' A node's weight is the token weight times the share-weighted mean of its
#' ancestor indicator, which makes the basket clear_multitier_market records as
#' unit_cost exactly the token weight times the demand-weighted mean LEAF
#' price. That identity is what lets the price-dispersion metric stay the
#' function it already is.
#'
#' @param anc    An ancestor-indicator matrix.
#' @param shares Named numeric vector of leaf shares.
#' @param w      Token weight.
#' @return A tibble of `tier` (the node) and `demand_weight`.
node_demand_weights <- function(anc, shares, w) {
  tibble(tier = colnames(anc),
         demand_weight = as.numeric(w * (shares[rownames(anc)] %*% anc)))
}

#' The node-indexed environment of one arm and cell.
#'
#' @param graph_type One of "tree", "sp", "entangled".
#' @param load_level One of "low", "medium", "high".
#' @param N          Agent population.
#' @param leaf_mix   One of "uniform", "skewed".
#' @param spec       Instance spec; defaults to the arm's own. A contracted
#'                   instance is handed in here, which is why the arm's
#'                   laminarity verdict is asserted in node_instance and not
#'                   in this function.
#' @return An environment list, carrying its spec, ancestor matrix and shares.
node_env <- function(graph_type, load_level, N, leaf_mix = "uniform",
                     spec = node_instance(graph_type)) {
  anc    <- ancestor_matrix(spec)
  shares <- node_leaf_shares(leaf_mix, rownames(anc))
  w      <- token_weight_of(spec, colnames(anc))
  graph  <- build_leaf_graph(spec, node_demand_weights(anc, shares, w))

  env <- init_environment(graph, load_level, n_agents = N, graph_type = graph_type,
                          capacities   = leaf_capacities(spec),
                          base_latency = leaf_base_latency(spec))
  env$recipes     <- leaf_recipes(spec, "unit")
  env$spec        <- spec
  env$anc         <- anc
  env$leaf_shares <- shares
  env$leaf_mix    <- leaf_mix

  # The restriction that keeps every arm inside the semantics the leaf-block
  # region is defined on: a task is one token at one leaf, so its recipe is
  # the token weight times that leaf's ancestor indicator and the recipe
  # matrix IS the leaf-block incidence matrix. Asserted, not commented.
  for (l in rownames(anc)) {
    stopifnot("a unit recipe is not one leaf's ancestor indicator" =
                isTRUE(all.equal(unname(env$recipes[[l]]),
                                 unname(w * anc[l, ]))))
  }
  env
}

#' The interface and the smoothing an architecture level crosses.
#'
#' The four level names of the factorial are kept and only what they construct
#' changes: on this substrate encapsulation IS the contraction of the
#' integrator's cluster, and smoothing is the exponential moving average the
#' posted price is put through. The two are separable by construction, because
#' the pack runs at the raw clearing prices and the average is applied to what
#' the agent is charged.
#'
#' @param architecture One of the four cells of the factorial.
#' @return A list of `interface` and `beta`.
node_architecture <- function(architecture = c("naive", "naive_ema",
                                               "hybrid_noema", "hybrid_ema")) {
  architecture <- match.arg(architecture)
  list(interface = if (architecture %in% c("hybrid_noema", "hybrid_ema"))
                     "inner" else "off",
       beta      = if (architecture %in% c("naive_ema", "hybrid_ema")) 0.8 else 0)
}

#' The cluster an arm's integrator exports.
#'
#' @param graph_type One of "tree", "sp", "entangled".
#' @return Character vector of nodes.
node_cluster <- function(graph_type) {
  if (graph_type == "sp") c("e1", "e2", "e3") else c("e1", "e2")
}

#' The environment one arm and interface level clears against.
#'
#' At `off` this is the instance itself. At an interface level it is the
#' QUOTIENT: the market clears against the advertised region while delivery is
#' evaluated against the true instance, which is the split the interface arms
#' exist to measure.
#'
#' @param graph_type One of "tree", "sp", "entangled".
#' @param load_level One of "low", "medium", "high".
#' @param N          Agent population.
#' @param leaf_mix   One of "uniform", "skewed".
#' @param interface  "off", "inner" or "maxflow".
#' @return An environment list.
node_run_env <- function(graph_type, load_level, N, leaf_mix = "uniform",
                         interface = "off", advertise_frac = NULL,
                         spec = node_instance(graph_type)) {
  if (interface != "off") {
    cl <- node_cluster(graph_type)
    spec <- contract_cluster(
      spec, cl, interface,
      scalar = if (is.null(advertise_frac)) NULL
               else node_advertised_scalar(spec, cl, advertise_frac))
  }
  node_env(graph_type, load_level, N, leaf_mix, spec = spec)
}

#' An advertised scalar between the safe one and the aggregate.
#'
#' At 0 the interface advertises what every mix of it can be delivered; at 1 it
#' advertises the cluster's own node-split max flow, which is deliverable in
#' the best mix and not in the worst. In between it claims that its internal
#' routing carries that share of the difference.
#'
#' @param spec    An instance spec.
#' @param cluster The nodes the integrator exports.
#' @param frac    Share of the gap between the two scalars, in [0, 1].
#' @return Scalar token capacity.
node_advertised_scalar <- function(spec, cluster, frac) {
  inner <- token_capacity(contract_cluster(spec, cluster, "inner"))[["J"]]
  mf    <- token_capacity(contract_cluster(spec, cluster, "maxflow"))[["J"]]
  inner + frac * (mf - inner)
}

#' The sensitivity sweep's encapsulation knob, on this substrate.
#'
#' The per-tier knob is an assumed reduction in a task's demand, which a
#' node-level market has no place for: every arm charges the recipe the tokens
#' actually place. What an integrator can assume here is that its internal
#' routing carries more of the aggregate than the safe scalar, so the level is
#' read as the share of the safe scalar it claims to need. At the baseline of
#' 1.0 it assumes nothing and advertises the safe one, exactly as the per-tier
#' baseline assumes no savings.
#'
#' @param integ_efficiency A level of the per-tier efficiency knob.
#' @return The advertised fraction, in [0, 1].
node_advertise_frac <- function(integ_efficiency) 1 - integ_efficiency


# ===========================================================================
# The population sweep
# ===========================================================================

#' The populations the sweep steps over.
#'
#' Twenty points at a step of ten, refined to a step of five around every arm's
#' onset. A grid that steps by ten past a boundary at 67 states a coincidence
#' at ten per cent resolution; these brackets put every onset inside a window
#' of five, which is five per cent of the populations in question.
#'
#' @return Sorted numeric vector of 28 populations.
node_sweep_points <- function() {
  sort(union(union(seq(10, 200, by = 10), c(35, 45, 55, 65, 75)),
             c(95, 105, 115)))
}

#' The sweep's branch grid.
#'
#' @param n_seeds Monte Carlo seeds per cell.
#' @return A tibble with one row per branch.
node_sweep_grid <- function(n_seeds) {
  tidyr::expand_grid(graph_type = c("tree", "sp", "entangled"),
                     N          = node_sweep_points(),
                     load_level = c("medium", "high"),
                     seed       = seq_len(n_seeds))
}

#' Bottleneck offered load of a node instance at one population and load.
#'
#' The onset statistic is read against this rather than against the per-tier
#' function, because a node-indexed environment's busiest RESOURCE is a service
#' node and its demand weights depend on the cell's leaf mix.
#'
#' @param graph_type One of "tree", "sp", "entangled".
#' @param n_agents   Agent population.
#' @param load_level One of "low", "medium", "high".
#' @param leaf_mix   One of "uniform", "skewed".
#' @return Scalar offered load.
node_rho_bottleneck <- function(graph_type, n_agents, load_level = "high",
                                leaf_mix = "uniform") {
  rho_bottleneck(graph_type, n_agents, load_level,
                 env = node_run_env(graph_type, load_level, n_agents, leaf_mix))
}

# ===========================================================================
# The measured workload as a node-level instance
# ===========================================================================

#' The union of one or more recordings, as an instance spec.
#'
#' Nodes, arcs, per-stage token weights and per-stage base delays all come out
#' of the recordings; nothing here is drawn. Whether the union's leaf-block
#' family is laminar is therefore a MEASUREMENT: a run whose stages degenerate
#' produces a different graph and the certificate reports it.
#'
#' Capacity splits each physical tier's total across the stages that sit in
#' it, so the tier totals are preserved exactly, with one exception. A planner
#' stage is an ancestor of every leaf of its own pattern, so giving it a share
#' would make it bind on every subset, flatten the rank function and return
#' the arm to the uniform-matroid case the rebuild exists to leave behind. The
#' planners carry their whole tier and the stages that do the work are what
#' binds.
#'
#' A stage recorded by more than one pattern takes the mean of its weights and
#' delays; it is one service and the patterns are two samples of it.
#'
#' @param paths         Paths to measured profiles.
#' @param tier_capacity Named numeric vector of per-tier capacity totals.
#' @param planner_tier  The tier whose stages are given the whole tier total.
#' @return An instance spec with a per-node weight vector and base delays.
agentic_union_spec <- function(paths,
                               tier_capacity = c(device = 200, edge = 300,
                                                 cloud = 500),
                               planner_tier = "device") {
  gs <- lapply(paths, agentic_graph)
  stopifnot("a profile carries no graph block" =
              !any(vapply(gs, is.null, logical(1))))
  nodes <- dplyr::distinct(dplyr::bind_rows(lapply(gs, `[[`, "nodes")))
  edges <- dplyr::distinct(dplyr::bind_rows(lapply(gs, `[[`, "edges")))

  st <- dplyr::bind_rows(lapply(paths, agentic_stages)) %>%
    dplyr::group_by(.data$node) %>%
    dplyr::summarise(demand_weight   = mean(.data$demand_weight),
                     mean_latency_ms = mean(.data$mean_latency_ms),
                     .groups = "drop")
  i <- match(nodes$node, st$node)
  stopifnot("a recorded node has no stage aggregate" = !anyNA(i))

  per_tier <- table(nodes$tier)
  cap <- ifelse(nodes$tier == planner_tier,
                unname(tier_capacity[nodes$tier]),
                unname(tier_capacity[nodes$tier]) / as.numeric(per_tier[nodes$tier]))

  list(nodes  = tibble(node = nodes$node, phys = nodes$tier, capacity = cap,
                       base_ms = st$mean_latency_ms[i]),
       edges  = edges,
       weight = setNames(st$demand_weight[i], nodes$node))
}

#' The node-level environment of the measured workload.
#'
#' @param load_level One of "low", "medium", "high".
#' @param N          Agent population.
#' @param paths      Paths to the measured profiles the arm runs on.
#' @param leaf_mix   Leaf-share mix; uniform over whatever leaves were
#'                   recorded.
#' @return An environment list.
node_agentic_env <- function(load_level, N, paths = agentic_profile_paths(),
                             leaf_mix = "uniform") {
  node_env("agentic", load_level, N, leaf_mix,
           spec = agentic_union_spec(paths))
}

#' Deadlines and value decay rescaled to a node-level agentic instance.
#'
#' The same rule the per-tier agentic environment is rescaled by, read off the
#' instance's own slowest leaf rather than off one round-level critical path.
#'
#' @param spec        The instance spec.
#' @param multipliers Multiples of the zero-queue critical path.
#' @return A list of `deadlines` and `lambda_l`.
node_agentic_constants <- function(spec, multipliers = c(1.25, 1.5, 1.75)) {
  d_bar <- max(critical_path_to_leaves(build_leaf_graph(spec),
                                       leaf_base_ms(spec), leaf_set(spec)))
  list(deadlines = as.integer(round(multipliers * d_bar / 100) * 100),
       lambda_l  = 0.005 * 135 / d_bar)
}


#' The instance spec at the environment's EFFECTIVE capacities.
#'
#' Capacity lives twice in an environment and a governance cap writes both
#' copies, but the spec the region functions read carries a third. This is the
#' one place that reconciles them, so no caller has to remember to.
#'
#' @param env Environment list from node_env().
#' @return The spec with its capacity column taken from env$capacities.
node_effective_spec <- function(env) {
  spec <- env$spec
  spec$nodes$capacity <- env$capacities$capacity[
    match(spec$nodes$node, env$capacities$tier)]
  spec
}

#' Token capacities of the environment's effective region.
#'
#' Read off `capacities` rather than off the spec, so a governance coupling --
#' a constraint row that is not a service node and has no place in the spec --
#' is part of the region the rank functions are computed on.
#'
#' @param env Environment list from node_env().
#' @return Named numeric vector of token capacities, in the ancestor matrix's
#'   column order.
node_token_capacity <- function(env) {
  cap <- tier_capacities(env)
  setNames(cap$capacity, cap$tier)[colnames(env$anc)] /
    token_weight_of(env$spec, colnames(env$anc))
}

#' The population at which the first node saturates under this cell's mix.
#'
#' @param env Environment list from node_env().
#' @return Scalar K_c, in tasks per round.
node_k_c <- function(env) {
  x <- env$demand_weights %>% left_join(env$capacities, by = "tier")
  min(x$capacity / pmax(x$demand_weight, 1e-12))
}


# ===========================================================================
# Governance: coordinate-wise caps on the leaf tokens
# ===========================================================================
#
# The theory's object is one region, X_gov = {x >= 0 : x_l <= u_l(t)}, with
# trust, locality and role as the three DETERMINANTS of u rather than three
# regions. The enforcement point needs no kernel code: a leaf node's leaf block
# is the singleton {l}, so x_l <= u_l is already a coordinate constraint of the
# region the packer and the tatonnement both enforce against the capacity
# table. Per-agent-class caps are outside the preservation result as stated and
# are not built.

#' Apply coordinate-wise caps to an environment's leaves.
#'
#' Capacity lives twice in an environment, in `capacities`, which admission
#' prices and packs against, and in the copy joined into `per_tier`, which
#' execution queues against. Setting one and not the other makes a knob bind at
#' admission and leave execution queueing on the uncapped node, so a sweep over
#' it measures a fraction of the parameter it names. Both are set here, next to
#' each other, for that reason.
#'
#' @param env Environment list from node_env().
#' @param u   Named numeric vector of TOKEN caps per leaf, or NULL for none.
#' @return The environment, its leaf capacities lowered to the caps.
apply_leaf_caps <- function(env, u) {
  if (is.null(u) || length(u) == 0L) return(env)
  cap_units <- u * token_weight_of(env$spec, names(u))
  set <- function(df) dplyr::mutate(df, capacity = ifelse(
    tier %in% names(cap_units), pmin(capacity, cap_units[tier]), capacity))
  env$capacities <- set(env$capacities)
  env$per_tier   <- set(env$per_tier)
  env
}

#' Assign a provider agent to each leaf, deterministically under the seed.
#'
#' init_agents already gives four agents in ten the provider role; what is
#' missing is which service a provider is behind, without which a
#' provider-side reputation has nothing to attach to.
#'
#' The pool is a draw, not a quota, so at the smallest populations of the
#' sweep it holds fewer providers than the instance has leaves. Leaves are
#' assigned round-robin over the pool for that reason: a provider then stands
#' behind several services, its reputation attaches to all of them at once,
#' and no leaf is left without an owner. Where the pool is at least as large
#' as the leaf set the wrap never comes round and the map is one-to-one.
#'
#' @param agents Agent tibble from init_agents().
#' @param leaves Character vector of leaves.
#' @return Named integer vector, one agent id per leaf.
leaf_providers <- function(agents, leaves) {
  ids <- agents$agent_id[agents$role == "provider"]
  stopifnot("no provider agents to stand behind the leaves" = length(ids) > 0L)
  setNames(ids[(seq_along(leaves) - 1L) %% length(ids) + 1L], leaves)
}

#' Move a provider's reputation with its own leaf's deadline misses.
#'
#' The deployed update applies its asymmetric reward and penalty to the task's
#' OWNING agent, so a provider's reputation never moves under it and a
#' governance instrument reading it gates consumers instead. Same two
#' constants, keyed on the provider behind each leaf that had a miss.
#'
#' @param agents    Agent tibble.
#' @param results_t Execution results with task_id and success.
#' @param leaf_of   Named character vector mapping task_id to leaf.
#' @param providers Named integer vector mapping leaf to agent id.
#' @param reward    Increment for a leaf that met every deadline.
#' @param penalty   Decrement for a leaf with any miss.
#' @return The agents tibble, provider trust updated.
update_provider_trust <- function(agents, results_t, leaf_of, providers,
                                  reward = 0.03, penalty = 0.08) {
  if (nrow(results_t) == 0L) return(agents)
  leaf <- unname(leaf_of[as.character(results_t$task_id)])
  ok   <- tapply(as.logical(results_t$success), leaf, all)
  fail_ids <- unique(unname(providers[names(ok)[!ok]]))
  good_ids <- setdiff(unique(unname(providers[names(ok)[ok]])), fail_ids)

  agents %>%
    dplyr::mutate(trust = dplyr::case_when(
      agent_id %in% fail_ids ~ pmax(0, trust - penalty),
      agent_id %in% good_ids ~ pmin(1, trust + reward),
      TRUE                   ~ trust))
}

#' The token caps one policy level implies on this environment.
#'
#' @param env       Environment list from node_env(), UNCAPPED.
#' @param policy    One of "none", "trust", "locality", "role", "residency",
#'                  "residency_sliced". The two residency levels are not
#'                  coordinate caps and return NULL here; they are built by
#'                  node_residency_coupling and node_domain_slices.
#' @param agents    Agent tibble, for the trust determinant.
#' @param providers Named integer vector mapping leaf to agent id.
#' @param tau       Trust threshold below which a leaf is closed.
#' @param alpha_role Share of capacity the restricted role class keeps.
#' @param permitted Leaves inside the permitted jurisdiction.
#' @param restricted Leaves in the restricted role class.
#' @return Named numeric vector of token caps per leaf, or NULL.
node_policy_caps <- function(policy, env, agents = NULL, providers = NULL,
                             tau = 0.75, alpha_role = 0.5,
                             permitted = c("l1", "l2"),
                             restricted = c("l2", "l4")) {
  L    <- rownames(env$anc)
  Ctok <- node_token_capacity(env)[L]
  switch(policy,
    none     = NULL,
    residency = NULL,
    residency_sliced = NULL,
    # Read from the PREVIOUS round's reputation, so the cap is an exogenous
    # parameter of the current state exactly as the congestion signal is.
    trust    = setNames(ifelse(
      agents$trust[match(providers[L], agents$agent_id)] >= tau, Ctok, 0), L),
    locality = setNames(ifelse(L %in% permitted, Ctok, 0), L),
    # A partial cap, which is what makes the arm a monotone dose rather than a
    # switch, and which never repairs a crossing region.
    role     = setNames(ifelse(L %in% restricted, floor(alpha_role * Ctok), Ctok), L),
    stop("unknown policy: ", policy))
}

#' Couple two leaves across a domain boundary with one joint bound.
#'
#' The case the coordinate caps do not cover. A bound on x_a + x_b is not
#' coordinate-wise, and on a laminar instance it makes two leaf blocks cross:
#' the block it introduces contains one leaf of an existing block and one leaf
#' outside it, so neither contains the other and neither is disjoint from it.
#'
#' It is a CONSTRAINT and not a service. It joins the capacity table, the
#' demand weights and the recipes, so the packer and the tatonnement enforce it
#' with no change of their own, and it stays off the dependency graph, so it
#' adds nothing to any latency path and the arm's critical path is the
#' instance's.
#'
#' @param env    Environment list from node_env().
#' @param leaves The pair the bound straddles.
#' @param tokens The joint budget, in tokens.
#' @param name   Label for the constraint row.
#' @return The environment, carrying the coupling.
node_residency_coupling <- function(env, leaves = c("l2", "l3"), tokens = 50,
                                    name = "residency") {
  stopifnot("a coupling needs one token weight for the whole instance" =
              length(env$spec$weight) == 1L)
  w     <- env$spec$weight
  units <- tokens * w
  L     <- rownames(env$anc)

  env$capacities <- dplyr::bind_rows(env$capacities,
                                     tibble(tier = name, capacity = units))
  env$per_tier <- dplyr::bind_rows(
    env$per_tier,
    tibble(tier = name, nodes = 1L, capacity = units, base_ms = 0))
  env$demand_weights <- dplyr::bind_rows(
    env$demand_weights,
    tibble(tier = name, demand_weight = w * sum(env$leaf_shares[leaves])))
  env$graph$demand_weights <- env$demand_weights

  env$anc <- cbind(env$anc, as.numeric(L %in% leaves))
  colnames(env$anc)[ncol(env$anc)] <- name
  env$recipes <- setNames(lapply(L, function(l)
    c(env$recipes[[l]], setNames(w * (l %in% leaves), name))), L)
  env
}

#' The two domain slices the coupled budget is split into.
#'
#' Each slice is a coordinate truncation of a laminar region, so exactness is
#' restored on both; what the recovery costs is the demand one budget would
#' have carried and two cannot.
#'
#' @param env      Environment list from node_env(), UNCOUPLED.
#' @param a        The coupled budget's share for each slice, in tokens.
#' @param eu       Leaves inside the domain.
#' @param coupled  The pair the joint bound straddles.
#' @return A list of two environments, `eu` and `non_eu`.
node_domain_slices <- function(env, a = 25, eu = c("l1", "l2"),
                               coupled = c("l2", "l3")) {
  L    <- rownames(env$anc)
  V    <- colnames(env$anc)
  Ctok <- node_token_capacity(env)

  # Each slice's leaf budget: its own leaves at their capacity, the leaf it
  # owns of the coupled pair at the split share, everything else closed.
  budget <- Ctok[L]
  budget[intersect(coupled, L)] <- a

  slice <- function(own) {
    b <- budget
    b[setdiff(L, own)] <- 0
    # The internal nodes are shared, so each slice takes the share of every
    # node that its own leaves account for. Without it two markets would each
    # pack against the whole of d and admit twice what the device can carry,
    # which is the over-commitment another arm exists to measure.
    frac <- vapply(V, function(v) {
      tot <- sum(budget[env$anc[, v] > 0])
      if (tot <= 0) 0 else sum(b[env$anc[, v] > 0]) / tot
    }, numeric(1))
    u <- floor(Ctok[V] * frac)
    u[L] <- b
    apply_leaf_caps(env, u)
  }
  list(eu = slice(eu), non_eu = slice(setdiff(L, eu)))
}

#' One price vector out of two slices' clearings.
#'
#' Each leaf is priced by the market that carries it; the internal nodes are
#' shared, so what the two slices pay for them is averaged. The result is the
#' price vector the induced leaf prices, and every statistic on them, is read
#' off.
#'
#' A leaf closed in a slice is priced by that slice at whatever its excess
#' demand drives the price to against a capacity of zero, which is a number
#' about the closure and not about the leaf. Averaging the two slices would
#' carry that into the leaf's price, so a leaf takes the price of the slice
#' that CARRIES it and the average is used only where both slices really
#' compete, on the internal nodes they share.
#'
#' @param p_a    Price tibble of the first slice.
#' @param p_b    Price tibble of the second.
#' @param own_a  Leaves the first slice carries.
#' @param leaves Every leaf of the instance.
#' @return A price tibble in p_a's row order.
node_combine_slice_prices <- function(p_a, p_b, own_a, leaves) {
  b       <- p_b$price[match(p_a$tier, p_b$tier)]
  is_leaf <- p_a$tier %in% leaves
  mine    <- p_a$tier %in% own_a
  p_a$price <- ifelse(is_leaf,
                      ifelse(mine, p_a$price, b),
                      (p_a$price + b) / 2)
  p_a
}

#' The demand one joint budget would have carried that two slices cannot.
#'
#' @param o_a    Tokens offered at the first coupled leaf.
#' @param o_b    Tokens offered at the second.
#' @param joint  The joint budget, in tokens.
#' @param a      Each slice's share of it.
#' @return Stranded tokens, never negative.
node_stranded_demand <- function(o_a, o_b, joint, a) {
  min(o_a + o_b, joint) - (min(o_a, a) + min(o_b, joint - a))
}


# ===========================================================================
# Prices, induced on the leaves
# ===========================================================================

#' Induced leaf prices: the sum of a leaf's ancestors' node prices.
#'
#' Every price statistic is defined on these and not on the node prices. The
#' map is p_leaf = A' p_node with A the ancestor-indicator matrix, whose rank
#' is the leaf count, so the node prices have a kernel: they are not identified
#' by demand and a dispersion statistic computed on them would report a flat
#' direction as volatility.
#'
#' @param prices_df Per-node price tibble (tier, price).
#' @param anc       An ancestor-indicator matrix.
#' @return Named numeric vector of leaf prices.
induced_leaf_prices <- function(prices_df, anc) {
  p <- prices_df$price[match(colnames(anc), prices_df$tier)]
  setNames(as.numeric(anc %*% p), rownames(anc))
}

#' Within-round dispersion of the induced leaf prices.
#'
#' Identically zero on sp, where every leaf has the identical ancestor set, and
#' inside a contracted block, where one multiplier covers several leaves. Both
#' zeros are structural and are recorded in advance rather than discovered:
#' this is a cross-sectional statistic and NOT an admissible response for the
#' encapsulation factor, which would read a dimension reduction as stability.
#'
#' @param prices_df Per-node price tibble (tier, price).
#' @param anc       An ancestor-indicator matrix.
#' @return Coefficient of variation across leaves, NA where there is no price.
cross_leaf_price_cv <- function(prices_df, anc) {
  p <- induced_leaf_prices(prices_df, anc)
  m <- mean(p)
  if (!is.finite(m) || m <= 0) return(NA_real_)
  as.numeric(stats::sd(p) / m)
}


# ===========================================================================
# One round: demand, and what the agents bid against
# ===========================================================================

#' One round's tasks, with the leaf each is destined for.
#'
#' The reseed is what matches the demand stream across supply arms: at one
#' population and one seed, two instances see the identical task counts,
#' values, deadlines and labels in every round, so the arms differ in supply
#' and in nothing else. Matched by construction rather than in expectation.
#'
#' @param env       Environment list from node_env().
#' @param agents    Agent tibble.
#' @param t         Round index.
#' @param seed      The run's seed.
#' @param deadlines Integer vector of possible deadlines (ms).
#' @return A tibble of tasks carrying a `recipe` column naming a leaf.
node_round_tasks <- function(env, agents, t, seed, deadlines) {
  set.seed(seed * 1009L + t)
  tasks <- bind_tasks(lapply(split(agents, agents$agent_id), function(a)
    generate_tasks(a, env, round = t, deadlines = deadlines)))
  if (nrow(tasks) == 0L) return(mutate(tasks, recipe = character()))
  tasks$recipe <- sample(names(env$leaf_shares), nrow(tasks),
                         replace = TRUE, prob = env$leaf_shares)
  tasks
}

#' The two per-task quantities a bid is formed from.
#'
#' Both are keyed on the task's own leaf and both are read from the PREVIOUS
#' round, so within a round they are exogenous parameters of the current state
#' and fixed throughout the allocation. That is the hypothesis the
#' separable-concave reduction rests on, and it is what makes the exact
#' reference computable at the evaluation populations rather than by an
#' enumeration capped at fourteen tasks.
#'
#' @param env       Environment list from node_env().
#' @param tasks     The round's tasks, carrying a `recipe` column.
#' @param prev_util Previous round's per-node utilisation, or NULL.
#' @return A list of `util_hat` and `base_latency`, one entry per task.
node_bid_inputs <- function(env, tasks, prev_util) {
  lab <- as.character(tasks$recipe)
  list(util_hat     = unname(leaf_util_hat(prev_util, env$anc)[lab]),
       base_latency = unname(base_latency_per_leaf(env)[lab]))
}

#' The execution model's own queue delay on each leaf's path.
#'
#' The bid-time congestion signal the baseline market reads is a capped power
#' law of a path-MEAN utilisation, which saturates far below what the queue at
#' capacity costs: a price discovered against it cannot carry the congestion
#' the admitted set creates. This is the execution model's queue term instead,
#' at the same constants, summed along the leaf's own path at the previous
#' round's per-node utilisation, and uncapped -- inheriting the execution
#' model's utilisation clamp and its per-node ceiling would restore exactly the
#' blindness the arm exists to remove.
#'
#' The signal is the PREVIOUS round's, so the arm is a cobweb: at a full round
#' the estimate runs into the thousands of milliseconds and prices the next
#' round out entirely, which empties the nodes and prices the one after that
#' at the zero-queue path. The measured arm two-cycles rather than settling.
#' That is the arm the design asks for -- damping it would be a second change
#' on top of the one being measured -- and it is why the arm is reported
#' beside the market and never instead of it.
#'
#' @param env       Environment list from node_env().
#' @param prev_util Previous round's per-node utilisation, or NULL.
#' @return Named numeric vector, one latency estimate per leaf.
node_queue_latency_per_leaf <- function(env, prev_util) {
  base <- base_latency_per_leaf(env)
  if (is.null(prev_util)) return(base)
  anc <- env$anc
  u   <- prev_util$util[match(colnames(anc), prev_util$tier)]
  u[is.na(u)] <- 0
  q   <- env$load_factor * (pmax(u, 0) / (1 - pmax(u, 0) + 1e-3)) * 2
  base + as.numeric(anc %*% q)
}

#' The round's per-leaf marginal values, descending.
#'
#' @param tasks  The round's tasks, carrying a `recipe` column.
#' @param ev     Per-task expected value.
#' @param leaves Character vector of leaves.
#' @return Named list, one descending numeric vector per leaf.
node_leaf_marginals <- function(tasks, ev, leaves) {
  lab <- as.character(tasks$recipe)
  setNames(lapply(leaves, function(l) sort(ev[lab == l], decreasing = TRUE)),
           leaves)
}

#' The round's true value per task: its value at zero queue.
#'
#' What a task is worth when nothing is queued in front of it, which is the
#' quantity both reference optima are computed on. It carries no utilisation
#' estimate and no success model, so it does not move with the arm's own
#' congestion state the way a bid-time expected value does.
#'
#' @param tasks            The round's tasks, carrying a `recipe` column.
#' @param base_ms          Per-leaf zero-queue critical path.
#' @param lambda_l_default Per-ms value-decay rate.
#' @param salvage          Value retained after a deadline miss.
#' @return Numeric vector, one entry per task.
node_true_value <- function(tasks, base_ms, lambda_l_default, salvage = 0) {
  if (nrow(tasks) == 0L) return(numeric(0))
  L   <- unname(base_ms[as.character(tasks$recipe)])
  lam <- if ("lambda_l" %in% names(tasks)) as.numeric(tasks$lambda_l)
         else lambda_l_default
  v   <- as.numeric(tasks$value_base) * exp(-lam * L)
  ifelse(!is.na(tasks$deadline) & L > as.numeric(tasks$deadline), salvage * v, v)
}

#' The welfare no admission policy can reach on the round.
#'
#' The exact leaf-block optimum on the true zero-queue values. It counts the
#' most valuable tokens the node capacities can carry and charges none of them
#' for the queue that carrying them creates, so it is an upper bound on every
#' arm and on the attainable optimum alike.
#'
#' @param env    Environment whose capacities define the region.
#' @param tasks  The round's tasks.
#' @param v_true Per-task true zero-queue value.
#' @return Scalar.
node_zero_queue_ceiling <- function(env, tasks, v_true) {
  if (nrow(tasks) == 0L) return(0)
  marg <- node_leaf_marginals(tasks, v_true, rownames(env$anc))
  lb_optimum(marg, env$anc, node_token_capacity(env))
}

#' The best realised welfare a planner could take out of the round.
#'
#' Over admission sets of the form "the top m tasks by true zero-queue value",
#' each scored through the execution model itself, so the queue the set creates
#' is charged to it. Unlike the ceiling this one is attainable, by a planner
#' who knows the round before it runs.
#'
#' # ponytail: a coarse scan of at most `n_grid` sizes plus one refinement over
#' # the gap it stepped across. The realised-welfare curve in m is unimodal in
#' # every measured cell (it rises with volume and falls with the queue), so
#' # the refinement finds the peak; a multi-modal curve would want the full
#' # scan, at four times the cost.
#'
#' @param tasks      The round's tasks.
#' @param v_true     Per-task true zero-queue value.
#' @param welfare_of Scores one admission set through the execution model.
#' @param n_grid     Sizes in the coarse scan.
#' @return Named vector of `value` and its argmax `m`.
node_ex_post_optimum <- function(tasks, v_true, welfare_of, n_grid = 25L) {
  n <- nrow(tasks)
  if (n == 0L) return(c(value = 0, m = 0))
  ord <- order(v_true, decreasing = TRUE)
  at  <- function(m) welfare_of(tasks[ord[seq_len(m)], , drop = FALSE])

  grid <- unique(round(seq(0, n, length.out = min(n + 1L, n_grid))))
  w    <- vapply(grid, at, numeric(1))
  step <- max(1L, as.integer(ceiling(n / max(1L, length(grid) - 1L))))
  peak <- grid[[which.max(w)]]
  near <- setdiff(seq(max(0, peak - step), min(n, peak + step)), grid)
  if (length(near) > 0L) {
    grid <- c(grid, near)
    w    <- c(w, vapply(near, at, numeric(1)))
  }
  i <- which.max(w)
  c(value = w[[i]], m = grid[[i]])
}

#' What value-greedy takes on a region and what the exact reference takes.
#'
#' Computed OFF the market, on the round's full instance, through the packers:
#' admission passes through the tatonnement's positive-surplus filter, which
#' rations on price and would bury a packing gap of a per cent inside a
#' rationing gap of forty.
#'
#' @param env   Environment whose capacities define the region.
#' @param tasks The round's tasks.
#' @param ev    Per-task expected value.
#' @return Named numeric vector of `greedy` and `exact`.
node_exact_pair <- function(env, tasks, ev) {
  if (nrow(tasks) == 0L) return(c(greedy = 0, exact = 0))
  anc  <- env$anc
  marg <- node_leaf_marginals(tasks, ev, rownames(anc))
  c(greedy = sum(ev[.greedy_pack_by(ev, tasks, env)]),
    exact  = lb_optimum(marg, anc, node_token_capacity(env)))
}


# ===========================================================================
# The driver
# ===========================================================================

#' Run a single node-level configuration (one seed).
#'
#' @param graph_type       Arm: "tree", "sp" or "entangled".
#' @param load_level       "low", "medium" or "high".
#' @param N                Agent population.
#' @param seed             Random seed.
#' @param n_rounds         Number of rounds.
#' @param deadlines        Integer vector of possible deadlines (ms).
#' @param lambda_l_default Per-ms value-decay rate.
#' @param leaf_mix         "uniform" or "skewed".
#' @param reserve_markup   Multiplier on the per-node reserve the market arms
#'                         clamp their prices to: the market's tuned knob, the
#'                         counterpart of the posted price's markup.
#' @param exact_reference  Whether to compute the greedy-versus-exact
#'                         instrument. Off on the cells whose primary
#'                         instrument is something else.
#' @param alpha            Congestion sensitivity.
#' @param p                Congestion exponent.
#' @param salvage          Value retained after a deadline miss.
#' @param iters            Tatonnement iterations per round.
#' @param eta              Price step size.
#' @param success_lr       Learning rate for the success model.
#' @return A single-row tibble of summary metrics.
node_run_single <- function(graph_type = c("tree", "sp", "entangled",
                                           "agentic"),
                            load_level = c("medium", "high", "low"),
                            N = 90L, seed = 1L, n_rounds = 200L,
                            deadlines = c(500L, 750L, 1000L),
                            lambda_l_default = node_lambda_l(),
                            leaf_mix = c("uniform", "skewed"),
                            architecture = c("naive", "naive_ema",
                                             "hybrid_noema", "hybrid_ema"),
                            interface = NULL,
                            policy = c("none", "trust", "locality", "role",
                                       "residency", "residency_sliced"),
                            cap_target = NA_character_,
                            mechanism = c("market", "random", "edf",
                                          "greedy_ev", "posted_price", "k8s",
                                          "market_cc", "posted_price_matched"),
                            p_post_k = 1,
                            advertise_frac = NULL, cap_scale = 1.0,
                            spec = NULL,
                            exact_reference = TRUE,
                            reserve_markup = 1,
                            alpha = 50, p = 1.2, salvage = 0.0,
                            iters = 15L, eta = price_eta, success_lr = 0.3) {
  graph_type <- match.arg(graph_type)
  load_level <- match.arg(load_level)
  leaf_mix   <- match.arg(leaf_mix)
  policy     <- match.arg(policy)
  architecture <- match.arg(architecture)
  mechanism  <- match.arg(mechanism)

  arch      <- node_architecture(architecture)
  interface <- match.arg(interface %||% arch$interface,
                         c("off", "inner", "maxflow"))
  beta      <- arch$beta

  set.seed(seed)
  # Two environments, and the difference between them IS the interface: the
  # market clears against the ADVERTISED region and delivery is evaluated
  # against the TRUE instance. At interface "off" they are the same region.
  spec_used <- spec %||% node_instance(graph_type)
  env_true <- scale_capacities(
    node_run_env(graph_type, load_level, N, leaf_mix, "off",
                 spec = spec_used), cap_scale)
  env_adv  <- scale_capacities(
    node_run_env(graph_type, load_level, N, leaf_mix, interface,
                 advertise_frac, spec = spec_used), cap_scale)
  agents   <- init_agents(N)
  providers <- leaf_providers(agents, rownames(env_true$anc))

  # The exactness-repair instrument: one leaf closed outright, which is what a
  # coordinate cap can do to a crossing region. A governance cap is a real
  # restriction, so it binds on both sides of the interface.
  caps_static <- if (!is.na(cap_target)) setNames(0, cap_target) else NULL
  env_true <- apply_leaf_caps(env_true, caps_static)
  env_adv  <- apply_leaf_caps(env_adv,  caps_static)
  # The coupling is an admission constraint and not a service, so it goes on
  # the advertised side alone. It is applied before any rank, certificate or
  # onset is read, so all three are computed on the EFFECTIVE region.
  if (policy == "residency") env_adv <- node_residency_coupling(env_adv)
  sliced <- policy == "residency_sliced"
  slices <- if (sliced) node_domain_slices(env_adv) else NULL
  eu_leaves <- c("l1", "l2")
  coupled_pair <- c("l2", "l3")

  anc    <- env_adv$anc
  leaves <- rownames(anc)
  # The zero-queue path of the TRUE instance: what a task is worth before
  # anything queues in front of it, and the quantity both references are
  # computed on. A governance cap moves capacities, never base latencies, so
  # this is a constant of the run.
  base_true <- base_latency_per_leaf(env_true)
  full   <- subset_name(leaves, leaves)
  fr     <- flow_rank(node_effective_spec(env_true), env_true$anc)
  flow_full <- fr$value[[subset_name(leaves, leaves)]]
  cap_true <- setNames(tier_capacities(env_true)$capacity,
                       tier_capacities(env_true)$tier)
  w      <- token_weight_of(env_true$spec, names(cap_true))

  # The certificate and the region constants are cached per BRANCH wherever the
  # effective region is constant across rounds, which is every policy level but
  # the trust one: there a provider's reputation moves, the caps move with it,
  # and a cached verdict would describe a region the round is not clearing over.
  dynamic <- policy == "trust"
  policy_env <- function(e, ag) {
    if (sliced) return(e)
    apply_leaf_caps(e, node_policy_caps(policy, e, ag, providers))
  }
  env  <- policy_env(env_adv,  agents)
  envT <- policy_env(env_true, agents)
  Ctok <- node_token_capacity(env)
  lb_full <- leaf_rank(anc, Ctok)[[full]]

  ms        <- init_market_state(if (sliced) slices$eu else env)
  ms_ne     <- init_market_state(if (sliced) slices$non_eu else env)
  prev_util <- NULL

  medL <- p95L <- utilV <- dropV <- numeric(n_rounds)
  welfareV <- oracleV <- effV <- numeric(n_rounds)
  unitCostV <- clearV <- servedV <- numeric(n_rounds)
  priceCvV  <- tokensV <- bindV <- numeric(n_rounds)
  ratioV    <- shapeV <- strandV <- rep(NA_real_, n_rounds)
  ceilV     <- optV <- optMV <- numeric(n_rounds)
  trueV     <- residV <- clearedCostV <- rep(NA_real_, n_rounds)
  overV     <- numeric(n_rounds)
  armV      <- rep(NA_real_, n_rounds)
  certV     <- rep(NA, n_rounds)

  for (t in seq_len(n_rounds)) {
    if (dynamic) {
      env  <- policy_env(env_adv,  agents)
      envT <- policy_env(env_true, agents)
      Ctok <- node_token_capacity(env)
      lb_full <- leaf_rank(anc, Ctok)[[full]]
    }
    tasks_all <- node_round_tasks(env, agents, t, seed, deadlines)
    n_gen     <- nrow(tasks_all)
    bid       <- node_bid_inputs(env, tasks_all, prev_util)
    if (dynamic || t == 1L) {
      certV[t] <- if (sliced) {
        all(vapply(slices, dsic_certificate, logical(1), tasks_all))
      } else {
        dsic_certificate(env, tasks_all)
      }
    }
    # The success model's stochastic gradient takes one number for the round;
    # the round's own signal is a vector over its tasks, so its mean is what
    # the update sees. The bid-time values keep the per-task vector.
    util_scalar <- if (n_gen == 0L) 0 else mean(bid$util_hat)

    clear_one <- function(tasks, e, state) {
      b <- node_bid_inputs(e, tasks, prev_util)
      # The congestion-consistent level bids against the execution model's own
      # queue on the task's path. Its alpha is zero because the congestion is
      # already inside that latency; the utilisation signal is untouched, so
      # the success model sees what it always saw.
      a <- alpha
      if (mechanism == "market_cc") {
        b$base_latency <- unname(node_queue_latency_per_leaf(e, prev_util)[
          as.character(tasks$recipe)])
        a <- 0
      }
      clear_multitier_market(
        tasks, e, b$util_hat, b$base_latency, state,
        alpha = a, p = p, lambda_l_default = lambda_l_default,
        salvage = salvage, iters = iters, eta = eta, beta = beta,
        reserve_markup = reserve_markup)
    }

    if (!mechanism %in% c("market", "market_cc")) {
      # The five arms that do not discover a price. Each produces a score and
      # the shared packing kernel admits by it under the node capacities, so
      # what separates them is the ranking key and, for the posted price, the
      # participation screen.
      scores <- switch(
        mechanism,
        random    = runif(n_gen, min = 0.01, max = 1.0),
        edf       = if (n_gen == 0L) numeric(0) else
                      (max(tasks_all$deadline, na.rm = TRUE) + 1) -
                        tasks_all$deadline,
        k8s       = k8s_rank_score(tasks_all, env),
        task_expected_value(
          tasks_all, bid$util_hat, bid$base_latency, ms$success_model,
          alpha = alpha, p = p,
          lambda_l_default = lambda_l_default, salvage = salvage))

      if (mechanism %in% c("posted_price", "posted_price_matched")) {
        p_task <- if (mechanism == "posted_price_matched") {
          # One dose for every instance, so the cross-instance ordering is
          # read at equal dose rather than at each instance's own path cost.
          rep(node_matched_anchor(p_post_k), n_gen)
        } else {
          unname(posted_price_anchor_per_leaf(env, anc,
                                              k = p_post_k)[
                                                as.character(tasks_all$recipe)])
        }
        allocation   <- posted_price_allocate(tasks_all, env, scores, p_task)
        # The price is what agents face whether or not they take it, so it is
        # recorded every round, as the market arm records what it cleared at.
        unitCostV[t] <- if (n_gen == 0L) NA_real_ else mean(p_task)
        # A posted price passes through no filter, so what the agent faces and
        # what the operator posted are one series.
        clearedCostV[t] <- unitCostV[t]
      } else {
        allocation   <- pack_tasks_greedy(tasks_all, scores, env)
        unitCostV[t] <- NA_real_
      }
      # A rank scheduler has no price process, so there is none to disperse.
      prices <- dplyr::transmute(tier_capacities(env), tier = tier, price = 0)

    } else if (sliced) {
      # Two independent markets over disjoint leaf sets, each a coordinate
      # truncation of a laminar region. The shared internal nodes are split
      # with the leaves, so the two together never admit more than the device
      # can carry.
      in_eu  <- as.character(tasks_all$recipe) %in% eu_leaves
      c_eu   <- clear_one(tasks_all[in_eu, ],  slices$eu,     ms)
      c_ne   <- clear_one(tasks_all[!in_eu, ], slices$non_eu, ms_ne)
      allocation <- dplyr::bind_rows(c_eu$allocation, c_ne$allocation)
      ms     <- append_price_history(c_eu$market_state, c_eu$market_state$prices)
      ms_ne  <- append_price_history(c_ne$market_state, c_ne$market_state$prices)
      prices <- node_combine_slice_prices(c_eu$clearing$prices,
                                          c_ne$clearing$prices, eu_leaves,
                                          leaves)
      unitCostV[t] <- mean(c(c_eu$clearing$unit_cost, c_ne$clearing$unit_cost))
      clearedCostV[t] <- mean(c(c_eu$clearing$unit_cost_cleared,
                                c_ne$clearing$unit_cost_cleared))
      residV[t]    <- mean(c(c_eu$clearing$resid_excess,
                             c_ne$clearing$resid_excess))
      offered <- table(factor(as.character(tasks_all$recipe), levels = leaves))
      strandV[t] <- node_stranded_demand(offered[[coupled_pair[1]]],
                                         offered[[coupled_pair[2]]], 50, 25)
    } else {
      cleared <- clear_one(tasks_all, env, ms)
      allocation   <- cleared$allocation
      ms           <- append_price_history(cleared$market_state,
                                           cleared$market_state$prices)
      prices       <- cleared$clearing$prices
      unitCostV[t] <- cleared$clearing$unit_cost
      clearedCostV[t] <- cleared$clearing$unit_cost_cleared
      residV[t]    <- cleared$clearing$resid_excess
    }
    priceCvV[t] <- cross_leaf_price_cv(prices, anc)

    # Executed against the TRUE instance: an advertised scalar the internal
    # routing cannot honour admits a mix the physical nodes still have to
    # queue, which is where an over-committing interface pays for itself.
    results_t <- execute_allocation(allocation, envT)
    if (nrow(results_t) > 0 &&
        !all(c("deadline", "value_base") %in% names(results_t))) {
      results_t <- results_t %>%
        left_join(allocation %>% select(task_id, deadline, value_base),
                  by = "task_id")
    }
    agents <- update_trust(agents, results_t)
    if (dynamic && nrow(allocation) > 0) {
      agents <- update_provider_trust(
        agents, results_t,
        setNames(as.character(allocation$recipe), allocation$task_id),
        providers)
    }

    if (nrow(results_t) == 0) {
      medL[t] <- NA; p95L[t] <- NA
    } else {
      medL[t] <- median(results_t$latency, na.rm = TRUE)
      p95L[t] <- quantile(results_t$latency, 0.95, na.rm = TRUE, names = FALSE)
    }

    # The bid-time signal is read off the ADVERTISED region, which is what the
    # agents can observe; the realised load is the true instance's and is what
    # the over-commitment is measured on.
    prev_util <- compute_utilisation_per_node(env, allocation)
    true_util <- compute_utilisation_per_node(envT, allocation)
    utilV[t]  <- mean(true_util$util, na.rm = TRUE)
    bindV[t]  <- as.numeric(max(true_util$util, na.rm = TRUE) >= 0.99)
    overV[t]  <- if (nrow(allocation) == 0L) 0 else {
      used <- colSums(task_recipes(allocation, envT))
      max(0, max((used[names(cap_true)] - cap_true) / w, na.rm = TRUE))
    }

    succ <- if (nrow(results_t) == 0) 0 else sum(results_t$success, na.rm = TRUE)
    if (n_gen == 0) {
      dropV[t]  <- 0
      clearV[t] <- NA_real_
    } else {
      dropV[t]  <- 1 - succ / n_gen
      clearV[t] <- nrow(allocation) / n_gen
    }
    servedV[t] <- if (nrow(allocation) == 0) NA_real_ else succ / nrow(allocation)
    tokensV[t] <- nrow(allocation)

    welfareV[t] <- compute_welfare(
      results_t, envT, ms$prices,
      lambda_l_default = lambda_l_default, salvage = salvage,
      cong_cost = TRUE, cong_gamma = 0.05)
    orc <- oracle_pack_realised(
      tasks_all, env, bid$util_hat, bid$base_latency, ms$success_model,
      alpha = alpha, p = p,
      lambda_l_default = lambda_l_default, salvage = salvage)
    oracleV[t] <- orc$oracle_value
    effV[t]    <- ifelse(oracleV[t] > 0, welfareV[t] / oracleV[t], NA_real_)

    # The two references, computed OFF the market on the true instance: the
    # ceiling counts the best tokens the node capacities carry and charges
    # none of them for the queue, the ex-post optimum charges every one of
    # them through the execution model. Neither draws, so the arm's own
    # stream does not move by their presence.
    v_true   <- node_true_value(tasks_all, base_true, lambda_l_default, salvage)
    ceilV[t] <- node_zero_queue_ceiling(envT, tasks_all, v_true)
    opt      <- node_ex_post_optimum(tasks_all, v_true, function(a)
      compute_welfare(execute_allocation(a, envT, latency_noise_cv = 0), envT,
                      ms$prices, lambda_l_default = lambda_l_default,
                      salvage = salvage, cong_cost = TRUE, cong_gamma = 0.05))
    optV[t]  <- opt[["value"]]
    optMV[t] <- opt[["m"]]
    # What this arm admitted, valued on the same zero-queue values as the
    # ceiling above it. Numerator and denominator are both off the market, so
    # neither end of the ratio moves with the arm's own congestion state.
    if (ceilV[t] > 0) {
      trueV[t] <- sum(v_true[match(allocation$task_id, tasks_all$task_id)]) /
        ceilV[t]
    }

    # The structural instrument, computed OFF the market on the round's full
    # instance: admission passes through the tatonnement's positive-surplus
    # filter, which rations on price and would bury a packing gap of a per cent
    # inside a rationing gap of forty.
    if (exact_reference && n_gen > 0) {
      ev <- task_expected_value(
        tasks_all, bid$util_hat, bid$base_latency, ms$success_model,
        alpha = alpha, p = p,
        lambda_l_default = lambda_l_default, salvage = salvage)
      pair <- if (sliced) {
        in_eu <- as.character(tasks_all$recipe) %in% eu_leaves
        node_exact_pair(slices$eu,     tasks_all[in_eu, ],  ev[in_eu]) +
          node_exact_pair(slices$non_eu, tasks_all[!in_eu, ], ev[!in_eu])
      } else {
        node_exact_pair(env, tasks_all, ev)
      }
      if (is.finite(pair[["exact"]]) && pair[["exact"]] > 0) {
        ratioV[t] <- pair[["greedy"]] / pair[["exact"]]
        # What THIS arm delivered against the same reference. The structural
        # ratio above compares the packing rule; this one compares the
        # mechanism, which is what the ablation is scored on.
        armV[t] <- sum(ev[match(allocation$task_id, tasks_all$task_id)]) /
          pair[["exact"]]
      }

      # How much of what the node capacities could route the leaf-block region
      # refuses on this round's MIX rather than on its total.
      offered   <- table(factor(as.character(tasks_all$recipe), levels = leaves))
      unit_marg <- setNames(lapply(leaves, function(l)
        rep(1, offered[[l]])), leaves)
      lb_tokens <- lb_optimum(unit_marg, anc, Ctok)
      routable  <- min(flow_full, n_gen)
      if (routable > 0) shapeV[t] <- max(0, 1 - lb_tokens / routable)
    }

    ms <- market_update_from_results(ms, util_scalar, results_t, lr = success_lr)
  }

  below <- ratioV < 1 - 1e-9
  # The steady state the references are read on, and the same window for the
  # welfare they are divided into: a ratio whose numerator and denominator
  # covered different rounds would be a comparison of two windows.
  w_tail    <- mean(post_burn_in(welfareV), na.rm = TRUE)
  ceil_tail <- mean(post_burn_in(ceilV), na.rm = TRUE)
  opt_tail  <- mean(post_burn_in(optV), na.rm = TRUE)
  over <- function(x) if (isTRUE(x > 0)) w_tail / x else NA_real_
  tibble(
    graph_type                 = graph_type,
    load_level                 = load_level,
    N                          = as.integer(N),
    seed                       = seed,
    leaf_mix                   = leaf_mix,
    architecture               = architecture,
    interface                  = interface,
    mechanism                  = mechanism,
    p_post_k                   = as.numeric(p_post_k),
    reserve_markup             = as.numeric(reserve_markup),
    # The dose a task faces at marginal cost on the region this cell cleared
    # over: the cheapest leaf's own path at the per-node reserve, which is the
    # unmatched control every cross-instance posted-price contrast carries.
    posted_anchor_k1           = min(posted_price_anchor_per_leaf(env, anc,
                                                                  k = 1)),
    policy                     = policy,
    cap_target                 = cap_target,
    median_latency             = mean(medL, na.rm = TRUE),
    p95_latency                = mean(p95L, na.rm = TRUE),
    utilisation                = mean(utilV, na.rm = TRUE),
    drop_rate                  = mean(dropV, na.rm = TRUE),
    clearing_fraction          = mean(clearV, na.rm = TRUE),
    served_among_admitted      = mean(servedV, na.rm = TRUE),
    welfare                    = mean(welfareV, na.rm = TRUE),
    oracle_welfare             = mean(oracleV, na.rm = TRUE),
    efficiency                 = mean(effV, na.rm = TRUE),
    # The welfare no admission policy can reach, the best one a planner who
    # knew the round could, the size of the set that reaches it, and where
    # this arm sits between them. All four on the post-burn-in rounds.
    ceiling_zero_queue         = ceil_tail,
    optimum_ex_post            = opt_tail,
    optimum_ex_post_m          = mean(post_burn_in(optMV), na.rm = TRUE),
    welfare_over_optimum       = over(opt_tail),
    welfare_over_ceiling       = over(ceil_tail),
    mean_unit_cost             = mean(unitCostV, na.rm = TRUE),
    mean_price_volatility      = agent_price_volatility(unitCostV),
    mean_price_volatility_tail = agent_price_volatility_tail(unitCostV),
    # The same dispersion on the price the round cleared at, before the
    # smoothing filter. Where no filter is applied the two coincide, which is
    # what makes their ratio on a smoothed arm readable as the filter's own.
    price_volatility_cleared   = agent_price_volatility_tail(clearedCostV),
    # What the tatonnement still could not clear at its terminal prices,
    # averaged over the post-burn-in rounds. NA where no price is discovered.
    resid_excess               = if (all(is.na(residV))) NA_real_
                                 else mean(post_burn_in(residV), na.rm = TRUE),
    # The reportable structural statistic is the round INCIDENCE and the tail
    # of the ratio, not its mean alone: a gap that opens in a fifth of the
    # rounds and closes in the rest averages to a number that looks like noise.
    greedy_exact_ratio         = mean(ratioV, na.rm = TRUE),
    greedy_exact_incidence     = if (all(is.na(ratioV))) NA_real_
                                 else mean(below, na.rm = TRUE),
    greedy_exact_worst         = if (all(is.na(ratioV))) NA_real_
                                 else min(ratioV, na.rm = TRUE),
    # The arm's admitted set on true zero-queue values, against the exact
    # leaf-block optimum of those same values: one reference for every arm.
    alloc_ratio_true           = mean(trueV, na.rm = TRUE),
    # Retired, kept for one release: the arm's set on its own BID-TIME values
    # against the optimum of those same values, so both ends move with the
    # arm's congestion estimate and no arm can rank above value-greedy on a
    # laminar instance.
    arm_exact_ratio            = mean(armV, na.rm = TRUE),
    flow_bound_ratio           = lb_full / flow_full,
    flow_bound_token_gap       = flow_full - lb_full,
    shape_refusal_fraction     = mean(shapeV, na.rm = TRUE),
    price_cv_cross             = mean(priceCvV, na.rm = TRUE),
    tokens_admitted            = mean(tokensV, na.rm = TRUE),
    binding_fraction           = mean(bindV, na.rm = TRUE),
    # Tokens by which the admitted mix exceeds a true node's capacity, at the
    # worst node of the round. Zero wherever the advertised region is inside
    # the true one, positive wherever an aggregate reading was optimistic.
    overcommitment             = mean(overV, na.rm = TRUE),
    # The verdict on the region this cell actually cleared over, computed
    # rather than inherited from the arm's name.
    certificate_ok             = all(certV[!is.na(certV)]),
    # What the domain slice cost: the demand one coupled budget would have
    # carried that two half budgets cannot. NA where nothing was sliced.
    stranded_demand            = if (all(is.na(strandV))) NA_real_
                                 else mean(strandV, na.rm = TRUE)
  )
}


# ===========================================================================
# The matched-control diagnostic table
# ===========================================================================

#' One row per arm and mix, every entry computed rather than typed.
#'
#' The match is a reported number and not an assumption, and the two controls
#' that cannot hold are reported as such rather than engineered away: K_c is
#' half on sp, which is the conservatism of the leaf-block region and cannot
#' also be a matched input, and a leaf of sp has five ancestors so its path
#' reserve cost is higher than on the other two.
#'
#' @param mixes Leaf mixes to report.
#' @return A tibble, one row per (graph_type, leaf_mix).
node_instance_diagnostics <- function(mixes = c("uniform", "skewed")) {
  N <- node_agents()
  grid <- tidyr::expand_grid(graph_type = names(N), leaf_mix = mixes)

  purrr::pmap_dfr(grid, function(graph_type, leaf_mix) {
    n_arm <- N[[graph_type]]
    env  <- node_env(graph_type, "high", n_arm, leaf_mix)
    spec <- node_effective_spec(env)
    anc  <- env$anc
    L    <- rownames(anc)
    full <- subset_name(L, L)
    Ctok <- node_token_capacity(env)

    fr   <- flow_rank(spec, anc)
    cert <- polymatroid_certificate(leaf_rank(anc, Ctok), L)
    dw   <- setNames(env$demand_weights$demand_weight, env$demand_weights$tier)
    cap  <- setNames(env$capacities$capacity, env$capacities$tier)
    kc   <- cap[names(dw)] / pmax(dw, 1e-12)
    phys <- setNames(spec$nodes$phys, spec$nodes$node)

    tibble(
      graph_type           = graph_type,
      leaf_mix             = leaf_mix,
      N                    = as.numeric(n_arm),
      n_nodes              = nrow(spec$nodes),
      n_leaves             = length(L),
      n_arcs               = nrow(spec$edges),
      basket_dim           = length(L),
      critical_path_ms     = critical_path_ms(build_leaf_graph(spec),
                                              leaf_base_ms(spec)),
      max_flow             = fr$value[[full]],
      max_flow_cut         = paste(fr$cut[[full]], collapse = ", "),
      leaf_block_capacity  = leaf_rank(anc, Ctok)[[full]],
      k_c                  = min(kc),
      binding_nodes        = paste(names(kc)[kc <= min(kc) * (1 + 1e-9)],
                                   collapse = ", "),
      rho_high             = env$load_factor * n_arm / min(kc),
      laminar              = leaf_blocks_laminar(anc),
      submodular           = unname(cert[["submodular"]]),
      # Unmatched and reported: a leaf's path reserve cost is the token
      # weight times its ancestor count times the per-node reserve, and the
      # cheapest leaf's is what a task can be had for. Every leaf of sp has
      # five ancestors, so sp's is higher and no contrast with it may lean
      # on this control.
      leaf_reserve_cost    = unname(spec$weight * min(rowSums(anc)) *
                                      (env$reserve_price %||% 0)),
      demand_device        = sum(dw[names(phys)[phys == "device"]]),
      demand_edge          = sum(dw[names(phys)[phys == "edge"]]),
      demand_cloud         = sum(dw[names(phys)[phys == "cloud"]])
    )
  })
}


# ===========================================================================
# Aggregation and statistics for the node arms
# ===========================================================================

#' Mean every measured column of a node result set, per cell.
#'
#' One aggregator for every node experiment rather than six. The per-tier
#' aggregators name their columns, and those lists do not describe a node row:
#' they average a compliance share, a coverage share, a slice price and a
#' deadline-satisfaction rate that this substrate does not produce, and they
#' omit the exactness gap, the over-commitment and the stranded demand that it
#' does. Averaging what a row actually carries is both smaller and closer to
#' the measurement.
#'
#' @param results_list Per-seed result rows.
#' @param by           Cell variables to group by.
#' @return One row per cell.
node_aggregate <- function(results_list, by) {
  bind_rows(results_list) %>%
    group_by(across(all_of(by))) %>%
    summarise(across(where(\(x) is.numeric(x) || is.logical(x)),
                     \(x) mean(as.numeric(x), na.rm = TRUE)),
              .groups = "drop")
}

#' The responses a node arm is contrasted on.
#'
#' @return Character vector of column names.
node_metrics <- function() {
  # arm_exact_ratio scores the arm's admitted set against the exact leaf-block
  # optimum of the same round; the bid-time oracle ratio is a forecast
  # calibration and is not contrasted.
  c("median_latency", "drop_rate", "utilisation", "welfare", "arm_exact_ratio",
    "mean_price_volatility", "mean_price_volatility_tail",
    "greedy_exact_ratio", "tokens_admitted", "served_among_admitted",
    "welfare_over_optimum", "alloc_ratio_true", "resid_excess",
    "price_volatility_cleared")
}

#' Per-cell summary of one factor over the node responses.
#'
#' The shape stat_exp1 and stat_exp4 produce, over the columns a node row
#' carries. A response that is constant across every cell is dropped rather
#' than bootstrapped, since a confidence interval on a structural zero is not
#' a measurement.
#'
#' @param raw_df    Per-seed node results.
#' @param group_var The factor the cells are contrasted on.
#' @param metrics   Responses to summarise.
#' @return A list of per-cell summaries and the ART interaction on welfare.
node_stat_factor <- function(raw_df, group_var, metrics = node_metrics(),
                             cell_vars = c("graph_type", "load_level"),
                             interaction_vars = group_var) {
  metrics <- intersect(metrics, names(raw_df))
  metrics <- metrics[vapply(metrics, function(m) {
    v <- raw_df[[m]][is.finite(raw_df[[m]])]
    length(v) > 0L && diff(range(v)) > 0
  }, logical(1))]

  # One summary per cell the caller names, so a crossed design is cut on every
  # factor it crosses and the contrasted factor is never pooled over another.
  by_cell <- raw_df %>%
    group_by(across(all_of(cell_vars))) %>%
    group_split() %>%
    setNames(., sapply(., function(d)
      paste(vapply(cell_vars, function(v) as.character(d[[v]][1]), ""),
            collapse = "_")))
  per_cell <- lapply(by_cell, function(d)
    stat_summary_single_factor(d, group_var, metrics))

  cells <- unique(c(interaction_vars, "graph_type", "load_level"))
  list(per_topo_load = per_cell,
       interaction = art_anova(
         raw_df %>% filter(is.finite(welfare)) %>%
           mutate(across(all_of(cells), factor)),
         stats::reformulate(paste(cells, collapse = " * "),
                            response = "welfare")))
}


# ===========================================================================
# The mechanism block on this substrate: the grid and the posted-price frontier
# ===========================================================================

#' The posted levels the node mechanism block runs.
#'
#' A posted level is a rationing dial rather than a mechanism, so a family of
#' three levels is three points on a curve and not three arms: between them the
#' curve is unmeasured, and a comparison against a discovered price at one
#' level compares two admitted volumes as much as two mechanisms. The three
#' original levels are kept so the rows run at them reproduce.
#'
#' @return Numeric vector of markups over marginal cost.
node_posted_levels <- function() c(1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 3, 4)

#' The posted anchor of the instance every other instance is dosed against.
#'
#' The anchor is a leaf's own path at the per-node reserve, so an instance
#' whose leaves pass through five nodes charges five thirds of one whose
#' leaves pass through three, on an identical value distribution, and a
#' cross-instance ordering under the posted price is partly an ordering of
#' doses. This is the reference instance's cheapest-path anchor, which the
#' matched level posts everywhere.
#'
#' @param k         Markup over marginal cost.
#' @param reference The instance whose dose the others are read at.
#' @return Scalar posted price per task.
node_matched_anchor <- function(k = 1, reference = "tree") {
  env <- node_env(reference, "high", node_agents()[[reference]])
  k * min(posted_price_anchor_per_leaf(env, env$anc, k = 1))
}

#' The node substrate's own mechanism grid.
#'
#' Its own function rather than a level added to the per-tier one: the two
#' experiments share a driver-level design and nothing else, and refining the
#' posted family here would otherwise re-run every per-tier branch under prices
#' its evaluation never posted.
#'
#' @param n_seeds Monte Carlo seeds per cell.
#' @return A tibble with one row per branch.
node_exp6_mechanism_grid <- function(n_seeds) {
  arms <- bind_rows(
    tidyr::expand_grid(
      mechanism = c("random", "edf", "greedy_ev", "market", "market_cc",
                    "k8s"),
      p_post_k  = 1),
    tidyr::expand_grid(mechanism = "posted_price",
                       p_post_k  = node_posted_levels()),
    # The matched level is a dose control rather than a curve, so it runs at
    # the three markups the cross-instance ordering is reported at.
    tidyr::expand_grid(mechanism = "posted_price_matched",
                       p_post_k  = c(1, 2, 4)))
  tidyr::expand_grid(
    arms,
    graph_type   = c("tree", "sp", "entangled"),
    load_level   = c("medium", "high"),
    architecture = c("naive", "hybrid"),
    seed         = seq_len(n_seeds))
}

#' Linear interpolation of a measured curve, with no extrapolation.
#'
#' Outside the measured range the answer is NA rather than the nearest level's
#' value: reading a concave curve past its last point would invent the number
#' the frontier exists to measure. Repeated x values are averaged first, which
#' is what two posted levels that both ration everything away look like.
#'
#' @param x,y  The measured points.
#' @param xout Where to read the curve.
#' @return Numeric vector the length of `xout`.
.curve_at <- function(x, y, xout) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 2L) return(rep(NA_real_, length(xout)))
  d <- stats::aggregate(list(y = y[ok]), list(x = x[ok]), mean)
  if (nrow(d) < 2L) return(rep(NA_real_, length(xout)))
  stats::approx(d$x, d$y, xout = xout)$y
}

#' Mean and 95 per cent t interval of a per-seed ratio.
#'
#' The pairing is inside the ratio: each seed's arm is divided by the posted
#' curve of that same seed, so the interval is on the paired differences in
#' logs' own scale rather than on two independent means.
#'
#' @param r Per-seed ratios.
#' @return A list of `mean`, `lo`, `hi`.
.ratio_interval <- function(r) {
  r <- r[is.finite(r)]
  if (length(r) < 2L || stats::sd(r) == 0) {
    return(list(mean = if (length(r) > 0L) mean(r) else NA_real_,
                lo = NA_real_, hi = NA_real_))
  }
  ci <- as.numeric(stats::t.test(r)$conf.int)
  list(mean = mean(r), lo = ci[[1]], hi = ci[[2]])
}

#' Every arm as a point in the posted family's own plane.
#'
#' Per cell the posted levels trace a curve of realised welfare against
#' admitted volume and against realised median latency. Every other arm is one
#' point in that plane, and the two matched columns divide its welfare by the
#' posted curve read at its own volume and at its own latency, seed by seed.
#' The posted rows carry no matched column: the family is the reference.
#'
#' @param raw_df    Per-seed node results.
#' @param cell_vars The variables a comparison is made inside.
#' @return One row per cell, mechanism and posted level.
node_frontier_table <- function(raw_df,
                                cell_vars = c("graph_type", "load_level",
                                              "architecture")) {
  cols <- c("tokens_admitted", "median_latency", "welfare", "alloc_ratio_true")
  stopifnot(all(c(cell_vars, "mechanism", "p_post_k", "seed", cols) %in%
                  names(raw_df)))
  df <- raw_df %>%
    select(all_of(c(cell_vars, "mechanism", "p_post_k", "seed", cols))) %>%
    rename(alloc_ratio = "alloc_ratio_true")

  curves <- df %>%
    filter(mechanism == "posted_price") %>%
    group_by(across(all_of(c(cell_vars, "seed")))) %>%
    summarise(curve = list(tibble(v = tokens_admitted,
                                  l = median_latency,
                                  w = welfare)),
              .groups = "drop")

  read_at <- function(curve, xout, on) {
    purrr::map2_dbl(curve, xout, function(cv, x)
      if (is.null(cv)) NA_real_ else .curve_at(cv[[on]], cv$w, x))
  }
  points <- df %>%
    filter(mechanism != "posted_price") %>%
    left_join(curves, by = c(cell_vars, "seed")) %>%
    mutate(r_volume  = welfare / read_at(curve, tokens_admitted, "v"),
           r_latency = welfare / read_at(curve, median_latency, "l")) %>%
    select(-curve)

  bind_rows(points, df %>% filter(mechanism == "posted_price") %>%
              mutate(r_volume = NA_real_, r_latency = NA_real_)) %>%
    group_by(across(all_of(c(cell_vars, "mechanism", "p_post_k")))) %>%
    summarise(
      n_seeds         = dplyr::n(),
      tokens_admitted = mean(tokens_admitted, na.rm = TRUE),
      median_latency  = mean(median_latency, na.rm = TRUE),
      welfare         = mean(welfare, na.rm = TRUE),
      alloc_ratio     = mean(alloc_ratio, na.rm = TRUE),
      welfare_vs_posted_at_matched_volume     = .ratio_interval(r_volume)$mean,
      welfare_vs_posted_at_matched_volume_lo  = .ratio_interval(r_volume)$lo,
      welfare_vs_posted_at_matched_volume_hi  = .ratio_interval(r_volume)$hi,
      welfare_vs_posted_at_matched_latency    = .ratio_interval(r_latency)$mean,
      welfare_vs_posted_at_matched_latency_lo = .ratio_interval(r_latency)$lo,
      welfare_vs_posted_at_matched_latency_hi = .ratio_interval(r_latency)$hi,
      .groups = "drop")
}


# ===========================================================================
# One tuned knob per mechanism, on seeds the tuning never sees
# ===========================================================================

#' The reserve markups the market arms are tuned over.
#'
#' The market's counterpart of the posted price's markup: both are one number
#' that rations, so a comparison at each mechanism's own best setting is a
#' comparison of mechanisms rather than of who was given a dial.
#'
#' @return Numeric vector of multipliers on the per-node reserve.
node_reserve_markups <- function() c(1, 1.25, 1.5, 2)

#' The seeds a knob is chosen on and the seeds it is reported on.
#'
#' Disjoint by construction, and the reported ones sit above every seed the
#' mechanism block runs, so no number in the tuned table was ever seen while
#' the knob was being chosen.
#'
#' @return A list of `tuning` and `evaluation` seed vectors.
node_tuning_split <- function() list(tuning = 1:5, evaluation = 11:20)

#' The grid the knobs are chosen on.
#'
#' @param seeds Tuning seeds.
#' @return A tibble with one row per branch.
node_tuning_grid <- function(seeds) {
  arms <- bind_rows(
    tidyr::expand_grid(mechanism = "posted_price",
                       p_post_k = node_posted_levels(), reserve_markup = 1),
    tidyr::expand_grid(mechanism = c("market", "market_cc"), p_post_k = 1,
                       reserve_markup = node_reserve_markups()))
  tidyr::expand_grid(
    arms,
    graph_type   = c("tree", "sp", "entangled"),
    load_level   = c("medium", "high"),
    architecture = c("naive", "hybrid"),
    seed         = seeds)
}

#' The grid the tuned mechanisms are reported on.
#'
#' Each tuned mechanism at the knob with the highest mean welfare on the
#' tuning seeds, plus the arms that have no knob at their only setting, run on
#' the held-out seeds.
#'
#' @param tuning_raw Per-seed results of the tuning grid.
#' @param seeds      Evaluation seeds.
#' @param cell_vars  The variables a knob is chosen inside.
#' @param fixed      Mechanisms with nothing to tune.
#' @return A tibble with one row per branch.
node_eval_grid <- function(tuning_raw, seeds,
                           cell_vars = c("graph_type", "load_level",
                                         "architecture"),
                           fixed = c("greedy_ev", "k8s")) {
  knobs <- tuning_raw %>%
    group_by(across(all_of(c(cell_vars, "mechanism", "p_post_k",
                             "reserve_markup")))) %>%
    summarise(tuning_welfare = mean(welfare, na.rm = TRUE), .groups = "drop") %>%
    group_by(across(all_of(c(cell_vars, "mechanism")))) %>%
    slice_max(tuning_welfare, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(-tuning_welfare)
  rest <- tidyr::expand_grid(distinct(tuning_raw, across(all_of(cell_vars))),
                             mechanism = fixed, p_post_k = 1,
                             reserve_markup = 1)
  tidyr::expand_grid(bind_rows(knobs, rest), seed = seeds)
}

#' The one value a column takes inside a cell.
#'
#' @param x A column that must be constant.
#' @return Its value.
.one_of <- function(x) {
  stopifnot("a tuned cell ran more than one knob" = length(unique(x)) == 1L)
  x[[1]]
}

#' What each mechanism reaches at its tuned knob, on the held-out seeds.
#'
#' @param eval_raw  Per-seed results of the evaluation grid.
#' @param cell_vars The variables a knob was chosen inside.
#' @return One row per cell and mechanism.
node_tuned_table <- function(eval_raw,
                             cell_vars = c("graph_type", "load_level",
                                           "architecture")) {
  eval_raw %>%
    group_by(across(all_of(c(cell_vars, "mechanism")))) %>%
    summarise(p_post_k             = .one_of(p_post_k),
              reserve_markup       = .one_of(reserve_markup),
              n_eval_seeds         = dplyr::n_distinct(seed),
              welfare              = mean(welfare, na.rm = TRUE),
              tokens_admitted      = mean(tokens_admitted, na.rm = TRUE),
              median_latency       = mean(median_latency, na.rm = TRUE),
              welfare_over_optimum = mean(welfare_over_optimum, na.rm = TRUE),
              .groups = "drop")
}


# ===========================================================================
# The onset law: the first node crossing
# ===========================================================================

#' Expected number of price-moving rounds per seed at a population.
#'
#' Price dispersion is exactly zero until some node's demand exceeds its
#' capacity in some round. A round's arrivals are Poisson with mean lambda N,
#' the arrivals whose leaf sits under node v are a thinning at v's leaf share,
#' so the tasks demanded at v are Poisson with mean s_v lambda N, and the
#' onset is the first node crossing. That node is the one with the largest
#' crossing probability, which is the node with the largest fluctuation
#' relative to its own capacity, and it need not be the node that sets K_c:
#' an edge node of fifty tasks fed by half the arrivals crosses before the
#' root of one hundred fed by all of them.
#'
#' @param spec       An instance spec.
#' @param leaf_mix   "uniform" or "skewed".
#' @param load_level "low", "medium" or "high".
#' @param N          Agent population (vectorised).
#' @param n_rounds   Rounds per seed.
#' @return A tibble with one row per N: `N`, `binding_node`, the per-node
#'   crossing probability `p_cross` and `expected_crossings` per seed.
node_onset_law <- function(spec, leaf_mix, load_level, N, n_rounds) {
  anc    <- ancestor_matrix(spec)
  Ctok   <- token_capacity(spec)[colnames(anc)]
  shares <- node_leaf_shares(leaf_mix, rownames(anc))
  s_v    <- setNames(as.numeric(crossprod(anc, shares)), colnames(anc))
  lambda <- c(low = 0.5, medium = 1.0, high = 1.5)[[load_level]]
  purrr::map_dfr(N, function(n) {
    p <- 1 - stats::ppois(floor(Ctok), s_v * lambda * n)
    v <- names(which.max(p))
    tibble::tibble(N = n, binding_node = v, p_cross = p[[v]],
                   expected_crossings = n_rounds * p[[v]])
  })
}
