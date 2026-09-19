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
  graph  <- build_leaf_graph(spec, node_demand_weights(anc, shares, spec$weight))

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
                                 unname(spec$weight * anc[l, ]))))
  }
  env
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
#' @param env Environment list from node_env().
#' @return Named numeric vector of token capacities, in the ancestor matrix's
#'   column order.
node_token_capacity <- function(env) {
  token_capacity(node_effective_spec(env))[colnames(env$anc)]
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
node_run_single <- function(graph_type = c("tree", "sp", "entangled"),
                            load_level = c("medium", "high", "low"),
                            N = 90L, seed = 1L, n_rounds = 200L,
                            deadlines = c(500L, 750L, 1000L),
                            lambda_l_default = node_lambda_l(),
                            leaf_mix = c("uniform", "skewed"),
                            exact_reference = TRUE,
                            alpha = 50, p = 1.2, salvage = 0.0,
                            iters = 15L, eta = price_eta, success_lr = 0.3) {
  graph_type <- match.arg(graph_type)
  load_level <- match.arg(load_level)
  leaf_mix   <- match.arg(leaf_mix)

  set.seed(seed)
  env    <- node_env(graph_type, load_level, N, leaf_mix)
  agents <- init_agents(N)

  anc    <- env$anc
  leaves <- rownames(anc)
  Ctok   <- node_token_capacity(env)
  fr     <- flow_rank(node_effective_spec(env), anc)
  flow_full <- fr$value[[subset_name(leaves, leaves)]]
  lb_full   <- leaf_rank(anc, Ctok)[[subset_name(leaves, leaves)]]

  ms        <- init_market_state(env)
  prev_util <- NULL

  medL <- p95L <- utilV <- dropV <- numeric(n_rounds)
  welfareV <- oracleV <- effV <- numeric(n_rounds)
  unitCostV <- clearV <- servedV <- numeric(n_rounds)
  priceCvV  <- tokensV <- bindV <- numeric(n_rounds)
  ratioV    <- shapeV <- rep(NA_real_, n_rounds)

  for (t in seq_len(n_rounds)) {
    tasks_all <- node_round_tasks(env, agents, t, seed, deadlines)
    n_gen     <- nrow(tasks_all)
    bid       <- node_bid_inputs(env, tasks_all, prev_util)
    # The success model's stochastic gradient takes one number for the round;
    # the round's own signal is a vector over its tasks, so its mean is what
    # the update sees. The bid-time values keep the per-task vector.
    util_scalar <- if (n_gen == 0L) 0 else mean(bid$util_hat)

    cleared <- clear_multitier_market(
      tasks_all, env, bid$util_hat, bid$base_latency, ms,
      alpha = alpha, p = p, lambda_l_default = lambda_l_default,
      salvage = salvage, iters = iters, eta = eta)
    allocation   <- cleared$allocation
    ms           <- append_price_history(cleared$market_state,
                                         cleared$market_state$prices)
    unitCostV[t] <- cleared$clearing$unit_cost
    priceCvV[t]  <- cross_leaf_price_cv(cleared$clearing$prices, anc)

    results_t <- execute_allocation(allocation, env)
    if (nrow(results_t) > 0 &&
        !all(c("deadline", "value_base") %in% names(results_t))) {
      results_t <- results_t %>%
        left_join(allocation %>% select(task_id, deadline, value_base),
                  by = "task_id")
    }
    agents <- update_trust(agents, results_t)

    if (nrow(results_t) == 0) {
      medL[t] <- NA; p95L[t] <- NA
    } else {
      medL[t] <- median(results_t$latency, na.rm = TRUE)
      p95L[t] <- quantile(results_t$latency, 0.95, na.rm = TRUE, names = FALSE)
    }

    util_df   <- compute_utilisation_per_node(env, allocation)
    utilV[t]  <- mean(util_df$util, na.rm = TRUE)
    bindV[t]  <- as.numeric(max(util_df$util, na.rm = TRUE) >= 0.99)
    prev_util <- util_df

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
      results_t, env, ms$prices,
      lambda_l_default = lambda_l_default, salvage = salvage,
      cong_cost = TRUE, cong_gamma = 0.05)
    orc <- oracle_pack_realised(
      tasks_all, env, bid$util_hat, bid$base_latency, ms$success_model,
      alpha = alpha, p = p,
      lambda_l_default = lambda_l_default, salvage = salvage)
    oracleV[t] <- orc$oracle_value
    effV[t]    <- ifelse(oracleV[t] > 0, welfareV[t] / oracleV[t], NA_real_)

    # The structural instrument, computed OFF the market on the round's full
    # instance: admission passes through the tatonnement's positive-surplus
    # filter, which rations on price and would bury a packing gap of a per cent
    # inside a rationing gap of forty.
    if (exact_reference && n_gen > 0) {
      ev     <- cleared$expected_value
      marg   <- node_leaf_marginals(tasks_all, ev, leaves)
      exact  <- lb_optimum(marg, anc, Ctok)
      greedy <- sum(ev[.greedy_pack_by(ev, tasks_all, env)])
      if (is.finite(exact) && exact > 0) ratioV[t] <- greedy / exact

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
  tibble(
    graph_type                 = graph_type,
    load_level                 = load_level,
    N                          = as.integer(N),
    seed                       = seed,
    leaf_mix                   = leaf_mix,
    median_latency             = mean(medL, na.rm = TRUE),
    p95_latency                = mean(p95L, na.rm = TRUE),
    utilisation                = mean(utilV, na.rm = TRUE),
    drop_rate                  = mean(dropV, na.rm = TRUE),
    clearing_fraction          = mean(clearV, na.rm = TRUE),
    served_among_admitted      = mean(servedV, na.rm = TRUE),
    welfare                    = mean(welfareV, na.rm = TRUE),
    oracle_welfare             = mean(oracleV, na.rm = TRUE),
    efficiency                 = mean(effV, na.rm = TRUE),
    mean_unit_cost             = mean(unitCostV, na.rm = TRUE),
    mean_price_volatility      = agent_price_volatility(unitCostV),
    mean_price_volatility_tail = agent_price_volatility_tail(unitCostV),
    # The reportable structural statistic is the round INCIDENCE and the tail
    # of the ratio, not its mean alone: a gap that opens in a fifth of the
    # rounds and closes in the rest averages to a number that looks like noise.
    greedy_exact_ratio         = mean(ratioV, na.rm = TRUE),
    greedy_exact_incidence     = if (all(is.na(ratioV))) NA_real_
                                 else mean(below, na.rm = TRUE),
    greedy_exact_worst         = if (all(is.na(ratioV))) NA_real_
                                 else min(ratioV, na.rm = TRUE),
    flow_bound_ratio           = lb_full / flow_full,
    flow_bound_token_gap       = flow_full - lb_full,
    shape_refusal_fraction     = mean(shapeV, na.rm = TRUE),
    price_cv_cross             = mean(priceCvV, na.rm = TRUE),
    tokens_admitted            = mean(tokensV, na.rm = TRUE),
    binding_fraction           = mean(bindV, na.rm = TRUE)
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
