# sim_exp11.R
# ---------------------------------------------------------------------------
# Experiment 11: heterogeneous per-task recipes over shared resources.
#
# The topology experiment cannot separate feasibility structure from demand:
# tree, SP and entangled differ in per-tier demand profile, bottleneck
# utilisation and offered load all at once. This experiment holds all of that
# fixed -- ONE DAG, one agent count, one load level, one seed per branch, and an
# aggregate per-tier demand matched to the last task -- and varies only whether
# tasks arrive carrying one recipe or two.
#
# Four cells, the 2x2 of (clearing regime) x (recipe set):
#
#                        | per-resource clearing | integrator slice
#   ---------------------+-----------------------+------------------
#   homogeneous recipes  | homogeneous           | homog_encapsulated
#   heterogeneous (A/B)  | hetero_naive          | hetero_encapsulated
#
# Type A = (2.0, 1.0, 1.5) and type B = (1.0, 2.0, 1.5) over (device, edge,
# cloud) are not scalar multiples of one another, so their catalogue is the
# two-slice counterexample scaled into the simulator's three priced tiers. Their
# 50/50 mix-average is exactly the homogeneous recipe (1.5, 1.5, 1.5), and types
# are assigned by the task's parity carried across the round boundary, so the
# realised aggregate matches rather than matching in expectation.
#
# NO INCENTIVE CLAIM IS MADE ON THIS EXPERIMENT. vcg_allocate is DSIC here only
# because value-greedy is the exact welfare maximiser under an identical bundle;
# once recipes differ the allocation rule is a multidimensional-knapsack
# heuristic and the Clarke clamp papers over the externalities it creates. The
# only incentive-adjacent number reported is the greedy-versus-exact welfare
# gap, which is allocative efficiency, not truthfulness. vcg_allocate refuses a
# recipe-carrying environment outright.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
})


# ===========================================================================
# The recipe catalogue and its assignment
# ===========================================================================

#' The two task types, as per-tier resource recipes.
#' @return A named list of named numeric vectors.
exp11_recipe_catalogue <- function() {
  list(A = c(device = 2.0, edge = 1.0, cloud = 1.5),
       B = c(device = 1.0, edge = 2.0, cloud = 1.5))
}

#' The mix-average recipe: the homogeneous arm's single bundle.
#' @return A named numeric vector over (device, edge, cloud).
exp11_mix_average <- function() {
  cat_ <- exp11_recipe_catalogue()
  (cat_$A + cat_$B) / 2
}

#' The arms, in reporting order.
exp11_arms <- function() {
  c("homogeneous", "hetero_naive", "hetero_encapsulated", "homog_encapsulated")
}

#' Prose labels for the arms, for anything a reader sees.
#' @return A named character vector indexed by exp11_arms().
exp11_arm_labels <- function() {
  c(homogeneous         = "One recipe, per-resource",
    hetero_naive        = "Two recipes, per-resource",
    hetero_encapsulated = "Two recipes, encapsulated",
    homog_encapsulated  = "One recipe, encapsulated")
}

#' Assign task types by alternating parity, carried across rounds.
#'
#' Alternating within the round alone leaves the aggregate matched only in
#' expectation once a round holds an odd number of tasks; carrying the offset
#' makes consecutive odd-sized rounds cancel pairwise.
#'
#' What this buys, exactly: the arms' realised offered per-tier demand matches
#' to the last bit whenever the branch holds an EVEN number of odd-sized rounds,
#' and otherwise to within half a task spread over the branch, the single
#' leftover of the unpaired odd round. Cloud matches either way, since both
#' recipes carry the same cloud demand. The reported agg_demand_* diagnostic
#' carries the realised figure, so the residual is visible rather than assumed
#' away. Note the scope: what is matched is the OFFERED demand of the
#' generated-and-truncated task stream, not expected demand and not executed
#' demand -- the admitted sets differ across arms by construction, and the
#' encapsulated arms over-admit on purpose.
#'
#' @param n      Number of tasks in the round.
#' @param offset Parity carried in from the previous round (0 or 1).
#' @return A character vector of recipe names.
exp11_assign_recipes <- function(n, offset = 0L) {
  if (n == 0L) return(character(0))
  ifelse((offset + seq_len(n) - 1L) %% 2L == 0L, "A", "B")
}

#' Build the Exp.11 environment: one fixed DAG, per-tier capacity, recipes.
#'
#' The DAG is the tree, so the critical-path tier sequence is identical across
#' arms by construction rather than by matching. Its demand weights are replaced
#' by the mix-average recipe, which makes the homogeneous arm literally today's
#' single-bundle model at a_bar and makes the integrator's slice capacity a
#' capacity computed from the MEAN recipe -- which is where over-admission comes
#' from when the realised mix is not the mean.
#'
#' @param cap    Per-tier capacity (the contention knob).
#' @param N      Number of agents.
#' @param hetero Whether tasks carry the A/B catalogue.
#' @return An environment list.
exp11_env <- function(cap, N, hetero) {
  env <- init_environment(build_dependency_graph("tree"), "medium",
                          n_agents = N, graph_type = "tree")
  a_bar <- exp11_mix_average()
  env$demand_weights <- tibble(tier = names(a_bar), demand_weight = unname(a_bar))
  env$capacities     <- tibble(tier = names(a_bar), capacity = as.numeric(cap))
  # per_tier carries its own copy of the capacity and is what execute_allocation
  # queues against, so a capacity override that misses it binds at admission and
  # not in execution.
  env$per_tier <- mutate(env$per_tier, capacity = as.numeric(cap))
  if (hetero) env$recipes <- exp11_recipe_catalogue()
  env
}

.exp11_arm_spec <- function(arm) {
  list(hetero       = startsWith(arm, "hetero"),
       encapsulated = grepl("encapsulated", arm, fixed = TRUE))
}


# ===========================================================================
# The runner
# ===========================================================================

#' Run a single Exp.11 cell (one arm, one capacity, one seed).
#'
#' @param arm       One of exp11_arms().
#' @param cap       Per-tier capacity.
#' @param seed      Random seed.
#' @param N         Number of agents.
#' @param n_rounds  Number of rounds.
#' @param max_tasks Per-round instance cap; the exact packer enumerates subsets.
#' @return A one-row tibble of summary metrics and diagnostics.
exp11_run_single <- function(arm = exp11_arms(), cap = 9, seed = 1L,
                             N = 8L, n_rounds = 100L,
                             deadlines = c(500L, 750L, 1000L),
                             alpha = 50, p = 1.2,
                             lambda_l_default = 0.005, salvage = 0.0,
                             iters = 15L, eta = price_eta, success_lr = 0.3,
                             integ_beta = 0.8, integ_eta = price_eta,
                             max_tasks = 14L) {
  arm  <- match.arg(arm)
  spec <- .exp11_arm_spec(arm)
  # The runner passes max_tasks straight through as the packer's enumeration
  # cap, so without this a caller could widen the contract instead of tripping
  # it. Production rounds do reach nineteen tasks, so the truncation is doing
  # real work and its ceiling is not a caller's choice.
  stopifnot("max_tasks is the enumeration cap and may not exceed 14" =
              max_tasks <= 14L)
  set.seed(seed)

  env    <- exp11_env(cap = cap, N = N, hetero = spec$hetero)
  agents <- init_agents(N)
  blb    <- base_latency_for_bids(env)
  ms_res <- init_market_state(env)
  # efficiency_factor = 1: the encapsulated arms differ from the per-resource
  # arms in the INTERFACE they expose, not in an operational demand reduction,
  # which would confound the measurement with a second effect.
  integrator <- integrator_init(beta = integ_beta, efficiency_factor = 1,
                                eta = integ_eta)
  prev_util <- NULL
  offset    <- 0L

  dropV <- servedV <- clearV <- unitCostV <- welfareV <- numeric(n_rounds)
  exactV <- allocV <- vgV <- bindV <- truncV <- numeric(n_rounds)
  aggD <- matrix(0, nrow = n_rounds, ncol = 3,
                 dimnames = list(NULL, names(exp11_mix_average())))

  for (t in seq_len(n_rounds)) {
    # Re-seed per round so the task stream is identical across arms in EVERY
    # round, not only the first: the arms consume different numbers of execution
    # noise draws, which would otherwise decouple their task lists after round 1
    # and leave the demand match holding only in expectation.
    set.seed(seed * 1009L + t)
    tasks_all <- bind_tasks(lapply(
      split(agents, agents$agent_id),
      function(a) generate_tasks(a, env, round = t, deadlines = deadlines)
    ))

    truncV[t] <- as.numeric(nrow(tasks_all) > max_tasks)
    tasks_all <- head(tasks_all, max_tasks)
    n_gen     <- nrow(tasks_all)
    if (spec$hetero && n_gen > 0L) {
      tasks_all$recipe <- exp11_assign_recipes(n_gen, offset)
      offset <- (offset + n_gen) %% 2L
    }

    util_hat <- if (is.null(prev_util)) 0 else mean(prev_util$util, na.rm = TRUE)
    ev <- task_expected_value(tasks_all, util_hat, blb, ms_res$success_model,
                              alpha = alpha, p = p,
                              lambda_l_default = lambda_l_default,
                              salvage = salvage)
    A  <- task_recipes(tasks_all, env)
    C  <- env$capacities$capacity[match(colnames(A), env$capacities$tier)]
    ex <- exact_pack_by_value(ev, A, C, max_n = max_tasks)
    # The packing rule measured on its own: value-greedy over the FULL instance,
    # against the enumerated optimum of that same instance. The admitted set is
    # not this number -- admission also passes through the tatonnement's
    # positive-surplus filter at the cleared prices, which rations on price and
    # would bury a packing gap of a per cent inside a rationing gap of forty.
    vgV[t] <- sum(ev[.greedy_pack_by(ev, tasks_all, env)])

    if (spec$encapsulated) {
      ic <- integrator_clear(tasks_all, env, util_hat, blb, ms_res$success_model,
                             integrator, alpha = alpha, p = p,
                             lambda_l_default = lambda_l_default,
                             salvage = salvage, iters = iters)
      allocation   <- ic$allocation
      integrator   <- ic$integrator
      unitCostV[t] <- ic$unit_cost
      ms_res       <- append_price_history(ms_res, ic$tier_prices)
    } else {
      cleared      <- clear_multitier_market(tasks_all, env, util_hat, blb, ms_res,
                                             alpha = alpha, p = p,
                                             lambda_l_default = lambda_l_default,
                                             salvage = salvage, iters = iters,
                                             eta = eta)
      allocation   <- cleared$allocation
      ms_res       <- append_price_history(cleared$market_state,
                                           cleared$market_state$prices)
      unitCostV[t] <- cleared$clearing$unit_cost
    }

    idx        <- match(allocation$task_id, tasks_all$task_id)
    allocV[t]  <- if (length(idx) > 0L) sum(ev[idx]) else 0
    exactV[t]  <- ex$value
    used       <- if (length(idx) > 0L) colSums(A[idx, , drop = FALSE]) else C * 0
    bindV[t]   <- as.numeric(any(used >= C - 1e-9))
    if (n_gen > 0L) aggD[t, ] <- colSums(A)[colnames(aggD)]

    results_t   <- execute_allocation(allocation, env)
    succ        <- if (nrow(results_t) == 0L) 0 else sum(results_t$success, na.rm = TRUE)
    dropV[t]    <- if (n_gen == 0L) NA_real_ else 1 - succ / n_gen
    clearV[t]   <- if (n_gen == 0L) NA_real_ else nrow(allocation) / n_gen
    servedV[t]  <- if (nrow(allocation) == 0L) NA_real_ else succ / nrow(allocation)
    welfareV[t] <- compute_welfare(results_t, env, NULL,
                                   lambda_l_default = lambda_l_default,
                                   salvage = salvage)

    ms_res    <- market_update_from_results(ms_res, util_hat, results_t, lr = success_lr)
    prev_util <- compute_utilisation_per_tier(env, n_gen)
  }

  ratio <- function(num) mean(ifelse(exactV > 0, num / exactV, NA_real_), na.rm = TRUE)
  tibble(
    arm  = arm,
    cap  = as.numeric(cap),
    N    = as.integer(N),
    seed = seed,
    # Price dispersion of the per-task cost the agent actually pays: the
    # mix-average basket on the per-resource arms, the posted slice price on the
    # encapsulated ones. One basket on both sides.
    price_cv = agent_price_volatility(unitCostV),
    # A WITHIN-ARM efficiency index, not a cross-arm welfare comparison, and not
    # bounded by 1 on principle: the numerator is realised, deadline-aware,
    # congestion-penalised welfare and the denominator an expected-value
    # optimum, so they are not in the same units. The denominator is also
    # arm-specific and path-dependent, since ev follows each arm's own evolving
    # success model. Compare arms on the `welfare` column below instead.
    welfare_ratio         = ratio(welfareV),
    # The packing gap: value-greedy over the enumerated optimum, both on the
    # round's full instance. Exactly 1 under identical recipes, since
    # value-greedy IS the argmax there; below 1 once recipes differ.
    greedy_exact_ratio    = ratio(vgV),
    # What the arm actually admitted over the same optimum. Rationing plus
    # packing, and on the encapsulated arms it can exceed 1 per round, because a
    # slice capacity computed from the mean recipe can over-commit a tier.
    admitted_exact_ratio  = ratio(allocV),
    drop_rate             = mean(dropV, na.rm = TRUE),
    clearing_fraction     = mean(clearV, na.rm = TRUE),
    served_among_admitted = mean(servedV, na.rm = TRUE),
    binding_fraction      = mean(bindV),
    truncation_rate       = mean(truncV),
    agg_demand_device     = mean(aggD[, "device"]),
    agg_demand_edge       = mean(aggD[, "edge"]),
    agg_demand_cloud      = mean(aggD[, "cloud"]),
    mean_unit_cost        = mean(unitCostV, na.rm = TRUE),
    welfare               = mean(welfareV, na.rm = TRUE),
    exact_welfare         = mean(exactV, na.rm = TRUE)
  )
}

#' Aggregate Exp.11 results across Monte Carlo seeds.
#'
#' @param results_list List of single-seed tibbles from exp11_run_single().
#' @return A tibble with one row per (arm, cap).
exp11_aggregate <- function(results_list) {
  bind_rows(results_list) %>%
    group_by(arm, cap) %>%
    summarise(
      across(
        c(price_cv, welfare_ratio, greedy_exact_ratio, admitted_exact_ratio,
          drop_rate, clearing_fraction, served_among_admitted, binding_fraction,
          truncation_rate, agg_demand_device, agg_demand_edge, agg_demand_cloud,
          mean_unit_cost, welfare, exact_welfare),
        \(x) mean(x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}


# ===========================================================================
# The over-commitment sweep (one resource, two slice sizes)
# ===========================================================================

#' Over-commitment of an advertised interface against its deliverable set.
#'
#' The smallest instance that produces a measurable number rather than a sign.
#' One internal resource of capacity kappa is exported as two slices identical
#' in every respect but size, lambda_1 = 1 and lambda_2. The two regions are
#' carried separately, which is the whole point: an implementation that cleared
#' against one and checked feasibility against the same one could not see the
#' effect at all.
#'
#'   advertised P_f:   x1 <= kappa,  x2 <= kappa / lambda_2,  x1 + x2 <= kappa
#'   deliverable F:    x1 + lambda_2 * x2 <= kappa
#'
#' Greedy on the advertised interface takes the heavy slice to its own bound and
#' then fills with the light slice up to the joint bound. Service is light slice
#' first, so what fails is the heavy commitment.
#'
#' Only rho is the theory's prediction. The unserved columns depend on that
#' light-first service order, and unserved_fraction is not monotone in lambda_2
#' (0.25 at 2, 0.1875 at 4) because fewer heavy commitments are made as the
#' heavy slice grows: it is not a cost curve and must not be quoted as one.
#'
#' @param lambda_2 Heavy-slice sizes to sweep.
#' @param kappa    Internal capacity per epoch.
#' @return A tibble, one row per lambda_2.
exp11_overcommitment_sweep <- function(lambda_2 = c(1, 1.25, 1.5, 2, 4),
                                       kappa = 12) {
  purrr::map_dfr(lambda_2, function(l2) {
    # Greedy point of the ADVERTISED interface.
    x2 <- min(kappa / l2, kappa)
    x1 <- max(0, min(kappa, kappa - x2))
    # Its consumption in the DELIVERABLE set.
    committed <- x1 * 1 + x2 * l2
    # Service against the true capacity, light slice first.
    served_1 <- min(x1, kappa)
    served_2 <- min(x2, max(0, kappa - served_1) / l2)

    tibble(
      lambda_2                = l2,
      x1                      = x1,
      x2                      = x2,
      rho_predicted           = 2 - 1 / l2,
      rho_measured            = committed / kappa,
      unserved_fraction       = 1 - (served_1 + served_2) / (x1 + x2),
      heavy_unserved_fraction = if (x2 > 0) 1 - served_2 / x2 else 0
    )
  })
}


# ===========================================================================
# The inner-exposure control (two resources, three slices)
# ===========================================================================

#' Raw catalogue exposure against inner-box exposure, on one fixed graph.
#'
#' The exposed catalogue is s1 = (1,0), s2 = (0,1), s3 = (1,1) over two internal
#' resources of unit capacity: a depth-1 tree quotient, so the structural
#' hypothesis holds while the guarantee fails, because the catalogue rank is not
#' submodular. Exposing the inner box (1/2, 1/2, 1/2) instead satisfies
#' A c <= C, so greedy is exact on it and every allocation is deliverable; the
#' welfare it forgoes is the price of the safe interface.
#'
#' The fractional-optimum cell (m = K = 3 with A = [[1,1,0],[0,1,1],[1,0,1]]) is
#' deliberately NOT built: its integrality gap is a unit-capacity effect that
#' vanishes at even capacities, so at simulator scale it measures nothing.
#'
#' @return A two-row tibble: regime, advertised, overcommitment, deliverable
#'   optimum, and forgone = optimum - advertised. Forgone is the price of the
#'   safe interface on the inner-exposure row; on the raw-catalogue row it is
#'   NEGATIVE, which is not welfare gained but welfare advertised and not
#'   deliverable -- the same over-commitment the factor beside it reports.
exp11_inner_exposure_control <- function() {
  A <- rbind(c(1, 0), c(0, 1), c(1, 1))   # slices x internal resources
  C <- c(1, 1)

  # Deliverable optimum, by enumeration rather than by assertion.
  opt <- exact_pack_by_value(rep(1, 3), A, C)$value

  # Raw catalogue: every slice is individually within its advertised bound, so
  # the interface admits all three.
  raw_x   <- c(1, 1, 1)
  # Inner box: the largest uniform exposure that is deliverable.
  inner_x <- c(0.5, 0.5, 0.5)

  overcommit <- function(x) max(as.vector(x %*% A) / C)

  tibble(
    regime               = c("raw_catalogue", "inner_exposure"),
    advertised           = c(sum(raw_x), sum(inner_x)),
    overcommitment       = c(overcommit(raw_x), overcommit(inner_x)),
    deliverable_optimum  = opt,
    forgone              = opt - c(sum(raw_x), sum(inner_x))
  )
}
