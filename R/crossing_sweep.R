# ===========================================================================
# The crossing sweep: existence and exactness away from this substrate
# ===========================================================================
#
# Every instance this simulator ships has a leaf-block family that is laminar
# or laminar plus one set, so its incidence matrix is totally unimodular, the
# round's packing relaxation is integral at every value vector, and a clearing
# price vector exists in every round. The topology-to-existence question
# therefore cannot be measured on the shipped instances: it is settled there
# by a theorem.
#
# This module leaves that region. It works entirely at the matrix level -- a
# task-by-node 0/1 matrix, per-task values and per-node token capacities --
# because that is all three instruments need: the integrality certificate
# (node_lp_ip_gap, reused as is), the greedy packer's exactness against the
# binary optimum from the same matrix, and the per-node onset law. Nothing
# here builds an environment, runs a price process or executes an allocation,
# so nothing here can move a mechanism number.
#
# A "family" is a leaf set, a named list of leaf blocks (one per internal
# node, plus each leaf's own singleton block, exactly as an ancestor matrix
# carries them), a token capacity per node, and the structural predicates that
# decide which of the theory's regions the family sits in:
#
#   laminar               every pair of blocks nested or disjoint
#   one_crossing          exactly one crossing pair
#   bilaminar_multi       several crossings, crossing graph 2-colourable,
#                         hence a union of two laminar families and still TU
#   oddcycle_interval     crossing graph has an odd cycle, but the blocks are
#                         intervals of some leaf order, hence still TU
#   oddcycle_nonInterval  neither; the only region where integrality can fail
#
# The predicates are computed on the INTERNAL blocks only. A leaf's singleton
# block crosses nothing, is an interval of every order, and enters the matrix
# as a unit column, and appending unit columns preserves total unimodularity,
# so including them would change no verdict and cost enumeration.


# ---------------------------------------------------------------------------
# Structural predicates
# ---------------------------------------------------------------------------

#' Permutations of seq_len(n), one per row, built once per width.
#'
#' The interval-order predicate is a search over leaf orders and the generator
#' asks for it a few thousand times, so the orders are enumerated once per
#' width and reused. Rows are read as position maps (entry j is the position
#' of leaf j); the set of all permutations is closed under inversion, so that
#' reading enumerates exactly the same orders as reading them as sequences.
#'
#' @param n Number of leaves, at most 8.
#' @return An integer matrix of factorial(n) rows and n columns.
.sweep_perms <- local({
  cache <- new.env(parent = emptyenv())
  function(n) {
    stopifnot("the interval search is brute force and stops at eight leaves" =
                n >= 1L && n <= 8L)
    key <- as.character(n)
    if (!is.null(cache[[key]])) return(cache[[key]])
    P <- matrix(1L, nrow = 1L, ncol = 1L)
    for (k in seq_len(n)[-1]) {
      m   <- nrow(P)
      out <- matrix(0L, m * k, k)
      for (j in seq_len(k)) {
        left  <- if (j > 1L) P[, seq_len(j - 1L), drop = FALSE] else P[, 0, drop = FALSE]
        right <- if (j < k)  P[, j:(k - 1L), drop = FALSE]      else P[, 0, drop = FALSE]
        out[((j - 1L) * m + 1L):(j * m), ] <- cbind(left, k, right)
      }
      P <- out
    }
    cache[[key]] <- P
    P
  }
})

#' The pairs of blocks that intersect without nesting.
#'
#' @param blocks Named list of leaf subsets.
#' @return A two-column integer matrix of crossing pairs, possibly empty.
.sweep_crossing_pairs <- function(blocks) {
  k <- length(blocks)
  if (k < 2L) return(matrix(integer(0), ncol = 2L))
  pairs <- utils::combn(k, 2L)
  cross <- apply(pairs, 2L, function(p) {
    a <- blocks[[p[1]]]
    b <- blocks[[p[2]]]
    length(intersect(a, b)) > 0L && !all(a %in% b) && !all(b %in% a)
  })
  t(pairs[, cross, drop = FALSE])
}

#' Is a graph 2-colourable?
#'
#' The crossing graph of a family is bipartite exactly when the family splits
#' into two laminar families, and the incidence matrix of two laminar families
#' is totally unimodular, so this flag is the cheap half of the TU verdict and
#' the one that survives above the brute force's size limit.
#'
#' @param n     Number of vertices.
#' @param edges Two-column matrix of edges.
#' @return TRUE when the graph has no odd cycle.
.sweep_bipartite <- function(n, edges) {
  if (nrow(edges) == 0L) return(TRUE)
  colour <- rep(NA_integer_, n)
  adj <- lapply(seq_len(n), function(v)
    c(edges[edges[, 1] == v, 2], edges[edges[, 2] == v, 1]))
  for (s in seq_len(n)) {
    if (!is.na(colour[s])) next
    colour[s] <- 0L
    queue <- s
    while (length(queue) > 0L) {
      v <- queue[1]; queue <- queue[-1]
      for (u in adj[[v]]) {
        if (is.na(colour[u])) {
          colour[u] <- 1L - colour[v]
          queue <- c(queue, u)
        } else if (colour[u] == colour[v]) return(FALSE)
      }
    }
  }
  TRUE
}

#' Does some leaf order make every block an interval?
#'
#' An interval matrix is totally unimodular, so a family with this property is
#' integral at every value vector however many crossings it has. Brute force
#' over the leaf orders, vectorised across them: a block is an interval of an
#' order exactly when the span of its members' positions equals its size.
#'
#' @param leaves Character vector of leaves, at most 8.
#' @param blocks Named list of leaf subsets.
#' @return TRUE when some order makes every block contiguous.
.sweep_interval_order <- function(leaves, blocks) {
  if (length(blocks) == 0L) return(TRUE)
  POS <- .sweep_perms(length(leaves))
  ok  <- rep(TRUE, nrow(POS))
  for (b in blocks) {
    idx <- match(b, leaves)
    if (length(idx) <= 1L) next
    cols <- lapply(idx, function(j) POS[, j])
    span <- do.call(pmax.int, cols) - do.call(pmin.int, cols) + 1L
    ok <- ok & (span == length(idx))
    if (!any(ok)) return(FALSE)
  }
  any(ok)
}

#' Brute-force total unimodularity of a 0/1 matrix.
#'
#' Every square submatrix's determinant has to be 0 or plus or minus 1. The
#' enumeration is exponential, so it is run only where the matrix is small and
#' reported as NA elsewhere; the bipartite-crossing-graph and interval-order
#' flags are the structural sufficient conditions that stay computable above
#' the limit.
#'
#' The limit is set above the widest matrix the sweep can build -- eight leaves
#' and six distinct internal blocks, fourteen rows plus columns -- so every
#' instance of the sweep gets a verdict rather than an NA. It is a limit and
#' not a formality: the enumeration is over all square submatrices of both
#' orders, and it stands down above the sweep's own widths rather than running
#' at whatever a caller hands it.
#'
#' @param M      A 0/1 matrix.
#' @param max_nm Largest rows-plus-columns the enumeration is run at.
#' @return TRUE, FALSE, or NA where the matrix is too large.
.sweep_tu <- function(M, max_nm = 20L) {
  nr <- nrow(M); nc <- ncol(M)
  if (nr == 0L || nc == 0L) return(TRUE)
  if (nr + nc > max_nm) return(NA)
  for (k in seq_len(min(nr, nc))) {
    for (ri in utils::combn(nr, k, simplify = FALSE)) {
      for (ci in utils::combn(nc, k, simplify = FALSE)) {
        d <- det(M[ri, ci, drop = FALSE])
        if (abs(abs(d) - round(abs(d))) > 1e-9 || round(abs(d)) > 1L) return(FALSE)
      }
    }
  }
  TRUE
}


# ---------------------------------------------------------------------------
# A family and its verdicts
# ---------------------------------------------------------------------------

#' A leaf-block family with its structural predicates computed once.
#'
#' The predicates do not move with the demand, the mix or the seed, so they are
#' computed at construction and carried on the instance rather than recomputed
#' per round.
#'
#' @param leaves          Character vector of leaves.
#' @param internal_blocks Named list of leaf subsets, one per internal node.
#' @param capacity        Named numeric vector of TOKEN capacities, covering
#'   every internal node and every leaf.
#' @return A list of `leaves`, `blocks` (internal blocks then leaf singletons),
#'   `internal`, `capacity`, `anc` (the leaf-by-node 0/1 matrix), and the
#'   predicates `crossing_count`, `crossing_graph_bipartite`, `interval_order`,
#'   `tu_verdict` and `stratum`.
sweep_family <- function(leaves, internal_blocks, capacity) {
  stopifnot("a block has to name leaves of this family" =
              all(unlist(internal_blocks) %in% leaves),
            "internal nodes need names distinct from the leaves" =
              length(intersect(names(internal_blocks), leaves)) == 0L)
  blocks <- c(internal_blocks, setNames(as.list(leaves), leaves))
  stopifnot("every node needs a capacity" = all(names(blocks) %in% names(capacity)))
  capacity <- capacity[names(blocks)]

  anc <- vapply(blocks, function(b) as.numeric(leaves %in% b),
                numeric(length(leaves)))
  anc <- matrix(anc, nrow = length(leaves),
                dimnames = list(leaves, names(blocks)))

  pairs     <- .sweep_crossing_pairs(internal_blocks)
  bipartite <- .sweep_bipartite(length(internal_blocks), pairs)
  interval  <- .sweep_interval_order(leaves, internal_blocks)
  distinct  <- internal_blocks[!duplicated(lapply(internal_blocks, sort))]
  tu <- .sweep_tu(anc[, names(distinct), drop = FALSE])

  n_cross <- nrow(pairs)
  stratum <- if (n_cross == 0L) "laminar"
    else if (n_cross == 1L) "one_crossing"
    else if (bipartite) "bilaminar_multi"
    else if (interval) "oddcycle_interval"
    else "oddcycle_nonInterval"

  list(leaves = leaves, blocks = blocks, internal = names(internal_blocks),
       capacity = capacity, anc = anc,
       n_leaves = length(leaves), n_internal = length(internal_blocks),
       crossing_count = as.integer(n_cross),
       crossing_graph_bipartite = bipartite,
       interval_order = interval,
       tu_verdict = tu,
       stratum = stratum)
}

#' The shipped instances as sweep families, at their own token capacities.
#'
#' So the sweep's rows on T, X and S can be read against the existence block's,
#' which ran the same certificate through the environment rather than through
#' the matrix.
#'
#' @param size "small" or "scale".
#' @return Named list of families.
sweep_substrate_families <- function(size = c("small", "scale")) {
  lapply(leaf_instance_specs(match.arg(size)), function(spec) {
    anc      <- ancestor_matrix(spec)
    leaves   <- rownames(anc)
    internal <- setdiff(colnames(anc), leaves)
    sweep_family(
      leaves = leaves,
      internal_blocks = setNames(
        lapply(internal, function(v) leaves[anc[, v] > 0]), internal),
      capacity = token_capacity(spec))
  })
}

#' The three-leaf triangle, at unit capacities.
#'
#' The smallest family that is not a union of two laminar families and admits
#' no interval order: three blocks, each a different pair of three leaves.
#' Every pair of blocks crosses, the crossing graph is a triangle, and the
#' relaxation of its packing is strictly fractional whenever the three best
#' tasks sit one per pair.
#'
#' @return A family.
sweep_triangle_family <- function() {
  sweep_family(
    leaves = c("a", "b", "c"),
    internal_blocks = list(ab = c("a", "b"), bc = c("b", "c"), ac = c("a", "c")),
    capacity = c(ab = 1, bc = 1, ac = 1, a = 1, b = 1, c = 1))
}

#' The same triangle, at the generator's own capacity rule.
#'
#' The unit-capacity triangle answers a question about the structure and
#' nothing else: at one token a node, three tasks one per block already
#' separate the relaxation from the optimum, so the row is fractional in every
#' round it is run. That is the worst case of the structure rather than the
#' behaviour of the structure, and reading it as the latter would attribute to
#' the crossing cycle what the tightness did.
#'
#' This is the same three blocks sized by `sweep_capacity()` -- a node at 0.6
#' of the demand expected under its own block, like every generated instance
#' and like the deployed row -- so the pair separates the two. The matrix is
#' the same one and is still not totally unimodular, so the theory still
#' certifies nothing about it; what the sweep then measures is whether the
#' relaxation is fractional anyway.
#'
#' @param n_agents      Agent population the capacities are sized at.
#' @param lambda        Arrivals per agent per round.
#' @param bind_fraction Share of its block's expected demand a node carries.
#' @param leaf_fraction The same for a leaf's own singleton block.
#' @return A family.
sweep_triangle_capacity_family <- function(n_agents = 90L, lambda = 1.5,
                                           bind_fraction = 0.6,
                                           leaf_fraction = 1.5) {
  leaves <- c("a", "b", "c")
  blocks <- list(ab = c("a", "b"), bc = c("b", "c"), ac = c("a", "c"))
  sweep_family(leaves, blocks,
               sweep_capacity(leaves, blocks, n_agents, lambda,
                              bind_fraction, leaf_fraction))
}


#' The deployed O-RAN pipeline templates as one named family.
#'
#' The companion deployment runs three eight-stage pipeline templates
#' concurrently over the same administrative domains, which is the sweep's
#' structure in another vocabulary: a domain is a capacity node, a template is
#' a leaf (a task is one execution of one template), and a template sits in a
#' domain's block exactly when it has a stage there. The three templates
#' compete for the same domains in the same window, so the domain's capacity is
#' shared across them, which is what makes the blocks overlap at all.
#'
#' The translation, and what it rests on:
#'
#'   - Five domain nodes, at the granularity the templates name their stages
#'     at: the distributed unit, the central unit, the near-real-time
#'     controller, the non-real-time controller and the management and
#'     orchestration plane. The deployment maps these onto four sites, the
#'     central unit and the near-real-time controller sharing one; the
#'     five-node reading is the finer of the two and the one the stage names
#'     support, and the coarser reading merges two blocks that are equal here
#'     anyway, so neither changes a predicate.
#'   - The chain template names all eight of its stages and crosses all five
#'     domains. The series-parallel template is four sources at the
#'     distributed and central units converging on four stages at the
#'     near-real-time controller; the entangled template fans out from a
#'     central-unit stage to two near-real-time controller detectors with a
#'     further central-unit stream and a distributed-unit-sourced stream. Both
#'     therefore occupy the first three domains and neither reaches the
#'     non-real-time controller or the management plane.
#'   - A stage-level variant is NOT built. Only the chain template's eight
#'     stages are individually named; the other two are described by their
#'     structure and their stage counts, so a stage-level family would be a
#'     guess at two thirds of its own leaves. The domain-level family is what
#'     the source specifies.
#'
#' Capacities follow the generated instances' rule exactly, so the named row is
#' read on the same scale as the strata.
#'
#' @param n_agents      Agent population the capacities are sized at.
#' @param lambda        Arrivals per agent per round.
#' @param bind_fraction Share of its block's expected demand a domain carries.
#' @param leaf_fraction The same for a template's own singleton block.
#' @return A family.
sweep_npubsub_family <- function(n_agents = 90L, lambda = 1.5,
                                 bind_fraction = 0.6, leaf_fraction = 1.5) {
  leaves <- c("cqi_chain", "anomaly_sp", "ran_entangled")
  blocks <- list(du = leaves, cu = leaves, near_rt_ric = leaves,
                 non_rt_ric = "cqi_chain", smo = "cqi_chain")
  sweep_family(leaves, blocks,
               sweep_capacity(leaves, blocks, n_agents, lambda,
                              bind_fraction, leaf_fraction))
}


# ---------------------------------------------------------------------------
# Demand: the substrate's own generator, in matrix coordinates
# ---------------------------------------------------------------------------

#' The leaf-share distribution a mix name selects, at any leaf count.
#'
#' node_leaf_shares() carries the substrate's own skewed vector, which is
#' written out for its four leaves; the sweep's families have three to eight.
#' This keeps that vector's SHAPE -- one dominant leaf at 0.55, one starved
#' leaf at 0.05, the remaining mass spread evenly -- and reproduces the
#' substrate's vector exactly at four leaves, which is asserted in the tests.
#'
#' @param leaf_mix "uniform" or "skewed".
#' @param leaves   Character vector of leaves, at least three.
#' @return Named numeric vector of shares, summing to one.
sweep_leaf_shares <- function(leaf_mix = c("uniform", "skewed"), leaves) {
  leaf_mix <- match.arg(leaf_mix)
  n <- length(leaves)
  stopifnot("the skewed shape needs at least three leaves" = n >= 3L)
  s <- switch(leaf_mix,
              uniform = rep(1 / n, n),
              skewed  = c(0.55, 0.05, rep(0.40 / (n - 2L), n - 2L)))
  setNames(s, leaves)
}

#' Token capacities that make the internal nodes the binding ones.
#'
#' A node's capacity is a fixed fraction of the demand expected under its own
#' block at high load, which is the rule the shipped specs follow by hand: the
#' fraction is below one so the internal nodes bind, and the leaves are held at
#' a multiple of their own expected demand so they do not. The shipped small
#' instance's ratios of capacity to leaf share are 6 at the tightest internal
#' node and 15 at a leaf, a factor of 2.5, which is where the leaf default
#' comes from.
#'
#' Capacities are set at UNIFORM shares and then held fixed across mixes: the
#' instance is the treatment and the mix is the demand drawn against it, so a
#' capacity that moved with the mix would confound the two.
#'
#' @param leaves          Character vector of leaves.
#' @param internal_blocks Named list of leaf subsets.
#' @param n_agents        Agent population.
#' @param lambda          Arrivals per agent per round.
#' @param bind_fraction   Share of its block's expected demand an internal node
#'   carries.
#' @param leaf_fraction   The same for a leaf's own singleton block.
#' @return Named numeric vector of token capacities.
sweep_capacity <- function(leaves, internal_blocks, n_agents = 90L,
                           lambda = 1.5, bind_fraction = 0.6,
                           leaf_fraction = 1.5) {
  shares <- sweep_leaf_shares("uniform", leaves)
  demand <- function(b) lambda * n_agents * sum(shares[b])
  c(setNames(vapply(internal_blocks,
                    function(b) max(1, round(bind_fraction * demand(b))),
                    numeric(1)), names(internal_blocks)),
    setNames(vapply(leaves, function(l) max(1, round(leaf_fraction * demand(l))),
                    numeric(1)), leaves))
}

#' The node the onset law expects to cross first, and how often.
#'
#' node_onset_law()'s statement, in the sweep's coordinates: a round's arrivals
#' are Poisson with mean lambda N, the arrivals destined for the leaves under
#' node v are a thinning at v's leaf share, so v's demand is Poisson with mean
#' s_v lambda N and it crosses when that exceeds its token capacity. The
#' spec-taking function cannot be called here because a generated family has no
#' dependency graph behind it; the tests assert the two agree on a family that
#' does.
#'
#' @param family   A family from sweep_family().
#' @param shares   Named numeric vector of leaf shares.
#' @param n_agents Agent population.
#' @param lambda   Arrivals per agent per round.
#' @return A list of `binding_node` and its `p_cross`.
sweep_onset_prediction <- function(family, shares, n_agents = 90L,
                                   lambda = 1.5) {
  anc <- family$anc
  s_v <- setNames(as.numeric(crossprod(anc, shares[rownames(anc)])),
                  colnames(anc))
  p <- 1 - stats::ppois(floor(family$capacity[colnames(anc)]),
                        s_v * lambda * n_agents)
  v <- names(which.max(p))
  list(binding_node = v, p_cross = p[[v]])
}


# ---------------------------------------------------------------------------
# The two instruments
# ---------------------------------------------------------------------------

#' The market's greedy packer, on a matrix rather than on an environment.
#'
#' The same rule as .greedy_pack_by(): descending rank, skip anything not
#' strictly positive or not finite, admit a task exactly when its whole recipe
#' fits in what is left. Ties break by row order, which is order()'s, so the
#' two admit the same set and not merely the same value. The fidelity test
#' packs a round of the shipped tree instance both ways and compares the task
#' identifiers.
#'
#' @param values     Per-task rank, the same quantity the packer sorts on.
#' @param A          Task-by-node 0/1 matrix, tasks in rows.
#' @param capacities Per-node token capacity, in A's column order.
#' @return Integer vector of admitted row indices, in admission order.
sweep_greedy_pack <- function(values, A, capacities) {
  if (length(values) == 0L) return(integer(0))
  remaining <- as.numeric(capacities)
  chosen    <- integer(0)
  for (i in order(values, decreasing = TRUE)) {
    if (!is.finite(values[i]) || values[i] <= 0) next
    need <- A[i, ]
    if (all(need <= remaining)) {
      remaining <- remaining - need
      chosen    <- c(chosen, i)
    }
  }
  chosen
}

#' One round of the sweep: the certificate, the exactness and the onset.
#'
#' Demand is drawn the way the substrate draws it: a Poisson count at lambda N
#' (the sum of the per-agent draws the round generator makes), a leaf per task
#' from the mix, a base value uniform on one to two, a deadline from the
#' shipped set, and the zero-queue true value through node_true_value() on a
#' synthetic base latency. That latency is the shipped small instance's own
#' zero-queue critical path, one physical delay per tier, so the share of a
#' task's value that survives its own pipeline is the one the decay rate is
#' calibrated at; every deadline exceeds it, so no draw is a deadline miss and
#' the value model contributes no variation beyond the base draw.
#'
#' Values are reserve-adjusted exactly as the existence certificate adjusts
#' them -- the zero-queue value less the common floor summed along the task's
#' path -- and tasks that cannot cover their own bundle at the floor are
#' dropped from both programs.
#'
#' @param family   A family from sweep_family().
#' @param shares   Named numeric vector of leaf shares.
#' @param n_agents Agent population.
#' @param lambda   Arrivals per agent per round.
#' @param reserve  The common per-node floor.
#' @param base_ms  Zero-queue critical path given to every leaf.
#' @param lambda_l Per-ms value-decay rate.
#' @param deadlines Integer vector of possible deadlines (ms).
#' @param onset    The prediction from sweep_onset_prediction().
#' @return A one-row tibble. `n_admit_greedy` and `n_admit_optimum` count the
#'   tasks each program admitted, the second read off the binary program's own
#'   0/1 solution rather than inferred from its value.
sweep_round <- function(family, shares, n_agents, lambda, reserve, base_ms,
                        lambda_l, deadlines, onset) {
  leaves <- family$leaves
  n      <- stats::rpois(1L, lambda * n_agents)
  if (n == 0L) {
    return(tibble(n_tasks = 0L, n_positive = 0L, lp_value = 0, ip_value = 0,
                  gap = 0, integral = TRUE, relative_gap = 0,
                  greedy_value = 0, exactness = 1,
                  binding_node = onset$binding_node,
                  p_cross_binding = onset$p_cross, binding_crossed = FALSE,
                  n_admit_greedy = 0L, n_admit_optimum = 0L))
  }
  tasks <- tibble(recipe     = sample(leaves, n, replace = TRUE, prob = shares),
                  value_base = stats::runif(n, 1, 2),
                  deadline   = sample(deadlines, n, replace = TRUE))
  v_true <- node_true_value(tasks, setNames(rep(base_ms, length(leaves)), leaves),
                            lambda_l)

  A    <- family$anc[tasks$recipe, , drop = FALSE]
  cap  <- family$capacity[colnames(A)]
  adj  <- v_true - reserve * rowSums(A)
  keep <- which(adj > 0)
  Ak   <- A[keep, , drop = FALSE]

  g <- node_lp_ip_gap(adj[keep], Ak, cap)
  greedy       <- sweep_greedy_pack(adj[keep], Ak, cap)
  greedy_value <- sum(adj[keep][greedy])

  arrivals <- colSums(A)
  tibble(n_tasks = n, n_positive = length(keep),
         lp_value = g$lp_value, ip_value = g$ip_value, gap = g$gap,
         integral = g$integral,
         relative_gap = if (g$ip_value > 0) g$gap / g$ip_value else 0,
         greedy_value = greedy_value,
         exactness = if (g$ip_value > 0) greedy_value / g$ip_value else 1,
         binding_node = onset$binding_node,
         p_cross_binding = onset$p_cross,
         binding_crossed = arrivals[[onset$binding_node]] >
           cap[[onset$binding_node]],
         # How many tasks each program served, beside how much value it got:
         # a ratio of values says how far the greedy fell short, and these say
         # over how many admissions, which is what a displacement is counted in.
         n_admit_greedy  = length(greedy),
         n_admit_optimum = sum(g$solution > 0.5))
}

#' The sweep over one instance and one mix.
#'
#' Off the market, like the existence block: no price process, no execution and
#' no trust update, because the question is what the offered demand admits.
#' Seeds are looped inside the branch rather than branched over, so the branch
#' count stays in the hundreds rather than the thousands.
#'
#' @param family   A family from sweep_family().
#' @param instance The instance's name, carried onto every row.
#' @param stratum  The instance's stratum, carried onto every row.
#' @param leaf_mix "uniform" or "skewed".
#' @param seeds    Monte Carlo seeds.
#' @param n_rounds Rounds per seed.
#' @param n_agents Agent population.
#' @param lambda   Arrivals per agent per round; 1.5 is the high-load rate.
#' @param reserve  The common per-node floor.
#' @param base_ms  Zero-queue critical path given to every leaf.
#' @param lambda_l Per-ms value-decay rate.
#' @param deadlines Integer vector of possible deadlines (ms).
#' @return One row per seed and round.
sweep_run <- function(family, instance, stratum, leaf_mix, seeds = 1:10,
                      n_rounds = 20L, n_agents = 90L, lambda = 1.5,
                      reserve = 0.04, base_ms = sum(phys_base_ms()),
                      lambda_l = node_lambda_l(),
                      deadlines = c(500L, 750L, 1000L)) {
  shares <- sweep_leaf_shares(leaf_mix, family$leaves)
  onset  <- sweep_onset_prediction(family, shares, n_agents, lambda)
  rows <- vector("list", length(seeds) * n_rounds)
  i <- 0L
  for (s in seeds) {
    for (t in seq_len(n_rounds)) {
      set.seed(s * 1009L + t)
      i <- i + 1L
      rows[[i]] <- bind_cols(
        tibble(instance = instance, stratum = stratum, leaf_mix = leaf_mix,
               seed = as.integer(s), round = t),
        sweep_round(family, shares, n_agents, lambda, reserve, base_ms,
                    lambda_l, deadlines, onset))
    }
  }
  bind_rows(rows)
}


# ---------------------------------------------------------------------------
# The generator
# ---------------------------------------------------------------------------

#' One random leaf-block family.
#'
#' Blocks are drawn at sizes two and up, because a singleton block duplicates a
#' leaf's own and can cross nothing; blocks are distinct and their union covers
#' every leaf, so no leaf sits outside the structure being varied. A draw that
#' fails either condition is discarded rather than repaired, which keeps the
#' distribution over families the uniform one the discard leaves.
#'
#' @param n_leaves      Leaves, three to eight.
#' @param n_internal    Internal nodes, two to six.
#' @param bind_fraction Share of its block's expected demand an internal node
#'   carries.
#' @param leaf_fraction The same for a leaf's own singleton block.
#' @param n_agents      Agent population the capacities are sized at.
#' @param lambda        Arrivals per agent per round.
#' @param tries         Draws before giving up.
#' @return A family, or NULL where no draw met the conditions.
sweep_generate <- function(n_leaves, n_internal, bind_fraction = 0.6,
                           leaf_fraction = 1.5, n_agents = 90L, lambda = 1.5,
                           tries = 20L) {
  leaves <- paste0("l", seq_len(n_leaves))
  for (i in seq_len(tries)) {
    blocks <- lapply(seq_len(n_internal), function(j)
      sort(sample(leaves, sample(seq(2L, n_leaves), 1L))))
    if (anyDuplicated(blocks)) next
    if (!setequal(unlist(blocks), leaves)) next
    names(blocks) <- paste0("v", seq_len(n_internal))
    return(sweep_family(leaves, blocks,
                        sweep_capacity(leaves, blocks, n_agents, lambda,
                                       bind_fraction, leaf_fraction)))
  }
  NULL
}

#' The instance table the sweep branches over.
#'
#' The hand-built families go in first -- the three-leaf triangle at unit
#' capacities and at the generator's own rule, and the shipped T, X and S at
#' their own -- so the sweep's rows on them can be read against the theory and
#' against the existence block. The shipped families are taken at the SCALE
#' size, which is the one the pipeline runs: the small specs are the unit
#' tests' six-node instance, and a substrate row drawn from them would be a
#' statement about a fixture rather than about the instances every other number
#' in the study is measured on.
#' Random families then fill each stratum to `n_per_stratum` and no further: a
#' stratum that is already full discards its draws, which keeps the block's
#' cost at five times the quota rather than at whatever the draw distribution
#' happens to favour.
#'
#' @param n_per_stratum Instances wanted in each stratum.
#' @param seed          Random seed for the draws.
#' @param max_draws     Draws before the generator gives up.
#' @param n_agents      Agent population the capacities are sized at.
#' @param lambda        Arrivals per agent per round.
#' @param bind_fraction Share of its block's expected demand an internal node
#'   carries.
#' @param leaf_fraction The same for a leaf's own singleton block.
#' @return A tibble with one row per instance, carrying the family in a list
#'   column beside its predicates.
sweep_instances <- function(n_per_stratum = 20L, seed = 1L, max_draws = 8000L,
                            n_agents = 90L, lambda = 1.5,
                            bind_fraction = 0.6, leaf_fraction = 1.5) {
  strata <- c("laminar", "one_crossing", "bilaminar_multi",
              "oddcycle_interval", "oddcycle_nonInterval")
  shipped <- sweep_substrate_families("scale")
  named   <- c(list(triangle = sweep_triangle_family()),
               setNames(shipped, paste0("substrate_", names(shipped))),
               list(npubsub_domains = sweep_npubsub_family(
                 n_agents, lambda, bind_fraction, leaf_fraction)),
               list(triangle_capacity = sweep_triangle_capacity_family(
                 n_agents, lambda, bind_fraction, leaf_fraction)))

  fams  <- named
  count <- table(factor(vapply(named, function(f) f$stratum, character(1)),
                        levels = strata))

  set.seed(seed)
  draws <- 0L
  while (any(count < n_per_stratum) && draws < max_draws) {
    draws <- draws + 1L
    f <- sweep_generate(sample(3:8, 1L), sample(2:6, 1L), bind_fraction,
                        leaf_fraction, n_agents, lambda)
    if (is.null(f) || count[[f$stratum]] >= n_per_stratum) next
    count[[f$stratum]] <- count[[f$stratum]] + 1L
    fams[[sprintf("gen_%03d", length(fams))]] <- f
  }
  if (any(count < n_per_stratum)) {
    message("sweep_instances: strata short of the quota after ", draws,
            " draws: ",
            paste(sprintf("%s %d", strata, as.integer(count)), collapse = ", "))
  }

  field <- function(what, mode) unname(vapply(fams, function(x) x[[what]], mode))
  tibble(instance   = unname(names(fams)),
         n_leaves   = field("n_leaves", integer(1)),
         n_internal = field("n_internal", integer(1)),
         crossing_count = field("crossing_count", integer(1)),
         crossing_graph_bipartite = field("crossing_graph_bipartite",
                                          logical(1)),
         interval_order = field("interval_order", logical(1)),
         tu_verdict = unname(vapply(fams, function(x) as.logical(x$tu_verdict),
                                    logical(1))),
         stratum = field("stratum", character(1)),
         family  = unname(fams))
}

#' The grid the sweep branches over: one branch per instance and mix.
#'
#' @param instances The instance table.
#' @param mixes     Leaf mixes.
#' @return A tibble with one row per branch.
sweep_grid <- function(instances, mixes = c("uniform", "skewed")) {
  bind_rows(lapply(mixes, function(m) mutate(instances, leaf_mix = m)))
}


# ---------------------------------------------------------------------------
# Summaries
# ---------------------------------------------------------------------------

#' The share of the optimum's admissions the greedy did not make.
#'
#' A round that admitted nobody displaced nobody, so its share is zero rather
#' than the nothing-over-nothing the division would give.
#'
#' @param n_optimum,n_greedy Admission counts from the same round.
#' @return Numeric vector on zero to one.
.sweep_displaced <- function(n_optimum, n_greedy) {
  ifelse(n_optimum > 0L, (n_optimum - n_greedy) / pmax(n_optimum, 1L), 0)
}

#' The sweep's reading, per stratum and for each named instance.
#'
#' The named instances get their own rows beside the strata because the
#' question they answer is about them and not about a population: each is a
#' family built by hand for a reason, at a tightness chosen by hand with it.
#'
#' A stratum's row is therefore a statement about the GENERATED families of
#' that stratum and about nothing else, and `named_excluded` is what makes it
#' one. Both triangles are in that list, and they are the reason it exists: the
#' unit-capacity triangle is fractional in every round it runs, so leaving it
#' inside `oddcycle_nonInterval` would put a hand-built worst case into the
#' mean that stratum reports and read the tightness as the structure. The
#' capacity triangle is its control and belongs beside it rather than in the
#' population either. The shipped and deployed rows are out for the same
#' reason and not for a different one -- a family somebody built is not a draw
#' from the distribution the generator samples.
#'
#' @param rows  Per-round rows from sweep_run().
#' @param named Instances reported on their own beside the strata.
#' @param named_excluded Instances the stratum rows leave out. The whole named
#'   set by default, so every stratum row is a generated-family mean.
#' @return One row per stratum and per named instance.
sweep_summary <- function(rows, named = c("triangle", "substrate_T",
                                          "substrate_X", "substrate_S",
                                          "npubsub_domains",
                                          "triangle_capacity"),
                          named_excluded = named) {
  measure <- function(d, label) {
    d %>% summarise(
      n_instances = dplyr::n_distinct(instance),
      n_rounds    = dplyr::n(),
      fraction_positive_gap = mean(!integral),
      mean_relative_gap = mean(relative_gap),
      max_relative_gap  = max(relative_gap),
      mean_exactness    = mean(exactness),
      worst_exactness   = min(exactness),
      onset_error = abs(mean(binding_crossed) - mean(p_cross_binding)),
      mean_admitted = mean(n_admit_optimum),
      displaced_over_admitted = mean(.sweep_displaced(n_admit_optimum,
                                                      n_admit_greedy)),
      .groups = "drop") %>%
      mutate(group = label, .before = 1)
  }
  by_stratum <- rows %>% filter(!instance %in% named_excluded) %>%
    group_by(stratum, leaf_mix) %>% measure("stratum") %>%
    rename(label = stratum)
  by_named <- rows %>% filter(instance %in% named) %>%
    group_by(instance, leaf_mix) %>% measure("instance") %>%
    rename(label = instance)
  bind_rows(by_stratum, by_named)
}

#' Every instance's predicates beside what the sweep measured on it.
#'
#' `mean_admitted` and `displaced_over_admitted` are appended after the columns
#' the table already reported: how many tasks the optimum served, and what
#' share of them the greedy did not, which is the exactness ratio counted in
#' admissions rather than in value.
#'
#' @param rows      Per-round rows from sweep_run().
#' @param instances The instance table.
#' @return One row per instance and mix.
sweep_by_instance <- function(rows, instances) {
  rows %>%
    group_by(instance, stratum, leaf_mix) %>%
    summarise(n_rounds = dplyr::n(),
              fraction_positive_gap = mean(!integral),
              mean_relative_gap = mean(relative_gap),
              max_relative_gap  = max(relative_gap),
              mean_exactness    = mean(exactness),
              worst_exactness   = min(exactness),
              onset_error = abs(mean(binding_crossed) - mean(p_cross_binding)),
              mean_admitted = mean(n_admit_optimum),
              displaced_over_admitted = mean(.sweep_displaced(n_admit_optimum,
                                                              n_admit_greedy)),
              .groups = "drop") %>%
    left_join(select(instances, instance, n_leaves, n_internal, crossing_count,
                     crossing_graph_bipartite, interval_order, tu_verdict),
              by = "instance") %>%
    relocate(n_leaves, n_internal, crossing_count, crossing_graph_bipartite,
             interval_order, tu_verdict, .after = stratum)
}
