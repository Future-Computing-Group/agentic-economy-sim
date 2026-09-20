# ===========================================================================
# Node-level dependency instances and their leaf-block structure
# ===========================================================================
#
# The tier-indexed environments carry one capacity row per physical tier, so a
# DAG's internal structure reaches the market only through a per-tier demand
# weight. This module carries the structure instead: resources are the service
# NODES of a dependency graph, a task is a throughput token at one leaf, and a
# node's constraint is the total of the tokens destined for the leaves it can
# reach.
#
# Nothing here knows about an experiment, an arm or a metric. It is data (the
# instances), three set functions over leaf subsets (the leaf-block rank, its
# polymatroid certificate, the node-split max-flow bound) and the two adapters
# that hand a node-indexed environment to the market kernel unchanged.

#' Per-tier base processing delay, in ms, resolved by a node's physical tier.
#'
#' The model's own 5 / 15 / 50, which the tier-indexed environments carry. A
#' node inherits the delay of the tier it physically sits in, so a device, edge
#' or cloud node costs the same wherever the arcs put it.
#'
#' @return Named numeric vector, named by physical tier.
phys_base_ms <- function() c(device = 5, edge = 15, cloud = 50)

#' The instance specs: node tiers, capacities, arcs and the token weight.
#'
#' Data, not code. Two sizes, each a named list of three instances on ONE node
#' set and ONE capacity vector, differing only in which leaves each internal
#' node reaches: `T` a rooted tree, `X` the tree plus one crossing arc, `S`
#' every internal node feeding every leaf. Arc counts rise T, X, S while
#' laminarity of the leaf-block family goes yes, no, yes, so arc density is not
#' what decides the structure.
#'
#' `capacity` is in capacity units, the unit the environment carries;
#' `weight` is the capacity units one throughput token consumes at a node.
#' `token_capacity()` converts, and the rank functions are stated in tokens.
#'
#' @param size "small" (six nodes, three leaves, unit tokens) or "scale"
#'   (eight nodes, four leaves, weight-2 tokens, per-tier capacity totals of
#'   200 / 300 / 500).
#' @return Named list of specs, each a list of `nodes`, `edges`, `weight`.
leaf_instance_specs <- function(size = c("small", "scale")) {
  size <- match.arg(size)
  arc  <- function(...) {
    v <- c(...)
    tibble(from = v[c(TRUE, FALSE)], to = v[c(FALSE, TRUE)])
  }

  if (size == "small") {
    nodes <- tibble(
      node     = c("d", "e1", "e2", "l1", "l2", "l3"),
      phys     = c("device", "edge", "edge", "cloud", "cloud", "cloud"),
      capacity = c(9, 4, 4, 5, 5, 5)
    )
    specs <- list(
      T = arc("d", "e1", "d", "e2",
              "e1", "l1", "e1", "l2",
              "e2", "l3"),
      X = arc("d", "e1", "d", "e2",
              "e1", "l1", "e1", "l2",
              "e2", "l2", "e2", "l3"),
      S = arc("d", "e1", "d", "e2",
              "e1", "l1", "e1", "l2", "e1", "l3",
              "e2", "l1", "e2", "l2", "e2", "l3")
    )
    weight <- 1
  } else {
    # The leaf capacities exceed the edge capacity that feeds them, so a
    # single-parent leaf can absorb more than one internal node's export and an
    # over-committed interface is visible rather than masked by a leaf cap.
    nodes <- tibble(
      node     = c("d", "e1", "e2", "e3", "l1", "l2", "l3", "l4"),
      phys     = c("device", "edge", "edge", "edge",
                   "cloud", "cloud", "cloud", "cloud"),
      capacity = c(200, 100, 100, 100, 150, 100, 150, 100)
    )
    specs <- list(
      T = arc("d", "e1", "d", "e2", "d", "e3",
              "e1", "l1", "e1", "l2",
              "e2", "l3",
              "e3", "l4"),
      X = arc("d", "e1", "d", "e2", "d", "e3",
              "e1", "l1", "e1", "l2",
              "e2", "l2", "e2", "l3",
              "e3", "l4"),
      S = arc("d", "e1", "d", "e2", "d", "e3",
              "e1", "l1", "e1", "l2", "e1", "l3", "e1", "l4",
              "e2", "l1", "e2", "l2", "e2", "l3", "e2", "l4",
              "e3", "l1", "e3", "l2", "e3", "l3", "e3", "l4")
    )
    weight <- 2
  }

  lapply(specs, function(e) list(nodes = nodes, edges = e, weight = weight))
}

#' The capacity units one throughput token consumes at each node.
#'
#' `spec$weight` is one number on the synthetic instances, where a token is a
#' token wherever it goes. A MEASURED instance carries one weight per node,
#' because a recorded stage's token count is its own: this is what reconciles
#' the two, so every caller reads a vector named by node and none of them has
#' to know which kind of spec it was handed.
#'
#' @param spec  An instance spec.
#' @param nodes Nodes to report, in the order wanted.
#' @return Named numeric vector of per-node token weights.
token_weight_of <- function(spec, nodes = spec$nodes$node) {
  w <- spec$weight
  if (length(w) == 1L) {
    return(setNames(rep(as.numeric(w), length(nodes)), nodes))
  }
  setNames(as.numeric(w[nodes]), nodes)
}

#' Node capacities in throughput tokens.
#'
#' @param spec An instance spec.
#' @return Named numeric vector of token capacities, named by node.
token_capacity <- function(spec) {
  setNames(spec$nodes$capacity / token_weight_of(spec), spec$nodes$node)
}

#' Per-node base processing delay, in ms, named by node.
#'
#' A spec whose nodes carry their own `base_ms` column is a MEASURED instance
#' and its delays are the recording's; the synthetic instances resolve theirs
#' from the physical tier a node sits in.
#'
#' @param spec An instance spec.
#' @return Named numeric vector, named by node.
leaf_base_ms <- function(spec) {
  if ("base_ms" %in% names(spec$nodes)) {
    return(setNames(as.numeric(spec$nodes$base_ms), spec$nodes$node))
  }
  setNames(unname(phys_base_ms()[spec$nodes$phys]), spec$nodes$node)
}

#' The mix-average per-task demand at each node, under uniform leaf shares.
#'
#' One token weighs `weight` at each of its leaf's ancestors, and a task's leaf
#' is uniform over the leaves, so a node's expected load per task is the weight
#' times the share of leaves it reaches. This is the environment's bundle: the
#' basket the agent-facing unit cost is computed on and the fallback recipe for
#' a task that carries no label.
#'
#' @param spec An instance spec.
#' @return A tibble with columns `tier` (the node) and `demand_weight`.
leaf_demand_weights <- function(spec) {
  anc <- ancestor_matrix(spec)
  tibble(tier = colnames(anc),
         demand_weight = unname(token_weight_of(spec, colnames(anc)) *
                                  colMeans(anc)))
}

#' A graph in build_dependency_graph's shape, with resources indexed by NODE.
#'
#' Setting `tier` equal to `node` is what makes every keyed function of the
#' market kernel node-indexed without touching any of them: they join on
#' whatever labels the environment carries. `phys` travels alongside so a
#' caller can still roll a per-node quantity up by physical tier.
#'
#' @param spec           An instance spec.
#' @param demand_weights A tibble of `tier` and `demand_weight`; defaults to
#'                       the instance's mix-average bundle.
#' @return A list of `nodes`, `edges`, `demand_weights`.
build_leaf_graph <- function(spec, demand_weights = leaf_demand_weights(spec)) {
  list(nodes = tibble(node = spec$nodes$node,
                      tier = spec$nodes$node,
                      phys = spec$nodes$phys),
       edges = spec$edges,
       demand_weights = demand_weights)
}

#' The leaves: service nodes with no outgoing arc to another service node.
#'
#' @param spec An instance spec.
#' @return Character vector of leaf nodes, in the spec's node order.
leaf_set <- function(spec) {
  setdiff(spec$nodes$node, spec$edges$from)
}

#' The |L| x |V| 0/1 ancestor-indicator matrix.
#'
#' Entry (l, v) is 1 when a token destined for leaf `l` passes through node
#' `v`, so column `v` is the leaf block of `v` and row `l` is the resource
#' recipe of one token at `l`. A leaf is its own ancestor.
#'
#' @param spec An instance spec.
#' @return A numeric matrix, rows named by leaf, columns by node.
ancestor_matrix <- function(spec) {
  V <- spec$nodes$node
  L <- leaf_set(spec)
  reach <- function(v) {                    # nodes reachable from v, v included
    seen <- v
    repeat {
      nxt <- union(seen, spec$edges$to[spec$edges$from %in% seen])
      if (setequal(nxt, seen)) return(seen)
      seen <- nxt
    }
  }
  m <- vapply(V, function(v) as.numeric(L %in% reach(v)), numeric(length(L)))
  matrix(m, nrow = length(L), dimnames = list(L, V))
}

#' Is the leaf-block family laminar: every pair nested or disjoint?
#'
#' A sufficient condition for the leaf-block region to be a polymatroid, and a
#' cheap pre-check rather than the verdict: a crossing family can still induce
#' one, which is why `polymatroid_certificate()` exists and why both flags are
#' reported per instance.
#'
#' @param anc An ancestor-indicator matrix.
#' @return TRUE when every pair of leaf blocks is nested or disjoint.
leaf_blocks_laminar <- function(anc) {
  blocks <- apply(anc, 2, function(col) which(col > 0), simplify = FALSE)
  all(utils::combn(length(blocks), 2, function(p) {
    a <- blocks[[p[1]]]
    b <- blocks[[p[2]]]
    length(intersect(a, b)) == 0 || all(a %in% b) || all(b %in% a)
  }))
}

# ---------------------------------------------------------------------------
# Leaf subsets, addressed by name
# ---------------------------------------------------------------------------
#
# Both set functions below return one value per leaf subset. Two enumerations
# of the subsets are in common use -- by bitmask and by cardinality -- and they
# agree on the first entries and then diverge, so a vector addressed by
# position can be read against the wrong subset without anything failing.
# Every entry is therefore named by its subset: "{}", "l1", "l1,l3".

#' Every subset of the leaves, in cardinality order, smallest first.
#'
#' @param L Character vector of leaves, in the order names are built in.
#' @return List of character vectors, starting with the empty subset.
leaf_subsets <- function(L) {
  c(list(character(0)),
    unlist(lapply(seq_along(L), function(k) utils::combn(L, k, simplify = FALSE)),
           recursive = FALSE))
}

#' The name of one leaf subset.
#'
#' @param s A character vector of leaves.
#' @param L The ground set, giving the canonical order.
#' @return "{}" for the empty subset, else the leaves comma-separated.
subset_name <- function(s, L = s) {
  if (length(s) == 0L) "{}" else paste(L[L %in% s], collapse = ",")
}

#' Minimal node sets whose leaf blocks together cover a leaf subset.
#'
#' Enumerated over node subsets, like the cut enumeration of `flow_rank`.
#' Used only as the upper bound of the rank search: if the nodes of `K` cover
#' `S` then the tokens of `S` all pass through `K`, so their total is at most
#' `K`'s total capacity. Sets that contain another cover are dropped, since
#' their capacity total can only be larger.
#'
#' @param L      Character vector of leaves.
#' @param blocks List of leaf blocks, one per node, in node order.
#' @return Named list, one entry per subset name, each a list of node-index
#'   vectors.
.minimal_covers <- function(L, blocks) {
  masks <- vapply(blocks, function(b) sum(bitwShiftL(1L, which(L %in% b) - 1L)),
                  integer(1))
  sets  <- lapply(0:(2^length(masks) - 1L),
                  function(m) which(bitwAnd(m, bitwShiftL(1L, seq_along(masks) - 1L)) > 0))
  cover <- vapply(sets, function(k) Reduce(bitwOr, masks[k], 0L), integer(1))
  ord   <- order(lengths(sets))          # smallest first, so minimality is one pass
  sets  <- sets[ord]
  cover <- cover[ord]

  out <- lapply(leaf_subsets(L), function(s) {
    want <- sum(bitwShiftL(1L, which(L %in% s) - 1L))
    cand <- sets[bitwAnd(cover, want) == want]
    keep <- list()
    for (k in cand) {
      if (!any(vapply(keep, function(q) all(q %in% k), logical(1)))) {
        keep <- c(keep, list(k))
      }
    }
    keep
  })
  setNames(out, vapply(leaf_subsets(L), subset_name, character(1), L = L))
}

#' Leaf-block rank: the largest total token count the node capacities support
#' on one leaf subset alone.
#'
#' rank(S) = max { sum_{l in S} x_l : x >= 0 integral, x(L_v) <= C_v for every
#' node v, x_l = 0 for l outside S }. This is the region the market clears
#' over, and whether it is a polymatroid is what decides greedy exactness.
#'
#' The search fixes one leaf at a time, largest value first, and prunes on the
#' cover bound of `.minimal_covers`. It is exact for any 0/1 ancestor matrix,
#' including a crossing family, where the min-over-antichain-covers closed form
#' is not available.
#'
#' # ponytail: exhaustive search under one bound rather than a grid over the
#' # capacity box, which is what makes the cost follow the leaf count and the
#' # arc pattern instead of the capacities: a box enumeration is 101^4 points
#' # at the larger instances. Where the bound is loose on every branch the
#' # upgrade is a linear program over the same constraints, never a new
#' # dependency: the constraint matrix is 0/1 and integral on these families.
#'
#' @param anc An ancestor-indicator matrix.
#' @param C   Named numeric vector of TOKEN capacities, named by node.
#' @return Named numeric vector, one entry per leaf subset, named by subset.
leaf_rank <- function(anc, C) {
  L <- rownames(anc)
  C <- C[colnames(anc)]
  stopifnot("every node needs a capacity" = !anyNA(C))

  covers <- .minimal_covers(L, lapply(colnames(anc), function(v) L[anc[, v] > 0]))
  bound  <- function(s, resid) {
    if (length(s) == 0L) return(0)
    min(vapply(covers[[subset_name(s, L)]], function(k) sum(resid[k]), numeric(1)))
  }
  best_total <- function(s, resid) {
    ub <- bound(s, resid)
    if (ub <= 0) return(0)
    a    <- anc[s[1], ]
    rest <- s[-1]
    best <- 0
    for (k in floor(min(resid[a > 0])):0) {   # tokens are integral
      left <- resid - k * a
      if (k + bound(rest, left) <= best) next
      best <- max(best, k + best_total(rest, left))
      if (best >= ub) break
    }
    best
  }

  subs <- leaf_subsets(L)
  setNames(vapply(subs, function(s) if (length(s) == 0L) 0 else best_total(s, C),
                  numeric(1)),
           vapply(subs, subset_name, character(1), L = L))
}

#' The smallest set of leaves whose deletion leaves the leaf-block family
#' laminar.
#'
#' Empty on a family that is already laminar. Fixing these leaves' token counts
#' leaves a laminar family on the survivors, hence a polymatroid, and that is
#' what makes the outer scan of `lb_optimum` exact.
#'
#' # ponytail: ascending subset search over the leaves, 15 subsets at four
#' # leaves. Above about ten leaves this wants a real crossing-pair analysis;
#' # these instances do not have ten leaves.
#'
#' @param anc An ancestor-indicator matrix.
#' @return Character vector of leaves, possibly empty.
crossing_leaves <- function(anc) {
  L <- rownames(anc)
  for (k in 0:length(L)) {
    for (s in utils::combn(L, k, simplify = FALSE)) {
      if (leaf_blocks_laminar(anc[setdiff(L, s), , drop = FALSE])) return(s)
    }
  }
  L
}

#' The tightest node capacity on a leaf's own path, in tokens.
#'
#' @param l   A leaf.
#' @param anc An ancestor-indicator matrix.
#' @param C   Named numeric vector of token capacities, in `anc`'s column order.
#' @return Scalar upper bound on that leaf's token count.
.leaf_cap <- function(l, anc, C) min(C[anc[l, ] > 0])

#' Greedy over a pre-ranked pool of tokens against residual node capacities.
#'
#' The pool is already in descending value order and `P` carries its rows of
#' the ancestor matrix, so the walk is one capacity test per token. On a
#' laminar family this is Edmonds' greedy and it is the exact maximiser of a
#' separable concave objective; `lb_optimum` is what makes the family laminar
#' before calling it.
#'
#' @param pool Descending numeric vector of token marginal values.
#' @param P    Matrix of ancestor-indicator rows, one row per pool entry.
#' @param remaining Named numeric vector of residual node capacities.
#' @return Total value admitted.
.laminar_greedy <- function(pool, P, remaining) {
  total <- 0
  for (i in seq_along(pool)) {
    need <- P[i, ]
    if (all(need <= remaining)) {
      remaining <- remaining - need
      total     <- total + pool[[i]]
    }
  }
  total
}

#' Exact optimum of a separable concave objective over the leaf-block region.
#'
#' Under unit demand an allocation is described by the leaf token counts and
#' the objective is separable and concave in them: at leaf j take the n_j
#' highest-valued tokens. Fixing the counts of the crossing leaves leaves a
#' laminar family, hence a polymatroid, on which greedy over the merged
#' marginals is exact; the outer scan is exhaustive over the fixed
#' coordinates, so the maximum over the scan is the optimum.
#'
#' The free-leaf pool is built and sorted ONCE rather than per outer step: the
#' crossing set does not move inside the scan, so neither does the pool.
#'
#' # ponytail: one capacity test per pooled token per outer step, 51 steps at
#' # the evaluation capacities. If that bites, rank the pool once and select by
#' # rank rather than re-walking it.
#'
#' @param marg Named list, one entry per leaf, each a DESCENDING vector of the
#'   marginal values of successive tokens at that leaf.
#' @param anc  An ancestor-indicator matrix.
#' @param C    Named numeric vector of TOKEN capacities, named by node.
#' @return Scalar optimum.
lb_optimum <- function(marg, anc, C) {
  L     <- rownames(anc)
  Cv    <- C[colnames(anc)]
  cross <- crossing_leaves(anc)
  free  <- setdiff(L, cross)

  pool <- unlist(lapply(free, function(l) {
    v <- marg[[l]]
    v <- v[is.finite(v) & v > 0]
    setNames(v, rep(l, length(v)))
  }))
  if (is.null(pool)) pool <- numeric(0)
  pool <- sort(pool, decreasing = TRUE)
  P    <- anc[names(pool), , drop = FALSE]

  if (length(cross) == 0L) return(.laminar_greedy(pool, P, Cv))

  cs   <- lapply(cross, function(l) c(0, cumsum(marg[[l]])))
  ks   <- lapply(seq_along(cross), function(i)
    0:min(length(marg[[cross[i]]]), .leaf_cap(cross[i], anc, Cv)))
  grid <- as.matrix(expand.grid(ks))

  best <- -Inf
  for (r in seq_len(nrow(grid))) {
    k   <- grid[r, ]
    rem <- Cv
    val <- 0
    for (i in seq_along(cross)) {
      if (k[i] > 0) {
        rem <- rem - k[i] * anc[cross[i], ]
        val <- val + cs[[i]][k[i] + 1L]
      }
    }
    if (any(rem < 0)) next
    v <- val + .laminar_greedy(pool, P, rem)
    if (v > best) best <- v
  }
  best
}

#' Leaf-block rank at scale, through the scan rather than the enumeration.
#'
#' rank(S) is the largest total token count supportable on S alone, which is
#' `lb_optimum` at unit marginals capped by each leaf's own tightest node. It
#' is the independent cross-check on `leaf_rank`: one enumeration under a cover
#' bound and one scan over the crossing coordinate, in two coordinate systems.
#'
#' @param anc An ancestor-indicator matrix.
#' @param C   Named numeric vector of TOKEN capacities, named by node.
#' @return Named numeric vector, one entry per leaf subset, named by subset.
lb_optimum_rank <- function(anc, C) {
  L    <- rownames(anc)
  Cv   <- C[colnames(anc)]
  subs <- leaf_subsets(L)
  setNames(
    vapply(subs, function(s) {
      if (length(s) == 0L) return(0)
      marg <- setNames(lapply(L, function(l)
        if (l %in% s) rep(1, .leaf_cap(l, anc, Cv)) else numeric(0)), L)
      lb_optimum(marg, anc, Cv)
    }, numeric(1)),
    vapply(subs, subset_name, character(1), L = L))
}

#' Is a rank function a polymatroid: normalised, monotone, submodular?
#'
#' Pairwise submodularity -- adding two elements one at a time is never worse
#' than adding both -- is equivalent to the full condition for a set function,
#' so the check is over subsets times leaf pairs rather than over subset pairs.
#'
#' A rank entry that is not a number gives NA rather than a verdict: a
#' certificate is reported per instance, and "not decided" is not "decided
#' against".
#'
#' @param r      A rank vector named by subset, from `leaf_rank` or `flow_rank`.
#' @param leaves The ground set; read off `r`'s names by default.
#' @return Named logical vector: normalised, monotone, submodular.
polymatroid_certificate <- function(r, leaves = NULL) {
  L <- leaves %||% unique(unlist(strsplit(setdiff(names(r), "{}"), ",", fixed = TRUE)))
  at <- function(s) r[[subset_name(s, L)]]

  subs <- leaf_subsets(L)
  c(normalised = at(character(0)) == 0,
    monotone   = all(vapply(subs, function(s) {
      all(vapply(setdiff(L, s), function(l) at(c(s, l)) >= at(s), logical(1)))
    }, logical(1))),
    submodular = all(vapply(subs, function(s) {
      rest <- setdiff(L, s)
      if (length(rest) < 2L) return(TRUE)
      all(apply(utils::combn(rest, 2), 2, function(p)
        at(c(s, p[1])) + at(c(s, p[2])) >= at(c(s, p)) + at(s)))
    }, logical(1))))
}

#' Max-flow bound to each leaf subset, by minimum node cut.
#'
#' Arcs are uncapacitated and only nodes carry capacity, so a minimum cut is a
#' set of service nodes whose removal leaves no leaf of the subset reachable
#' from a source. The leaf-block region is contained in this one on every
#' graph, since every token through a node is destined for a leaf that node
#' reaches; where the two differ, the leaf-block region is discarding mixes the
#' node capacities would carry.
#'
#' The cut travels with the value because it names WHICH nodes bind, which is
#' what a capacity reading is read against.
#'
#' # ponytail: subset enumeration over the nodes, exact and dependency-free at
#' # these sizes. Above about twenty nodes the upgrade is push-relabel on the
#' # node-split network.
#'
#' @param spec An instance spec.
#' @param anc  Its ancestor-indicator matrix.
#' @return A list of `value` (named numeric, by subset) and `cut` (named list
#'   of node vectors, by subset).
flow_rank <- function(spec, anc) {
  V   <- spec$nodes$node
  C   <- token_capacity(spec)[V]
  L   <- rownames(anc)
  src <- setdiff(V, spec$edges$to)

  reach_without <- function(cut) {
    seen <- setdiff(src, cut)
    repeat {
      nxt <- union(seen, setdiff(spec$edges$to[spec$edges$from %in% seen], cut))
      if (setequal(nxt, seen)) return(seen)
      seen <- nxt
    }
  }
  cuts    <- lapply(0:(2^length(V) - 1L),
                    function(m) V[bitwAnd(m, bitwShiftL(1L, seq_along(V) - 1L)) > 0])
  reached <- lapply(cuts, reach_without)
  weight  <- vapply(cuts, function(s) sum(C[s]), numeric(1))
  # Smallest cut first, so the reported one is the fewest nodes at the minimum
  # weight rather than whichever the enumeration reached first.
  ord     <- order(lengths(cuts))

  subs <- leaf_subsets(L)
  vals <- lapply(subs, function(s) {
    if (length(s) == 0L) return(list(value = 0, cut = character(0)))
    blocks <- ord[vapply(reached[ord], function(r) !any(s %in% r), logical(1))]
    best   <- blocks[which.min(weight[blocks])]
    list(value = weight[best], cut = cuts[[best]])
  })
  nms <- vapply(subs, subset_name, character(1), L = L)
  list(value = setNames(vapply(vals, `[[`, numeric(1), "value"), nms),
       cut   = setNames(lapply(vals, `[[`, "cut"), nms))
}

#' The max-flow bound region as packing constraints in LEAF coordinates.
#'
#' One row per non-empty leaf subset, the row the subset's 0/1 indicator and
#' its capacity the subset's max-flow bound. A leaf mix satisfies every row
#' exactly when a node-split flow can deliver it, so this is both the packing
#' matrix an exact packer takes and the test a witness mix has to pass.
#'
#' @param fr A `flow_rank` result.
#' @return A list of `A` (subsets by leaves, 0/1) and `C` (the bounds).
flow_region_constraints <- function(fr) {
  nms <- setdiff(names(fr$value), "{}")
  L   <- unique(unlist(strsplit(nms, ",", fixed = TRUE)))
  A   <- t(vapply(strsplit(nms, ",", fixed = TRUE),
                  function(s) as.numeric(L %in% s), numeric(length(L))))
  list(A = matrix(A, nrow = length(nms), dimnames = list(nms, L)),
       C = fr$value[nms])
}

#' The per-task resource recipes, in env$recipes' shape.
#'
#' Under unit demand a task is one token at one leaf, so its recipe is that
#' leaf's ancestor indicator at the token weight, which makes the recipe matrix
#' the leaf-block incidence matrix. Under bundle demand a task is one token at
#' each of two leaves, all or nothing, so its recipe is the sum of the two
#' indicators and a node that covers both leaves is loaded twice.
#'
#' The weight is read off the spec rather than passed, since a recipe built off
#' the bare indicator charges a fraction of the load its tokens place.
#'
#' @param spec   An instance spec.
#' @param demand "unit" or "bundle".
#' @return Named list of numeric vectors, each named by node.
leaf_recipes <- function(spec, demand = c("unit", "bundle")) {
  demand <- match.arg(demand)
  anc <- ancestor_matrix(spec)
  L   <- rownames(anc)
  w   <- token_weight_of(spec, colnames(anc))
  if (demand == "unit") {
    return(setNames(lapply(L, function(l) w * anc[l, ]), L))
  }
  pairs <- utils::combn(L, 2, simplify = FALSE)
  setNames(lapply(pairs, function(p) w * (anc[p[1], ] + anc[p[2], ])),
           vapply(pairs, paste, character(1), collapse = "+"))
}

#' The longest distance in arcs from a source to each node.
#'
#' @param spec An instance spec.
#' @return Named integer vector, named by node.
.node_depth <- function(spec) {
  d <- setNames(rep(0L, nrow(spec$nodes)), spec$nodes$node)
  repeat {
    nd <- d
    for (i in seq_len(nrow(spec$edges))) {
      nd[spec$edges$to[i]] <- max(nd[spec$edges$to[i]], nd[spec$edges$from[i]] + 1L)
    }
    if (identical(nd, d)) return(d)
    d <- nd
  }
}

#' The controls an instance is matched on, computed rather than asserted.
#'
#' Everything a contrast between two instances has to hold equal, and the two
#' quantities that cannot be held equal and are reported instead: `k_c`, the
#' round population at which the first node saturates under unit demand with
#' uniform leaf shares, and the max-flow bound the leaf-block region is read
#' against.
#'
#' @param spec An instance spec.
#' @return A named list of the controls.
matched_controls <- function(spec) {
  anc  <- ancestor_matrix(spec)
  L    <- rownames(anc)
  Ctok <- token_capacity(spec)
  cap  <- setNames(spec$nodes$capacity, spec$nodes$node)

  # Tasks per round at which a node saturates: its capacity over the load one
  # task places on it, which is the weight times the share of leaves it reaches.
  per_task <- setNames(leaf_demand_weights(spec)$demand_weight, colnames(anc))
  kc_node  <- cap[names(per_task)] / per_task
  kc       <- min(kc_node)

  fl   <- flow_rank(spec, anc)
  full <- subset_name(L, L)
  tier <- tapply(spec$nodes$phys, .node_depth(spec)[spec$nodes$node], unique)
  stopifnot("each depth carries one physical tier" = all(lengths(tier) == 1))

  list(
    n_nodes             = nrow(spec$nodes),
    n_leaves            = length(L),
    n_arcs              = nrow(spec$edges),
    capacity            = cap,
    token_capacity      = Ctok,
    basket_dim          = length(L),          # one price per leaf good
    max_flow            = fl$value[[full]],
    max_flow_cut        = fl$cut[[full]],
    k_c                 = kc,
    binding_nodes       = names(kc_node)[kc_node <= kc * (1 + 1e-9)],
    critical_path_ms    = critical_path_ms(build_leaf_graph(spec), leaf_base_ms(spec)),
    critical_path_tiers = as.character(unlist(tier)),
    laminar             = leaf_blocks_laminar(anc),
    certificate         = polymatroid_certificate(leaf_rank(anc, Ctok), L)
  )
}

#' Contract a sub-DAG into one node advertising a single scalar.
#'
#' What an integrator exports is a quotient: the cluster's internal nodes are
#' replaced by one node, its arcs are relabelled onto that node, and what the
#' cluster can carry is advertised as ONE number. Two ways to choose that
#' number, and they differ in whether the advertised region is inside the true
#' one or over it:
#'
#'   "inner"   the largest scalar every mix of which the cluster can deliver.
#'             A leaf reachable through one cluster member alone is what bounds
#'             it, so the scalar is the smallest total the members reaching any
#'             one leaf can carry. Conservative, and exact.
#'   "maxflow" the cluster's own node-split max flow to its leaves, which is
#'             what an aggregate capacity reading reports. It is deliverable in
#'             the best mix and not in the worst, so it over-commits wherever
#'             the exported units are not interchangeable.
#'
#' The contracted node inherits the cluster's physical tier, so the quotient's
#' zero-queue critical path is the instance's and an interface arm is not a
#' latency treatment in disguise.
#'
#' @param spec      An instance spec.
#' @param cluster   Character vector of nodes to contract.
#' @param advertise "inner" or "maxflow".
#' @param name      Label for the contracted node.
#' @return An instance spec whose leaf-block family is laminar.
#' @param scalar    Advertised token capacity, overriding what `advertise`
#'                  would compute. A sensitivity sweep over how much of the
#'                  aggregate an integrator claims to route is a sweep over
#'                  this number.
contract_cluster <- function(spec, cluster, advertise = c("inner", "maxflow"),
                             name = "J", scalar = NULL) {
  advertise <- match.arg(advertise)
  stopifnot("a contraction needs one token weight for the whole instance" =
              length(spec$weight) == 1L)
  anc  <- ancestor_matrix(spec)
  Ctok <- token_capacity(spec)
  L_J  <- rownames(anc)[rowSums(anc[, cluster, drop = FALSE]) > 0]

  scalar <- scalar %||% switch(advertise,
    # Every exported token loads every cluster node on its ancestor path, so
    # the safe scalar is the least capacity among the nodes that load it, not
    # the sum of parallel capacities: three parallel nodes of fifty carry
    # fifty tokens of joint throughput, not one hundred and fifty.
    inner = min(vapply(L_J, function(l)
      min(Ctok[cluster[anc[l, cluster] > 0]]), numeric(1))),
    maxflow = {
      sub <- list(
        nodes  = spec$nodes[spec$nodes$node %in% c(cluster, L_J), ],
        edges  = spec$edges[spec$edges$from %in% cluster &
                              spec$edges$to %in% L_J, ],
        weight = spec$weight)
      sa <- ancestor_matrix(sub)
      flow_rank(sub, sa)$value[[subset_name(rownames(sa), rownames(sa))]]
    })

  keep  <- !(spec$nodes$node %in% cluster)
  nodes <- dplyr::bind_rows(
    spec$nodes[keep, ],
    tibble(node = name,
           phys = unique(spec$nodes$phys[spec$nodes$node %in% cluster]),
           capacity = scalar * spec$weight))
  stopifnot("a cluster spans one physical tier" = nrow(nodes) == sum(keep) + 1L)

  e <- spec$edges
  e$from[e$from %in% cluster] <- name
  e$to[e$to %in% cluster]     <- name
  e <- dplyr::distinct(e[e$from != e$to, ])

  out <- list(nodes = nodes, edges = e, weight = spec$weight)
  stopifnot("the quotient's leaf-block family is not laminar" =
              leaf_blocks_laminar(ancestor_matrix(out)))
  out
}

#' The instance's capacities in init_environment's shape, labelled by node.
#'
#' @param spec An instance spec.
#' @return A tibble of `tier` (the node) and `capacity`.
leaf_capacities <- function(spec) {
  tibble(tier = spec$nodes$node, capacity = spec$nodes$capacity)
}

#' The instance's base delays in init_environment's shape, labelled by node.
#'
#' @param spec An instance spec.
#' @return A tibble of `tier` (the node) and `base_ms`.
leaf_base_latency <- function(spec) {
  tibble(tier = spec$nodes$node, base_ms = unname(leaf_base_ms(spec)))
}
