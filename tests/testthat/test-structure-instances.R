# test-structure-instances.R
# ---------------------------------------------------------------------------
# The node-level substrate: instance specs, the ancestor-indicator matrix, the
# leaf-block rank and its polymatroid certificate, the recipe map and the
# matched-control summary.
#
# Every rank value asserted here is hand-computed from the capacity vector and
# the arc list, and every subset is addressed BY NAME. A rank vector addressed
# by position silently reorders when the subset enumeration changes, and the
# two orders agree by coincidence on some instances, so a positional assertion
# can pass on a set function that is wrong.
# ---------------------------------------------------------------------------

small <- leaf_instance_specs("small")
scale <- leaf_instance_specs("scale")

test_that("the small instances share one node set, one capacity vector and one arc-count ladder", {
  for (nm in c("T", "X", "S")) {
    expect_equal(small[[nm]]$nodes$node, c("d", "e1", "e2", "l1", "l2", "l3"))
    expect_equal(small[[nm]]$nodes$capacity, c(9, 4, 4, 5, 5, 5))
    expect_equal(small[[nm]]$nodes$phys,
                 c("device", "edge", "edge", "cloud", "cloud", "cloud"))
    expect_equal(small[[nm]]$weight, 1)
  }
  expect_equal(vapply(small[c("T", "X", "S")], function(s) nrow(s$edges), numeric(1)),
               c(T = 5, X = 6, S = 8))
})

test_that("the scale instances share one node set, one capacity vector and one arc-count ladder", {
  for (nm in c("T", "X", "S")) {
    expect_equal(scale[[nm]]$nodes$node,
                 c("d", "e1", "e2", "e3", "l1", "l2", "l3", "l4"))
    expect_equal(scale[[nm]]$nodes$capacity, c(200, 100, 100, 100, 150, 100, 150, 100))
    expect_equal(scale[[nm]]$nodes$phys,
                 c("device", "edge", "edge", "edge",
                   "cloud", "cloud", "cloud", "cloud"))
    expect_equal(scale[[nm]]$weight, 2)
  }
  expect_equal(vapply(scale[c("T", "X", "S")], function(s) nrow(s$edges), numeric(1)),
               c(T = 7, X = 8, S = 15))
  # The token column the rank function is stated in is the capacity column over
  # the per-token demand weight, and it is integral on every node.
  expect_equal(token_capacity(scale$T),
               c(d = 100, e1 = 50, e2 = 50, e3 = 50,
                 l1 = 75, l2 = 50, l3 = 75, l4 = 50))
  expect_equal(token_capacity(small$T),
               c(d = 9, e1 = 4, e2 = 4, l1 = 5, l2 = 5, l3 = 5))
})

test_that("build_leaf_graph indexes resources by node and carries the physical tier", {
  g <- build_leaf_graph(small$X)
  expect_equal(names(g), c("nodes", "edges", "demand_weights"))
  expect_equal(g$nodes$node, g$nodes$tier)             # tier = node: node indexing
  expect_equal(g$nodes$phys,
               c("device", "edge", "edge", "cloud", "cloud", "cloud"))
  expect_identical(g$edges, small$X$edges)
  expect_equal(g$demand_weights$tier, small$X$nodes$node)
  # The mix-average bundle under uniform leaf shares: the weight times the
  # share of leaves a node is an ancestor of.
  expect_equal(setNames(g$demand_weights$demand_weight, g$demand_weights$tier),
               c(d = 1, e1 = 2 / 3, e2 = 2 / 3, l1 = 1 / 3, l2 = 1 / 3, l3 = 1 / 3))
})

test_that("the zero-queue critical path is 70 ms on every instance at both sizes", {
  for (spec in c(small, scale)) {
    expect_equal(critical_path_ms(build_leaf_graph(spec), leaf_base_ms(spec)), 70)
  }
})

test_that("a leaf is a node with no outgoing arc, and the ancestor matrix is the design's", {
  expect_equal(leaf_set(small$T), c("l1", "l2", "l3"))
  expect_equal(leaf_set(scale$X), c("l1", "l2", "l3", "l4"))

  expect_equal(ancestor_matrix(small$T),
               matrix(c(1, 1, 0, 1, 0, 0,
                        1, 1, 0, 0, 1, 0,
                        1, 0, 1, 0, 0, 1),
                      nrow = 3, byrow = TRUE,
                      dimnames = list(c("l1", "l2", "l3"),
                                      c("d", "e1", "e2", "l1", "l2", "l3"))))
  expect_equal(ancestor_matrix(small$X)["l2", ],
               c(d = 1, e1 = 1, e2 = 1, l1 = 0, l2 = 1, l3 = 0))
  expect_equal(ancestor_matrix(small$S),
               matrix(c(1, 1, 1, 1, 0, 0,
                        1, 1, 1, 0, 1, 0,
                        1, 1, 1, 0, 0, 1),
                      nrow = 3, byrow = TRUE,
                      dimnames = list(c("l1", "l2", "l3"),
                                      c("d", "e1", "e2", "l1", "l2", "l3"))))

  expect_equal(ancestor_matrix(scale$T)["l2", ],
               c(d = 1, e1 = 1, e2 = 0, e3 = 0, l1 = 0, l2 = 1, l3 = 0, l4 = 0))
  expect_equal(ancestor_matrix(scale$X)["l2", ],
               c(d = 1, e1 = 1, e2 = 1, e3 = 0, l1 = 0, l2 = 1, l3 = 0, l4 = 0))
  expect_equal(ancestor_matrix(scale$S)["l3", ],
               c(d = 1, e1 = 1, e2 = 1, e3 = 1, l1 = 0, l2 = 0, l3 = 1, l4 = 0))
})

test_that("the leaf-block family is laminar on T and S and not on X, at both sizes", {
  expect_true(leaf_blocks_laminar(ancestor_matrix(small$T)))
  expect_false(leaf_blocks_laminar(ancestor_matrix(small$X)))
  expect_true(leaf_blocks_laminar(ancestor_matrix(small$S)))

  expect_true(leaf_blocks_laminar(ancestor_matrix(scale$T)))
  expect_false(leaf_blocks_laminar(ancestor_matrix(scale$X)))
  expect_true(leaf_blocks_laminar(ancestor_matrix(scale$S)))
})

# --- the leaf-block rank, and the certificate that decides the region -------

test_that("the small leaf-block rank reproduces the hand-computed table, by subset name", {
  rk <- function(nm) leaf_rank(ancestor_matrix(small[[nm]]), token_capacity(small[[nm]]))

  expect_equal(rk("T"), c("{}" = 0, "l1" = 4, "l2" = 4, "l3" = 4,
                          "l1,l2" = 4, "l1,l3" = 8, "l2,l3" = 8, "l1,l2,l3" = 8))
  expect_equal(rk("X"), c("{}" = 0, "l1" = 4, "l2" = 4, "l3" = 4,
                          "l1,l2" = 4, "l1,l3" = 8, "l2,l3" = 4, "l1,l2,l3" = 8))
  expect_equal(rk("S"), c("{}" = 0, "l1" = 4, "l2" = 4, "l3" = 4,
                          "l1,l2" = 4, "l1,l3" = 4, "l2,l3" = 4, "l1,l2,l3" = 4))
})

test_that("the scale leaf-block rank reproduces the hand-computed table, by subset name", {
  rk <- function(nm) leaf_rank(ancestor_matrix(scale[[nm]]), token_capacity(scale[[nm]]))
  pairs   <- c("l1,l2", "l1,l3", "l1,l4", "l2,l3", "l2,l4", "l3,l4")
  bigger  <- c("l1,l2,l3", "l1,l2,l4", "l1,l3,l4", "l2,l3,l4", "l1,l2,l3,l4")
  singles <- c("l1", "l2", "l3", "l4")

  for (nm in c("T", "X")) {
    expect_equal(rk(nm)[["{}"]], 0)
    expect_equal(unname(rk(nm)[singles]), rep(50, 4))
    expect_equal(rk(nm)[["l1,l2"]], 50)                 # one internal node covers both
    expect_equal(unname(rk(nm)[bigger]), rep(100, 5))
  }
  # The crossing arc is the whole difference: it puts l2 and l3 under one node.
  expect_equal(unname(rk("T")[pairs]), c(50, 100, 100, 100, 100, 100))
  expect_equal(unname(rk("X")[pairs]), c(50, 100, 100, 50, 100, 100))

  expect_equal(rk("S"), c(setNames(0, "{}"),
                          setNames(rep(50, 15), c(singles, pairs, bigger))))
})

test_that("the polymatroid certificate is TRUE on T and S and FALSE on X, at both sizes", {
  cert <- function(spec) polymatroid_certificate(
    leaf_rank(ancestor_matrix(spec), token_capacity(spec)))

  for (family in list(small, scale)) {
    expect_equal(cert(family$T),
                 c(normalised = TRUE, monotone = TRUE, submodular = TRUE))
    expect_equal(cert(family$S),
                 c(normalised = TRUE, monotone = TRUE, submodular = TRUE))
    expect_equal(cert(family$X),
                 c(normalised = TRUE, monotone = TRUE, submodular = FALSE))
  }
})

test_that("the submodularity failure on X is a marginal that rises with the conditioning set", {
  r_small <- leaf_rank(ancestor_matrix(small$X), token_capacity(small$X))
  expect_equal(r_small[["l2,l3"]] - r_small[["l2"]], 0)          # marginal of l3 over {l2}
  expect_equal(r_small[["l1,l2,l3"]] - r_small[["l1,l2"]], 4)    # ... over {l1,l2}

  r_scale <- leaf_rank(ancestor_matrix(scale$X), token_capacity(scale$X))
  expect_equal(r_scale[["l1,l2"]] - r_scale[["l2"]], 0)          # marginal of l1 over {l2}
  expect_equal(r_scale[["l1,l2,l3"]] - r_scale[["l2,l3"]], 50)   # ... over {l2,l3}
})

test_that("a rank entry that is not a number gives NA verdicts and not a passing certificate", {
  # A missing or non-finite rank must not read as a verdict: a certificate is
  # reported per instance, and NA says "not decided" where FALSE would say
  # "decided against".
  r <- leaf_rank(ancestor_matrix(small$T), token_capacity(small$T))
  r[["l1,l3"]] <- NaN
  expect_equal(polymatroid_certificate(r),
               c(normalised = TRUE, monotone = NA, submodular = NA))
})

test_that("the rank search agrees with a brute-force enumeration of the capacity box", {
  # The search prunes on a cover bound, so a wrong bound could cut the branch
  # the optimum sits on and still return a plausible number. The oracle is the
  # whole integer box, which is only affordable at the small instances.
  brute <- function(spec) {
    anc <- ancestor_matrix(spec)
    C   <- token_capacity(spec)[colnames(anc)]
    pts <- as.matrix(expand.grid(rep(list(0:max(C)), nrow(anc))))
    ok  <- rowSums(pts %*% anc > matrix(C, nrow(pts), length(C), byrow = TRUE)) == 0
    vapply(leaf_subsets(rownames(anc)), function(s) {
      keep <- ok & (rowSums(pts[, !(rownames(anc) %in% s), drop = FALSE]) == 0)
      max(rowSums(pts[keep, , drop = FALSE]))
    }, numeric(1))
  }
  for (nm in c("T", "X", "S")) {
    expect_equal(unname(leaf_rank(ancestor_matrix(small[[nm]]),
                                  token_capacity(small[[nm]]))),
                 unname(brute(small[[nm]])))
  }
})

# --- the recipe map, and the matched controls -------------------------------

test_that("a unit recipe is one leaf's ancestor indicator at the token weight", {
  r <- leaf_recipes(small$T, "unit")
  expect_equal(names(r), c("l1", "l2", "l3"))
  anc <- ancestor_matrix(small$T)
  for (l in names(r)) expect_equal(r[[l]], anc[l, ])

  # At the larger instances a token weighs 2 at each of its ancestors, so the
  # recipe is the indicator scaled: a recipe built off the bare indicator would
  # charge half the load it places.
  r2   <- leaf_recipes(scale$X, "unit")
  anc2 <- ancestor_matrix(scale$X)
  expect_equal(names(r2), c("l1", "l2", "l3", "l4"))
  expect_equal(r2$l2, 2 * anc2["l2", ])
  expect_equal(r2$l2[["e2"]], 2)                 # the crossing arc, charged
})

test_that("a bundle recipe is the sum of two ancestor indicators, doubling shared nodes", {
  r <- leaf_recipes(small$T, "bundle")
  expect_equal(names(r), c("l1+l2", "l1+l3", "l2+l3"))
  expect_equal(r[["l1+l2"]], c(d = 2, e1 = 2, e2 = 0, l1 = 1, l2 = 1, l3 = 0))
  expect_equal(r[["l1+l3"]], c(d = 2, e1 = 1, e2 = 1, l1 = 1, l2 = 0, l3 = 1))

  rx <- leaf_recipes(small$X, "bundle")
  expect_equal(rx[["l2+l3"]], c(d = 2, e1 = 1, e2 = 2, l1 = 0, l2 = 1, l3 = 1))

  expect_equal(length(leaf_recipes(scale$T, "bundle")), 6)
})

test_that("the matched controls hold across the scale triple where they can", {
  mc <- lapply(scale[c("T", "X", "S")], matched_controls)

  for (nm in c("T", "X", "S")) {
    expect_equal(mc[[nm]]$n_nodes, 8)
    expect_equal(mc[[nm]]$n_leaves, 4)
    expect_equal(mc[[nm]]$basket_dim, 4)               # one price per leaf
    expect_equal(mc[[nm]]$capacity,
                 c(d = 200, e1 = 100, e2 = 100, e3 = 100,
                   l1 = 150, l2 = 100, l3 = 150, l4 = 100))
    expect_equal(mc[[nm]]$max_flow, 100)
    expect_equal(mc[[nm]]$critical_path_ms, 70)
    expect_equal(mc[[nm]]$critical_path_tiers, c("device", "edge", "cloud"))
  }
  expect_equal(vapply(mc, function(m) m$n_arcs, numeric(1)), c(T = 7, X = 8, S = 15))

  # K_c is matched across T and X by construction and is half on S, which is
  # the conservatism of the leaf-block region and cannot also be a matched
  # input.
  expect_equal(vapply(mc, function(m) m$k_c, numeric(1)), c(T = 100, X = 100, S = 50))
  expect_equal(mc$T$binding_nodes, c("d", "e1"))
  expect_equal(mc$X$binding_nodes, c("d", "e1", "e2"))
  expect_equal(mc$S$binding_nodes, c("e1", "e2", "e3"))

  expect_equal(vapply(mc, function(m) m$laminar, logical(1)),
               c(T = TRUE, X = FALSE, S = TRUE))
  expect_equal(vapply(mc, function(m) m$certificate[["submodular"]], logical(1)),
               c(T = TRUE, X = FALSE, S = TRUE))
})

test_that("the matched controls hold across the small triple where they can", {
  mc <- lapply(small[c("T", "X", "S")], matched_controls)
  expect_equal(vapply(mc, function(m) m$k_c, numeric(1)), c(T = 6, X = 6, S = 4))
  expect_equal(vapply(mc, function(m) m$max_flow, numeric(1)), c(T = 8, X = 8, S = 8))
  expect_equal(vapply(mc, function(m) m$basket_dim, numeric(1)), c(T = 3, X = 3, S = 3))
  expect_equal(mc$T$binding_nodes, "e1")
  expect_equal(mc$X$binding_nodes, c("e1", "e2"))
})
