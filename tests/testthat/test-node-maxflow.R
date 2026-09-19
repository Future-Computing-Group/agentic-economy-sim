# test-node-maxflow.R
# ---------------------------------------------------------------------------
# The node-split max-flow bound to each leaf subset, and the two things it is
# read for: the reference the leaf-block rank is compared against, and the
# routable-mix test a witness point has to pass.
#
# Arcs are uncapacitated and only nodes carry capacity, so a minimum cut is a
# set of service nodes whose removal leaves no leaf of the subset reachable
# from a source. Values here are in tokens, like the rank.
# ---------------------------------------------------------------------------

small <- leaf_instance_specs("small")
scale <- leaf_instance_specs("scale")

fr <- function(spec) flow_rank(spec, ancestor_matrix(spec))

test_that("the small max-flow bound is the hand-computed table, by subset name", {
  expect_equal(fr(small$T)$value,
               c("{}" = 0, "l1" = 4, "l2" = 4, "l3" = 4,
                 "l1,l2" = 4, "l1,l3" = 8, "l2,l3" = 8, "l1,l2,l3" = 8))
  # l2 has two parents on X, so no single internal node cuts it off and its own
  # capacity is what binds.
  expect_equal(fr(small$X)$value,
               c("{}" = 0, "l1" = 4, "l2" = 5, "l3" = 4,
                 "l1,l2" = 8, "l1,l3" = 8, "l2,l3" = 8, "l1,l2,l3" = 8))
  expect_equal(fr(small$S)$value,
               c("{}" = 0, "l1" = 5, "l2" = 5, "l3" = 5,
                 "l1,l2" = 8, "l1,l3" = 8, "l2,l3" = 8, "l1,l2,l3" = 8))
})

test_that("the max-flow bound equals the leaf-block rank on the tree and exceeds it on X and S", {
  for (nm in c("T", "X", "S")) {
    rk <- leaf_rank(ancestor_matrix(small[[nm]]), token_capacity(small[[nm]]))
    fl <- fr(small[[nm]])$value
    expect_true(all(rk <= fl))                       # containment, every instance
    if (nm == "T") expect_equal(rk, fl)              # coincidence on the tree
  }
  rk_x <- leaf_rank(ancestor_matrix(small$X), token_capacity(small$X))
  fl_x <- fr(small$X)$value
  # On X the gap sits at proper subsets only: the totals coincide.
  expect_equal(unname(fl_x[c("l2", "l1,l2", "l2,l3")] - rk_x[c("l2", "l1,l2", "l2,l3")]),
               c(1, 4, 4))
  expect_equal(fl_x[["l1,l2,l3"]], rk_x[["l1,l2,l3"]])

  rk_s <- leaf_rank(ancestor_matrix(small$S), token_capacity(small$S))
  fl_s <- fr(small$S)$value
  # On S the gap reaches the full set: it is a gap in the total.
  expect_equal(unname(fl_s[-1] - rk_s[-1]), c(1, 1, 1, 4, 4, 4, 4))
})

test_that("the minimising cut is reported with the value", {
  for (nm in c("T", "X", "S")) {
    expect_equal(fr(small[[nm]])$cut[["l1,l2,l3"]], c("e1", "e2"))
  }
  # At scale the device node alone is the cheaper cut.
  for (nm in c("T", "X", "S")) {
    expect_equal(fr(scale[[nm]])$value[["l1,l2,l3,l4"]], 100)
    expect_equal(fr(scale[[nm]])$cut[["l1,l2,l3,l4"]], "d")
  }
  expect_equal(fr(small$T)$cut[["l1,l2"]], "e1")
})

test_that("the helper reproduces a parallel pair feeding one downstream service", {
  # Two parallel nodes of capacity 1 feeding a service of capacity 2: the
  # leaf-block region allows 1, a node-split flow carries 2. An instance built
  # by a caller, not one of the shipped specs, so the helpers are pinned on a
  # spec they did not ship with.
  spec <- list(
    nodes = tibble(node = c("a1", "a2", "s"),
                   phys = c("edge", "edge", "cloud"),
                   capacity = c(1, 1, 2)),
    edges = tibble(from = c("a1", "a2"), to = c("s", "s")),
    weight = 1
  )
  anc <- ancestor_matrix(spec)
  expect_equal(leaf_set(spec), "s")
  expect_equal(leaf_rank(anc, token_capacity(spec))[["s"]], 1)
  expect_equal(flow_rank(spec, anc)$value[["s"]], 2)
})

test_that("the witness mixes the shape instrument reports are routable on X", {
  spec <- small$X
  anc  <- ancestor_matrix(spec)
  C    <- token_capacity(spec)
  fc   <- flow_region_constraints(fr(spec))
  routable <- function(x) all(fc$A %*% x[colnames(fc$A)] <= fc$C)

  # (l1, l2, l3) = (4, 4, 0): l1's tokens through e1 and l2's through e2.
  x1 <- c(l1 = 4, l2 = 4, l3 = 0)
  expect_true(routable(x1))
  expect_true(any(as.vector(t(anc) %*% x1[rownames(anc)]) > C))   # refused by the leaf blocks
  load1 <- c(d = 8, e1 = 4, e2 = 4, l1 = 4, l2 = 4, l3 = 0)       # the explicit routing
  expect_true(all(load1 <= C[names(load1)]))

  # (0, 4, 4): l3's tokens through e2 and l2's through e1, the other way round.
  x2 <- c(l1 = 0, l2 = 4, l3 = 4)
  expect_true(routable(x2))
  expect_true(any(as.vector(t(anc) %*% x2[rownames(anc)]) > C))
  load2 <- c(d = 8, e1 = 4, e2 = 4, l1 = 0, l2 = 4, l3 = 4)
  expect_true(all(load2 <= C[names(load2)]))
})

test_that("a mix that loads one single-parent leaf past its parent is not deliverable at scale", {
  fc <- flow_region_constraints(fr(scale$X))
  routable <- function(x) all(fc$A %*% x[colnames(fc$A)] <= fc$C)

  # l1 hangs off e1 alone, so 75 tokens at l1 need 75 through a node of 50,
  # while the advertised total of 100 is respected.
  over <- c(l1 = 75, l2 = 0, l3 = 25, l4 = 0)
  expect_equal(sum(over), 100)
  expect_false(routable(over))
  expect_gt(over[["l1"]], fc$C[["l1"]])

  ok <- c(l1 = 50, l2 = 0, l3 = 25, l4 = 0)
  expect_true(routable(ok))
})
