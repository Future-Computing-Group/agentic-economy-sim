# test-node-instances.R
# ---------------------------------------------------------------------------
# The three evaluation instances under the level names the pipeline carries,
# and the exact reference the structural instrument is measured against.
#
# One node set, one capacity vector, one critical-path tier sequence: the arms
# differ only in which leaves each internal node reaches. Arc counts rise 7, 8,
# 15 while laminarity of the leaf-block family goes yes, no, yes, so arc
# density is not what decides the structure.
# ---------------------------------------------------------------------------

arms <- c("tree", "sp", "entangled")


test_that("the three arms name the tree, the parallel-in-series and the crossing instance", {
  for (a in arms) {
    spec <- node_instance(a)
    expect_equal(spec$nodes$node, c("d", "e1", "e2", "e3", "l1", "l2", "l3", "l4"))
    expect_equal(spec$nodes$capacity, c(200, 100, 100, 100, 150, 100, 150, 100))
    expect_equal(spec$weight, 2)
    expect_equal(leaf_set(spec), c("l1", "l2", "l3", "l4"))
    expect_equal(critical_path_ms(build_leaf_graph(spec), leaf_base_ms(spec)), 70)
  }
  expect_equal(vapply(arms, function(a) nrow(node_instance(a)$edges), numeric(1)),
               c(tree = 7, sp = 15, entangled = 8))
  # The verdict the level names carry, asserted once so the mapping cannot be
  # read off a level name alone.
  expect_equal(
    vapply(arms, function(a) leaf_blocks_laminar(ancestor_matrix(node_instance(a))),
           logical(1)),
    c(tree = TRUE, sp = TRUE, entangled = FALSE))
})

test_that("a leaf mix is a share per leaf and the skewed one loads e1 past its capacity", {
  L <- c("l1", "l2", "l3", "l4")
  expect_equal(node_leaf_shares("uniform", L), setNames(rep(0.25, 4), L))
  expect_equal(node_leaf_shares("skewed", L),
               setNames(c(0.55, 0.05, 0.20, 0.20), L))
  expect_error(node_leaf_shares("nonesuch", L))

  # At the region maximum of 100 tokens the skewed mix puts 60 tokens through
  # e1 against a token capacity of 50; the uniform mix puts exactly 50, an
  # exact knife-edge at which an over-commitment arm measures a null.
  anc <- ancestor_matrix(node_instance("entangled"))
  e1_share <- function(mix) sum(node_leaf_shares(mix, rownames(anc)) * anc[, "e1"])
  expect_equal(100 * e1_share("skewed"), 60)
  expect_equal(100 * e1_share("uniform"), 50)
})

test_that("the demand weights are the token weight times the share-weighted ancestors", {
  spec <- node_instance("entangled")
  anc  <- ancestor_matrix(spec)
  dw   <- node_demand_weights(anc, node_leaf_shares("uniform", rownames(anc)),
                              spec$weight)
  expect_equal(dw$tier, colnames(anc))
  # d is an ancestor of every leaf, so its weight is the full token weight.
  expect_equal(dw$demand_weight[dw$tier == "d"], 2)
  # e1 reaches l1 and l2, e2 reaches l2 and l3, e3 reaches l4.
  expect_equal(dw$demand_weight[dw$tier == "e1"], 2 * 0.5)
  expect_equal(dw$demand_weight[dw$tier == "e3"], 2 * 0.25)
})

test_that("a unit recipe is the token weight times one leaf's ancestor indicator", {
  for (a in arms) {
    spec <- node_instance(a)
    anc  <- ancestor_matrix(spec)
    r    <- leaf_recipes(spec, "unit")
    expect_equal(names(r), rownames(anc))
    for (l in names(r)) expect_equal(r[[l]], spec$weight * anc[l, ])
  }
})


# ---- the exact reference ---------------------------------------------------

test_that("crossing_leaves is empty on the laminar arms and one leaf on the crossing arm", {
  expect_equal(crossing_leaves(ancestor_matrix(node_instance("tree"))),
               character(0))
  expect_equal(crossing_leaves(ancestor_matrix(node_instance("sp"))),
               character(0))
  cross <- crossing_leaves(ancestor_matrix(node_instance("entangled")))
  expect_length(cross, 1L)
  # Deleting it leaves the family laminar, which is the whole property: the
  # outer scan is exact whichever of the crossing pair the search reaches.
  anc <- ancestor_matrix(node_instance("entangled"))
  expect_true(leaf_blocks_laminar(anc[setdiff(rownames(anc), cross), ]))
})

test_that("the scan reproduces the enumerated rank on the divided twin, subset by subset", {
  # Two implementations in two coordinate systems: leaf_rank enumerates under a
  # cover bound on a capacity vector small enough to enumerate, lb_optimum_rank
  # scans the crossing coordinate at the evaluation capacities. They must agree
  # up to the factor the capacities were divided by.
  for (a in arms) {
    spec <- node_instance(a)
    anc  <- ancestor_matrix(spec)
    twin <- spec
    twin$nodes$capacity <- spec$nodes$capacity / 25

    expect_equal(lb_optimum_rank(anc, token_capacity(spec)),
                 25 * leaf_rank(anc, token_capacity(twin)),
                 info = a)
  }
})

test_that("the certificate locates the crossing arm's submodularity failure", {
  cert <- function(a) {
    spec <- node_instance(a)
    polymatroid_certificate(
      lb_optimum_rank(ancestor_matrix(spec), token_capacity(spec)),
      leaf_set(spec))
  }
  expect_equal(cert("tree"), c(normalised = TRUE, monotone = TRUE, submodular = TRUE))
  expect_equal(cert("sp"),   c(normalised = TRUE, monotone = TRUE, submodular = TRUE))
  expect_equal(cert("entangled"),
               c(normalised = TRUE, monotone = TRUE, submodular = FALSE))

  # The failure is a marginal that RISES with the conditioning set, which is
  # the definition failing rather than an arithmetic slip.
  spec <- node_instance("entangled")
  r <- lb_optimum_rank(ancestor_matrix(spec), token_capacity(spec))
  expect_equal(r[["l1,l2"]] - r[["l2"]], 0)
  expect_equal(r[["l1,l2,l3"]] - r[["l2,l3"]], 50)
})

test_that("the crossing arm's internal pair is the two-node three-leaf counterexample", {
  spec <- node_instance("entangled")
  anc  <- ancestor_matrix(spec)
  sub  <- anc[c("l1", "l2", "l3"), c("e1", "e2")]
  expect_equal(unname(t(sub)),
               matrix(c(1, 1, 0,
                        0, 1, 1), nrow = 2, byrow = TRUE))

  # The crossing leaf is l2: it shares e1 with l1 and e2 with l3, so adding
  # it to either single leaf buys nothing and adding the pair around it
  # buys the second node. That is submodularity failing, at the two-node
  # three-leaf shape the counterexample is stated on.
  C <- c(e1 = 50, e2 = 50)
  r <- leaf_rank(sub, C)
  expect_equal(unname(r[c("l2", "l1,l2", "l2,l3")]), c(50, 50, 50))
  expect_equal(r[["l1,l2,l3"]], 100)
  expect_equal(r[["l1,l2,l3"]] - r[["l1,l2"]], 50)   # marginal of l3 over {l1,l2}
  expect_equal(r[["l2,l3"]] - r[["l2"]], 0)          # ... and over {l2} alone
})

test_that("the scan agrees with the brute-force packer on the twin", {
  for (a in arms) {
    spec <- node_instance(a)
    twin <- spec
    twin$nodes$capacity <- spec$nodes$capacity / 25
    anc  <- ancestor_matrix(twin)
    env  <- node_env(a, "high", 8L, spec = twin)

    set.seed(20L + which(arms == a))
    for (draw in 1:10) {
      tasks <- tibble(task_id = sprintf("t%d", 1:8), agent_id = 1L,
                      deadline = 1000, value_base = 1,
                      recipe = sample(rownames(anc), 8, replace = TRUE))
      v <- runif(8, 1, 2)
      A <- task_recipes(tasks, env)
      C <- env$capacities$capacity[match(colnames(A), env$capacities$tier)]
      marg <- setNames(lapply(rownames(anc), function(l)
        sort(v[tasks$recipe == l], decreasing = TRUE)), rownames(anc))

      expect_equal(lb_optimum(marg, anc, token_capacity(twin)),
                   exact_pack_by_value(v, A, C)$value,
                   info = sprintf("%s draw %d", a, draw))
    }
  }
})

test_that("value-greedy falls below the scan on the designed crossing witness", {
  # Four tokens at the shared leaf valued above the eight at the two
  # single-parent leaves. Greedy takes the four, which fills both internal
  # nodes; the optimum leaves them and takes the eight.
  spec <- leaf_instance_specs("small")$X
  env  <- node_env("entangled", "medium", 8L, spec = spec)
  anc  <- ancestor_matrix(spec)

  tasks <- tibble(task_id = sprintf("t%02d", 1:12), agent_id = 1L,
                  deadline = 1000, value_base = 1,
                  recipe = rep(c("l2", "l1", "l3"), each = 4))
  v <- c(rep(10, 4), rep(9, 4), rep(9, 4))
  marg <- setNames(lapply(rownames(anc), function(l)
    sort(v[tasks$recipe == l], decreasing = TRUE)), rownames(anc))

  greedy <- sum(v[.greedy_pack_by(v, tasks, env)])
  expect_equal(greedy, 40)
  expect_equal(lb_optimum(marg, anc, token_capacity(spec)), 72)
  expect_lt(greedy / lb_optimum(marg, anc, token_capacity(spec)), 1)
})
