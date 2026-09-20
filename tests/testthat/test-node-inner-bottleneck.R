# The inner exposure of a contracted cluster is the largest scalar whose
# advertised region lies inside the true region under every leaf mix. Every
# exported token loads every cluster node on its ancestor path, so the scalar
# is the least capacity among the cluster nodes that load it, not the sum of
# parallel capacities: three parallel nodes of fifty tokens each carry fifty
# tokens of joint throughput, not one hundred and fifty.

test_that("the inner scalar is the bottleneck of the loaded cluster nodes", {
  q_sp <- contract_cluster(node_instance("sp"), c("e1", "e2", "e3"), "inner")
  expect_equal(unname(token_capacity(q_sp)[["J"]]), 50)
  for (a in c("tree", "entangled")) {
    q <- contract_cluster(node_instance(a), c("e1", "e2"), "inner")
    expect_equal(unname(token_capacity(q)[["J"]]), 50)
  }
})

test_that("the inner region never exceeds a true node's capacity at any mix", {
  for (a in c("tree", "sp", "entangled")) {
    spec <- node_instance(a)
    cl   <- if (a == "sp") c("e1", "e2", "e3") else c("e1", "e2")
    q    <- contract_cluster(spec, cl, "inner")
    anc  <- ancestor_matrix(spec)
    Ctok <- token_capacity(spec)
    L_J  <- rownames(anc)[rowSums(anc[, cl, drop = FALSE]) > 0]
    cap  <- unname(token_capacity(q)[["J"]])
    # every exposed leaf, alone, at the advertised scalar, loads each of its
    # cluster ancestors by the full scalar
    for (l in L_J) for (v in cl[anc[l, cl] > 0]) expect_lte(cap, Ctok[[v]])
  }
})
