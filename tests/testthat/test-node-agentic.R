# test-node-agentic.R
# ---------------------------------------------------------------------------
# The measured workload's own graph, read as data.
#
# The recording used to carry its structure as a free-text string that no code
# read, so the simulator ran the agentic arm on a hand-typed DAG and the
# measurement reached it only through three per-tier weights. The graph block
# is now nodes and edges, derived from the executed call sequence, and the
# environment is built from it.
#
# Every structural verdict below is COMPUTED from whatever the recording
# produced and never asserted from the design: if a run degenerates, the
# certificate says so and the arm is reported as what it is.
# ---------------------------------------------------------------------------

fx <- function(nm) test_path("fixtures", sprintf("agentic-profile-%s.json", nm))


test_that("the profile's graph block round-trips into nodes and edges", {
  g <- agentic_graph(fx("a"))
  expect_equal(g$nodes$node, c("plan", "tool0", "tool1", "aggregate"))
  expect_equal(g$nodes$tier, c("device", "edge", "edge", "cloud"))
  expect_equal(paste(g$edges$from, g$edges$to),
               c("plan tool0", "plan tool1", "tool0 aggregate",
                 "tool1 aggregate"))
  expect_equal(g$leaves, "aggregate")

  b <- agentic_graph(fx("b"))
  expect_equal(b$nodes$node,
               c("plan_b", "retrieve", "tool1", "summary", "citations"))
  expect_equal(b$leaves, c("summary", "citations"))
  # A profile with no graph block is not a graph: the caller falls back rather
  # than running on half a measurement.
  expect_null(agentic_graph(fx("noblock")))
})

test_that("build_dependency_graph reads the block and falls back without one", {
  g <- build_dependency_graph("agentic")
  rec <- agentic_graph(agentic_profile_path())
  expect_equal(g$nodes$node, rec$nodes$node)
  expect_equal(g$nodes$tier, rec$nodes$tier)
  expect_equal(nrow(g$edges), nrow(rec$edges))

  # Without a block the typed graph stands, so a missing profile fails loudly
  # somewhere else rather than silently running a half-measured environment.
  fb <- agentic_typed_graph()
  expect_equal(fb$nodes$node, c("plan", "tool0", "tool1", "aggregate"))
  expect_equal(nrow(fb$edges), 4L)
})


# ---- the union of the two recordings --------------------------------------

test_that("the union of the two recordings has three leaves and crosses", {
  spec <- agentic_union_spec(c(fx("a"), fx("b")))
  anc  <- ancestor_matrix(spec)

  expect_setequal(spec$nodes$node,
                  c("plan", "plan_b", "tool0", "tool1", "retrieve",
                    "aggregate", "summary", "citations"))
  expect_equal(nrow(spec$edges), 9L)
  expect_setequal(leaf_set(spec), c("aggregate", "summary", "citations"))

  # The crossing is the shared tool stage against the retrieval stage: neither
  # block contains the other and they are not disjoint.
  blk <- function(v) rownames(anc)[anc[, v] > 0]
  expect_setequal(blk("tool1"), c("aggregate", "summary"))
  expect_setequal(blk("retrieve"), c("summary", "citations"))
  expect_false(leaf_blocks_laminar(anc))

  # Each pattern on its own is laminar, so the crossing is a property of the
  # union rather than of either recording.
  for (p in c("a", "b")) {
    expect_true(leaf_blocks_laminar(ancestor_matrix(agentic_union_spec(fx(p)))))
  }
})

test_that("the weights and latencies are the recording's own, per stage", {
  spec <- agentic_union_spec(c(fx("a"), fx("b")))
  w    <- token_weight_of(spec, spec$nodes$node)
  expect_equal(unname(w[c("plan", "tool0", "tool1")]), c(1, 1, 2))
  expect_equal(unname(w[c("summary", "citations")]), c(3, 3))

  # The per-node base delay is the measured per-stage latency, and the critical
  # path is the longest source-to-leaf path over them, one value per leaf.
  d <- critical_path_to_leaves(build_leaf_graph(spec), leaf_base_ms(spec),
                               leaf_set(spec))
  expect_equal(unname(d[["aggregate"]]), 100 + 200 + 400)
  expect_equal(unname(d[["citations"]]), 100 + 200 + 400)
  expect_true(all(d > 0))
})

test_that("the capacity split lets the edge stages bind and the planners not", {
  spec <- agentic_union_spec(c(fx("a"), fx("b")))
  cap  <- setNames(spec$nodes$capacity, spec$nodes$node)
  # Each planner carries its whole physical tier, so it never binds; the edge
  # and cloud totals are split across the stages that sit in them. Without
  # that the planners bind on every subset, the union's rank is constant, and
  # the arm degenerates to the uniform-matroid case the rebuild exists to
  # leave behind.
  expect_equal(unname(cap[c("plan", "plan_b")]), c(200, 200))
  expect_equal(sum(cap[c("tool0", "tool1", "retrieve")]), 300)
  expect_equal(sum(cap[c("aggregate", "summary", "citations")]), 500)

  tok <- token_capacity(spec)
  expect_lt(min(tok[c("tool1", "retrieve")]), min(tok[c("plan", "plan_b")]))
})

test_that("the certificate on the recorded union is computed and not typed", {
  cert <- function(spec) {
    anc <- ancestor_matrix(spec)
    polymatroid_certificate(leaf_rank(anc, token_capacity(spec)),
                            rownames(anc))[["submodular"]]
  }
  union <- agentic_union_spec(c(fx("a"), fx("b")))
  expect_false(cert(union))
  expect_true(cert(agentic_union_spec(fx("a"))))
  expect_true(cert(agentic_union_spec(fx("b"))))

  # Deleting the arc that makes the tool stage shared leaves a laminar union,
  # and the verdict follows the recording rather than the design.
  cut <- union
  cut$edges <- cut$edges[!(cut$edges$from == "tool1" &
                             cut$edges$to == "summary"), ]
  expect_true(leaf_blocks_laminar(ancestor_matrix(cut)))
  expect_true(cert(cut))
})

test_that("the node-level agentic environment clears and certifies per arm", {
  env <- node_agentic_env("high", 40L, c(fx("a"), fx("b")))
  expect_setequal(env$capacities$tier,
                  c("plan", "plan_b", "tool0", "tool1", "retrieve",
                    "aggregate", "summary", "citations"))
  expect_equal(names(env$recipes), rownames(env$anc))
  # A stage's recipe is its own measured weight at each of the leaf's
  # ancestors, so a heavier stage charges more wherever it is on the path.
  expect_equal(env$recipes$summary[["retrieve"]], 2)
  expect_equal(env$recipes$summary[["summary"]], 3)
  expect_equal(env$recipes$summary[["citations"]], 0)

  # The value model is rescaled to the instance's own critical path, as the
  # per-tier agentic environment's is: the nominal decay rate belongs to a
  # pipeline of 135 ms and leaves nothing of a task whose stages run for
  # seconds, so the market would clear nothing and the test would assert an
  # empty round.
  k <- node_agentic_constants(agentic_union_spec(c(fx("a"), fx("b"))))
  tasks <- node_round_tasks(env, init_agents(40L), 1L, 1L, k$deadlines)
  res <- clear_multitier_market(tasks, env, util_hat = 0.5,
                                base_latency = base_latency_for_bids(env),
                                market_state = init_market_state(env),
                                lambda_l_default = k$lambda_l)
  expect_true(is.finite(res$clearing$unit_cost))
  expect_gt(nrow(res$allocation), 0)
  used <- colSums(task_recipes(res$allocation, env))
  expect_true(all(used <= env$capacities$capacity[
    match(names(used), env$capacities$tier)]))

  # The union arm is uncertified and each single pattern is certified, both
  # computed on the environment the round actually cleared over.
  expect_false(dsic_certificate(env, tasks))
  for (p in c("a", "b")) {
    e <- node_agentic_env("high", 40L, fx(p))
    expect_true(dsic_certificate(e, node_round_tasks(
      e, init_agents(40L), 1L, 1L, k$deadlines)))
  }
})
