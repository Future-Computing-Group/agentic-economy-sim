# test-node-governance.R
# ---------------------------------------------------------------------------
# Governance as coordinate-wise caps on the leaf tokens, with trust, locality
# and role as the three DETERMINANTS of one object rather than three objects.
#
# The enforcement point is the leaf node's own capacity: a leaf's leaf block is
# the singleton, so x_l <= u_l is already a coordinate constraint of the region
# the packer and the tatonnement both enforce, and no kernel code moves.
#
# The residency coupling is the case the caps do NOT cover. A joint bound
# across two leaves is not coordinate-wise, it makes two leaf blocks cross on
# an instance that was laminar, and the recovery is to slice by domain at a
# welfare cost set by the stranded cross-domain demand.
# ---------------------------------------------------------------------------

gov_env <- function(a = "entangled", mix = "uniform") {
  node_env(a, "high", node_agents()[[a]], mix)
}

gov_tasks <- function(env, n = 40L, seed = 1L) {
  set.seed(seed)
  tibble(task_id = sprintf("t%02d", seq_len(n)), agent_id = rep_len(1:8, n),
         deadline = 1000, value_base = runif(n, 1, 2),
         recipe = rep_len(rownames(env$anc), n))
}


# ---- the enforcement point ------------------------------------------------

test_that("a leaf cap binds at admission and at execution alike", {
  env <- gov_env("tree")
  capped <- apply_leaf_caps(env, c(l2 = 0, l3 = 25))

  # Capacity lives twice: `capacities`, which admission prices and packs
  # against, and the copy in `per_tier`, which execution queues against.
  for (df in list(capped$capacities, capped$per_tier)) {
    got <- setNames(df$capacity, df$tier)
    expect_equal(got[["l2"]], 0)
    expect_equal(got[["l3"]], 50)          # 25 tokens at weight 2
    expect_equal(got[["l1"]], 150)         # untouched
    expect_equal(got[["e1"]], 100)         # not a leaf, never capped
  }
  # A cap only ever lowers: it is a min against what is already there.
  expect_equal(apply_leaf_caps(env, c(l2 = 10000))$capacities$capacity,
               env$capacities$capacity)
  expect_equal(apply_leaf_caps(env, NULL)$capacities, env$capacities)
})

test_that("the packer and the tatonnement both refuse a capped leaf", {
  env    <- apply_leaf_caps(gov_env("tree"), c(l2 = 0))
  tasks  <- gov_tasks(env)
  chosen <- .greedy_pack_by(runif(nrow(tasks), 1, 2), tasks, env)
  expect_false(any(tasks$recipe[chosen] == "l2"))

  res <- clear_multitier_market(tasks, env, util_hat = 0.5,
                                base_latency = base_latency_for_bids(env),
                                market_state = init_market_state(env))
  expect_false(any(res$allocation$recipe == "l2"))
})


# ---- the three determinants -----------------------------------------------

test_that("the three determinants cap the leaves each names and no others", {
  env    <- gov_env("entangled")
  agents <- init_agents(90L)
  prov   <- leaf_providers(agents, rownames(env$anc))
  Ctok   <- node_token_capacity(env)[rownames(env$anc)]

  expect_null(node_policy_caps("none", env, agents, prov))

  # Locality: the permitted set is the EU pair, so the other two are closed.
  loc <- node_policy_caps("locality", env, agents, prov)
  expect_equal(unname(loc[c("l3", "l4")]), c(0, 0))
  expect_equal(unname(loc[c("l1", "l2")]), unname(Ctok[c("l1", "l2")]))

  # Role: a cap on an agent CLASS, so it is no coordinate cap on the leaves;
  # it binds at admission (node_role_admission) and reads the agent's role.
  expect_null(node_policy_caps("role", env, agents, prov))

  # Trust: keyed on the PROVIDER behind the leaf, read from the round before.
  agents$trust[agents$agent_id == prov[["l3"]]] <- 0.5
  tr <- node_policy_caps("trust", env, agents, prov)
  expect_equal(tr[["l3"]], 0)
  expect_equal(unname(tr[c("l1", "l2", "l4")]), unname(Ctok[c("l1", "l2", "l4")]))
})

test_that("a provider pool smaller than the leaf count still covers every leaf", {
  # The sweep's smallest population puts fewer agents in the provider role than
  # the instance has leaves, and a leaf with no provider has no reputation for
  # the trust determinant to read. Providers are agents, so the assignment
  # wraps round-robin: a provider owns several leaves and the determinant
  # applies to all of them. No leaf is dropped and no agent is invented.
  agents <- tibble(agent_id = 1:5,
                   role = c("provider", "consumer", "provider",
                            "consumer", "consumer"),
                   trust = 0.8)
  leaves <- c("l1", "l2", "l3", "l4")
  prov   <- leaf_providers(agents, leaves)

  expect_equal(names(prov), leaves)                       # every leaf owned
  expect_true(all(agents$role[match(prov, agents$agent_id)] == "provider"))
  expect_setequal(unique(unname(prov)), c(1L, 3L))        # every provider used
  expect_equal(leaf_providers(agents, leaves), prov)      # deterministic

  # The trust determinant then closes BOTH leaves a distrusted provider owns.
  env  <- gov_env("tree")
  Ctok <- node_token_capacity(env)[rownames(env$anc)]
  agents$trust[agents$agent_id == 1L] <- 0.5
  tr <- node_policy_caps("trust", env, agents, prov)
  expect_equal(unname(tr[c("l1", "l3")]), c(0, 0))
  expect_equal(unname(tr[c("l2", "l4")]), unname(Ctok[c("l2", "l4")]))

  # ... and the cell that errored builds and runs a round end to end.
  out <- node_run_single("tree", "medium", N = 10L, seed = 7L, n_rounds = 1L,
                         exact_reference = FALSE)
  expect_equal(nrow(out), 1L)
})

test_that("a provider's reputation moves with its own leaf's deadline misses", {
  # The deployed update is keyed by the task's OWNING agent, so a provider's
  # trust never moves under it and the governance instrument it feeds gates
  # consumers instead. This is the provider-side sibling.
  agents <- init_agents(40L)
  leaves <- c("l1", "l2", "l3", "l4")
  prov   <- leaf_providers(agents, leaves)
  expect_length(prov, 4L)
  expect_equal(names(prov), leaves)
  expect_true(all(agents$role[match(prov, agents$agent_id)] == "provider"))
  expect_equal(leaf_providers(agents, leaves), prov)      # deterministic

  consumer <- setdiff(agents$agent_id, prov)[1]
  results <- tibble(task_id = c("a", "b", "c", "d"),
                    agent_id = consumer, success = c(TRUE, FALSE, TRUE, TRUE))
  leaf_of <- c(a = "l1", b = "l2", c = "l3", d = "l3")

  after <- update_provider_trust(agents, results, leaf_of, prov)
  tr <- function(x, l) x$trust[x$agent_id == prov[[l]]]
  expect_equal(tr(after, "l2"), 0.8 - 0.08)        # its leaf missed
  expect_equal(tr(after, "l1"), 0.8 + 0.03)        # its leaf met
  expect_equal(tr(after, "l3"), 0.8 + 0.03)
  expect_equal(tr(after, "l4"), 0.8)               # no tasks, no movement
  # The deployed consumer-side update leaves every provider where it was: it
  # keys on the task's owning agent and never on the leaf that served it.
  expect_equal(update_trust(agents, results)$trust[
    match(prov, agents$agent_id)], rep(0.8, 4))
})


# ---- the cap-target factor: which cap repairs exactness --------------------

test_that("a cap repairs the crossing arm exactly when it deletes the right leaf", {
  # A coordinate cap repairs the region when it DELETES a leaf whose deletion
  # leaves the crossing blocks nested or disjoint on the survivors, and a
  # partial cap never repairs: the marginal test fails for every positive
  # residue.
  env   <- gov_env("entangled")
  tasks <- gov_tasks(env)
  cert  <- function(u) dsic_certificate(apply_leaf_caps(env, u), tasks)

  expect_false(cert(NULL))
  expect_true(cert(c(l1 = 0)))
  expect_true(cert(c(l2 = 0)))
  expect_true(cert(c(l3 = 0)))
  expect_false(cert(c(l4 = 0)))
  expect_false(cert(c(l2 = 25)))

  # On the laminar arms every cap level leaves the certificate standing: that
  # is a control and not a result.
  for (a in c("tree", "sp")) {
    e <- gov_env(a)
    for (l in c("l1", "l2", "l3", "l4")) {
      u <- setNames(0, l)
      expect_true(dsic_certificate(apply_leaf_caps(e, u), gov_tasks(e)),
                  info = paste(a, l))
    }
  }
})

test_that("the cap-target arm closes the measured exactness gap", {
  gap <- function(target) {
    node_run_single("entangled", "high", N = 90L, seed = 2L, n_rounds = 20L,
                    cap_target = target)$greedy_exact_incidence
  }
  expect_gt(gap(NA), 0)
  expect_equal(gap("l2"), 0)
  expect_equal(gap("l1"), 0)
})


# ---- the residency coupling and its slice ---------------------------------

test_that("the residency coupling makes two blocks cross on the laminar arm", {
  # With l1, l2 in one domain and l3, l4 outside it, a joint bound on the pair
  # that straddles the boundary reproduces the crossing arm's own witness on an
  # instance that was laminar.
  env <- node_residency_coupling(gov_env("tree"))
  anc <- env$anc
  expect_true("residency" %in% colnames(anc))
  expect_equal(unname(anc[, "residency"]), c(0, 1, 1, 0))
  expect_false(leaf_blocks_laminar(anc))

  r <- leaf_rank(anc, node_token_capacity(env))
  expect_equal(r[["l2"]], 50)
  expect_equal(r[["l2,l3"]], 50)
  expect_equal(r[["l1,l2"]], 50)
  expect_equal(r[["l1,l2,l3"]], 100)
  expect_false(dsic_certificate(env, gov_tasks(env)))

  # The coupling is a constraint, not a service: it prices and packs, and it
  # adds nothing to any latency path.
  expect_equal(base_latency_for_bids(env), 70)
  expect_equal(unname(base_latency_per_leaf(env)), rep(70, 4))
  expect_true("residency" %in% tier_capacities(env)$tier)
  expect_equal(env$recipes$l2[["residency"]], 2)
  expect_equal(env$recipes$l1[["residency"]], 0)
})

test_that("slicing the coupling by domain restores the certificate on both slices", {
  env    <- node_residency_coupling(gov_env("tree"))
  slices <- node_domain_slices(gov_env("tree"), a = 25)

  expect_setequal(names(slices), c("eu", "non_eu"))
  for (s in slices) {
    expect_true(leaf_blocks_laminar(s$anc))
    expect_true(dsic_certificate(s, gov_tasks(s)))
  }
  # Each slice is a coordinate truncation: the EU slice carries l1 and 25
  # tokens of l2 and nothing outside its domain.
  eu <- setNames(slices$eu$capacities$capacity, slices$eu$capacities$tier)
  expect_equal(unname(eu[c("l1", "l2", "l3", "l4")]), c(150, 50, 0, 0))
  ne <- setNames(slices$non_eu$capacities$capacity, slices$non_eu$capacities$tier)
  expect_equal(unname(ne[c("l1", "l2", "l3", "l4")]), c(0, 0, 50, 100))
})

test_that("each leaf is priced by the slice that carries it", {
  # A leaf a slice closed is priced there by its excess demand against a
  # capacity of zero, which says something about the closure and nothing about
  # the leaf. Averaging the slices would carry that into the leaf's price.
  leaves <- c("l1", "l2", "l3", "l4")
  p_eu <- tibble(tier = c("d", leaves), price = c(0.2, 0.3, 0.4, 900, 900))
  p_ne <- tibble(tier = c("d", leaves), price = c(0.4, 900, 900, 0.5, 0.6))

  got <- node_combine_slice_prices(p_eu, p_ne, c("l1", "l2"), leaves)
  expect_equal(got$price[got$tier == "l1"], 0.3)
  expect_equal(got$price[got$tier == "l4"], 0.6)
  expect_equal(got$price[got$tier == "d"], 0.3)      # shared: the average
  expect_true(all(got$price < 1))
})

test_that("the slice strands the demand the joint budget would have carried", {
  # The welfare cost of the recovery, computed from the round's own offered
  # counts: what one budget of 50 tokens would have carried across the pair,
  # less what two budgets of 25 carry.
  expect_equal(node_stranded_demand(30, 5, 50, 25), 5)
  expect_equal(node_stranded_demand(5, 5, 50, 25), 0)
  expect_equal(node_stranded_demand(40, 40, 50, 25), 0)
  expect_equal(node_stranded_demand(60, 0, 50, 25), 25)
})


# ---- the policy factor through the driver ---------------------------------

test_that("every policy level runs and reports what it is instrumented for", {
  # The residency arms run at the skewed mix, which is what the coupled pair
  # needs to be asymmetric: at uniform shares both sides of the split are
  # over-subscribed, the two half budgets are both exhausted, and the slice
  # strands nothing at all.
  run <- function(pol, mix = "skewed")
    node_run_single("tree", "high", N = 90L, seed = 1L, n_rounds = 12L,
                    policy = pol, leaf_mix = mix)
  none <- run("none")
  expect_true(is.na(none$stranded_demand))
  expect_true(none$certificate_ok)

  # Closing two of four leaves is a dose: the market clears strictly less.
  loc <- run("locality")
  expect_lt(loc$clearing_fraction, none$clearing_fraction)
  expect_true(loc$certificate_ok)

  # A cap on one agent class at half of each leaf's capacity never admits
  # more than no cap, and less than closing two leaves outright. At this
  # operating point the consumer class never reaches its quota, so the two
  # are equal: the level is slack here, and the unit tests below show it
  # binds where the class does reach it.
  role <- run("role")
  expect_lte(role$clearing_fraction, none$clearing_fraction)
  expect_gt(role$clearing_fraction, loc$clearing_fraction)

  trust <- run("trust")
  expect_true(is.finite(trust$clearing_fraction))

  # The coupling breaks the laminar arm's certificate; the slice restores it
  # and reports what the recovery cost.
  res <- run("residency")
  expect_false(res$certificate_ok)
  sliced <- run("residency_sliced")
  expect_true(sliced$certificate_ok)
  expect_gt(sliced$stranded_demand, 0)
})


# ---- the role class reads the agent's role ---------------------------------

role_fixture <- function() {
  env    <- gov_env("tree")
  agents <- tibble::tibble(agent_id = 1:2, role = c("consumer", "provider"),
                           trust = 0.8)
  leaf   <- rownames(env$anc)[[1]]
  quota  <- floor(0.5 * node_token_capacity(env)[[leaf]])
  n      <- quota + 5L
  alloc  <- tibble::tibble(
    task_id = sprintf("t%03d", seq_len(2L * n)),
    agent_id = rep(1:2, each = n), deadline = 1000, value_base = 1.5,
    recipe = leaf)
  list(env = env, agents = agents, alloc = alloc, quota = quota, n = n)
}

test_that("an agent of the capped role is bound at the fraction the level states", {
  f <- role_fixture()
  kept <- node_role_admission(f$alloc, f$agents, f$env)
  expect_equal(sum(kept$agent_id == 1L), f$quota)
  # The ones kept are the first the mechanism admitted, in its order.
  expect_equal(kept$task_id[kept$agent_id == 1L],
               head(f$alloc$task_id[f$alloc$agent_id == 1L], f$quota))
})

test_that("an agent of the other role is not bound", {
  f <- role_fixture()
  kept <- node_role_admission(f$alloc, f$agents, f$env)
  expect_equal(sum(kept$agent_id == 2L), f$n)
  # Naming the other role as the capped one swaps who is bound.
  swapped <- node_role_admission(f$alloc, f$agents, f$env, role = "provider")
  expect_equal(sum(swapped$agent_id == 1L), f$n)
  expect_equal(sum(swapped$agent_id == 2L), f$quota)
})

test_that("the role level reads the role, so a population with no capped agent is uncapped", {
  run <- function(pol) node_run_single("tree", "high", N = 90L, seed = 1L,
                                       n_rounds = 8L, policy = pol)
  expect_lte(run("role")$tokens_admitted, run("none")$tokens_admitted)
  f <- role_fixture()
  f$agents$role <- "provider"
  expect_identical(node_role_admission(f$alloc, f$agents, f$env), f$alloc)
})
