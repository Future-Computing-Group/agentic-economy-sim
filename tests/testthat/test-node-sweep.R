# test-node-sweep.R
# ---------------------------------------------------------------------------
# The population sweep and the sensitivity sweep on the node substrate.
#
# The sweep's own result, that two arms whose K_c is matched by construction
# share an onset, is a construction check on the instrument and not evidence
# about structure. What it has to be able to do is PLACE the onset, and a grid
# that steps by ten past a boundary at 67 cannot: it states a coincidence at
# ten per cent resolution and calls it agreement.
# ---------------------------------------------------------------------------

test_that("the sweep brackets every onset at five per cent resolution", {
  g <- node_sweep_points()
  expect_length(g, 28L)
  expect_equal(g, sort(unique(g)))
  expect_true(all(seq(10, 200, by = 10) %in% g))

  # The onsets the grid has to place: the parallel arm's at medium and high
  # load, and the tree and crossing arms' at both.
  onsets <- c(34, 50, 67, 100)
  for (o in onsets) {
    below <- max(g[g <= o])
    above <- min(g[g >= o])
    expect_lte(above - below, 5, label = paste("window around onset", o))
  }
})

test_that("the sweep grid crosses the arms with the loads and the seeds", {
  grid <- node_sweep_grid(n_seeds = 3L)
  expect_setequal(names(grid), c("graph_type", "N", "load_level", "seed"))
  expect_setequal(unique(grid$graph_type), c("tree", "sp", "entangled"))
  expect_setequal(unique(grid$load_level), c("medium", "high"))
  expect_equal(nrow(grid), 3L * 28L * 2L * 3L)
})

test_that("the onset is read against the environment's own leaf-block K_c", {
  # K_c is a property of the region and of the cell's leaf mix, neither of
  # which a topology name can reconstruct, so the sweep hands the onset table
  # the function that reads it off the environment.
  for (a in c("tree", "sp", "entangled")) {
    env <- node_run_env(a, "high", 90L)
    expect_equal(node_rho_bottleneck(a, 90L, "high"),
                 1.5 * 90 / node_k_c(env), info = a)
  }
  # The arms sit at one offered load at their own matched populations.
  expect_equal(node_rho_bottleneck("tree", 90L, "high"), 1.35)
  expect_equal(node_rho_bottleneck("sp", 45L, "high"), 1.35)
  expect_equal(node_rho_bottleneck("entangled", 90L, "high"), 1.35)
  # Half the arrival rate, half the offered load at the same population.
  expect_equal(node_rho_bottleneck("tree", 90L, "medium"), 0.9)
})

test_that("the onset table takes the offered-load function as an argument", {
  # A fixture that disperses above one population per arm, so the onset is a
  # known row rather than a measurement.
  raw <- tidyr::expand_grid(graph_type = c("tree", "sp"), load_level = "high",
                            N = c(20L, 40L, 60L), seed = 1:4) %>%
    dplyr::mutate(
      mean_price_volatility =
        ifelse(N >= ifelse(graph_type == "tree", 40L, 20L), 0.3 + 0.01 * seed, 0),
      median_latency = 100 + N, drop_rate = N / 200,
      utilisation = N / 100, welfare = 50 - N / 10)

  per_tier <- exp2_price_onset(raw)
  per_node <- exp2_price_onset(raw, rho_fn = node_rho_bottleneck)

  expect_equal(per_tier$N_onset, per_node$N_onset)      # the same rows
  expect_false(isTRUE(all.equal(per_tier$rho_onset, per_node$rho_onset)))
  # The node reading is the offered load the node instances actually sit at.
  tree_onset <- per_node$N_onset[per_node$graph_type == "tree"]
  expect_equal(per_node$rho_onset[per_node$graph_type == "tree"],
               node_rho_bottleneck("tree", tree_onset, "high"))
  # And stat_exp2 hands it through rather than rebuilding the environment.
  expect_equal(stat_exp2(raw, rho_fn = node_rho_bottleneck)$onset, per_node)
})


# ---- the sensitivity sweep -------------------------------------------------

test_that("the advertised scalar interpolates between the safe one and the aggregate", {
  spec <- node_instance("entangled")
  cl   <- node_cluster("entangled")
  expect_equal(node_advertised_scalar(spec, cl, 0), 50)
  expect_equal(node_advertised_scalar(spec, cl, 1), 100)
  expect_equal(node_advertised_scalar(spec, cl, 0.5), 75)

  # And the environment built at a fraction advertises exactly that.
  env <- node_run_env("entangled", "high", 90L, "uniform", "inner",
                      advertise_frac = 0.5)
  expect_equal(env$capacities$capacity[env$capacities$tier == "J"], 150)
})

test_that("the sensitivity grid keeps its thirteen cells on the node substrate", {
  grid <- exp14_sweep_grid()
  expect_equal(nrow(grid), 13L)
  expect_equal(sum(grid$is_baseline), 4L)
})

test_that("a sensitivity row on the node substrate reports the volatile cells", {
  row <- exp14_sensitivity_row(
    "cap_scale", 1.0, topologies = "entangled", seeds = 1:2,
    N = node_agents(), load_level = "high", n_rounds = 20L,
    substrate = "node")
  expect_equal(row$parameter, "cap_scale")
  expect_true(row$is_baseline)
  expect_gte(row$n_volatile, 1L)
  expect_true(is.finite(row$median_reduction))
})

test_that("the encapsulation knob is the advertised scalar on the node substrate", {
  # The per-tier knob is an assumed demand reduction, which this substrate has
  # no place for; what an integrator can assume here is that its internal
  # routing carries more of the aggregate than the safe scalar. At the
  # baseline it assumes nothing and advertises the safe one.
  expect_equal(node_advertise_frac(1.0), 0)
  expect_equal(node_advertise_frac(0.75), 0.25)
  expect_gt(node_advertise_frac(0.65), node_advertise_frac(0.85))
})
