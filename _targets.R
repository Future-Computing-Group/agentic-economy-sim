# _targets.R
# ---------------------------------------------------------------------------
# Targets pipeline for the simulation study.
#
# Paper: "Agentic Service Markets Across the Computing Continuum:
#         A Polymatroidal Architecture" -- IEEE Transactions on Services Computing.
#
# Usage:
#   targets::tar_make()           # run the full pipeline
#   targets::tar_visnetwork()     # visualise the DAG of targets
#   targets::tar_outdated()       # check what needs to be rebuilt
#   targets::tar_read(exp1_summary_table)  # load a result
# ---------------------------------------------------------------------------

library(targets)
library(tarchetypes)
library(tidyverse)
library(crew)

# Parallel branch execution via crew (local process workers). Functions defined
# in R/ (sourced below into the targets environment) are shipped to workers as
# global dependencies of each target's command. Falls back to sequential if the
# controller is unavailable.
tar_option_set(
  packages   = c("tidyverse", "RColorBrewer", "patchwork", "scales", "future", "boot"),
  controller = crew::crew_controller_local(
    workers = as.integer(Sys.getenv("SIM_WORKERS", "8")), seconds_idle = 60)
)

# Auto-source all R/ files (sim_helpers, sim_market, experiments, plots)
purrr::walk(list.files("R", full.names = TRUE, pattern = "\\.[Rr]$"), source)

# ===========================================================================
# Simulation parameters (tab:sim-baseline-params in the paper)
# ===========================================================================
# "linear" is dropped from the experiment grid -- a linear chain is a degenerate
# 1-leaf tree (linear ~ tree empirically, sigma_p=0 for both), so it adds a
# redundant arm. build_dependency_graph("linear") is kept in sim_helpers.R for
# completeness.
graph_types    <- c("tree", "sp", "entangled")
load_levels    <- c("low", "medium", "high")
# Operating point, one agent count per topology. The knob is the agent count;
# capacities {200,300,500} and deadlines {500,750,1000} ms are unchanged. The
# criterion: at high load the market contends -- bottleneck offered load
# rho = max_r(w_r/C_r) * lambda * N in [1.1, 1.7] -- without collapsing, at
# medium load it still clears, and at low load it is slack. One shared count
# cannot do that under the corrected bid-time latency: the topologies differ by
# 2.5x in bottleneck demand per task, so at N = 75 sp and entangled are already
# collapsed (clearing fraction 0.007 and 0.001) while tree is only at rho 1.12.
# Measured on the naive arm, 3 seeds x 100 rounds, clearing fraction (share of
# generated tasks admitted) and drop rate at high / medium load:
#   tree       N = 90  rho 1.35  clearing 0.69 / 0.99  drop 0.31 / 0.01
#   sp         N = 55  rho 1.10  clearing 0.75 / 0.92  drop 0.29 / 0.08
#   entangled  N = 35  rho 1.31  clearing 0.56 / 0.82  drop 0.47 / 0.18
# One grid step up the clearing fraction falls, but not uniformly, and only sp
# collapses. Over seeds 1 to 6 at high load, 30 rounds: tree at N = 95 clears
# 0.55 to 0.64, indistinguishable from N = 90's 0.61 to 0.68; entangled at
# N = 40 degrades smoothly to 0.25 to 0.37; sp at N = 60 clears 0.27 to 0.58 on
# five seeds and collapses on the sixth, to 0.029, the online success model
# having learnt that nothing succeeds and the market admitting nothing for tens
# of rounds. The band is the criterion; that one seed-dependent collapse is why
# sp sits on the band's lower edge rather than at its centre.
# Pinned by tests/testthat/test-operating-point.R, swept by
# R/calibrate_operating_point.R.
n_agents       <- c(tree = 90L, sp = 55L, entangled = 35L)
n_rounds       <- 200L
n_seeds        <- 10L
task_deadlines <- c(500L, 750L, 1000L)     # ms; cross-continuum agentic round-trips (device-edge-cloud, 0.5-1s)
lambda_l       <- 0.005                     # per-ms latency decay; delta(T) = exp(-0.005*T)
# Integrator efficiency: no assumed demand reduction in any headline arm, on
# either topology. The factor is a modelling assumption no experiment measures,
# so at 1.0 the integrator's reported benefit is purely architectural and the
# SP-versus-entangled asymmetry stops confounding topology with assumed savings.
# Its sensitivity is reported by Exp.14, which sweeps it including this level.
integ_efficiency_sp  <- 1.0                 # integrator efficiency factor for SP topology
integ_efficiency_ent <- 1.0                 # integrator efficiency factor for entangled topology
integ_eta      <- price_eta                 # slice price step; common to every arm (R/sim_market.R)

# IEEE TSC figure dimensions (single-column ~3.5 in, 600 dpi)
fig_width  <- 3.5
fig_height <- 2.8
fig_dpi    <- 600


list(

  # ===========================================================================
  # Experiment 1: DAG topology x load
  # ===========================================================================
  tar_target(
    exp1_param_grid,
    crossing(
      graph_type = graph_types,
      load_level = load_levels,
      seed       = seq_len(n_seeds)
    )
  ),
  tar_target(
    exp1_results_raw,
    exp1_run_single(
      graph_type       = exp1_param_grid$graph_type,
      load_level       = exp1_param_grid$load_level,
      seed             = exp1_param_grid$seed,
      n_agents         = n_agents[[exp1_param_grid$graph_type]],
      n_rounds         = n_rounds,
      deadlines        = task_deadlines,
      lambda_l_default = lambda_l
    ),
    pattern   = map(exp1_param_grid),
    iteration = "vector"
  ),
  tar_target(exp1_summary_table, exp1_aggregate(exp1_results_raw)),

  # ===========================================================================
  # Experiment 2: Agent scaling x topology
  # ===========================================================================
  tar_target(
    exp2_param_grid,
    # Two load levels, one block each, medium first and row for row as it was:
    # the arrival rate lambda enters the bottleneck offered load
    # rho = max_r(w_r/C_r) * lambda * N linearly, so a threshold stated on rho
    # predicts the high-load onset at two thirds of the medium-load population,
    # and a threshold stated on N predicts no change at all. One sweep
    # separates them.
    purrr::map_dfr(
      c("medium", "high"),
      \(load) tidyr::expand_grid(
        graph_type = c("tree", "sp", "entangled"),
        # The grid reaches past every topology's price-dispersion onset. sp and
        # entangled leave the reserve well inside the first half of it; tree, the
        # slackest of the three in bottleneck demand per task, does not, so a grid
        # that stopped at 60 reported a flat zero for tree and could not place the
        # transition. stat_exp2() computes the onset from these runs.
        N          = seq(10, 120, by = 10),
        seed       = seq_len(n_seeds),
        load_level = load
      )
    )
  ),
  tar_target(
    exp2_results_raw,
    exp2_run_single(
      N                = exp2_param_grid$N,
      load_level       = exp2_param_grid$load_level,
      seed             = exp2_param_grid$seed,
      graph_type       = exp2_param_grid$graph_type,
      n_rounds         = n_rounds,
      deadlines        = task_deadlines,
      lambda_l_default = lambda_l
    ),
    pattern   = map(exp2_param_grid),
    iteration = "vector"
  ),
  tar_target(exp2_summary_table, exp2_aggregate(exp2_results_raw)),

  # The medium-load block on its own, for the two Exp.2 figures.
  tar_target(exp2_medium_results,
             bind_rows(exp2_results_raw) %>% filter(load_level == "medium")),

  # The measured agentic DAG as a fourth arm of the same sweep. Its demand
  # weights are measured, not constructed, so they match none of the three
  # synthetic topologies, and its base latencies are its own -- hence the
  # deadline set and value-decay rate Exp.9 runs it with, unchanged, with N the
  # only knob. Its busiest tier is the edge tier at 1.81/300, which carries both
  # tool calls of every task, so a threshold at rho = 0.75 puts the onset near
  # N = 125; the grid steps by 20, which is 0.121 in rho, and N = 120 lands at
  # rho = 0.72, inside the band the synthetic onsets occupy.
  tar_target(
    exp2b_param_grid,
    tidyr::expand_grid(
      N    = seq(40, 200, by = 20),
      seed = seq_len(n_seeds)
    )
  ),
  tar_target(
    exp2b_results_raw,
    exp2_run_single(
      N                = exp2b_param_grid$N,
      load_level       = "medium",
      seed             = exp2b_param_grid$seed,
      graph_type       = "agentic",
      n_rounds         = n_rounds,
      deadlines        = agentic_deadlines(path = agentic_profile_file),
      lambda_l_default = agentic_lambda_l(path = agentic_profile_file)
    ),
    pattern   = map(exp2b_param_grid),
    iteration = "vector"
  ),

  # ===========================================================================
  # Experiment 3: Governance policies
  # ===========================================================================
  tar_target(
    exp3_param_grid,
    tidyr::expand_grid(
      policy     = c("none", "moderate", "strict"),
      graph_type = c("tree", "entangled"),
      load_level = c("medium", "high"),
      seed       = seq_len(n_seeds)
    )
  ),
  tar_target(
    exp3_results_raw,
    exp3_run_single(
      policy           = exp3_param_grid$policy,
      graph_type       = exp3_param_grid$graph_type,
      load_level       = exp3_param_grid$load_level,
      N                = n_agents[[exp3_param_grid$graph_type]],
      seed             = exp3_param_grid$seed,
      n_rounds         = n_rounds,
      deadlines        = task_deadlines,
      lambda_l_default = lambda_l
    ),
    pattern   = map(exp3_param_grid),
    iteration = "vector"
  ),
  tar_target(exp3_summary_table, exp3_aggregate(exp3_results_raw)),

  # ===========================================================================
  # Experiment 4: the architecture x smoothing factorial
  #
  # Encapsulation (per-tier market versus integrator slice) crossed with EMA
  # price smoothing, all four cells at efficiency 1.0 and one common price
  # step, so neither factor carries the other's effect. The legacy `hybrid`
  # level is not a cell: it adds an assumed demand reduction, and it is now
  # reported only by the sensitivity sweep.
  # ===========================================================================
  tar_target(
    exp4_param_grid,
    tidyr::expand_grid(
      architecture = c("naive", "naive_ema", "hybrid_noema", "hybrid_ema"),
      graph_type   = c("sp", "entangled"),
      load_level   = c("medium", "high"),
      N            = c(20L, 40L, 60L, 80L),
      seed         = seq_len(n_seeds)
    )
  ),
  tar_target(
    exp4_results_raw,
    exp4_run_single(
      architecture     = exp4_param_grid$architecture,
      graph_type       = exp4_param_grid$graph_type,
      load_level       = exp4_param_grid$load_level,
      N                = exp4_param_grid$N,
      seed             = exp4_param_grid$seed,
      n_rounds         = n_rounds,
      deadlines        = task_deadlines,
      lambda_l_default = lambda_l,
      integ_efficiency = ifelse(
        exp4_param_grid$graph_type == "sp",
        integ_efficiency_sp,
        integ_efficiency_ent
      ),
      # One tatonnement step for both the per-tier market and the slice, named
      # at the call site because a difference here is a confound, not a setting.
      eta              = integ_eta,
      integ_eta        = integ_eta
    ),
    pattern   = map(exp4_param_grid),
    iteration = "vector"
  ),
  tar_target(exp4_summary_table, exp4_aggregate(exp4_results_raw)),

  # ===========================================================================
  # Experiment 5: Hybrid x Governance interaction (ablation completion)
  # ===========================================================================
  tar_target(
    exp5_param_grid,
    tidyr::expand_grid(
      architecture = c("naive", "hybrid"),
      policy       = c("none", "strict"),
      graph_type   = c("tree", "sp", "entangled"),
      load_level   = c("medium", "high"),
      seed         = seq_len(n_seeds)
    )
  ),
  tar_target(
    exp5_results_raw,
    exp5_run_single(
      architecture     = exp5_param_grid$architecture,
      policy           = exp5_param_grid$policy,
      graph_type       = exp5_param_grid$graph_type,
      load_level       = exp5_param_grid$load_level,
      N                = n_agents[[exp5_param_grid$graph_type]],
      seed             = exp5_param_grid$seed,
      n_rounds         = n_rounds,
      deadlines        = task_deadlines,
      lambda_l_default = lambda_l,
      integ_efficiency = ifelse(
        exp5_param_grid$graph_type == "entangled",
        integ_efficiency_ent,
        integ_efficiency_sp
      ),
      integ_eta        = integ_eta
    ),
    pattern   = map(exp5_param_grid),
    iteration = "vector"
  ),
  tar_target(exp5_summary_table, exp5_aggregate(exp5_results_raw)),

  # ===========================================================================
  # Experiment 6: Mechanism Ablation
  # ===========================================================================
  tar_target(exp6_param_grid, exp6_mechanism_grid(n_seeds)),
  tar_target(
    exp6_results_raw,
    exp6_run_single(
      mechanism        = exp6_param_grid$mechanism,
      architecture     = exp6_param_grid$architecture,
      graph_type       = exp6_param_grid$graph_type,
      load_level       = exp6_param_grid$load_level,
      p_post_k         = exp6_param_grid$p_post_k,
      N                = n_agents[[exp6_param_grid$graph_type]],
      seed             = exp6_param_grid$seed,
      n_rounds         = n_rounds,
      deadlines        = task_deadlines,
      lambda_l_default = lambda_l,
      integ_efficiency = ifelse(
        exp6_param_grid$graph_type == "entangled",
        integ_efficiency_ent,
        integ_efficiency_sp
      ),
      integ_eta        = integ_eta
    ),
    pattern   = map(exp6_param_grid),
    iteration = "vector"
  ),
  tar_target(exp6_summary_table, exp6_aggregate(exp6_results_raw)),

  # ===========================================================================
  # Measured agentic workload profile (tracked as a file dependency)
  # ===========================================================================
  # The profile parameterises the agentic environment's per-tier base latencies,
  # its deadlines and its value-decay rate. Tracking it as a file target means a
  # regenerated profile invalidates the results it parameterises, rather than
  # leaving them stale until someone runs the unit tests.
  tar_target(agentic_profile_file, agentic_profile_files()[1], format = "file"),

  # ===========================================================================
  # Experiment 7: VCG/DSIC incentive compatibility (strategic-bidding regret)
  # ===========================================================================
  tar_target(
    exp7_param_grid,
    tidyr::expand_grid(
      graph_type = c("tree", "sp", "agentic"),
      seed       = seq_len(n_seeds)
    )
  ),
  tar_target(
    exp7_results_raw,
    exp7a_run_single(
      graph_type = exp7_param_grid$graph_type,
      load_level = "high",
      N          = 8L,
      seed       = exp7_param_grid$seed,
      cap        = ifelse(exp7_param_grid$graph_type == "agentic", 12, 30),
      # The incentive arm and the stability arm share one agentic environment:
      # deadlines and value-decay rate rescaled to its measured critical path.
      deadlines  = if (exp7_param_grid$graph_type == "agentic") {
        agentic_deadlines(path = agentic_profile_file)
      } else {
        task_deadlines
      },
      lambda_l_default = if (exp7_param_grid$graph_type == "agentic") {
        agentic_lambda_l(path = agentic_profile_file)
      } else {
        lambda_l
      },
      n_rounds   = 30L
    ),
    pattern   = map(exp7_param_grid),
    iteration = "vector"
  ),
  tar_target(exp7_summary_table, exp7_aggregate(exp7_results_raw)),

  # Exp.7b: the second arm of the same experiment. Same grid, same cells, same
  # saturating caps; the agent's strategy set is the named finite set of
  # non-uniform JOINT misreports and the reported statistic is the worst case
  # over it (br_gain_*: a profitable deviation is positive).
  tar_target(
    exp7b_results_raw,
    exp7b_run_single(
      graph_type = exp7_param_grid$graph_type,
      load_level = "high",
      N          = 8L,
      seed       = exp7_param_grid$seed,
      cap        = ifelse(exp7_param_grid$graph_type == "agentic", 12, 30),
      deadlines  = if (exp7_param_grid$graph_type == "agentic") {
        agentic_deadlines(path = agentic_profile_file)
      } else {
        task_deadlines
      },
      lambda_l_default = if (exp7_param_grid$graph_type == "agentic") {
        agentic_lambda_l(path = agentic_profile_file)
      } else {
        lambda_l
      },
      n_rounds   = 30L
    ),
    pattern   = map(exp7_param_grid),
    iteration = "vector"
  ),
  tar_target(exp7b_summary_table, exp7b_aggregate(exp7b_results_raw)),

  # ===========================================================================
  # Experiment 9: Real agentic workload (exp4 on agentic topology, N=200)
  # ===========================================================================
  tar_target(
    exp9_param_grid,
    tidyr::expand_grid(
      architecture = c("naive", "hybrid"),
      load_level   = c("medium", "high"),
      N            = 200L,
      seed         = seq_len(n_seeds)
    )
  ),
  tar_target(
    exp9_results_raw,
    exp4_run_single(
      architecture     = exp9_param_grid$architecture,
      graph_type       = "agentic",
      load_level       = exp9_param_grid$load_level,
      N                = exp9_param_grid$N,
      seed             = exp9_param_grid$seed,
      n_rounds         = n_rounds,
      # Rescaled to the agentic environment's own measured critical path; the
      # nominal set is unchanged for every other experiment.
      deadlines        = agentic_deadlines(path = agentic_profile_file),
      lambda_l_default = agentic_lambda_l(path = agentic_profile_file),
      integ_efficiency = integ_efficiency_sp,
      integ_eta        = integ_eta
    ),
    pattern   = map(exp9_param_grid),
    iteration = "vector"
  ),
  tar_target(exp9_summary_table, exp4_aggregate(exp9_results_raw)),

  # ===========================================================================
  # Experiment 10: Prop.3 faithfulness violation (hybrid, slice_inflation swept)
  # ===========================================================================
  tar_target(
    exp10_param_grid,
    tidyr::expand_grid(
      slice_inflation = c(1.0, 1.5, 2.0),
      graph_type      = c("sp", "entangled"),
      load_level      = "high",
      seed            = seq_len(n_seeds)
    )
  ),
  tar_target(
    exp10_results_raw,
    dplyr::mutate(
      exp4_run_single(
        "hybrid",
        graph_type       = exp10_param_grid$graph_type,
        load_level       = exp10_param_grid$load_level,
        N                = n_agents[[exp10_param_grid$graph_type]],
        seed             = exp10_param_grid$seed,
        n_rounds         = n_rounds,
        deadlines        = task_deadlines,
        lambda_l_default = lambda_l,
        integ_efficiency = ifelse(
          exp10_param_grid$graph_type == "entangled",
          integ_efficiency_ent,
          integ_efficiency_sp
        ),
        integ_eta        = integ_eta,
        slice_inflation  = exp10_param_grid$slice_inflation
      ),
      slice_inflation = exp10_param_grid$slice_inflation
    ),
    pattern   = map(exp10_param_grid),
    iteration = "vector"
  ),
  tar_target(exp10_summary_table, exp10_aggregate(exp10_results_raw)),

  # ===========================================================================
  # Experiment 11: heterogeneous per-task recipes over one fixed DAG
  # ===========================================================================
  # One DAG (tree), one agent count, one load level. Contention is varied
  # through CAPACITY rather than arrival rate so the per-round task count stays
  # inside the exact packer's enumeration budget. N = 8 for the same reason: the
  # welfare gap this experiment reports is measured against a brute-force
  # optimum, which is what makes it the one welfare number in the study that is
  # not greedy measured against greedy.
  tar_target(
    exp11_param_grid,
    tidyr::expand_grid(
      arm  = exp11_arms(),
      cap  = c(6, 9, 12),
      seed = seq_len(n_seeds)
    )
  ),
  tar_target(
    exp11_results_raw,
    exp11_run_single(
      arm              = exp11_param_grid$arm,
      cap              = exp11_param_grid$cap,
      seed             = exp11_param_grid$seed,
      N                = 8L,
      n_rounds         = 100L,
      deadlines        = task_deadlines,
      lambda_l_default = lambda_l
    ),
    pattern   = map(exp11_param_grid),
    iteration = "vector"
  ),
  tar_target(exp11_summary_table, exp11_aggregate(exp11_results_raw)),
  # The two closed-form instruments: measured over-commitment against the
  # predicted factor, and the price of the safe interface. Both are exact and
  # cost nothing, so they are targets rather than prose.
  tar_target(exp11_overcommitment_table, exp11_overcommitment_sweep()),
  tar_target(exp11_inner_exposure_table, exp11_inner_exposure_control()),

  # ===========================================================================
  # Experiment 12: Encapsulation overhead (hybrid, enc_overhead_ms swept)
  # ===========================================================================
  tar_target(
    exp12_param_grid,
    tidyr::expand_grid(
      enc_overhead_ms = c(0, 25, 50),
      graph_type      = c("sp", "entangled"),
      load_level      = "high",
      seed            = seq_len(n_seeds)
    )
  ),
  tar_target(
    exp12_results_raw,
    dplyr::mutate(
      exp4_run_single(
        "hybrid",
        graph_type       = exp12_param_grid$graph_type,
        load_level       = exp12_param_grid$load_level,
        N                = n_agents[[exp12_param_grid$graph_type]],
        seed             = exp12_param_grid$seed,
        n_rounds         = n_rounds,
        deadlines        = task_deadlines,
        lambda_l_default = lambda_l,
        integ_efficiency = ifelse(
          exp12_param_grid$graph_type == "entangled",
          integ_efficiency_ent,
          integ_efficiency_sp
        ),
        integ_eta        = integ_eta,
        enc_overhead_ms  = exp12_param_grid$enc_overhead_ms
      ),
      enc_overhead_ms = exp12_param_grid$enc_overhead_ms
    ),
    pattern   = map(exp12_param_grid),
    iteration = "vector"
  ),
  tar_target(exp12_summary_table, exp12_aggregate(exp12_results_raw)),

  # ===========================================================================
  # Experiment 14: Parameter sensitivity table, one branch per (parameter, level)
  # ===========================================================================
  # Branching rather than looping inside one target: the cells are independent
  # (each run reseeds), so they run on all workers instead of one, and a single
  # cell can be invalidated or retried without recomputing the whole sweep.
  tar_target(exp14_sweep_cells, exp14_sweep_grid()),
  tar_target(
    exp14_sensitivity,
    exp14_sensitivity_row(
      parameter  = exp14_sweep_cells$parameter,
      level      = exp14_sweep_cells$level,
      topologies = c("sp", "entangled"),
      seeds      = seq_len(5),
      N          = n_agents,
      load_level = "high",
      n_rounds   = n_rounds
    ),
    pattern   = map(exp14_sweep_cells),
    iteration = "vector"
  ),

  # ===========================================================================
  # The node-level evaluation: the leaf-block market at the evaluation's scale
  # ===========================================================================
  # One node set, one capacity vector, one arrival stream. The arms differ in
  # which leaves each internal node reaches. Populations put all three
  # instances at the same offered load, so the level names carry a structural
  # treatment and not a demand profile.
  #
  # Every target here takes a NEW name, so no per-tier object is overwritten
  # and the two evaluations sit side by side in one store.
  tar_target(node_agent_counts, node_agents()),
  tar_target(node_lambda, node_lambda_l()),
  tar_target(node_instance_table, node_instance_diagnostics()),

  # -- structure at matched load ---------------------------------------------
  tar_target(
    node_exp1_param_grid,
    crossing(graph_type = graph_types, load_level = load_levels,
             seed = seq_len(n_seeds))
  ),
  tar_target(
    node_exp1_results_raw,
    node_run_single(
      graph_type       = node_exp1_param_grid$graph_type,
      load_level       = node_exp1_param_grid$load_level,
      seed             = node_exp1_param_grid$seed,
      N                = node_agent_counts[[node_exp1_param_grid$graph_type]],
      n_rounds         = n_rounds,
      deadlines        = task_deadlines,
      lambda_l_default = node_lambda
    ),
    pattern   = map(node_exp1_param_grid),
    iteration = "vector"
  ),
  tar_target(node_exp1_summary_table,
             node_aggregate(node_exp1_results_raw, c("graph_type", "load_level"))),
  tar_target(node_stats_exp1, stat_exp1(bind_rows(node_exp1_results_raw))),

  # -- the population sweep --------------------------------------------------
  tar_target(node_exp2_param_grid, node_sweep_grid(n_seeds)),
  tar_target(
    node_exp2_results_raw,
    node_run_single(
      graph_type       = node_exp2_param_grid$graph_type,
      load_level       = node_exp2_param_grid$load_level,
      seed             = node_exp2_param_grid$seed,
      N                = node_exp2_param_grid$N,
      n_rounds         = n_rounds,
      deadlines        = task_deadlines,
      lambda_l_default = node_lambda,
      # The sweep's instrument is the onset of a non-zero price series, which
      # the exact reference has no part in and which pays for it 1680 times.
      exact_reference  = FALSE
    ),
    pattern   = map(node_exp2_param_grid),
    iteration = "vector"
  ),
  tar_target(node_exp2_summary_table,
             node_aggregate(node_exp2_results_raw,
                            c("graph_type", "load_level", "N"))),
  tar_target(node_stats_exp2, stat_exp2(bind_rows(node_exp2_results_raw),
                                        rho_fn = node_rho_bottleneck)),
  # The onset law read against the sweep: the expected number of price-moving
  # rounds per seed at each grid population, from the first node crossing.
  tar_target(
    node_exp2_onset_law,
    purrr::pmap_dfr(
      dplyr::distinct(node_exp2_param_grid, graph_type, load_level),
      function(graph_type, load_level)
        node_onset_law(node_instance(graph_type), "uniform", load_level,
                       sort(unique(node_exp2_param_grid$N)), n_rounds) %>%
          dplyr::mutate(graph_type = graph_type, load_level = load_level,
                        .before = 1))
  ),

  # -- governance: the dose-and-determinant instrument -----------------------
  tar_target(
    node_exp3_param_grid,
    tidyr::expand_grid(
      policy     = c("none", "trust", "locality", "role",
                     "residency", "residency_sliced"),
      graph_type = graph_types,
      load_level = c("medium", "high"),
      # Two mixes, two questions. The exactness question needs the crossing to
      # bind, which it does only at uniform shares (under the skewed mix the
      # crossing instance binds on e1 alone). The coupled residency pair needs
      # asymmetric shares for the slice to strand anything: at uniform shares
      # both half budgets are exhausted. No contrast crosses mixes.
      leaf_mix   = c("uniform", "skewed"),
      seed       = seq_len(n_seeds))
  ),
  tar_target(
    node_exp3_results_raw,
    node_run_single(
      graph_type       = node_exp3_param_grid$graph_type,
      load_level       = node_exp3_param_grid$load_level,
      seed             = node_exp3_param_grid$seed,
      N                = node_agent_counts[[node_exp3_param_grid$graph_type]],
      policy           = node_exp3_param_grid$policy,
      leaf_mix         = node_exp3_param_grid$leaf_mix,
      n_rounds         = n_rounds,
      deadlines        = task_deadlines,
      lambda_l_default = node_lambda
    ),
    pattern   = map(node_exp3_param_grid),
    iteration = "vector"
  ),
  tar_target(node_exp3_summary_table,
             node_aggregate(node_exp3_results_raw,
                            c("policy", "graph_type", "load_level", "leaf_mix"))),
  tar_target(node_stats_exp3,
             node_stat_factor(bind_rows(node_exp3_results_raw), "policy",
                              cell_vars = c("graph_type", "load_level", "leaf_mix"))),

  # -- the cap-target factor: the exactness-repair instrument ----------------
  tar_target(
    node_exp3b_param_grid,
    tidyr::expand_grid(cap_target = c("l1", "l2", "l3", "l4"),
                       load_level = c("medium", "high"),
                       seed       = seq_len(n_seeds))
  ),
  tar_target(
    node_exp3b_results_raw,
    node_run_single(
      graph_type       = "entangled",
      load_level       = node_exp3b_param_grid$load_level,
      seed             = node_exp3b_param_grid$seed,
      N                = node_agent_counts[["entangled"]],
      cap_target       = node_exp3b_param_grid$cap_target,
      n_rounds         = n_rounds,
      deadlines        = task_deadlines,
      lambda_l_default = node_lambda
    ),
    pattern   = map(node_exp3b_param_grid),
    iteration = "vector"
  ),
  tar_target(node_exp3b_summary_table,
             node_aggregate(node_exp3b_results_raw,
                            c("cap_target", "load_level"))),

  # -- the architecture x smoothing factorial --------------------------------
  tar_target(
    node_exp4_param_grid,
    tidyr::expand_grid(
      architecture = c("naive", "naive_ema", "hybrid_noema", "hybrid_ema"),
      graph_type   = graph_types,
      load_level   = c("medium", "high"),
      seed         = seq_len(n_seeds))
  ),
  tar_target(
    node_exp4_results_raw,
    node_run_single(
      graph_type       = node_exp4_param_grid$graph_type,
      load_level       = node_exp4_param_grid$load_level,
      seed             = node_exp4_param_grid$seed,
      N                = node_agent_counts[[node_exp4_param_grid$graph_type]],
      architecture     = node_exp4_param_grid$architecture,
      n_rounds         = n_rounds,
      deadlines        = task_deadlines,
      lambda_l_default = node_lambda
    ),
    pattern   = map(node_exp4_param_grid),
    iteration = "vector"
  ),
  tar_target(node_exp4_summary_table,
             node_aggregate(node_exp4_results_raw,
                            c("architecture", "graph_type", "load_level"))),
  tar_target(node_stats_exp4, stat_exp4(bind_rows(node_exp4_results_raw))),
  # One matched population per topology, so N is not a cell variable here: as a
  # factor of the model it would carry a single level inside every cell.
  tar_target(node_stats_exp4_factorial,
             stat_exp4_factorial(bind_rows(node_exp4_results_raw),
                                 cell_vars = c("graph_type", "load_level"))),

  # -- the interface block, the node-level successor of the faithfulness arm --
  tar_target(
    node_exp10_param_grid,
    tidyr::expand_grid(interface  = c("off", "inner", "maxflow"),
                       graph_type = graph_types,
                       # The skewed mix is where an over-stated scalar
                       # over-commits on the exposed node (at uniform shares
                       # that over-commitment is exactly zero, a knife-edge);
                       # the uniform mix is the matched-load cell every other
                       # experiment reads at. No contrast crosses mixes.
                       leaf_mix   = c("uniform", "skewed"),
                       seed       = seq_len(n_seeds))
  ),
  tar_target(
    node_exp10_results_raw,
    node_run_single(
      graph_type       = node_exp10_param_grid$graph_type,
      load_level       = "high",
      seed             = node_exp10_param_grid$seed,
      N                = node_agent_counts[[node_exp10_param_grid$graph_type]],
      interface        = node_exp10_param_grid$interface,
      leaf_mix         = node_exp10_param_grid$leaf_mix,
      n_rounds         = n_rounds,
      deadlines        = task_deadlines,
      lambda_l_default = node_lambda
    ),
    pattern   = map(node_exp10_param_grid),
    iteration = "vector"
  ),
  tar_target(node_exp10_summary_table,
             node_aggregate(node_exp10_results_raw,
                            c("interface", "graph_type", "leaf_mix"))),
  tar_target(node_stats_exp10,
             node_stat_factor(bind_rows(node_exp10_results_raw), "interface",
                              cell_vars = c("graph_type", "leaf_mix"))),

  # -- architecture x governance ---------------------------------------------
  tar_target(
    node_exp5_param_grid,
    tidyr::expand_grid(architecture = c("naive", "hybrid_ema"),
                       policy       = c("none", "locality"),
                       graph_type   = graph_types,
                       load_level   = c("medium", "high"),
                       seed         = seq_len(n_seeds))
  ),
  tar_target(
    node_exp5_results_raw,
    node_run_single(
      graph_type       = node_exp5_param_grid$graph_type,
      load_level       = node_exp5_param_grid$load_level,
      seed             = node_exp5_param_grid$seed,
      N                = node_agent_counts[[node_exp5_param_grid$graph_type]],
      architecture     = node_exp5_param_grid$architecture,
      policy           = node_exp5_param_grid$policy,
      n_rounds         = n_rounds,
      deadlines        = task_deadlines,
      lambda_l_default = node_lambda
    ),
    pattern   = map(node_exp5_param_grid),
    iteration = "vector"
  ),
  tar_target(node_exp5_summary_table,
             node_aggregate(node_exp5_results_raw,
                            c("architecture", "policy", "graph_type",
                              "load_level"))),
  tar_target(node_stats_exp5,
             node_stat_factor(bind_rows(node_exp5_results_raw), "policy",
                              cell_vars = c("architecture", "graph_type", "load_level"),
                              interaction_vars = c("architecture", "policy"))),

  # -- the mechanism ablation ------------------------------------------------
  tar_target(node_exp6_param_grid, node_exp6_mechanism_grid(n_seeds)),
  tar_target(
    node_exp6_results_raw,
    node_run_single(
      graph_type       = node_exp6_param_grid$graph_type,
      load_level       = node_exp6_param_grid$load_level,
      seed             = node_exp6_param_grid$seed,
      N                = node_agent_counts[[node_exp6_param_grid$graph_type]],
      mechanism        = node_exp6_param_grid$mechanism,
      p_post_k         = node_exp6_param_grid$p_post_k,
      architecture     = ifelse(node_exp6_param_grid$architecture == "hybrid",
                                "hybrid_noema", "naive"),
      exec_clamp       = node_exp6_param_grid$exec_clamp,
      queue_coef       = node_exp6_param_grid$queue_coef,
      n_rounds         = n_rounds,
      deadlines        = task_deadlines,
      lambda_l_default = node_lambda
    ) %>% dplyr::mutate(congestion = node_exp6_param_grid$congestion),
    pattern   = map(node_exp6_param_grid),
    iteration = "vector"
  ),
  # The congestion level is a factor of the design: every arm runs at both, so
  # a table grouped without it averages the two queue terms into a setting no
  # cell was run at.
  tar_target(node_exp6_summary_table,
             node_aggregate(node_exp6_results_raw,
                            c("mechanism", "p_post_k", "architecture",
                              "graph_type", "load_level", "congestion"))),
  tar_target(node_stats_exp6, stat_exp6(bind_rows(node_exp6_results_raw))),
  # Every arm as a point in the posted family's own plane, so the comparison
  # is read at matched congestion rather than at whatever volume each arm
  # happened to admit.
  tar_target(node_exp6_frontier,
             node_frontier_table(bind_rows(node_exp6_results_raw))),

  # -- one tuned knob per mechanism, chosen and reported on disjoint seeds ---
  # The knob is chosen on the tuning seeds and every reported number comes
  # from seeds no cell of the mechanism block has run, so a maximum over
  # levels cannot travel into the comparison it is part of.
  tar_target(node_exp6_tuning_grid,
             node_tuning_grid(node_tuning_split()$tuning)),
  tar_target(
    node_exp6_tuning_raw,
    node_run_single(
      graph_type       = node_exp6_tuning_grid$graph_type,
      load_level       = node_exp6_tuning_grid$load_level,
      seed             = node_exp6_tuning_grid$seed,
      N                = node_agent_counts[[node_exp6_tuning_grid$graph_type]],
      mechanism        = node_exp6_tuning_grid$mechanism,
      p_post_k         = node_exp6_tuning_grid$p_post_k,
      reserve_markup   = node_exp6_tuning_grid$reserve_markup,
      architecture     = ifelse(node_exp6_tuning_grid$architecture == "hybrid",
                                "hybrid_noema", "naive"),
      exec_clamp       = node_exp6_tuning_grid$exec_clamp,
      queue_coef       = node_exp6_tuning_grid$queue_coef,
      n_rounds         = n_rounds,
      deadlines        = task_deadlines,
      lambda_l_default = node_lambda
    ) %>% dplyr::mutate(congestion = node_exp6_tuning_grid$congestion),
    pattern   = map(node_exp6_tuning_grid),
    iteration = "vector"
  ),
  tar_target(node_exp6_eval_grid,
             node_eval_grid(bind_rows(node_exp6_tuning_raw),
                            node_tuning_split()$evaluation)),
  tar_target(
    node_exp6_eval_raw,
    node_run_single(
      graph_type       = node_exp6_eval_grid$graph_type,
      load_level       = node_exp6_eval_grid$load_level,
      seed             = node_exp6_eval_grid$seed,
      N                = node_agent_counts[[node_exp6_eval_grid$graph_type]],
      mechanism        = node_exp6_eval_grid$mechanism,
      p_post_k         = node_exp6_eval_grid$p_post_k,
      reserve_markup   = node_exp6_eval_grid$reserve_markup,
      architecture     = node_eval_architecture(node_exp6_eval_grid$architecture),
      exec_clamp       = node_exp6_eval_grid$exec_clamp,
      queue_coef       = node_exp6_eval_grid$queue_coef,
      n_rounds         = n_rounds,
      deadlines        = task_deadlines,
      lambda_l_default = node_lambda
    ) %>% dplyr::mutate(congestion = node_exp6_eval_grid$congestion),
    pattern   = map(node_exp6_eval_grid),
    iteration = "vector"
  ),
  # The tuning frame travels with the evaluation results: whether the welfare
  # beside a chosen knob was flat is a property of the frame the knob was
  # chosen on, and it is what separates a knob the grid limited from one the
  # tie broke inside a plateau.
  tar_target(node_exp6_tuned,
             node_tuned_table(bind_rows(node_exp6_eval_raw),
                              bind_rows(node_exp6_tuning_raw))),

  # -- the sensitivity beside the numbers, not after them -------------------
  # One factor at a time from the headline setting, plus the queue term
  # retuned to the load response the emulated testbed measured.
  # -- does the price process find a clearing vector, and in how many steps -
  # The residual column says how far a truncated walk got; only complementary
  # slackness separates a market that cleared from one that overshot. Its own
  # driver and its own targets: the arms already measured must not move.
  tar_target(node_exp6_convergence_grid, node_convergence_grid(n_seeds)),
  tar_target(
    node_exp6_convergence,
    node_convergence_run(
      graph_type       = node_exp6_convergence_grid$graph_type,
      mechanism        = node_exp6_convergence_grid$mechanism,
      load_level       = "high",
      N                = node_agent_counts[[node_exp6_convergence_grid$graph_type]],
      seed             = node_exp6_convergence_grid$seed,
      leaf_mix         = node_exp6_convergence_grid$leaf_mix,
      iters            = node_exp6_convergence_grid$iters,
      save_profile     = node_exp6_convergence_grid$save_profile,
      n_rounds         = 20L,
      deadlines        = task_deadlines,
      lambda_l_default = node_lambda
    ),
    pattern   = map(node_exp6_convergence_grid),
    iteration = "vector"
  ),
  tar_target(node_exp6_convergence_summary,
             node_convergence_summary(bind_rows(node_exp6_convergence))),

  # -- is the price a property of the round or of where the walk began ------
  tar_target(node_exp6_determinacy_grid, node_determinacy_grid(n_seeds)),
  tar_target(
    node_exp6_determinacy,
    node_determinacy_run(
      graph_type       = node_exp6_determinacy_grid$graph_type,
      mechanism        = node_exp6_determinacy_grid$mechanism,
      load_level       = "high",
      N                = node_agent_counts[[node_exp6_determinacy_grid$graph_type]],
      seed             = node_exp6_determinacy_grid$seed,
      leaf_mix         = node_exp6_determinacy_grid$leaf_mix,
      n_rounds         = 20L,
      iters            = 1000L,
      deadlines        = task_deadlines,
      lambda_l_default = node_lambda
    ),
    pattern   = map(node_exp6_determinacy_grid),
    iteration = "vector"
  ),
  tar_target(node_exp6_determinacy_summary,
             node_determinacy_summary(bind_rows(node_exp6_determinacy))),

  # -- what a small change in one report does to the whole allocation -------
  tar_target(node_exp6_report_stability_grid, node_report_stability_grid(n_seeds)),
  tar_target(
    node_exp6_report_stability,
    node_report_stability_run(
      graph_type       = node_exp6_report_stability_grid$graph_type,
      load_level       = "high",
      N                = node_agent_counts[[node_exp6_report_stability_grid$graph_type]],
      seed             = node_exp6_report_stability_grid$seed,
      leaf_mix         = node_exp6_report_stability_grid$leaf_mix,
      n_rounds         = 20L,
      deadlines        = task_deadlines,
      lambda_l_default = node_lambda
    ),
    pattern   = map(node_exp6_report_stability_grid),
    iteration = "vector"
  ),
  tar_target(node_exp6_report_stability_summary,
             node_report_stability_summary(bind_rows(node_exp6_report_stability))),

  # -- what the market does after something breaks --------------------------
  tar_target(node_exp6_shock_grid, node_shock_grid(n_seeds)),
  tar_target(
    node_exp6_shock,
    node_shock_run(
      graph_type       = node_exp6_shock_grid$graph_type,
      mechanism        = node_exp6_shock_grid$mechanism,
      architecture     = node_exp6_shock_grid$architecture,
      shock            = node_exp6_shock_grid$shock,
      load_level       = "high",
      N                = node_agent_counts[[node_exp6_shock_grid$graph_type]],
      seed             = node_exp6_shock_grid$seed,
      leaf_mix         = node_exp6_shock_grid$leaf_mix,
      n_rounds         = n_rounds,
      deadlines        = task_deadlines,
      lambda_l_default = node_lambda
    ),
    pattern   = map(node_exp6_shock_grid),
    iteration = "vector"
  ),
  tar_target(node_exp6_shock_summary,
             node_shock_summary(bind_rows(node_exp6_shock))),

  # -- does a clearing price exist for the round at all ---------------------
  # Prior to whether the walk finds one: the round's packing is relaxed and
  # solved twice, and an anonymous linear price supports an allocation only
  # where the two optima agree. Off the market, on the offered demand.
  tar_target(node_exp6_existence_grid, node_existence_grid(n_seeds)),
  tar_target(
    node_exp6_existence,
    node_existence_run(
      graph_type       = node_exp6_existence_grid$graph_type,
      leaf_mix         = node_exp6_existence_grid$leaf_mix,
      load_level       = "high",
      N                = node_agent_counts[[node_exp6_existence_grid$graph_type]],
      seed             = node_exp6_existence_grid$seed,
      n_rounds         = 20L,
      deadlines        = task_deadlines,
      lambda_l_default = node_lambda
    ),
    pattern   = map(node_exp6_existence_grid),
    iteration = "vector"
  ),
  tar_target(node_exp6_existence_summary,
             node_existence_summary(bind_rows(node_exp6_existence))),

  # -- where existence and exactness actually fail --------------------------
  # Every shipped instance is laminar or laminar plus one set, so its packing
  # relaxation is integral at every value vector and the topology question is
  # settled there by a theorem. This block leaves that region: generated
  # leaf-block families stratified by their crossing structure, plus the
  # three-leaf triangle, the shipped T / X / S and the deployed pipeline
  # templates as named rows. Off the market, on a task-by-node matrix, like
  # the existence block. Seeds loop inside a branch so the branch count is
  # instances times mixes rather than times seeds as well.
  tar_target(node_exp6_sweep_instances, sweep_instances(n_per_stratum = 20L)),
  tar_target(node_exp6_sweep_grid, sweep_grid(node_exp6_sweep_instances)),
  tar_target(
    node_exp6_sweep,
    sweep_run(
      family    = node_exp6_sweep_grid$family[[1]],
      instance  = node_exp6_sweep_grid$instance,
      stratum   = node_exp6_sweep_grid$stratum,
      leaf_mix  = node_exp6_sweep_grid$leaf_mix,
      seeds     = seq_len(n_seeds),
      n_rounds  = 20L,
      deadlines = task_deadlines,
      lambda_l  = node_lambda
    ),
    pattern   = map(node_exp6_sweep_grid),
    iteration = "vector"
  ),
  tar_target(node_exp6_sweep_summary,
             sweep_summary(bind_rows(node_exp6_sweep))),
  tar_target(node_exp6_sweep_by_instance,
             sweep_by_instance(bind_rows(node_exp6_sweep),
                               node_exp6_sweep_instances)),

  tar_target(node_exp6_sensitivity_grid, node_sensitivity_grid()),
  tar_target(
    node_exp6_sensitivity,
    node_run_single(
      graph_type       = node_exp6_sensitivity_grid$graph_type,
      load_level       = node_exp6_sensitivity_grid$load_level,
      seed             = node_exp6_sensitivity_grid$seed,
      N                = node_agent_counts[[node_exp6_sensitivity_grid$graph_type]],
      mechanism        = node_exp6_sensitivity_grid$mechanism,
      p_post_k         = node_exp6_sensitivity_grid$p_post_k,
      architecture     = node_eval_architecture(
                           node_exp6_sensitivity_grid$architecture),
      alpha            = node_exp6_sensitivity_grid$alpha,
      exec_clamp       = node_exp6_sensitivity_grid$exec_clamp,
      queue_coef       = node_exp6_sensitivity_grid$queue_coef,
      n_rounds         = n_rounds,
      deadlines        = task_deadlines,
      lambda_l_default = node_exp6_sensitivity_grid$lambda_l
    ) %>% dplyr::mutate(setting = node_exp6_sensitivity_grid$setting),
    pattern   = map(node_exp6_sensitivity_grid),
    iteration = "vector"
  ),

  # -- the incentive arms, under the certificate -----------------------------
  # The certified arms run at the evaluation populations, where the capacity
  # vector already binds; the uncertified converse runs at the population the
  # joint-misreport strategy set can be enumerated at, with the capacity
  # scaled so it binds there too.
  tar_target(
    node_exp7_param_grid,
    tidyr::expand_grid(graph_type = graph_types, seed = seq_len(n_seeds))
  ),
  tar_target(
    node_exp7a_results_raw,
    exp7a_run_single(
      graph_type = node_exp7_param_grid$graph_type,
      load_level = "high",
      N          = ifelse(node_exp7_param_grid$graph_type == "entangled", 8L,
                          node_agent_counts[[node_exp7_param_grid$graph_type]]),
      seed       = node_exp7_param_grid$seed,
      substrate  = "node",
      cap_scale  = ifelse(node_exp7_param_grid$graph_type == "entangled", 0.1, 1.0),
      deadlines  = task_deadlines,
      lambda_l_default = node_lambda,
      n_rounds   = 30L
    ),
    pattern   = map(node_exp7_param_grid),
    iteration = "vector"
  ),
  tar_target(node_exp7a_summary_table,
             exp7_aggregate(node_exp7a_results_raw)),
  tar_target(node_stats_exp7a, stat_exp7(bind_rows(node_exp7a_results_raw))),
  tar_target(
    node_exp7b_results_raw,
    exp7b_run_single(
      graph_type = node_exp7_param_grid$graph_type,
      load_level = "high",
      N          = ifelse(node_exp7_param_grid$graph_type == "entangled", 8L,
                          node_agent_counts[[node_exp7_param_grid$graph_type]]),
      seed       = node_exp7_param_grid$seed,
      substrate  = "node",
      cap_scale  = ifelse(node_exp7_param_grid$graph_type == "entangled", 0.1, 1.0),
      deadlines  = task_deadlines,
      lambda_l_default = node_lambda,
      n_rounds   = 30L
    ),
    pattern   = map(node_exp7_param_grid),
    iteration = "vector"
  ),
  tar_target(node_exp7b_summary_table,
             exp7b_aggregate(node_exp7b_results_raw)),
  tar_target(node_stats_exp7b, stat_exp7b(bind_rows(node_exp7b_results_raw))),

  # -- the measured workload on the node substrate ---------------------------
  # Both recordings are tracked as files, so a re-recording invalidates every
  # arm that reads them instead of leaving them stale.
  tar_target(node_agentic_files, agentic_profile_files(), format = "file"),
  tar_target(node_agentic_specs, list(
    single = agentic_union_spec(node_agentic_files[1]),
    union  = agentic_union_spec(node_agentic_files))),
  tar_target(
    node_exp9_param_grid,
    tidyr::expand_grid(pattern      = c("single", "union"),
                       architecture = c("naive", "naive_ema"),
                       load_level   = c("medium", "high"),
                       seed         = seq_len(n_seeds))
  ),
  tar_target(
    node_exp9_results_raw,
    {
      sp <- node_agentic_specs[[node_exp9_param_grid$pattern]]
      k  <- node_agentic_constants(sp)
      node_run_single(
        graph_type       = "agentic",
        load_level       = node_exp9_param_grid$load_level,
        seed             = node_exp9_param_grid$seed,
        N                = 200L,
        architecture     = node_exp9_param_grid$architecture,
        spec             = sp,
        n_rounds         = n_rounds,
        deadlines        = k$deadlines,
        lambda_l_default = k$lambda_l
      ) %>% dplyr::mutate(pattern = node_exp9_param_grid$pattern)
    },
    pattern   = map(node_exp9_param_grid),
    iteration = "vector"
  ),
  tar_target(node_exp9_summary_table,
             node_aggregate(node_exp9_results_raw,
                            c("pattern", "architecture", "load_level"))),

  # -- one-at-a-time parameter sensitivity -----------------------------------
  tar_target(
    node_exp14_sensitivity,
    exp14_sensitivity_row(
      parameter  = exp14_sweep_cells$parameter,
      level      = exp14_sweep_cells$level,
      topologies = graph_types,
      seeds      = seq_len(5),
      N          = node_agent_counts,
      load_level = "high",
      n_rounds   = n_rounds,
      substrate  = "node"
    ),
    pattern   = map(exp14_sweep_cells),
    iteration = "vector"
  ),

  # -- the machine-written statistics dump for the node arms -----------------
  # Its own report rather than rows added to the per-tier one: that target
  # depends on every per-tier statistics target, so building it would pull the
  # whole per-tier pipeline in behind it.
  tar_target(node_stats_report, make_stats_report(list(
    exp1 = node_stats_exp1, exp2 = node_stats_exp2, exp3 = node_stats_exp3,
    exp4 = node_stats_exp4, exp4_factorial = node_stats_exp4_factorial,
    exp5 = node_stats_exp5, exp6 = node_stats_exp6,
    exp10 = node_stats_exp10,
    exp7a = node_stats_exp7a, exp7b = node_stats_exp7b,
    # The measured tables of the mechanism block, transcribed in the same
    # shape: a statistic, the seeds behind it, and no test. Both tables are
    # keyed by the congestion level, so the label carries it: without it one
    # key holds the two levels' numbers with nothing to tell them apart.
    # The frontier is a curve as well, and its posted rows differ in nothing
    # but the level they were run at, so the level is part of its key too.
    # The tuned table reports one row per mechanism per cell, so its key is
    # already unique and the chosen level stays a column rather than a label.
    exp6_frontier = node_stats_rows(
      node_exp6_frontier,
      c("welfare", "tokens_admitted", "median_latency", "alloc_ratio",
        "welfare_vs_posted_at_matched_volume",
        "welfare_vs_posted_at_matched_latency"),
      "mechanism",
      cell_vars = c("graph_type", "load_level", "architecture", "congestion",
                    "p_post_k"),
      n_col = "n_seeds"),
    exp6_tuned = node_stats_rows(
      node_exp6_tuned,
      c("welfare", "tokens_admitted", "median_latency", "welfare_over_optimum",
        "p_post_k", "reserve_markup"),
      "mechanism",
      cell_vars = c("graph_type", "load_level", "architecture", "congestion"),
      n_col = "n_eval_seeds"),
    # Averaged over its seeds first, so a cell of the sweep is one number per
    # response rather than one per seed.
    exp6_sensitivity = node_stats_rows(
      node_aggregate(node_exp6_sensitivity,
                     c("setting", "graph_type", "load_level", "architecture",
                       "mechanism")),
      c("welfare", "welfare_over_optimum", "tokens_admitted"),
      "mechanism",
      cell_vars = c("setting", "graph_type", "load_level", "architecture"))))),
  tar_target(
    node_stats_report_file,
    {
      dir.create("results", showWarnings = FALSE, recursive = TRUE)
      readr::write_csv(node_stats_report, "results/node-stats-report.csv")
      "results/node-stats-report.csv"
    },
    format = "file"
  ),

  # ===========================================================================
  # Statistical analysis (all experiments)
  # ===========================================================================
  tar_target(stats_exp1, stat_exp1(bind_rows(exp1_results_raw))),
  tar_target(stats_exp2, stat_exp2(bind_rows(exp2_results_raw,
                                             exp2b_results_raw))),
  tar_target(stats_exp3, stat_exp3(bind_rows(exp3_results_raw))),
  tar_target(stats_exp4, stat_exp4(bind_rows(exp4_results_raw))),
  tar_target(stats_exp4_factorial, stat_exp4_factorial(bind_rows(exp4_results_raw))),
  tar_target(stats_exp4_volatility_reduction,
             stat_exp4_volatility_reduction(bind_rows(exp4_results_raw))),
  tar_target(stats_exp5, stat_exp5(bind_rows(exp5_results_raw))),
  tar_target(stats_exp6, stat_exp6(bind_rows(exp6_results_raw))),
  tar_target(stats_exp7, stat_exp7(bind_rows(exp7_results_raw))),
  tar_target(stats_exp7b, stat_exp7b(bind_rows(exp7b_results_raw))),
  tar_target(stats_exp11, stat_exp11(bind_rows(exp11_results_raw))),

  # Single machine-written dump of every Kruskal-Wallis test and of Exp.7a's
  # regret standard errors, each row carrying the sample size it was computed
  # on. This is what the supplement's statistics are transcribed from.
  tar_target(stats_report, make_stats_report(list(
    exp1 = stats_exp1, exp2 = stats_exp2, exp3 = stats_exp3,
    exp4 = stats_exp4, exp4_factorial = stats_exp4_factorial,
    exp5 = stats_exp5, exp6 = stats_exp6,
    exp7a = stats_exp7, exp7b = stats_exp7b,
    exp11 = stats_exp11))),
  tar_target(
    stats_report_file,
    {
      dir.create("results", showWarnings = FALSE, recursive = TRUE)
      readr::write_csv(stats_report, "results/stats-report.csv")
      "results/stats-report.csv"
    },
    format = "file"
  ),

  # The architecture x smoothing decomposition does not pass through the flat
  # dump, which collects Kruskal-Wallis rows: it is four contrasts with their
  # own CIs and their own sample size. It gets its own written file for the
  # same reason -- the numbers that answer the attribution question are
  # transcribed from a machine-written row, not read off a console.
  tar_target(
    stats_exp4_decomposition_file,
    {
      dir.create("results", showWarnings = FALSE, recursive = TRUE)
      readr::write_csv(stats_exp4_factorial$decomposition,
                       "results/exp4-decomposition.csv")
      "results/exp4-decomposition.csv"
    },
    format = "file"
  ),

  # The headline tail-dispersion reduction is a ratio of tail CVs on a restricted
  # cell set, so neither the flat dump nor the decomposition file can carry it:
  # it gets its own machine-written file, with the per-cell reductions, the cell
  # count and the exclusions beside the median, so the sentence in the abstract
  # is transcribed from a row rather than from a console.
  tar_target(
    stats_exp4_volatility_reduction_file,
    {
      dir.create("results", showWarnings = FALSE, recursive = TRUE)
      readr::write_csv(stats_exp4_volatility_reduction,
                       "results/exp4-volatility-reduction.csv")
      "results/exp4-volatility-reduction.csv"
    },
    format = "file"
  ),

  # ===========================================================================
  # Figures: Experiment 1
  # ===========================================================================
  tar_target(exp1_plot_combined,
             make_exp1_combined(bind_rows(exp1_results_raw))),
  tar_target(
    exp1_fig_combined,
    {
      dir.create("fig/exp1", recursive = TRUE, showWarnings = FALSE)
      ggsave("fig/exp1/exp1_combined.pdf", exp1_plot_combined,
             width = fig_width, height = fig_height, dpi = fig_dpi)
      "fig/exp1/exp1_combined.pdf"
    },
    format = "file"
  ),
  tar_target(
    exp1_png_combined,
    {
      dir.create("fig/exp1", recursive = TRUE, showWarnings = FALSE)
      ggsave("fig/exp1/exp1_combined.png", exp1_plot_combined,
             width = fig_width, height = fig_height, dpi = 200)
      "fig/exp1/exp1_combined.png"
    },
    format = "file"
  ),
  # --- Tufte-style alternative (opt-in; originals above are untouched) ---
  tar_target(exp1_plot_tufte,
             make_exp1_tufte(bind_rows(exp1_results_raw))),
  tar_target(
    exp1_fig_tufte,
    {
      dir.create("fig/tufte", recursive = TRUE, showWarnings = FALSE)
      ggsave("fig/tufte/exp1_tufte.pdf", exp1_plot_tufte,
             width = 5.4, height = 2.0, dpi = fig_dpi)
      ggsave("fig/tufte/exp1_tufte.png", exp1_plot_tufte,
             width = 5.4, height = 2.0, dpi = 200)
      "fig/tufte/exp1_tufte.pdf"
    },
    format = "file"
  ),

  # ===========================================================================
  # Figures: Experiment 2
  # ===========================================================================
  # Both Exp.2 figures draw one line per topology against N. The load axis and
  # the agentic arm are onset statistics, not extra lines: fed in here they
  # would be averaged into the same line rather than drawn beside it, so the
  # figures keep the medium-load synthetic sweep they plot.
  tar_target(exp2_plot_combined,
             make_exp2_combined(exp2_medium_results)),
  tar_target(
    exp2_fig_combined,
    {
      dir.create("fig/exp2", recursive = TRUE, showWarnings = FALSE)
      ggsave("fig/exp2/exp2_combined.pdf", exp2_plot_combined,
             width = fig_width, height = fig_height, dpi = fig_dpi)
      "fig/exp2/exp2_combined.pdf"
    },
    format = "file"
  ),
  tar_target(
    exp2_png_combined,
    {
      dir.create("fig/exp2", recursive = TRUE, showWarnings = FALSE)
      ggsave("fig/exp2/exp2_combined.png", exp2_plot_combined,
             width = fig_width, height = fig_height, dpi = 200)
      "fig/exp2/exp2_combined.png"
    },
    format = "file"
  ),

  # ===========================================================================
  # Figures: Experiment 3
  # ===========================================================================
  tar_target(exp3_plot_combined,
             make_exp3_combined(bind_rows(exp3_results_raw))),
  tar_target(
    exp3_fig_combined,
    {
      dir.create("fig/exp3", recursive = TRUE, showWarnings = FALSE)
      ggsave("fig/exp3/exp3_combined.pdf", exp3_plot_combined,
             width = fig_width, height = fig_height, dpi = fig_dpi)
      "fig/exp3/exp3_combined.pdf"
    },
    format = "file"
  ),
  tar_target(
    exp3_png_combined,
    {
      dir.create("fig/exp3", recursive = TRUE, showWarnings = FALSE)
      ggsave("fig/exp3/exp3_combined.png", exp3_plot_combined,
             width = fig_width, height = fig_height, dpi = 200)
      "fig/exp3/exp3_combined.png"
    },
    format = "file"
  ),

  # ===========================================================================
  # Figures: Experiment 4 (two vertical figures, 4 facets each)
  # ===========================================================================
  tar_target(exp4_plot_combined_a,
             make_exp4_combined_a(bind_rows(exp4_results_raw))),
  tar_target(exp4_plot_combined_b,
             make_exp4_combined_b(bind_rows(exp4_results_raw))),
  tar_target(
    exp4_fig_combined_a,
    {
      dir.create("fig/exp4", recursive = TRUE, showWarnings = FALSE)
      ggsave("fig/exp4/exp4_combined_a.pdf", exp4_plot_combined_a,
             width = fig_width, height = fig_height, dpi = fig_dpi)
      "fig/exp4/exp4_combined_a.pdf"
    },
    format = "file"
  ),
  tar_target(
    exp4_fig_combined_b,
    {
      dir.create("fig/exp4", recursive = TRUE, showWarnings = FALSE)
      ggsave("fig/exp4/exp4_combined_b.pdf", exp4_plot_combined_b,
             width = fig_width, height = fig_height, dpi = fig_dpi)
      "fig/exp4/exp4_combined_b.pdf"
    },
    format = "file"
  ),

  # ===========================================================================
  # Figures: Experiment 5
  # ===========================================================================
  tar_target(exp5_plot_combined,
             make_exp5_combined(bind_rows(exp5_results_raw))),
  tar_target(
    exp5_fig_combined,
    {
      dir.create("fig/exp5", recursive = TRUE, showWarnings = FALSE)
      ggsave("fig/exp5/exp5_combined.pdf", exp5_plot_combined,
             width = fig_width, height = fig_height, dpi = fig_dpi)
      "fig/exp5/exp5_combined.pdf"
    },
    format = "file"
  ),
  tar_target(
    exp5_png_combined,
    {
      dir.create("fig/exp5", recursive = TRUE, showWarnings = FALSE)
      ggsave("fig/exp5/exp5_combined.png", exp5_plot_combined,
             width = fig_width, height = fig_height, dpi = 200)
      "fig/exp5/exp5_combined.png"
    },
    format = "file"
  ),

  # ===========================================================================
  # Figures: Experiment 6
  # ===========================================================================
  tar_target(exp6_plot_combined,
             make_exp6_combined(bind_rows(exp6_results_raw))),
  tar_target(
    exp6_fig_combined,
    {
      dir.create("fig/exp6", recursive = TRUE, showWarnings = FALSE)
      ggsave("fig/exp6/exp6_combined.pdf", exp6_plot_combined,
             width = fig_width, height = fig_height, dpi = fig_dpi)
      "fig/exp6/exp6_combined.pdf"
    },
    format = "file"
  ),
  tar_target(
    exp6_png_combined,
    {
      dir.create("fig/exp6", recursive = TRUE, showWarnings = FALSE)
      ggsave("fig/exp6/exp6_combined.png", exp6_plot_combined,
             width = fig_width, height = fig_height, dpi = 200)
      "fig/exp6/exp6_combined.png"
    },
    format = "file"
  ),

  # ===========================================================================
  # Tufte-style ALTERNATIVE figures (opt-in; originals above are untouched)
  # ===========================================================================
  tar_target(exp2_plot_tufte, make_exp2_tufte(exp2_medium_results)),
  tar_target(exp2_fig_tufte, { dir.create("fig/tufte", recursive = TRUE, showWarnings = FALSE)
    ggsave("fig/tufte/exp2_tufte.pdf", exp2_plot_tufte, width = fig_width, height = fig_height, dpi = fig_dpi)
    ggsave("fig/tufte/exp2_tufte.png", exp2_plot_tufte, width = fig_width, height = fig_height, dpi = 200)
    "fig/tufte/exp2_tufte.pdf" }, format = "file"),
  tar_target(exp3_plot_tufte, make_exp3_tufte(bind_rows(exp3_results_raw))),
  tar_target(exp3_fig_tufte, { dir.create("fig/tufte", recursive = TRUE, showWarnings = FALSE)
    ggsave("fig/tufte/exp3_tufte.pdf", exp3_plot_tufte, width = fig_width, height = fig_height, dpi = fig_dpi)
    ggsave("fig/tufte/exp3_tufte.png", exp3_plot_tufte, width = fig_width, height = fig_height, dpi = 200)
    "fig/tufte/exp3_tufte.pdf" }, format = "file"),
  tar_target(exp4a_plot_tufte, make_exp4_tufte(bind_rows(exp4_results_raw), "a")),
  tar_target(exp4a_fig_tufte, { dir.create("fig/tufte", recursive = TRUE, showWarnings = FALSE)
    ggsave("fig/tufte/exp4_a_tufte.pdf", exp4a_plot_tufte, width = fig_width, height = fig_height, dpi = fig_dpi)
    ggsave("fig/tufte/exp4_a_tufte.png", exp4a_plot_tufte, width = fig_width, height = fig_height, dpi = 200)
    "fig/tufte/exp4_a_tufte.pdf" }, format = "file"),
  tar_target(exp4b_plot_tufte, make_exp4_tufte(bind_rows(exp4_results_raw), "b")),
  tar_target(exp4b_fig_tufte, { dir.create("fig/tufte", recursive = TRUE, showWarnings = FALSE)
    ggsave("fig/tufte/exp4_b_tufte.pdf", exp4b_plot_tufte, width = fig_width, height = fig_height, dpi = fig_dpi)
    ggsave("fig/tufte/exp4_b_tufte.png", exp4b_plot_tufte, width = fig_width, height = fig_height, dpi = 200)
    "fig/tufte/exp4_b_tufte.pdf" }, format = "file"),
  tar_target(exp5_plot_tufte, make_exp5_tufte(bind_rows(exp5_results_raw))),
  tar_target(exp5_fig_tufte, { dir.create("fig/tufte", recursive = TRUE, showWarnings = FALSE)
    ggsave("fig/tufte/exp5_tufte.pdf", exp5_plot_tufte, width = fig_width, height = fig_height, dpi = fig_dpi)
    ggsave("fig/tufte/exp5_tufte.png", exp5_plot_tufte, width = fig_width, height = fig_height, dpi = 200)
    "fig/tufte/exp5_tufte.pdf" }, format = "file"),
  tar_target(exp11_plot_tufte, make_exp11_tufte(bind_rows(exp11_results_raw))),
  tar_target(exp11_fig_tufte, { dir.create("fig/tufte", recursive = TRUE, showWarnings = FALSE)
    ggsave("fig/tufte/exp11_tufte.pdf", exp11_plot_tufte, width = fig_width, height = fig_height, dpi = fig_dpi)
    ggsave("fig/tufte/exp11_tufte.png", exp11_plot_tufte, width = fig_width, height = fig_height, dpi = 200)
    "fig/tufte/exp11_tufte.pdf" }, format = "file"),
  tar_target(exp6_plot_tufte, make_exp6_tufte(bind_rows(exp6_results_raw))),
  tar_target(exp6_fig_tufte, { dir.create("fig/tufte", recursive = TRUE, showWarnings = FALSE)
    ggsave("fig/tufte/exp6_tufte.pdf", exp6_plot_tufte, width = fig_width, height = fig_height, dpi = fig_dpi)
    ggsave("fig/tufte/exp6_tufte.png", exp6_plot_tufte, width = fig_width, height = fig_height, dpi = 200)
    "fig/tufte/exp6_tufte.pdf" }, format = "file")
)
