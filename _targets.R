# _targets.R
# ---------------------------------------------------------------------------
# Targets pipeline for the simulation study.
#
# Paper: "Real-Time AI Service Economy: A Framework for Agentic Computing
#         Across the Continuum" -- IEEE Transactions on Services Computing.
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
  controller = crew::crew_controller_local(workers = 8L, seconds_idle = 60)
)

# Auto-source all R/ files (sim_helpers, sim_market, experiments, plots)
purrr::walk(list.files("R", full.names = TRUE, pattern = "\\.[Rr]$"), source)

# ===========================================================================
# Simulation parameters (see Table V in the paper)
# ===========================================================================
# "linear" is dropped from the experiment grid -- a linear chain is a degenerate
# 1-leaf tree (linear ~ tree empirically, sigma_p=0 for both), so it adds a
# redundant arm. build_dependency_graph("linear") is kept in sim_helpers.R for
# completeness.
graph_types    <- c("tree", "sp", "entangled")
load_levels    <- c("low", "medium", "high")
n_agents       <- 75L                      # operating point: high load contends (rho~1.1-2.9), no collapse
n_rounds       <- 200L
n_seeds        <- 10L
task_deadlines <- c(500L, 750L, 1000L)     # ms; cross-continuum agentic round-trips (device-edge-cloud, 0.5-1s)
lambda_l       <- 0.005                     # per-ms latency decay; delta(T) = exp(-0.005*T)
integ_efficiency_sp  <- 0.75                # integrator efficiency factor for SP topology
integ_efficiency_ent <- 0.85                # integrator efficiency factor for entangled topology
integ_eta      <- 0.15                      # integrator slice price step size

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
      n_agents         = n_agents,
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
    tidyr::expand_grid(
      graph_type = c("tree", "sp", "entangled"),
      N          = seq(10, 60, by = 10),
      seed       = seq_len(n_seeds),
      load_level = "medium"
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

  # ===========================================================================
  # Experiment 3: Governance policies
  # ===========================================================================
  tar_target(
    exp3_param_grid,
    tidyr::expand_grid(
      policy     = c("none", "moderate", "strict"),
      graph_type = c("tree", "entangled"),
      load_level = c("medium", "high"),
      N          = n_agents,
      seed       = seq_len(n_seeds)
    )
  ),
  tar_target(
    exp3_results_raw,
    exp3_run_single(
      policy           = exp3_param_grid$policy,
      graph_type       = exp3_param_grid$graph_type,
      load_level       = exp3_param_grid$load_level,
      N                = exp3_param_grid$N,
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
  # Experiment 4: Naive vs hybrid (EMA only) vs hybrid (full) architecture
  # ===========================================================================
  tar_target(
    exp4_param_grid,
    tidyr::expand_grid(
      architecture = c("naive", "hybrid_ema", "hybrid"),
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
      N                = n_agents,
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
  tar_target(
    exp6_param_grid,
    tidyr::expand_grid(
      mechanism    = c("random", "edf", "greedy_ev", "market"),
      architecture = c("naive", "hybrid"),
      graph_type   = c("tree", "sp", "entangled"),
      load_level   = c("medium", "high"),
      seed         = seq_len(n_seeds)
    )
  ),
  tar_target(
    exp6_results_raw,
    exp6_run_single(
      mechanism        = exp6_param_grid$mechanism,
      architecture     = exp6_param_grid$architecture,
      graph_type       = exp6_param_grid$graph_type,
      load_level       = exp6_param_grid$load_level,
      N                = n_agents,
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
      n_rounds   = 30L
    ),
    pattern   = map(exp7_param_grid),
    iteration = "vector"
  ),
  tar_target(exp7_summary_table, exp7_aggregate(exp7_results_raw)),

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
      deadlines        = task_deadlines,
      lambda_l_default = lambda_l,
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
        N                = n_agents,
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
        N                = n_agents,
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
  # Experiment 14: Parameter sensitivity table (self-contained, loops internally)
  # ===========================================================================
  tar_target(
    exp14_sensitivity,
    exp14_sensitivity_table(
      topologies = c("sp", "entangled"),
      seeds      = seq_len(5),
      N          = n_agents,
      load_level = "high",
      n_rounds   = n_rounds
    )
  ),

  # ===========================================================================
  # Statistical analysis (all experiments)
  # ===========================================================================
  tar_target(stats_exp1, stat_exp1(bind_rows(exp1_results_raw))),
  tar_target(stats_exp2, stat_exp2(bind_rows(exp2_results_raw))),
  tar_target(stats_exp3, stat_exp3(bind_rows(exp3_results_raw))),
  tar_target(stats_exp4, stat_exp4(bind_rows(exp4_results_raw))),
  tar_target(stats_exp5, stat_exp5(bind_rows(exp5_results_raw))),
  tar_target(stats_exp6, stat_exp6(bind_rows(exp6_results_raw))),

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

  # ===========================================================================
  # Figures: Experiment 2
  # ===========================================================================
  tar_target(exp2_plot_combined,
             make_exp2_combined(bind_rows(exp2_results_raw))),
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
  )
)
