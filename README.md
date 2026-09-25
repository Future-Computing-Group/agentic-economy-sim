# Simulation Code for: Agentic Service Markets Across the Computing Continuum

Simulation study for the paper:

> **Agentic Service Markets Across the Computing Continuum: A Polymatroidal Architecture**
> Preprint: arXiv:2603.05614.

## Research context

The paper models an agentic service market over a service-dependency DAG in which a task is a token at one leaf and loads every node on that leaf's path, so the feasible allocations are the leaf-block region of the DAG. Its claims, which this pipeline measures:

- Where the leaf-block family is laminar the region is a polymatroid: value-greedy admission is exact and the VCG mechanism is dominant-strategy incentive compatible. A crossing in the family costs both, exactness and truthfulness, while clearing prices still exist wherever the family's incidence matrix is totally unimodular.
- A cross-domain integrator that exposes an inner region of a sub-DAG (the largest box its internal capacities can always deliver) restores exactness at the market layer, at a cost in admitted volume.
- Laminarity does not order price stability: whether a family is laminar does not decide how much the market's prices move.

## What this project simulates

The reported evaluation runs on a node-level substrate: eight service nodes of one dependency graph (a device node, three edge nodes and four leaves) on one capacity vector and one arrival stream. Three instances differ only in which leaves each internal node reaches:

- **T**, a rooted tree (laminar);
- **X**, the tree plus one crossing arc (one crossing pair, still totally unimodular);
- **S**, a parallel fan in which every edge node feeds every leaf (laminar).

The crossing sweep adds generated families across the theory's regions and a two-terminal series-parallel instance in which a parallel pair of edge nodes rejoins at one node, the series-parallel case that is not a tree. Autonomous agents generate deadline-bound tasks whose value decays with latency; admission runs through the arm's mechanism against the region it advertises, and execution queues against the true instance.

**Congestion level.** The reported level is the testbed-calibrated one: the (utilisation clamp, queue coefficient) pair whose latency elasticity over the node grid's own medium-to-high step is closest to the one the emulated testbed measured. It is read from `results/calibration/node-congestion-calibration.csv` and is the default of every node driver. The steep level is the sensitivity: the mechanism grids run both levels by name, and the one-at-a-time sweep varies from it.

**Comparator tiers.**

- **Tier A, reference optima** (yardsticks, unobtainable in deployment): the exact leaf-block optimum on true values at zero queue, and the ex-post prefix optimum through the realised congestion model.
- **Tier B, truth-assuming planners**: value-greedy, a Kubernetes-style rank, the value-ranked posted price (a posted level screens, participants are packed by value), and EDF and random as ablation controls.
- **Tier C, cross-domain mechanisms**: the discovered-price market (tatonnement), a congestion-consistent diagnostic variant, the arrival-order posted price and the deadline-priority posted price (the same screen, participants served in their arrival order or by deadline), and a mixed rule that posts a price for the contracted slice.

### Node-level experiment blocks

| Block | Targets | What varies |
|---|---|---|
| Structure | `node_exp1_results_raw` | Instance x load |
| Population sweep | `node_exp2_results_raw`, `node_exp2_onset_law` | Agent population x instance x load, and the per-node onset law |
| Governance caps | `node_exp3_results_raw`, `node_exp3b_results_raw` | Coordinate-wise caps from trust and locality, the role class (a cap on one agent role, enforced at admission), residency coupling and domain slices; and the exactness-repair cap probe |
| Architecture | `node_exp4_results_raw`, `node_exp4_overhead_raw`, `node_exp4_overhead_summary` | Encapsulation x price smoothing; and the translation overhead charged on the contracted cluster's exported leaves, at both congestion levels |
| Architecture x governance | `node_exp5_results_raw` | Architecture x cap level |
| Mechanisms | `node_exp6_results_raw`, `node_exp6_frontier`, `node_exp6_tuning_raw`, `node_exp6_eval_raw`, `node_exp6_tuned`, `node_exp6_misset_posted`, `node_exp6_sensitivity` | Every arm of the three tiers at both congestion levels, each posted discipline at every posted level (the frontier); one tuned knob per mechanism, chosen on tuning seeds and reported on disjoint held-out seeds; posted levels set before the cell is known, read from the runs already made; and a one-at-a-time sensitivity |
| Price-process battery | `node_exp6_convergence`, `node_exp6_determinacy`, `node_exp6_report_stability`, `node_exp6_shock` | Convergence of the price process, determinacy of the clearing vector, stability under a changed report, recovery after a capacity or demand shock |
| Existence and the crossing sweep | `node_exp6_existence`, `node_exp6_sweep`, `node_exp6_sweep_summary`, `node_exp6_sweep_by_instance` | The LP integrality certificate of clearing prices on the shipped instances, and existence and exactness over generated families and the series-parallel rejoin instance |
| Incentives | `node_exp7a_results_raw`, `node_exp7b_results_raw` | Uniform bid shading; joint misreports at the enumerable point, at the evaluation point and over a contention sweep of population x capacity, with the greedy exactness shortfall beside the gain |
| Measured agentic workload | `node_exp9_results_raw` | The union of two recorded LLM tool-use patterns as a node instance, stage weights on one common token unit |
| Interface probe | `node_exp10_results_raw` | The advertised scalar of a contracted cluster (off, inner, max-flow); and, with the inner scalar held, whether the exported units are interchangeable |
| Sensitivity | `node_exp14_sensitivity` | One-at-a-time parameter sensitivity on the node substrate |
| Calibration | `node_calibration_rows`, `node_calibration_file` | The congestion calibration sweep, written to `results/calibration/node-congestion-calibration.csv` |

Every node block also records the structural fields of its runs: the greedy-versus-exact ratio and its incidence, the flow bound, the over-commitment of the advertised region, the incentive certificate, and whether the advertised region is the true one or strictly inside it.

**Released results.** `node_stats_report_file` writes `results/node-stats-report.csv`, the machine-written statistics of every node block; `node_intervals` writes `results/node-intervals.csv`, the mean, Student t 95 percent interval and n of every reported node-level response per block and cell. The calibration table is shipped with the code, and the calibrated level is read from it.

### The per-tier pipeline

The earlier per-tier environment, one capacity row per physical tier with a per-tier demand profile, remains in `_targets.R` under the unprefixed target names (`exp1_*` to `exp14_*`). Three of its arms are reported in the supplement, and none of them carries a node-level claim:

- **Recipe heterogeneity** (`exp11_*`): one recipe against two at fixed tier set, load and aggregate demand, against a brute-force exact optimum. Beside it are the catalogue over-commitment sweep, a simulated arm with proportional recipes that measures the catalogue interface's over-commitment factor, and the integral inner-exposure control with its LP certificate.
- **Per-tier encapsulation overhead** (`exp12_*`).
- **Architecture parameter sensitivity** (`exp14_sensitivity`), sweeping `cap_scale`, `integ_eta`, `lambda_l` and `integ_efficiency`. The integrator's efficiency factor is held at one in every reported arm, and its levels in this sweep are not reported.

The other per-tier targets still build and are not reported.

The agentic profiles are generated by `agentic/run_agent_workload.py` (a real multi-step tool-using LLM agent via Ollama), which writes `agentic/agentic_profile.json` and `agentic/agentic_profile_b.json`. Their per-stage weights are measured token counts per task. The simulator reads them from the profiles, so regenerating them against another model or another agent changes the environment with no code edit.

## Requirements

- **R** (>= 4.1)
- R packages: `targets`, `tarchetypes`, `tidyverse`, `RColorBrewer`, `patchwork`, `scales`, `future`, `boot`, `crew`, `jsonlite`, `here`, `lpSolve`
- For the agentic workload (optional): Python 3 + a local [Ollama](https://ollama.com) server
- For the emulation testbed (optional): Docker Compose + a local Ollama server
- Tests additionally need `testthat`, `withr`, `digest` and `yaml`; the ART interaction analysis in `R/stat_analysis.R` is skipped unless `ARTool` is installed

Install all R dependencies:

```r
install.packages(c("targets", "tarchetypes", "tidyverse", "RColorBrewer", "patchwork", "scales", "future", "boot", "crew", "jsonlite", "here", "lpSolve"))
```

## Running the pipeline

```r
targets::tar_make()              # run the full pipeline
targets::tar_visnetwork()        # visualise the target DAG
targets::tar_outdated()          # check what needs rebuilding
targets::tar_read(node_exp1_summary_table)  # load a result
```

Branches run in parallel through `crew` local workers; `SIM_WORKERS` sets their number (default 8). The full pipeline is long: the node mechanism grids and the joint-misreport block dominate it, and together they take days of CPU time, so run the full build on a machine or cluster node with many cores. A single target can be rebuilt on its own, for example `targets::tar_make(names = node_exp6_existence)`.

The test suite runs without the pipeline's store and takes about twenty minutes:

```sh
Rscript tests/testthat.R
```

## Emulation testbed

The testbed replays the market's admitted allocations on three tier containers running the real multi-step agent: the simulator decides, the testbed executes, neither re-decides. The replay directory (`alloc_<load>.csv`, `env_<load>.json`, `sim_tasks_<load>.csv`) is written by the Exp 4 driver when it is given an export directory, `exp4_run_single(..., alloc_out = <replay-dir>)`. Run data never lands in this repository: `--out-dir` has no default and is refused if it resolves inside the tree.

The two runs behind the reported testbed tables, both against the same exported market, both at the tier caps 2:3:5 the concurrency probe fixed (`emul/probe_concurrency.py`) and at the offered scale it implies.

Clean calibration run, run directory `emul-clean-20260919T055429Z`:

```sh
docker compose -f emul/docker-compose.yaml up -d
python3 emul/load_gen.py --replay-dir <replay-dir> --out-dir <run-dir> \
  --run-id clean \
  --load low --load high --max-rounds 50 --block-rounds 10 \
  --offered-scale 0.016667 --subsample-seed 1 --transport ollama \
  --timeout 300 --network-delay-ms device=5,edge=15,cloud=50
docker compose -f emul/docker-compose.yaml down --remove-orphans
```

Failure-injection run, run directory `emul-failure-20260919T055429Z`: high load only, the cloud tier killed before round 25. It exists so the calibration statistics are never computed on a run containing a kill.

```sh
docker compose -f emul/docker-compose.yaml -f emul/docker-compose.failure.yaml up -d
python3 emul/load_gen.py --replay-dir <replay-dir> --out-dir <run-dir> \
  --run-id failure \
  --load high --max-rounds 50 --block-rounds 10 \
  --offered-scale 0.016667 --subsample-seed 1 --transport ollama \
  --timeout 300 --network-delay-ms device=5,edge=15,cloud=50 \
  --t-kill 25 --kill-service cloud
docker compose -f emul/docker-compose.yaml -f emul/docker-compose.failure.yaml down --remove-orphans
```

Both runs assert the model fingerprint and the installed netem qdisc before replaying, and refuse to start if either disagrees with the run's own metadata. The statistics are then computed in R from a run directory: `emul_compare(run_dir, n_boot = 2000)` on the clean run, which refuses a run that contains a kill, and `emul_failure_gap(run_dir)` on the failure run. `emul/smoke.sh` runs the same path end to end on the sleep transport, in three rounds, with no model server needed.


## File structure

```
_targets.R            # pipeline definition
R/
  structure_instances.R # node instances, leaf-block rank, polymatroid certificate, flow bound, contraction
  sim_nodelevel.R     # node-level driver, grids, summaries, calibration, intervals
  crossing_sweep.R    # the crossing sweep over generated leaf-block families
  sim_helpers.R       # shared: environments, agents, tasks, execution, trust, metrics
  sim_market.R        # market engine: tatonnement, packing kernel, posted prices, integrator slice
  sim_exp1.R ... sim_exp14.R  # per-tier drivers; sim_exp7.R also runs the node incentive blocks
  stat_analysis.R     # bootstrap CIs, nonparametric tests, effect sizes, report tables
  emul_export.R       # environment + admitted allocations, exported for the testbed
  emul_compare.R      # simulator vs testbed comparison on dimensionless statistics
  plot_nodelevel.R    # node-level figures (fig/node/)
  plots_tufte.R, plots_exp*.R, plot_helpers.R  # per-tier figures and shared plot utilities
agentic/
  run_agent_workload.py   # real tool-using LLM agent (Ollama); writes the profiles
  agentic_profile.json, agentic_profile_b.json  # measured per-stage demand/latency profiles
results/calibration/  # the shipped congestion calibration table
emul/                 # container testbed that replays admitted allocations, and its tests
tests/testthat/       # unit/integration tests (run via testthat)
tests/smoke/          # bounded end-to-end runs of each experiment path
```

## Key model parameters

Parameters are set in `_targets.R` and, for the node substrate, in `R/sim_nodelevel.R`.

| Parameter | Where | Role |
|---|---|---|
| `node_agents()` | `R/sim_nodelevel.R` | Per-instance population, matched on offered load at the leaf-block capacity |
| `n_rounds` | `_targets.R` | Rounds per simulation run (200) |
| `n_seeds` | `_targets.R` | Monte Carlo seeds per condition (10) |
| `node_tuning_split()` | `R/sim_nodelevel.R` | Tuning seeds and the disjoint held-out evaluation seeds of the tuned comparison |
| `task_deadlines` | `_targets.R` | Task deadlines, {500, 750, 1000} ms |
| `node_lambda_l()` | `R/sim_nodelevel.R` | Per-ms value decay, the nominal rate rescaled to the node instances' critical path |
| `node_congestion_default()` | `R/sim_nodelevel.R` | The reported congestion level, read from the calibration table |
| `testbed_latency_elasticity` | `_targets.R` | The testbed's measured elasticity, the calibration's target |
| `integ_efficiency_sp`, `integ_efficiency_ent` | `_targets.R` | Per-tier integrator efficiency, 1.0 (no assumed demand reduction) in every reported arm |
| `integ_eta` | `_targets.R` | Slice price step; the common `price_eta`, used by every arm |

## AI assistance

Generative AI tools (Claude, Anthropic) were used under the authors' direction to write and test parts of this code and its documentation. The authors reviewed and verified all of it and take full responsibility for it.

## Citation

If you use this code, please cite:

```bibtex
@misc{loven2026agentic,
  title         = {Agentic Service Markets Across the Computing Continuum: A Polymatroidal Architecture},
  author        = {Lov\'{e}n, Lauri and Saleh, Alaa and Farahani, Reza and Murturi, Ilir
                   and Gujar, Sujit and Bordallo L\'{o}pez, Miguel and Donta, Praveen Kumar
                   and Dustdar, Schahram},
  year          = {2026},
  eprint        = {2603.05614},
  archivePrefix = {arXiv},
  primaryClass  = {cs.DC}
}
```

The software itself is archived on Zenodo; `CITATION.cff` carries its concept DOI and the version DOI of this release.
