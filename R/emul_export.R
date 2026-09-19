# emul_export.R
# ---------------------------------------------------------------------------
# Export of simulator-side constants for the emulation testbed.
#
# The testbed replays the simulator's admitted allocations on real containers:
# the market clears here, the testbed executes, neither re-decides. It must
# therefore not re-derive the environment it is replaying, so the environment
# travels with the run as a file and the testbed reads its netem delays,
# capacities and deadline checks off that file. When the environment is
# re-derived (per-tier base latencies from the measured profile, deadlines from
# the critical-path rule), the testbed follows with no code change.
# ---------------------------------------------------------------------------

#' The simulator's own commit, or NA.
#'
#' Rooted at the simulator's tree rather than at the process working directory:
#' rsync deploys land wherever they land, and `git rev-parse` run in the working
#' directory silently records an unrelated repository's HEAD whenever the tree
#' sits inside one. A provenance field that looks authoritative and is wrong is
#' worse than a missing one, so the answer is kept only if git agrees that this
#' tree is the repository root, and it carries a -dirty suffix if the tree has
#' uncommitted changes.
#'
#' @return 40-hex commit, optionally suffixed "-dirty", or NA_character_.
sim_git_sha <- function() {
  root <- here::here()
  git  <- function(...) tryCatch(
    system2("git", c("-C", shQuote(root), ...), stdout = TRUE, stderr = FALSE),
    error   = function(e) character(0),
    warning = function(w) character(0)
  )

  top <- git("rev-parse", "--show-toplevel")
  same <- length(top) == 1L &&
    identical(normalizePath(top, mustWork = FALSE),
              normalizePath(root, mustWork = FALSE))
  if (!same) return(NA_character_)

  sha <- git("rev-parse", "HEAD")
  if (length(sha) != 1L) return(NA_character_)
  if (length(git("status", "--porcelain")) > 0L) paste0(sha, "-dirty") else sha
}

#' Write env_<load_level>.json for the emulation testbed.
#'
#' Fields: graph_type, load_level, load_factor, n_agents, n_rounds, seed,
#' capacities (per tier), base_ms (per tier), demand_weight (per tier),
#' deadlines, sim_git_sha.
#'
#' @param env       Environment list from init_environment().
#' @param out_dir   Directory to write into (created if absent). Keep it outside
#'                  the repository: emulation runs are data, not code.
#' @param n_rounds  Rounds the replayed run covers.
#' @param seed      Seed of the replayed run.
#' @param deadlines Task deadlines (ms) the run was generated with.
#' @return The path written (invisibly).
export_env_json <- function(env, out_dir, n_rounds, seed,
                            deadlines = c(500L, 750L, 1000L)) {
  by_tier <- function(df, col) as.list(setNames(as.numeric(df[[col]]), df$tier))

  spec <- list(
    graph_type    = env$graph_type,
    load_level    = env$load_level,
    load_factor   = env$load_factor,
    n_agents      = as.integer(env$n_agents),
    n_rounds      = as.integer(n_rounds),
    seed          = as.integer(seed),
    capacities    = by_tier(env$capacities, "capacity"),
    base_ms       = by_tier(env$base_latency, "base_ms"),
    demand_weight = by_tier(env$demand_weights, "demand_weight"),
    deadlines     = as.integer(deadlines),
    sim_git_sha   = sim_git_sha()
  )

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(out_dir, sprintf("env_%s.json", env$load_level))
  jsonlite::write_json(spec, path, auto_unbox = TRUE, digits = NA)
  invisible(path)
}


# ---------------------------------------------------------------------------
# Per-round allocation export.
#
# Three files leave one simulated run, all of them decided here: the tasks the
# market generated and which of them it admitted (what the testbed replays),
# the environment they were decided in (what the testbed must not re-derive),
# and this run's own per-task outcomes (what the comparison holds the testbed's
# against, restricted to the tasks actually replayed).
# ---------------------------------------------------------------------------

ALLOC_COLUMNS <- c("round", "task_id", "agent_id", "deadline_ms", "value_base",
                   "admitted")
SIM_TASK_COLUMNS <- c("round", "task_id", "deadline_ms", "latency_ms",
                      "met_deadline")

.emul_append <- function(path, rows) {
  if (nrow(rows) == 0L) return(invisible(path))
  utils::write.table(rows, path, sep = ",", row.names = FALSE,
                     col.names = FALSE, append = TRUE)
  invisible(path)
}

#' Open a run's export sinks, truncating anything an earlier run left.
#'
#' @param out_dir    Directory to write into. Keep it outside the repository:
#'                   emulation runs are data, not code.
#' @param load_level The run's load level; it names both files.
#' @return A list of the two paths, to hand to emul_export_round().
emul_export_open <- function(out_dir, load_level) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- list(
    alloc     = file.path(out_dir, sprintf("alloc_%s.csv", load_level)),
    sim_tasks = file.path(out_dir, sprintf("sim_tasks_%s.csv", load_level))
  )
  # Truncate rather than append: a rerun into the same directory that inherited
  # the previous run's rounds would replay a file no single run ever produced.
  writeLines(paste(ALLOC_COLUMNS, collapse = ","), paths$alloc)
  writeLines(paste(SIM_TASK_COLUMNS, collapse = ","), paths$sim_tasks)
  paths
}

#' Append one round to a run's export sinks.
#'
#' Every generated task is written, admitted or not, so the testbed inherits
#' the market's own drop denominator rather than recomputing one. `admitted` is
#' a logical and reaches the file as TRUE or FALSE; it is never NA, which the
#' testbed's reader refuses rather than reading as "not admitted".
#'
#' @param paths      List from emul_export_open().
#' @param round      Round index.
#' @param tasks_all  Every task generated this round.
#' @param allocation The tasks the market admitted this round.
#' @param results    Execution results for the admitted tasks (latency, success).
emul_export_round <- function(paths, round, tasks_all, allocation, results) {
  if (nrow(tasks_all) > 0L) {
    .emul_append(paths$alloc, data.frame(
      round       = as.integer(round),
      task_id     = as.character(tasks_all$task_id),
      agent_id    = as.integer(tasks_all$agent_id),
      deadline_ms = as.numeric(tasks_all$deadline),
      value_base  = as.numeric(tasks_all$value_base),
      admitted    = as.character(tasks_all$task_id) %in%
                      as.character(allocation$task_id),
      stringsAsFactors = FALSE
    ))
  }
  if (nrow(results) > 0L) {
    .emul_append(paths$sim_tasks, data.frame(
      round        = as.integer(round),
      task_id      = as.character(results$task_id),
      deadline_ms  = as.numeric(results$deadline),
      latency_ms   = as.numeric(results$latency),
      met_deadline = as.logical(results$success),
      stringsAsFactors = FALSE
    ))
  }
  invisible(paths)
}
