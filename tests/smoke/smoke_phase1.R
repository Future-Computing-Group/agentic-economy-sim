#!/usr/bin/env Rscript
# Bounded real run of every experiment path this round's configuration and
# export changes touch: the factorial driver at the headline configuration, one
# cell of the sensitivity sweep, and the agentic workload with the replay
# export and the testbed's own reader over what it wrote.
#
# Usage:  Rscript tests/smoke/smoke_phase1.R [out_dir]
#
# "Ran to completion" is not "exited 0": a run that admits nothing, or admits
# tasks and scores none of them, exits 0 with an empty result. Every cell
# therefore reports what it produced -- exported rows, the share of generated
# tasks admitted, the share of admitted tasks served, and how many per-task
# outcomes were actually scored -- and the script fails if any of those is
# empty. The numbers are from short runs and are not results; they are
# evidence that the path runs.
#
# Artefacts land outside the repository, under the campaign's runs directory:
# emulation runs are data, not code.

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})
purrr::walk(list.files(here::here("R"), full.names = TRUE, pattern = "[.][Rr]$"),
            source)

args    <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args)) args[[1]] else file.path(
  dirname(here::here()), "runs",
  format(Sys.time(), "smoke-%Y%m%dT%H%M%SZ", tz = "UTC"))
# Refuse before creating anything: a refused run must not leave a directory
# behind inside the repository.
target <- if (startsWith(out_dir, "/")) out_dir else file.path(getwd(), out_dir)
if (startsWith(normalizePath(target, mustWork = FALSE),
               normalizePath(here::here()))) {
  stop("smoke artefacts must not land inside the repository: ", out_dir)
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# The headline configuration, pinned by tests/testthat/test-headline-config.R:
# no assumed demand reduction, one price step for every arm.
efficiency <- 1.0
cells      <- list()

# admitted_share is the clearing fraction where the driver reports one;
# served_share is the deadline-met share, among admitted tasks for the drivers
# that count admissions and among generated tasks for Exp.1, which does not.
add_cell <- function(cell, rows, scored, admitted = NA_real_,
                     served = NA_real_, note = "") {
  cells[[length(cells) + 1L]] <<- data.frame(
    cell = cell, rows = rows, scored = scored,
    admitted_share = admitted, served_share = served,
    ok = rows > 0L && scored > 0L &&
      (is.na(admitted) || (is.finite(admitted) && admitted > 0)) &&
      (is.na(served) || (is.finite(served) && served > 0)),
    note = note, stringsAsFactors = FALSE
  )
}

exported <- function(dir, load) {
  alloc <- utils::read.csv(file.path(dir, sprintf("alloc_%s.csv", load)),
                           stringsAsFactors = FALSE)
  sim   <- utils::read.csv(file.path(dir, sprintf("sim_tasks_%s.csv", load)),
                           stringsAsFactors = FALSE)
  list(rows = nrow(alloc), admitted = sum(alloc$admitted), scored = nrow(sim))
}

# --- Exp.4: the factorial driver at the SP operating point -------------------
dir4 <- file.path(out_dir, "exp4-hybrid-sp-high")
res4 <- exp4_run_single("hybrid", graph_type = "sp", load_level = "high",
                        N = 55L, seed = 1L, n_rounds = 20L,
                        integ_efficiency = efficiency, integ_eta = price_eta,
                        alloc_out = dir4)
e4 <- exported(dir4, "high")
add_cell("exp4 hybrid sp high N=55 rounds=20", e4$rows, e4$scored,
         res4$clearing_fraction, res4$served_among_admitted,
         sprintf("eta=%.2f efficiency=%.2f", price_eta, efficiency))

# --- Exp.1: the same price step, on a driver that is not Exp.4 ---------------
# The price step is one line shared by the Exp.1, Exp.2, Exp.3, Exp.5 and Exp.6
# drivers. Exp.1 is the cheapest of the five to run and stands for them here;
# all five move with the step and all five are regenerated with the pipeline.
res1 <- exp1_run_single(graph_type = "sp", load_level = "high", seed = 1L,
                        n_agents = 55L, n_rounds = 10L)
add_cell("exp1 sp high N=55 rounds=10", nrow(res1),
         sum(is.finite(c(res1$median_latency, res1$welfare, res1$efficiency))),
         served = 1 - res1$drop_rate,
         note = sprintf("median_latency=%.1f welfare=%.1f",
                        res1$median_latency, res1$welfare))

# --- Exp.14: one branch of the sensitivity sweep -----------------------------
# The cell added this round, at the per-topology operating point the pipeline
# sweeps over, bounded to one seed and a short horizon.
row14 <- exp14_sensitivity_row("integ_efficiency", 1.0,
                               topologies = c("sp", "entangled"), seeds = 1L,
                               N = c(sp = 55L, entangled = 35L),
                               load_level = "high", n_rounds = 40L)
add_cell("exp14 branch integ_efficiency=1.0 seeds=1 rounds=40",
         nrow(row14), row14$n_volatile,
         note = sprintf("median_reduction=%s",
                        format(row14$median_reduction, digits = 4)))

# --- Exp.9: the agentic workload, exported for replay ------------------------
profile <- agentic_profile_path()
dir9    <- file.path(out_dir, "exp9-naive-agentic-high")
res9    <- exp4_run_single("naive", graph_type = "agentic", load_level = "high",
                           N = 200L, seed = 1L, n_rounds = 10L,
                           deadlines = agentic_deadlines(path = profile),
                           lambda_l_default = agentic_lambda_l(path = profile),
                           alloc_out = dir9)
e9 <- exported(dir9, "high")
add_cell("exp9 naive agentic high N=200 rounds=10", e9$rows, e9$scored,
         res9$clearing_fraction, res9$served_among_admitted,
         sprintf("admitted=%d", e9$admitted))

# --- The testbed's own reader, over what the export just wrote ---------------
py <- sprintf(paste0(
  "import sys; sys.path.insert(0, '%s'); import replay; ",
  "rounds = replay.load_rounds('%s'); env = replay.load_env('%s'); ",
  "print(len(rounds), sum(len(r['tasks']) for r in rounds), env['load_level'])"),
  here::here("emul"), file.path(dir9, "alloc_high.csv"),
  file.path(dir9, "env_high.json"))
parsed <- suppressWarnings(system2("python3", c("-c", shQuote(py)),
                                   stdout = TRUE, stderr = TRUE))
if (Sys.which("python3") == "" || !is.null(attr(parsed, "status"))) {
  add_cell("replay.py parse of the exp9 export", 0L, 0L,
           note = paste(utils::tail(parsed, 1L), collapse = " "))
} else {
  counts <- strsplit(utils::tail(parsed, 1L), " ")[[1]]
  add_cell("replay.py parse of the exp9 export",
           as.integer(counts[1]), as.integer(counts[2]),
           note = sprintf("rounds/admitted read back, load_level=%s", counts[3]))
}

report <- do.call(rbind, cells)
cat("\nartefacts:", out_dir, "\n\n")
print(report, row.names = FALSE, digits = 4)
cat("\n", sum(report$ok), "of", nrow(report), "cells produced work\n")
if (!all(report$ok)) quit(status = 1L)
