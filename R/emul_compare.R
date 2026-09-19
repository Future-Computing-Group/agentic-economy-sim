# emul_compare.R
# ---------------------------------------------------------------------------
# The two worlds, side by side.
#
# The testbed executes the tasks the simulator admitted, so the two sides can
# be compared task for task. They cannot be compared in milliseconds: real
# inference and the model's per-tier base latencies differ by orders of
# magnitude. What is compared is the shape of the queueing response, through
# three dimensionless statistics computed identically on both sides:
#
#   latency elasticity   (L_high/L_med - 1) / (rho_high/rho_med - 1)
#   tail ratio           p95/p50 of end-to-end latency, per load
#   drop-rate step       deadline-miss rate among replayed tasks, high - med
#
# rho is the offered load: the number of tasks the market GENERATED in a round,
# admitted or not, read from the simulator's own alloc_<load>.csv. Both worlds
# divide by the same number, because both replay the same market, so neither
# world's own tier utilisation enters the statistic and the manuscript must not
# describe it as though it did. It is deliberately not the admitted count:
# admission saturates at the integrator's slice capacity, so the admitted
# counts tie across loads at the bound the runs sit at and that denominator is
# exactly zero there. A zero denominator is reported as no measurement rather
# than as an infinite elasticity.
#
# Both sides are restricted to the tasks the testbed actually replayed, not
# merely to the same rounds: the generator subsamples each round's admitted
# list, so "the same rounds" on the simulator side would be a different set of
# tasks with systematically different deadlines.
#
# Intervals are bootstrapped over ROUNDS within one seed, BCa, on each side
# separately, following the convention the rest of the pipeline uses
# (R/stat_analysis.R). They are not seed-level intervals and nothing here
# licenses a claim that they are.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(boot)
})

# Read a flag written by either world, or refuse. Python writes True/False and
# R writes TRUE/FALSE; anything else is an error rather than a silent FALSE,
# for the reason the testbed's own reader gives: a missing value read as "not
# admitted" or "missed its deadline" moves a statistic without a word.
.emul_flag <- function(x, what) {
  flag <- if (is.logical(x)) x else as.logical(trimws(as.character(x)))
  if (any(is.na(flag))) {
    stop(sprintf("%s: %d value(s) are neither true nor false", what,
                 sum(is.na(flag))), call. = FALSE)
  }
  flag
}

.emul_key <- function(load, round, id) paste(load, round, id, sep = "\r")

# The load levels in the order the simulator declares them (R/sim_helpers.R
# maps them to lambda 0.5/1.0/1.5). Ties in the offered load are broken by this
# order and never alphabetically: "high" sorts below "medium" by name, which
# would put the step the wrong way round in every statistic reported per side.
EMUL_LOAD_ORDER <- c("low", "medium", "high")

.emul_load_rank <- function(load) {
  match(load, EMUL_LOAD_ORDER, nomatch = length(EMUL_LOAD_ORDER) + 1L)
}

.emul_need <- function(df, columns, what) {
  missing <- setdiff(columns, names(df))
  if (length(missing)) {
    stop(sprintf("%s is missing column(s): %s", what,
                 paste(missing, collapse = ", ")), call. = FALSE)
  }
  df
}

#' The (load, round, task) triples the testbed replayed.
#'
#' Above a round's admitted count the generator replays a task more than once,
#' under the id "<task_id>#<n>"; those ids exist only in the testbed, so each
#' one also carries the id the simulator knows it by.
#'
#' @param meta Parsed run_meta.json (simplifyVector = FALSE).
#' @return A data frame: load, round, task_id, base_id.
emul_replayed_keys <- function(meta) {
  replayed <- meta$replayed_task_ids
  if (!length(replayed)) {
    stop("run_meta.json carries no replayed_task_ids", call. = FALSE)
  }
  rows <- list()
  for (load in names(replayed)) {
    for (rnd in names(replayed[[load]])) {
      ids <- as.character(unlist(replayed[[load]][[rnd]]))
      if (!length(ids)) next
      rows[[length(rows) + 1L]] <- data.frame(
        load = load, round = as.integer(rnd), task_id = ids,
        base_id = sub("#[0-9]+$", "", ids), stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

#' The testbed's per-task rows, restricted to what it replayed.
emul_testbed_tasks <- function(path, keys) {
  recs <- .emul_need(utils::read.csv(path, stringsAsFactors = FALSE),
                     c("load", "round", "task_id", "e2e_ms", "completed",
                       "met_deadline"),
                     basename(path))
  keep <- .emul_key(recs$load, recs$round, recs$task_id) %in%
    .emul_key(keys$load, keys$round, keys$task_id)
  recs <- recs[keep, ]
  data.frame(
    load = recs$load, round = as.integer(recs$round),
    latency_ms = as.numeric(recs$e2e_ms),
    completed = .emul_flag(recs$completed, basename(path)),
    met_deadline = .emul_flag(recs$met_deadline, basename(path)),
    stringsAsFactors = FALSE
  )
}

#' The simulator's per-task rows, restricted to the tasks the testbed replayed.
emul_sim_tasks <- function(sim_dir, keys) {
  rows <- list()
  for (load in unique(keys$load)) {
    path <- file.path(sim_dir, sprintf("sim_tasks_%s.csv", load))
    if (!file.exists(path)) {
      stop(sprintf("no simulator rows for load %s (%s)", load, path),
           call. = FALSE)
    }
    recs <- .emul_need(utils::read.csv(path, stringsAsFactors = FALSE),
                       c("round", "task_id", "latency_ms", "met_deadline"),
                       basename(path))
    want <- keys[keys$load == load, ]
    keep <- .emul_key(load, recs$round, recs$task_id) %in%
      .emul_key(load, want$round, want$base_id)
    recs <- recs[keep, ]
    rows[[load]] <- data.frame(
      load = load, round = as.integer(recs$round),
      latency_ms = as.numeric(recs$latency_ms),
      met_deadline = .emul_flag(recs$met_deadline, basename(path)),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

#' The offered load per replayed round: what the market generated, not what it
#' admitted.
#'
#' One row per generated task per round is exactly what the allocation export
#' writes, so the count of rows is the offered load. Both worlds read the same
#' file, so the elasticity's denominator is one number rather than two that
#' nearly cancel.
#'
#' @param sim_dir Directory holding alloc_<load>.csv.
#' @param keys    Replayed keys from emul_replayed_keys().
#' @return A named numeric vector, one entry per (load, round).
emul_offered_per_round <- function(sim_dir, keys) {
  out <- numeric(0)
  for (load in unique(keys$load)) {
    path <- file.path(sim_dir, sprintf("alloc_%s.csv", load))
    if (!file.exists(path)) {
      stop(sprintf(paste("no offered-load export for load %s (%s); the elasticity",
                         "divides by the tasks the market generated and only",
                         "alloc_%s.csv carries them"), load, path, load),
           call. = FALSE)
    }
    recs <- .emul_need(utils::read.csv(path, stringsAsFactors = FALSE),
                       c("round", "task_id", "admitted"), basename(path))
    rounds <- sort(unique(keys$round[keys$load == load]))
    n <- tapply(seq_len(nrow(recs)), as.integer(recs$round),
                length)[as.character(rounds)]
    if (anyNA(n)) {
      stop(sprintf("%s has no rows for replayed round(s): %s", basename(path),
                   paste(rounds[is.na(n)], collapse = ", ")), call. = FALSE)
    }
    out <- c(out, setNames(as.numeric(n), .emul_key(load, rounds, "")))
  }
  out
}

#' Per-round bootstrap units: the round is the resampling unit on both sides.
#'
#' @param tasks   Per-task rows for one side.
#' @param offered Offered load per (load, round), from emul_offered_per_round().
emul_round_units <- function(tasks, offered) {
  parts <- split(seq_len(nrow(tasks)),
                 .emul_key(tasks$load, tasks$round, ""))
  absent <- setdiff(names(parts), names(offered))
  if (length(absent)) {
    stop(sprintf("no offered load for %d replayed round(s)", length(absent)),
         call. = FALSE)
  }
  list(
    load   = vapply(parts, function(i) tasks$load[i[1]], character(1),
                    USE.NAMES = FALSE),
    n      = vapply(parts, length, integer(1), USE.NAMES = FALSE),
    off    = unname(offered[names(parts)]),
    n_miss = vapply(parts, function(i) sum(!tasks$met_deadline[i]), integer(1),
                    USE.NAMES = FALSE),
    med    = vapply(parts, function(i) median(tasks$latency_ms[i]), numeric(1),
                    USE.NAMES = FALSE),
    lat    = unname(lapply(parts, function(i) tasks$latency_ms[i]))
  )
}

#' The three primary statistics on one side, over a set of rounds.
#'
#' @param u   Units from emul_round_units().
#' @param idx Unit indices (a bootstrap resample, or all of them).
#' @param lo,hi The two load levels, ordered by offered load.
emul_primary_stats <- function(u, idx, lo, hi) {
  at   <- function(load) idx[u$load[idx] == load]
  L    <- function(w) median(u$med[w])
  rho  <- function(w) mean(u$off[w])
  tail <- function(w) {
    q <- unname(stats::quantile(unlist(u$lat[w]), c(0.5, 0.95)))
    q[2] / q[1]
  }
  drop <- function(w) sum(u$n_miss[w]) / sum(u$n[w])

  w_lo <- at(lo)
  w_hi <- at(hi)
  paired <- !identical(lo, hi) && length(w_lo) > 0L && length(w_hi) > 0L
  # Two loads that offered the same number of tasks per round do not measure an
  # infinite sensitivity to offered load; they measure none, and NA says so.
  den <- if (paired) rho(w_hi) / rho(w_lo) - 1 else NA_real_
  c(elasticity = if (isTRUE(is.finite(den)) && abs(den) > 1e-9)
                   (L(w_hi) / L(w_lo) - 1) / den
                 else NA_real_,
    tail_lo   = tail(w_lo),
    tail_hi   = tail(w_hi),
    drop_lo   = drop(w_lo),
    drop_hi   = drop(w_hi),
    drop_step = if (paired) drop(w_hi) - drop(w_lo) else NA_real_)
}

# BCa, falling back to the percentile interval where BCa cannot be computed
# (a degenerate resample distribution, or too few rounds for the acceleration
# term), which is what bootstrap_ci() does for the rest of the pipeline.
.emul_ci <- function(b, index, conf) {
  none <- c(NA_real_, NA_real_)
  if (!is.finite(b$t0[index]) ||
      length(unique(b$t[, index][is.finite(b$t[, index])])) < 2L) return(none)
  bca <- tryCatch(boot::boot.ci(b, conf = conf, type = "bca", index = index),
                  error = function(e) NULL, warning = function(w) NULL)
  if (!is.null(bca$bca)) return(c(bca$bca[4], bca$bca[5]))
  pct <- tryCatch(boot::boot.ci(b, conf = conf, type = "perc", index = index),
                  error = function(e) NULL, warning = function(w) NULL)
  if (!is.null(pct$percent)) return(c(pct$percent[4], pct$percent[5]))
  none
}

#' One side's statistics with bootstrap intervals over its rounds.
emul_side_stats <- function(tasks, offered, lo, hi, n_boot = 2000L,
                            conf = 0.95) {
  u <- emul_round_units(tasks, offered)
  d <- data.frame(i = seq_along(u$load))
  b <- boot::boot(d, function(d, w) emul_primary_stats(u, d$i[w], lo, hi),
                  R = n_boot, strata = factor(u$load))
  est <- emul_primary_stats(u, d$i, lo, hi)
  ci  <- vapply(seq_along(est), function(k) .emul_ci(b, k, conf), numeric(2))
  data.frame(key = names(est), estimate = unname(est),
             lo = ci[1, ], hi = ci[2, ], stringsAsFactors = FALSE)
}

#' The rounds a calibration statistic may be computed on.
#'
#' The three primary statistics describe a system with all three tiers up. A
#' run that lost one halfway through is not a quieter version of the same run,
#' so a run_meta carrying a kill is refused outright unless the caller says, in
#' so many words, that it wants the clean prefix of that run.
.emul_clean_keys <- function(keys, t_kill, clean_only) {
  if (is.null(t_kill) || !length(t_kill)) return(keys)
  first_degraded <- as.integer(t_kill$round)
  if (!isTRUE(clean_only)) {
    stop(sprintf(paste("this run killed %s before round %s: the calibration",
                       "statistics are computed on clean runs. Point at a run",
                       "with no kill, or pass clean_only = TRUE to compute them",
                       "on rounds 1..%d of this one"),
                 t_kill$service, t_kill$round, first_degraded - 1L),
         call. = FALSE)
  }
  keep <- keys[keys$round < first_degraded, ]
  if (nrow(keep) == 0L) {
    stop(sprintf("the kill landed before round %d, so this run has no clean rounds",
                 first_degraded), call. = FALSE)
  }
  keep
}

#' The completion gap after a permanent tier loss: the failure run's statistic.
#'
#' The simulator has no failure model, so it goes on scoring the tasks the
#' testbed can no longer complete. The per-round divergence is the quantity the
#' model does not represent, and it is reported from the failure run alone,
#' never from a run that was supposed to be clean.
#'
#' @param run_dir Directory holding task_records.csv and run_meta.json.
#' @param sim_dir Directory holding sim_tasks_<load>.csv (default: run_dir).
#' @return A data frame: load, round, post_kill, n, emul_completed,
#'   sim_met_deadline, gap.
emul_failure_gap <- function(run_dir, sim_dir = run_dir) {
  meta <- jsonlite::fromJSON(file.path(run_dir, "run_meta.json"),
                             simplifyVector = FALSE)
  if (is.null(meta$t_kill) || !length(meta$t_kill)) {
    stop("run_meta.json records no kill: the post-kill completion gap is a ",
         "statistic of the failure run", call. = FALSE)
  }
  first_degraded <- as.integer(meta$t_kill$round)
  keys <- emul_replayed_keys(meta)
  emul <- emul_testbed_tasks(file.path(run_dir, "task_records.csv"), keys)
  sim  <- emul_sim_tasks(sim_dir, keys)

  unit <- unique(data.frame(load = emul$load, round = emul$round,
                            stringsAsFactors = FALSE))
  unit <- unit[order(.emul_load_rank(unit$load), unit$round), ]
  k <- .emul_key(unit$load, unit$round, "")
  rate <- function(df, col) {
    tapply(as.numeric(df[[col]]), .emul_key(df$load, df$round, ""), mean)[k]
  }
  done <- unname(rate(emul, "completed"))
  met  <- unname(rate(sim, "met_deadline"))
  data.frame(
    load = unit$load, round = unit$round,
    post_kill = unit$round >= first_degraded,
    n = unname(tapply(rep(1L, nrow(emul)),
                      .emul_key(emul$load, emul$round, ""), sum)[k]),
    emul_completed = done, sim_met_deadline = met, gap = met - done,
    stringsAsFactors = FALSE, row.names = NULL
  )
}

.emul_counts <- function(tasks, loads) {
  rows <- tasks[tasks$load %in% loads, ]
  c(rounds = length(unique(paste(rows$load, rows$round))), tasks = nrow(rows))
}

#' Compare the emulated run against the simulated run it replayed.
#'
#' @param run_dir  Directory holding task_records.csv and run_meta.json.
#' @param sim_dir  Directory holding sim_tasks_<load>.csv (default: run_dir).
#' @param n_boot   Bootstrap resamples (default: 2000, the pipeline's).
#' @param conf     Interval level.
#' @param clean_only Compute the statistics on the rounds before a recorded
#'   kill. Required, and refused by default, on a run that contains one; see
#'   emul_failure_gap() for the statistic that run does support.
#' @return A data frame, one row per statistic: both point estimates, both
#'   intervals, whether each side's estimate lies inside the other's interval,
#'   whether the intervals overlap, whether the signs match, and the number of
#'   rounds and tasks each side computed on.
emul_compare <- function(run_dir, sim_dir = run_dir, n_boot = 2000L,
                         conf = 0.95, clean_only = FALSE) {
  meta <- jsonlite::fromJSON(file.path(run_dir, "run_meta.json"),
                             simplifyVector = FALSE)
  keys <- .emul_clean_keys(emul_replayed_keys(meta), meta$t_kill, clean_only)
  emul <- emul_testbed_tasks(file.path(run_dir, "task_records.csv"), keys)
  sim  <- emul_sim_tasks(sim_dir, keys)
  for (side in list(list("testbed", emul), list("simulator", sim))) {
    if (nrow(side[[2]]) == 0L) {
      stop(sprintf("no %s rows for any replayed task", side[[1]]), call. = FALSE)
    }
  }

  # Order the loads by the offered load the market generated, so "high" is
  # whichever load offered more tasks per round rather than whichever ran more
  # (the admitted counts tie once admission saturates) or whichever is named
  # that. A tie in the offered load falls back to the declared order of the
  # levels, never to the alphabet.
  offered <- emul_offered_per_round(sim_dir, keys)
  load_lv <- unique(emul$load)
  rho_of  <- vapply(load_lv, function(l)
    mean(offered[.emul_key(l, sort(unique(emul$round[emul$load == l])), "")]),
    numeric(1))
  ordered <- load_lv[order(rho_of, .emul_load_rank(load_lv))]
  lo <- ordered[1]
  hi <- ordered[length(ordered)]

  sim_stats  <- emul_side_stats(sim,  offered, lo, hi, n_boot = n_boot, conf = conf)
  emul_stats <- emul_side_stats(emul, offered, lo, hi, n_boot = n_boot, conf = conf)

  spec <- data.frame(
    key       = c("elasticity", "tail_lo", "tail_hi", "drop_lo", "drop_hi",
                  "drop_step"),
    statistic = c("latency_elasticity", "tail_ratio", "tail_ratio",
                  "drop_rate", "drop_rate", "drop_rate_step"),
    load      = c(paste(hi, lo, sep = "/"), lo, hi, lo, hi,
                  paste(hi, lo, sep = "-")),
    stringsAsFactors = FALSE
  )
  # With one load the two sides of the step are the same side: the cross-load
  # statistics cannot be measured at all, and the per-side rows would otherwise
  # be emitted twice, which reads as two measurements and doubles every count.
  if (identical(lo, hi)) {
    spec <- spec[!spec$key %in% c("elasticity", "drop_step", "tail_hi", "drop_hi"), ]
  }
  loads_of <- lapply(spec$key, function(k) {
    if (k %in% c("tail_lo", "drop_lo")) lo
    else if (k %in% c("tail_hi", "drop_hi")) hi
    else unique(c(lo, hi))
  })

  s <- sim_stats[match(spec$key, sim_stats$key), ]
  e <- emul_stats[match(spec$key, emul_stats$key), ]
  # A statistic that is constant over rounds has no interval. The table says so
  # with NA rather than reporting the other side as lying outside one.
  inside <- function(x, lo_, hi_) ifelse(is.na(lo_) | is.na(hi_), NA, x >= lo_ & x <= hi_)
  n_sim  <- vapply(loads_of, function(l) .emul_counts(sim, l), numeric(2))
  n_emul <- vapply(loads_of, function(l) .emul_counts(emul, l), numeric(2))

  data.frame(
    statistic = spec$statistic,
    load      = spec$load,
    sim = s$estimate, sim_lo = s$lo, sim_hi = s$hi,
    emul = e$estimate, emul_lo = e$lo, emul_hi = e$hi,
    sim_in_emul_ci = inside(s$estimate, e$lo, e$hi),
    emul_in_sim_ci = inside(e$estimate, s$lo, s$hi),
    ci_overlap     = ifelse(is.na(s$lo) | is.na(e$lo), NA,
                            s$lo <= e$hi & e$lo <= s$hi),
    same_sign      = !is.na(s$estimate) & !is.na(e$estimate) &
                       sign(s$estimate) == sign(e$estimate),
    n_rounds_sim = n_sim[1, ], n_tasks_sim = n_sim[2, ],
    n_rounds_emul = n_emul[1, ], n_tasks_emul = n_emul[2, ],
    stringsAsFactors = FALSE, row.names = NULL
  )
}
