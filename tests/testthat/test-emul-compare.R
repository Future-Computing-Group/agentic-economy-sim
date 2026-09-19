# Tests for the side-by-side comparison of the two worlds.
#
# The fixtures are constructed, not measured: one pair of worlds that agree by
# construction (the simulator's latencies are the testbed's on a different
# scale, which is exactly what the three scale-free statistics are supposed to
# see through) and one pair that disagree in both of the ways the procedure is
# meant to name -- no elasticity, and a drop-rate step of the opposite sign.

# Per load: tasks replayed per round (n), deadline misses per round, and tasks
# the market GENERATED per round (offered). The elasticity divides by the
# offered load, so `offered` is what the two loads must differ in; `n` can tie
# across loads and does so whenever admission saturates at the slice capacity.
FIX <- list(medium = list(n = 6L, base = 100, miss = 1L, flip = 3L, offered = 6L),
            high   = list(n = 9L, base = 150, miss = 3L, flip = 1L, offered = 9L))

emul_fixture <- function(dir, n_rounds = 12L, sim_scale = 3.5,
                         sim_high_gain = 1, sim_flip_drop = FALSE,
                         replicas = FALSE, fix = FIX, kill_round = NULL) {
  jitter <- function(rnd, i) 1 + ((rnd * 3L + i * 7L) %% 11L) / 40
  emul <- sim <- list()
  replayed <- list()

  for (load in names(fix)) {
    f <- fix[[load]]
    alloc <- list()
    gain <- if (load == "high") sim_high_gain else 1
    ids_by_round <- list()
    for (rnd in seq_len(n_rounds)) {
      base_id <- sprintf("%s%d_%d", substr(load, 1L, 1L), rnd, seq_len(f$n))
      lat     <- f$base * jitter(rnd, seq_len(f$n))
      met     <- seq_len(f$n) > f$miss
      sim_met <- if (sim_flip_drop) seq_len(f$n) > f$flip else met
      run_id  <- if (replicas && load == "high") paste0(base_id, "#", seq_len(f$n)) else base_id

      # After the kill the sink tier is gone: the task errors out, so it
      # neither completes nor meets its deadline.
      alive <- is.null(kill_round) || rnd < kill_round
      emul[[length(emul) + 1L]] <- data.frame(
        run_id = "fixture", round = rnd, task_id = run_id,
        deadline_ms = 1000, e2e_ms = lat,
        completed = ifelse(alive, "True", "False"),
        met_deadline = ifelse(met & alive, "True", "False"),
        error = ifelse(alive, "", "URLError"), load = load,
        stringsAsFactors = FALSE
      )
      # A row in the testbed's own records that was never replayed: an
      # aborted round, a warm-up, a record from the failure run read by
      # mistake. The restriction has to hold on this side too.
      emul[[length(emul) + 1L]] <- data.frame(
        run_id = "fixture", round = rnd,
        task_id = paste0("stray_", load, "_", rnd),
        deadline_ms = 1000, e2e_ms = 50000, completed = "True",
        met_deadline = "False", error = "", load = load,
        stringsAsFactors = FALSE
      )
      sim[[load]][[length(sim[[load]]) + 1L]] <- data.frame(
        round = rnd, task_id = base_id, deadline_ms = 1000,
        latency_ms = sim_scale * gain * lat, met_deadline = sim_met,
        stringsAsFactors = FALSE
      )
      # Tasks the market admitted but the testbed did not replay. They are in
      # the simulator's rows and must not reach any statistic.
      sim[[load]][[length(sim[[load]]) + 1L]] <- data.frame(
        round = rnd, task_id = paste0("decoy_", load, "_", rnd), deadline_ms = 1000,
        latency_ms = 50000, met_deadline = FALSE, stringsAsFactors = FALSE
      )
      # The market's own export: one row per GENERATED task, admitted or not.
      # The rows beyond the admitted count are the offered load the testbed
      # never sees in its own records.
      alloc[[length(alloc) + 1L]] <- data.frame(
        round = rnd,
        task_id = c(base_id, sprintf("%s%d_rej%d", substr(load, 1L, 1L), rnd,
                                     seq_len(max(0L, f$offered - f$n)))),
        agent_id = seq_len(f$offered), deadline_ms = 1000, value_base = 1,
        admitted = seq_len(f$offered) <= f$n, stringsAsFactors = FALSE
      )
      ids_by_round[[as.character(rnd)]] <- run_id
    }
    replayed[[load]] <- ids_by_round
    utils::write.csv(do.call(rbind, sim[[load]]),
                     file.path(dir, sprintf("sim_tasks_%s.csv", load)),
                     row.names = FALSE)
    utils::write.csv(do.call(rbind, alloc),
                     file.path(dir, sprintf("alloc_%s.csv", load)),
                     row.names = FALSE)
  }

  utils::write.csv(do.call(rbind, emul), file.path(dir, "task_records.csv"),
                   row.names = FALSE)
  meta <- list(run_id = "fixture", subsample_seed = 7L,
               replayed_task_ids = replayed)
  # The shape the harness writes: absent when the run was clean, a block naming
  # the service and the round when it was not.
  if (!is.null(kill_round)) {
    meta$t_kill <- list(service = "cloud", round = kill_round, t = 0, rc = 0)
  }
  jsonlite::write_json(meta, file.path(dir, "run_meta.json"), auto_unbox = TRUE)
  invisible(dir)
}

compare_fixture <- function(...) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  emul_fixture(dir, ...)
  set.seed(1L)
  emul_compare(dir, n_boot = 200L)
}

pick <- function(tab, stat, load = NULL) {
  hit <- tab[tab$statistic == stat, ]
  if (!is.null(load)) hit <- hit[hit$load == load, ]
  expect_equal(nrow(hit), 1L)
  hit
}


test_that("worlds that differ only in scale agree on all three statistics", {
  tab <- compare_fixture()

  expect_setequal(unique(tab$statistic),
                  c("latency_elasticity", "tail_ratio", "drop_rate",
                    "drop_rate_step"))

  # Scale-free by construction: a simulator running 3.5x slower reports the
  # same elasticity and the same tail ratio as the testbed.
  for (row in list(pick(tab, "latency_elasticity"),
                   pick(tab, "tail_ratio", "medium"),
                   pick(tab, "tail_ratio", "high"))) {
    expect_equal(row$sim, row$emul)
    expect_true(is.finite(row$sim))
    expect_true(row$sim_in_emul_ci)
    expect_true(row$emul_in_sim_ci)
    expect_true(row$ci_overlap)
  }

  # Drop rates are rates already: they are compared at their level, and the
  # decoy rows would move them if the replayed-id restriction were not applied.
  expect_equal(pick(tab, "drop_rate", "medium")$sim, 1 / 6)
  expect_equal(pick(tab, "drop_rate", "high")$sim, 1 / 3)
  expect_equal(pick(tab, "drop_rate", "medium")$emul, 1 / 6)
  expect_equal(pick(tab, "drop_rate", "high")$emul, 1 / 3)

  step <- pick(tab, "drop_rate_step")
  expect_equal(step$sim, 1 / 6)
  expect_equal(step$emul, 1 / 6)
  expect_true(step$same_sign)

  # A statistic that is the same in every round has no interval to report; the
  # table says that with NA rather than calling the two sides separated.
  no_ci <- pick(tab, "drop_rate", "medium")
  expect_true(all(is.na(no_ci[c("sim_lo", "sim_in_emul_ci", "ci_overlap")])))

  # N travels with every statistic, on both sides.
  expect_true(all(tab$n_rounds_sim > 0 & tab$n_rounds_emul > 0))
  expect_true(all(tab$n_tasks_sim > 0 & tab$n_tasks_emul > 0))
  expect_equal(pick(tab, "drop_rate", "high")$n_tasks_emul, 12L * 9L)
})


test_that("a world with no elasticity and a flipped drop step is named as one", {
  tab <- compare_fixture(sim_high_gain = 1 / 1.5, sim_flip_drop = TRUE)

  el <- pick(tab, "latency_elasticity")
  expect_lt(abs(el$sim), 0.2)      # the simulator's latency barely moves with load
  expect_gt(el$emul, 0.8)          # the testbed's tracks the offered load
  expect_false(el$sim_in_emul_ci)
  expect_false(el$emul_in_sim_ci)
  expect_false(el$ci_overlap)

  step <- pick(tab, "drop_rate_step")
  expect_lt(step$sim, 0)
  expect_gt(step$emul, 0)
  expect_false(step$same_sign)
})


test_that("replica task ids map back to the task the simulator ran", {
  # Above the admitted count the generator replays a task twice, under the id
  # "<task_id>#<n>". Those ids exist only in the testbed; a comparison that
  # looked them up verbatim would silently drop every replayed high-load task.
  plain    <- compare_fixture()
  replicas <- compare_fixture(replicas = TRUE)
  expect_equal(replicas, plain)
})


test_that("the elasticity divides by the offered load, not by what was admitted", {
  # Admission saturates at the slice capacity, so both loads replay the same
  # number of tasks per round and differ only in what the market generated.
  # Dividing by the replayed count makes the denominator exactly zero and the
  # statistic +/-Inf on both sides; dividing by the offered load leaves it the
  # ratio of ratios it is defined to be.
  tab <- compare_fixture(fix = list(
    medium = list(n = 6L, base = 100, miss = 1L, flip = 3L, offered = 6L),
    high   = list(n = 6L, base = 150, miss = 3L, flip = 1L, offered = 9L)))

  el <- pick(tab, "latency_elasticity")
  expect_equal(el$load, "high/medium")
  for (x in c(el$sim, el$emul)) {
    expect_true(is.finite(x))
    expect_gt(x, 0)
  }
  # (150/100 - 1) / (9/6 - 1) = 1, on both sides: the denominator is the
  # market's, so it is the same number in both worlds.
  expect_equal(el$emul, 1)
  expect_equal(el$sim, 1)
})


test_that("loads with the same offered load report NA for the elasticity, not Inf", {
  tab <- compare_fixture(fix = list(
    medium = list(n = 6L, base = 100, miss = 1L, flip = 3L, offered = 6L),
    high   = list(n = 6L, base = 150, miss = 3L, flip = 1L, offered = 6L)))

  el <- pick(tab, "latency_elasticity")
  # A zero denominator is not an infinite elasticity, it is no measurement.
  expect_true(is.na(el$sim))
  expect_true(is.na(el$emul))
  expect_false(isTRUE(is.infinite(el$sim)) || isTRUE(is.infinite(el$emul)))

  # The tie is broken by the declared order of the load levels. Alphabetically
  # "high" sorts below "medium", which would silently invert every statistic
  # that is reported per side of the step.
  expect_equal(el$load, "high/medium")
  expect_equal(pick(tab, "drop_rate_step")$load, "high-medium")
  expect_equal(pick(tab, "tail_ratio", "medium")$emul,
               pick(tab, "tail_ratio", "high")$emul)
})


test_that("a run whose offered load was never exported is refused", {
  dir <- withr::local_tempdir()
  emul_fixture(dir)
  file.remove(file.path(dir, "alloc_high.csv"))
  expect_error(emul_compare(dir, n_boot = 20L), "alloc_high.csv")
})


test_that("a run that contains a kill is refused for the calibration statistics", {
  dir <- withr::local_tempdir()
  emul_fixture(dir, kill_round = 7L)

  # The three primary statistics are a property of clean rounds. A run whose
  # second half has a tier missing is not a quieter version of the same run.
  expect_error(emul_compare(dir, n_boot = 20L), "clean_only")
  expect_error(emul_compare(dir, n_boot = 20L), "cloud")

  set.seed(1L)
  tab <- emul_compare(dir, n_boot = 200L, clean_only = TRUE)
  # Rounds 1..6: the kill lands before round 7, so round 7 is already degraded.
  expect_equal(unique(tab$n_rounds_emul[tab$statistic == "tail_ratio"]), 6)
  expect_equal(unique(tab$n_rounds_sim[tab$statistic == "tail_ratio"]), 6)
  expect_true(all(is.finite(tab$emul[tab$statistic == "tail_ratio"])))
  # Every surviving round completed, so the clean drop rates are the fixture's
  # own miss rates and none of the post-kill zeros reached them.
  expect_equal(pick(tab, "drop_rate", "medium")$emul, 1 / 6)
  expect_equal(pick(tab, "drop_rate", "high")$emul, 1 / 3)
})


test_that("the post-kill completion gap is a statistic of the failure run only", {
  clean <- withr::local_tempdir()
  emul_fixture(clean)
  expect_error(emul_failure_gap(clean), "no kill")

  dir <- withr::local_tempdir()
  emul_fixture(dir, kill_round = 7L)
  gap <- emul_failure_gap(dir)

  expect_equal(nrow(gap), 2L * 12L)
  expect_setequal(unique(gap$load), c("medium", "high"))
  expect_equal(sort(unique(gap$round[gap$post_kill])), 7:12)

  post <- gap[gap$post_kill, ]
  pre  <- gap[!gap$post_kill, ]
  # The measurement: the testbed stops completing the tasks the simulator goes
  # on scoring, and the gap is what the model does not represent.
  expect_true(all(post$emul_completed == 0))
  expect_true(all(pre$emul_completed == 1))
  expect_equal(post$gap, post$sim_met_deadline)
  expect_true(all(post$gap > 0))
  expect_true(all(post$n > 0))
})


test_that("a single-load run reports each statistic once", {
  # With one load the "low side" and the "high side" are the same side, so the
  # per-side rows are the same row. Emitting both makes the table look like two
  # measurements where there is one, and doubles every count a reader sums.
  dir <- withr::local_tempdir()
  emul_fixture(dir, fix = FIX["high"])
  set.seed(1L)
  tab <- emul_compare(dir, n_boot = 100L)

  expect_equal(nrow(tab), 2L)
  expect_equal(tab$statistic, c("tail_ratio", "drop_rate"))
  expect_true(all(tab$load == "high"))
  expect_false(any(duplicated(tab[c("statistic", "load")])))
  # The cross-load statistics are absent, not NA rows: one load cannot measure
  # a step between loads.
  expect_false(any(tab$statistic %in% c("latency_elasticity", "drop_rate_step")))
})
