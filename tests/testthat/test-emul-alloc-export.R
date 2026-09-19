# Tests for the allocation export the emulation testbed replays.
#
# The market clears here and the testbed executes: it runs the tasks this file
# says were admitted, and decides nothing. Two properties therefore have to
# hold exactly. Every generated task appears, so the testbed inherits the
# market's own drop denominator instead of recomputing one. And `admitted` is
# written in an encoding the testbed's reader accepts, never as NA: R renders a
# missing logical as NA, and a reader that took NA for "not admitted" would
# shrink the replayed set without a word.

test_that("the writer marks exactly the allocated rows admitted", {
  dir   <- withr::local_tempdir()
  tasks <- tibble::tibble(
    task_id    = c("a1_t1_1", "a1_t1_2", "a2_t1_1"),
    agent_id   = c(1L, 1L, 2L),
    deadline   = c(500, 750, 1000),
    value_base = c(1.1, 1.9, 1.5)
  )
  alloc   <- tasks[c(1L, 3L), ]
  results <- dplyr::mutate(alloc, latency = c(420, 1100), success = latency <= deadline)

  paths <- emul_export_open(dir, "medium")
  emul_export_round(paths, 1L, tasks, alloc, results)

  rows <- utils::read.csv(paths$alloc, stringsAsFactors = FALSE)
  expect_equal(names(rows),
               c("round", "task_id", "agent_id", "deadline_ms", "value_base",
                 "admitted"))
  expect_equal(nrow(rows), 3L)
  expect_equal(rows$task_id[rows$admitted], c("a1_t1_1", "a2_t1_1"))
  expect_false(any(is.na(rows$admitted)))

  # The literal encoding, as it reaches the reader: TRUE/FALSE, never NA.
  raw <- readLines(paths$alloc)
  expect_true(all(grepl(",(TRUE|FALSE)$", raw[-1])))

  sim <- utils::read.csv(paths$sim_tasks, stringsAsFactors = FALSE)
  expect_equal(names(sim),
               c("round", "task_id", "deadline_ms", "latency_ms", "met_deadline"))
  expect_equal(sim$task_id, alloc$task_id)
  expect_equal(sim$met_deadline, c(TRUE, FALSE))
})


test_that("alloc_out writes one row per generated task per round", {
  # A rationing cell on purpose: where the market admits everything, a writer
  # that marked every row admitted would satisfy every identity below.
  dir <- withr::local_tempdir()
  res <- exp4_run_single("naive", graph_type = "sp", load_level = "high",
                         N = 55L, seed = 3L, n_rounds = 8L, alloc_out = dir)
  expect_lt(res$clearing_fraction, 1)

  rows <- utils::read.csv(file.path(dir, "alloc_high.csv"),
                          stringsAsFactors = FALSE)
  expect_gt(nrow(rows), 0L)
  expect_true(all(rows$round %in% seq_len(8L)))
  expect_false(any(duplicated(paste(rows$round, rows$task_id))))

  # The row count is the market's own generated-task count: the share of rows
  # marked admitted, averaged over the rounds that generated anything, is the
  # clearing fraction the run reports. Rows that were not generated tasks, or a
  # task missing from the file, would both break this identity.
  per_round <- tapply(rows$admitted, rows$round, mean)
  expect_equal(mean(per_round), res$clearing_fraction)

  # The environment being replayed travels with the allocations.
  env_spec <- jsonlite::fromJSON(file.path(dir, "env_high.json"))
  expect_equal(env_spec$graph_type, "sp")
  expect_equal(env_spec$load_level, "high")
  expect_equal(env_spec$n_agents, 55L)
  expect_equal(env_spec$n_rounds, 8L)
  expect_equal(env_spec$seed, 3L)

  # So do the simulator's own outcomes for the admitted tasks, which is what
  # the comparison restricts to the replayed ids.
  sim <- utils::read.csv(file.path(dir, "sim_tasks_high.csv"),
                         stringsAsFactors = FALSE)
  expect_equal(nrow(sim), sum(rows$admitted))
  expect_setequal(paste(sim$round, sim$task_id),
                  paste(rows$round, rows$task_id)[rows$admitted])
  expect_false(any(is.na(sim$latency_ms)))
  expect_equal(sim$met_deadline, sim$latency_ms <= sim$deadline_ms)
})


test_that("the export parses with the testbed's own reader", {
  skip_if(Sys.which("python3") == "", "python3 is not on PATH")
  dir <- withr::local_tempdir()
  exp4_run_single("hybrid", graph_type = "sp", load_level = "high",
                  N = 12L, seed = 5L, n_rounds = 4L, alloc_out = dir)

  path <- file.path(dir, "alloc_high.csv")
  py <- sprintf(paste0("import sys; sys.path.insert(0, '%s'); import replay; ",
                       "rounds = replay.load_rounds('%s'); ",
                       "print(len(rounds), sum(len(r['tasks']) for r in rounds))"),
                here::here("emul"), path)
  out <- suppressWarnings(system2("python3", c("-c", shQuote(py)),
                                  stdout = TRUE, stderr = TRUE))
  expect_null(attr(out, "status"))

  rows   <- utils::read.csv(path, stringsAsFactors = FALSE)
  counts <- as.integer(strsplit(tail(out, 1L), " ")[[1]])
  expect_equal(counts[1], length(unique(rows$round)))
  expect_equal(counts[2], sum(rows$admitted))
})
