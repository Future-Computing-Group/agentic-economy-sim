# test-node-report-stability.R
# ---------------------------------------------------------------------------
# What a small change in one report does to the whole allocation.
#
# On a matroidal region the greedy allocation is stable in the reports: moving
# one element's value a little either leaves the admitted set alone or swaps
# that element against one exchange partner. Where the region is not a
# matroid there is no such bound, and a one per cent change in one task's
# value can reshuffle the round.
#
# The instrument perturbs one admitted task at a time, re-clears the same
# round from the same carried state, and counts how many tasks changed side.
# ---------------------------------------------------------------------------

test_that("the distance between two admitted sets counts the tasks that changed side", {
  expect_equal(node_hamming(c("a", "b"), c("b", "a")), 0)
  expect_equal(node_hamming(c("a", "b"), c("a", "c")), 2)
  expect_equal(node_hamming(c("a", "b"), character(0)), 2)
  expect_equal(node_hamming(character(0), character(0)), 0)
  expect_equal(node_hamming(c("a"), c("a", "b")), 1)
})

test_that("a perturbation of nothing changes nothing", {
  # The zero step is the instrument's own control: same round, same state,
  # same clearing, so every response must be exactly zero.
  rows <- node_report_stability_run("tree", seed = 1L, n_rounds = 2L,
                                    n_tasks = 3L, steps = 0)
  expect_equal(nrow(rows), 2L * 3L)
  expect_true(all(rows$hamming == 0))
  expect_true(all(rows$welfare_delta == 0))
  expect_true(all(!rows$own_changed))
})

test_that("a perturbation large enough to matter reaches the admitted set", {
  # Collapsing a task's value takes its surplus below zero, so the task it
  # was perturbed on must leave: the perturbation reaches the clearing rather
  # than stopping at the bid.
  rows <- node_report_stability_run("tree", seed = 1L, n_rounds = 2L,
                                    n_tasks = 3L, steps = -0.99)
  expect_true(all(rows$own_changed))
  expect_true(all(rows$hamming >= 1))
})

test_that("the run perturbs admitted tasks of the round it is pricing", {
  rows <- node_report_stability_run("tree", seed = 1L, n_rounds = 2L,
                                    n_tasks = 4L, steps = c(-0.01, 0.01))
  expect_equal(nrow(rows), 2L * 4L * 2L)
  expect_setequal(rows$step, c(-0.01, 0.01))
  expect_equal(sort(unique(rows$round)), 1:2)
  expect_true(all(rows$n_admitted > 0))
  expect_true(all(rows$task_id != ""))
  # The same branch twice is the same branch.
  expect_equal(node_report_stability_run("tree", seed = 1L, n_rounds = 2L,
                                         n_tasks = 4L, steps = c(-0.01, 0.01)),
               rows)
})

test_that("the round stream is the pipeline's own", {
  rows <- node_report_stability_run("tree", seed = 1L, n_rounds = 3L,
                                    n_tasks = 2L, steps = 0)
  ref  <- node_convergence_run("tree", seed = 1L, n_rounds = 3L, iters = 15L)
  expect_equal(unique(rows$n_admitted), ref$admitted)
})

test_that("the summary reports the reshuffles beyond an exchange", {
  rows <- tidyr::expand_grid(graph_type = c("tree", "entangled"),
                             step = c(-0.01, 0.01), round = 1:5) %>%
    dplyr::mutate(seed = 1L, task_id = "t", n_admitted = 50,
                  own_changed = TRUE, welfare_delta = 0.5,
                  hamming = ifelse(graph_type == "entangled" & round > 3, 6, 2),
                  hamming_fixed = 2, own_changed_fixed = TRUE,
                  welfare_delta_fixed = 0.5)
  s <- node_report_stability_summary(rows)
  expect_equal(nrow(s), 4L)
  expect_equal(s$beyond_exchange[s$graph_type == "tree"], c(0, 0))
  expect_equal(s$beyond_exchange[s$graph_type == "entangled"], c(0.4, 0.4))
  expect_equal(s$hamming[s$graph_type == "tree"], c(2, 2))
})

test_that("the sweep runs three instances over the pipeline's seeds", {
  g <- node_report_stability_grid(n_seeds = 10L)
  expect_setequal(g$graph_type, c("tree", "sp", "entangled"))
  expect_equal(nrow(g), 3L * 10L)
})

test_that("the pipeline carries the stability block as its own targets", {
  src <- paste(readLines(here::here("_targets.R")), collapse = "\n")
  for (nm in c("node_exp6_report_stability_grid", "node_exp6_report_stability",
               "node_exp6_report_stability_summary")) {
    expect_true(grepl(paste0("tar_target\\(\\s*", nm, "[,\\s]"), src), info = nm)
  }
})


# ---- the same perturbation with the prices held still -----------------------
#
# The re-clear moves two things at once: the admitted set and the price vector
# the set is packed at. The matroidal bound is a statement about the packing
# rule alone, so the perturbed round is also re-packed at the BASE clearing's
# terminal prices, with no tatonnement. What changes there is the allocation
# rule responding to one report; what the re-clear adds on top of it is the
# price process responding as well.

test_that("the fixed-price re-pack is reported beside the re-clear", {
  rows <- node_report_stability_run("tree", seed = 1L, n_rounds = 2L,
                                    n_tasks = 3L, steps = c(-0.05, 0.05))

  expect_true(all(c("hamming_fixed", "own_changed_fixed",
                    "welfare_delta_fixed") %in% names(rows)))
  # Appended: every column the run already reported is where it was.
  expect_equal(names(rows)[1:12],
               c("graph_type", "load_level", "N", "seed", "round", "task_id",
                 "step", "n_admitted", "hamming", "own_changed",
                 "welfare_delta", "hamming_fixed"))
  expect_true(all(rows$hamming_fixed >= 0))
  expect_true(all(rows$hamming_fixed == round(rows$hamming_fixed)))
  expect_true(all(rows$hamming_fixed <= rows$n_admitted + 1))
  expect_true(is.logical(rows$own_changed_fixed))
  expect_true(all(is.finite(rows$welfare_delta_fixed)))
})

test_that("a perturbation of nothing moves the fixed-price re-pack either", {
  rows <- node_report_stability_run("tree", seed = 1L, n_rounds = 2L,
                                    n_tasks = 3L, steps = 0)
  expect_true(all(rows$hamming_fixed == 0))
  expect_true(all(rows$welfare_delta_fixed == 0))
  expect_true(all(!rows$own_changed_fixed))
})

test_that("a task priced out of its own path leaves, and takes at most one partner", {
  # Collapsing the value takes the task's surplus below zero at the prices the
  # round cleared at, so it must leave. At those same prices no other task's
  # surplus moved, so the only other seat that can change is the one its
  # capacity frees: this is the exchange bound the matroidal region gives.
  rows <- node_report_stability_run("tree", seed = 1L, n_rounds = 2L,
                                    n_tasks = 3L, steps = -0.99)
  expect_true(all(rows$own_changed_fixed))
  expect_true(all(rows$hamming_fixed >= 1))
  expect_true(all(rows$hamming_fixed <= 2))
})

test_that("the summary carries the fixed-price responses beside the re-clear's", {
  rows <- tidyr::expand_grid(graph_type = c("tree", "entangled"),
                             step = c(-0.01, 0.01), round = 1:5) %>%
    dplyr::mutate(seed = 1L, task_id = "t", n_admitted = 50,
                  own_changed = TRUE, welfare_delta = 0.5,
                  hamming = ifelse(graph_type == "entangled" & round > 3, 6, 2),
                  hamming_fixed = ifelse(graph_type == "entangled" & round > 4,
                                         4, 2),
                  own_changed_fixed = round > 2,
                  welfare_delta_fixed = -0.25)
  s <- node_report_stability_summary(rows)

  expect_equal(names(s)[1:7],
               c("graph_type", "step", "n", "hamming_worst", "beyond_exchange",
                 "own_changed", "welfare_delta"))
  expect_equal(s$hamming_fixed_worst[s$graph_type == "tree"], c(2, 2))
  expect_equal(s$hamming_fixed_worst[s$graph_type == "entangled"], c(4, 4))
  expect_equal(s$beyond_exchange_fixed[s$graph_type == "tree"], c(0, 0))
  expect_equal(s$beyond_exchange_fixed[s$graph_type == "entangled"], c(0.2, 0.2))
  expect_equal(s$own_changed_fixed, rep(0.6, 4L))
  expect_equal(s$welfare_delta_fixed, rep(0.25, 4L))
})
