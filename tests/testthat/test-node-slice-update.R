# The domain slice is two markets, and each learns from its own round.
#
# The sliced residency arm clears the EU leaves and the non-EU leaves against
# two separate market states. Each state's success model is what its bids are
# formed from, so each has to be updated every round from the tasks it
# cleared, exactly as the single market's model is updated from its own.

slice_results <- function() tibble::tibble(
  task_id = sprintf("t%02d", 1:8), agent_id = 1:8,
  success = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE))

test_that("each slice's success model is updated from its own tasks", {
  env <- node_run_env("tree", "high", 90L, "uniform", "off")
  ms  <- init_market_state(env); ms_ne <- init_market_state(env)
  res <- slice_results()
  up  <- node_slice_success_update(ms, ms_ne, 0.6, res, eu_ids = res$task_id[1:4],
                                   lr = 0.3)
  one <- function(state, r) market_update_from_results(state, 0.6, r, lr = 0.3)
  expect_identical(up$eu, one(ms, res[1:4, ]))
  expect_identical(up$non_eu, one(ms_ne, res[5:8, ]))
  expect_false(identical(up$non_eu$success_model, ms_ne$success_model))
})

test_that("both slices' models move over a ten-round sliced run", {
  seen <- list()
  orig <- node_slice_success_update
  assign("node_slice_success_update", function(...) {
    out <- orig(...)
    seen[[length(seen) + 1L]] <<- out
    out
  }, envir = globalenv())
  on.exit(assign("node_slice_success_update", orig, envir = globalenv()))
  node_run_single("tree", "high", N = 90L, seed = 1L, n_rounds = 10L,
                  policy = "residency_sliced")
  expect_length(seen, 10L)
  first <- seen[[1]]; last <- seen[[10]]
  init  <- init_success_model()
  expect_false(identical(last$eu$success_model, init))
  expect_false(identical(last$non_eu$success_model, init))
  expect_false(identical(first$non_eu$success_model, last$non_eu$success_model))
})
