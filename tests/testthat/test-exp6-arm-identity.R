# Bit-neutrality guard for the allocation path the deployed-practice arms extend.
#
# Two mechanism levels enter Exp.6 through code every existing arm also runs:
# the score chain, the multi-tier greedy packer and the slice-capacity packer.
# This fixture was recorded from the four original mechanisms, on both
# architectures, BEFORE any of that code was touched, and in a cell where
# capacity binds (drop rates 0.36 to 0.61 across the arms) so that a packing
# change cannot hide in a slack cell where everything clears anyway.
# Re-recording it would destroy the only evidence that adding the arms moved
# nothing.

test_that("adding mechanism levels leaves the existing Exp.6 arms bit-identical", {
  expected <- readRDS(test_path("fixtures", "exp6-arms-sp-high-n55-seed1.rds"))
  actual   <- purrr::pmap_dfr(
    expected[c("mechanism", "architecture")],
    function(mechanism, architecture) {
      exp6_run_single(mechanism, architecture, "sp", "high",
                      N = 55L, seed = 1L, n_rounds = 10L)
    }
  )
  # Compared on the columns the fixture recorded: a column added later is a
  # further statistic of the same runs and cannot move the recorded ones.
  expect_identical(actual[names(expected)], expected)
})
