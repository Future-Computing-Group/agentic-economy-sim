# Bit-neutrality guard for the node-level driver.
#
# Every column added to a node row is a further statistic of the same run, so
# the run itself must not move: not its arrival stream, not its clearing path
# and not its execution draws. A reference computed inside the round loop is
# the way that breaks, because anything that draws from the random number
# generator shifts every later round of the arm it was added to.
#
# This fixture was recorded from three arms on a congested cell BEFORE the
# reference optima were added. Re-recording it would destroy the only evidence
# that the columns came for free.

test_that("columns added to a node row leave the arms bit-identical", {
  expected <- readRDS(test_path("fixtures", "node-arms-high-seed1.rds"))
  actual   <- purrr::pmap_dfr(
    expected[c("graph_type", "seed", "mechanism", "p_post_k", "architecture")],
    function(graph_type, seed, mechanism, p_post_k, architecture) {
      node_run_single(graph_type, "high", N = 90L, seed = seed, n_rounds = 10L,
                      mechanism = mechanism, p_post_k = p_post_k,
                      architecture = architecture,
                      lambda_l_default = node_lambda_l())
    })
  # Compared on the columns the fixture recorded. Bit identity holds on the
  # recording platform; on another CPU the last binary digit of a
  # floating-point sum can differ, so the guard compares at 1e-12, twelve
  # orders below the smallest change a clearing or execution edit produces.
  expect_equal(actual[names(expected)], expected, tolerance = 1e-12)
})
