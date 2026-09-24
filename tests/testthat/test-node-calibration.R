# The congestion calibration, written by the pipeline and read back.
#
# The calibrated level is the (clamp, coefficient) pair whose node-grid
# latency elasticity over the medium-to-high step is closest to the one the
# emulated testbed measured. The sweep that selects it is a pipeline output,
# and the level every node block runs at is read from that output rather than
# typed.

cal_csv <- function() here::here("results", "calibration",
                                 "node-congestion-calibration.csv")

test_that("the calibrated level is looked up from the shipped sweep", {
  expect_true(file.exists(cal_csv()))
  sel <- node_calibrated_congestion(cal_csv())
  expect_equal(c(sel$exec_clamp, sel$queue_coef), c(0.95, 0.75))
  # and it is what the level table and the default read
  lv <- node_congestion_levels()
  expect_equal(unlist(lv[lv$congestion == "calibrated",
                         c("exec_clamp", "queue_coef")], use.names = FALSE),
               c(0.95, 0.75))
})

test_that("the sweep grid is the shipped sweep's grid", {
  shipped <- readr::read_csv(cal_csv(), show_col_types = FALSE)
  g <- node_calibration_grid()
  expect_equal(nrow(g), nrow(shipped))
  expect_equal(g$exec_clamp, shipped$exec_clamp)
  expect_equal(g$queue_coef, shipped$queue_coef)
})

test_that("the writer reproduces the shipped row of the selected pair", {
  shipped <- readr::read_csv(cal_csv(), show_col_types = FALSE)
  want <- shipped[shipped$exec_clamp == 0.95 & shipped$queue_coef == 0.75, ]
  row  <- node_calibration_row(0.95, 0.75,
                               target_elasticity = want$target_elasticity)
  expect_equal(names(row), names(shipped))
  expect_equal(row$elasticity, want$elasticity, tolerance = 1e-9)
  expect_equal(row$gap, want$gap, tolerance = 1e-9)
  expect_equal(row$n_seeds, 3L)
  expect_equal(row$n_rounds, 10L)
})

test_that("the pipeline writes the sweep and checks it selects the shipped pair", {
  src <- paste(readLines(here::here("_targets.R")), collapse = "\n")
  expect_true(grepl("results/calibration/node-congestion-calibration.csv", src,
                    fixed = TRUE))
  expect_true(grepl("tar_target\\(\\s*node_calibration_rows[,\\s]", src))
})
