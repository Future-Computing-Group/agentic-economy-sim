# Condition (ii) of the export: interchangeable exported units.
#
# The faithfulness probe varies the advertised scalar, condition (i). Here the
# scalar is held at the inner one and the exported units are made
# non-interchangeable instead: two leaf classes whose tasks carry different
# sizes, on the instance whose leaves share one ancestry (S), so any
# over-commitment of the contracted interface is the units', not the scalar's.

test_that("leaf sizes scale the recipes except on the nodes that count units", {
  env <- node_run_env("sp", "high", 45L, "uniform", "inner")
  s   <- c(l1 = 1, l2 = 1, l3 = 2, l4 = 2)
  out <- node_apply_leaf_size(env, s, counted = "J")
  expect_equal(out$recipes$l3[["J"]], env$recipes$l3[["J"]])
  expect_equal(out$recipes$l3[["d"]], 2 * env$recipes$l3[["d"]])
  expect_equal(out$recipes$l1, env$recipes$l1)
  full <- node_apply_leaf_size(env, s)
  expect_equal(full$recipes$l4, 2 * env$recipes$l4)
})

test_that("the unit classes are a grid factor", {
  expect_null(node_unit_sizes("interchangeable"))
  expect_equal(node_unit_sizes("two_class"), c(l1 = 1, l2 = 1, l3 = 2, l4 = 2))
})

run_units <- function(iface, units) node_run_single(
  "sp", "high", N = 45L, seed = 1L, n_rounds = 10L, interface = iface,
  leaf_mix = "skewed", leaf_size = node_unit_sizes(units),
  exact_reference = FALSE)

test_that("with the inner scalar held, non-interchangeable units over-commit", {
  expect_equal(run_units("inner", "interchangeable")$overcommitment, 0)
  expect_gt(run_units("inner", "two_class")$overcommitment, 0)
})

test_that("an uncontracted market charges the sizes it clears against", {
  expect_equal(run_units("off", "two_class")$overcommitment, 0)
})

test_that("the probe's condition-(ii) rows are appended to the interface grid", {
  src <- paste(readLines(here::here("_targets.R")), collapse = "\n")
  expect_true(grepl("node_exp10_param_grid, node_exp10_grid(seq_len(n_seeds))",
                    src, fixed = TRUE))
  g <- node_exp10_grid(1:10)
  old <- 3L * 3L * 2L * 10L
  expect_true(all(g$units[seq_len(old)] == "interchangeable"))
  new <- g[-seq_len(old), ]
  expect_equal(nrow(new), 2L * 2L * 10L)
  expect_true(all(new$units == "two_class" & new$graph_type == "sp"))
  expect_setequal(unique(new$interface), c("off", "inner"))
})
