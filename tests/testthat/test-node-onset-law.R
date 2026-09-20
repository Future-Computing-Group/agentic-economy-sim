# The onset of price dispersion is the first node crossing, and the node that
# crosses first is the one with the largest fluctuation relative to its own
# capacity, not the one that sets K_c.

test_that("on T and X the binding node is an edge node and the law coincides", {
  grid <- c(35, 40, 45)
  t_law <- node_onset_law(node_instance("tree"), "uniform", "high", grid, 200)
  x_law <- node_onset_law(node_instance("entangled"), "uniform", "high", grid, 200)
  expect_true(all(t_law$binding_node %in% c("e1", "e2", "e3")))
  expect_true(all(x_law$binding_node %in% c("e1", "e2", "e3")))
  expect_equal(t_law$expected_crossings, x_law$expected_crossings)
  # ten seeds of 200 rounds: below a tenth of a crossing at 35, about one half
  # at 40, several at 45, which is where the sweep first sees dispersion
  expect_lt(10 * t_law$expected_crossings[1], 0.1)
  expect_gt(10 * t_law$expected_crossings[2], 0.3)
  expect_gt(10 * t_law$expected_crossings[3], 3)
})

test_that("on S every task loads every edge node, so the share is one", {
  s_law <- node_onset_law(node_instance("sp"), "uniform", "medium", c(20, 30), 200)
  expect_true(all(s_law$binding_node %in% c("e1", "e2", "e3")))
  expect_gt(10 * s_law$expected_crossings[2], 0.3)
  expect_lt(10 * s_law$expected_crossings[1], 0.01)
})

test_that("the law is monotone in N and the root is not the binding node", {
  law <- node_onset_law(node_instance("tree"), "uniform", "medium", seq(30, 90, by = 10), 200)
  expect_true(all(diff(law$expected_crossings) >= 0))
  expect_false(any(law$binding_node == "d"))
})
