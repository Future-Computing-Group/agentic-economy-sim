# Tests for the Exp.7b strategy set: the named, finite set of joint misreports a
# single agent can play over its own tasks while every other agent stays
# truthful. The set is pinned here so the manuscript's description of it and the
# code cannot drift apart.

test_that("the strategy set has the documented membership and size", {
  # 1 truthful + 5 uniform shades + 2 swaps + 2^k independent + 2 withholding,
  # de-duplicated by multiplier vector: the all-low and all-high corners of the
  # independent grid ARE the 0.7 and 1.3 uniform shades, at k=2 its mixed
  # corners are the two swaps, and at k=1 keep_highest_only is truthful.
  expect_length(misreport_strategy_set(c(3)), 7L)             # k=1: 10 - 3
  expect_length(misreport_strategy_set(c(3, 1)), 10L)         # k=2: 14 - 4
  expect_length(misreport_strategy_set(c(3, 2, 1)), 16L)      # k=3: 18 - 2
  expect_length(misreport_strategy_set(c(4, 3, 2, 1)), 24L)   # k=4: 26 - 2
  # k=5: the exhaustive grid falls back to the 2k single-task deviations, none
  # of which is a uniform shade, so nothing is de-duplicated there.
  S5 <- misreport_strategy_set(c(5, 4, 3, 2, 1))
  expect_length(S5, 20L)                                      # k=5: 1+5+2+10+2
  expect_true(attr(S5, "independent_fallback"))
  expect_false(attr(misreport_strategy_set(c(4, 3, 2, 1)), "independent_fallback"))

  nms <- names(misreport_strategy_set(c(3, 2, 1)))
  expect_true(all(c("truthful", "uniform_0.5", "uniform_0.7", "uniform_0.9",
                    "uniform_1.1", "uniform_1.3", "swap_high_low",
                    "swap_low_high", "drop_highest",
                    "keep_highest_only") %in% nms))
  expect_equal(sum(grepl("^independent_", nms)), 6L)   # LLL and HHH are shades
  expect_false(anyDuplicated(nms) > 0L)
})

test_that("no member repeats another's multiplier vector", {
  for (k in 1:6) {
    S <- misreport_strategy_set(rev(seq_len(k)) + 0.5)
    expect_false(anyDuplicated(vapply(S, paste, character(1),
                                      collapse = "/")) > 0L,
                 info = paste("k =", k))
  }
})

test_that("every member is a deterministic function of the true values", {
  ev <- c(2.5, 0.4, 1.1, 9.0)
  expect_identical(misreport_strategy_set(ev), misreport_strategy_set(ev))
})

test_that("members are multiplier vectors of the agent's task count", {
  ev <- c(2.5, 0.4, 1.1)
  S  <- misreport_strategy_set(ev)
  expect_true(all(vapply(S, length, integer(1)) == 3L))
  expect_equal(S$truthful, c(1, 1, 1))
  expect_equal(S$uniform_0.7, c(0.7, 0.7, 0.7))
})

test_that("swaps and withholding act on the highest / lowest expected value", {
  ev <- c(2.5, 0.4, 1.1)   # highest = task 1, lowest = task 2
  S  <- misreport_strategy_set(ev)
  expect_equal(S$swap_high_low, c(1.3, 0.7, 1))
  expect_equal(S$swap_low_high, c(0.7, 1.3, 1))
  expect_equal(S$drop_highest, c(0, 1, 1))
  expect_equal(S$keep_highest_only, c(1, 0, 0))
})

test_that("the independent block is the exhaustive two-level grid", {
  # Every vector of the grid is still evaluated at k <= 4, under whichever name
  # comes first in S order once duplicates are dropped.
  S <- misreport_strategy_set(c(2, 1))
  for (v in list(c(0.7, 0.7), c(1.3, 0.7), c(0.7, 1.3), c(1.3, 1.3))) {
    expect_true(any(vapply(S, function(m) isTRUE(all.equal(m, v)), logical(1))),
                info = paste(v, collapse = "/"))
  }
  # Above the grid cap, each task deviates alone, low and high.
  ind5 <- misreport_strategy_set(1:5)[grepl("^independent_",
                                            names(misreport_strategy_set(1:5)))]
  expect_length(ind5, 10L)
  expect_true(all(vapply(ind5, function(v) sum(v != 1) == 1L, logical(1))))
})
