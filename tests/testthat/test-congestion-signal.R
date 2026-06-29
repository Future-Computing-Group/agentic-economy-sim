# Tests for the saturating bid-time congestion signal.
#
# The bid-time latency model estimate_latency() takes a utilisation signal and
# returns an estimated latency used by agents to value tasks. Utilisation is a
# fraction of capacity and CANNOT exceed 1 in steady state: offered load above
# capacity manifests as a saturated queue (bounded expected latency) plus
# admission drops, NOT unbounded delay. The execution-time latency model
# (execute_allocation) already caps its queueing utilisation at 0.99; the
# bid-time signal must be consistent. estimate_latency() therefore saturates its
# utilisation input at util_cap (default 1.0). Without this, offered load of
# several times capacity produces runaway estimated latency that zeroes task
# value at the contention boundary, so demand vanishes before prices can clear
# and the market's price mechanism never engages.

test_that("estimate_latency saturates: util above the cap gives the cap's latency", {
  cap_latency <- estimate_latency(1.0, base_latency = 50, alpha = 50, p = 1.2)
  expect_equal(estimate_latency(2.0,  base_latency = 50, alpha = 50, p = 1.2), cap_latency)
  expect_equal(estimate_latency(8.5,  base_latency = 50, alpha = 50, p = 1.2), cap_latency)
  expect_equal(estimate_latency(100,  base_latency = 50, alpha = 50, p = 1.2), cap_latency)
})

test_that("estimate_latency is unchanged below the cap (monotone increasing)", {
  l25 <- estimate_latency(0.25, base_latency = 50, alpha = 50, p = 1.2)
  l50 <- estimate_latency(0.50, base_latency = 50, alpha = 50, p = 1.2)
  l90 <- estimate_latency(0.90, base_latency = 50, alpha = 50, p = 1.2)
  expect_lt(l25, l50)
  expect_lt(l50, l90)
  # below the cap the value matches the raw power law (no clipping)
  expect_equal(l50, 50 + 50 * (0.5 ^ 1.2))
})

test_that("util_cap is configurable (sensitivity knob)", {
  # A higher cap admits more congestion latency before saturating.
  expect_lt(estimate_latency(1.5, alpha = 50, util_cap = 1.0),
            estimate_latency(1.5, alpha = 50, util_cap = 2.0))
})

test_that("saturated bid latency stays within the deadline range at alpha = 50", {
  # At full utilisation the worst-case congestion latency (base + alpha) must be
  # feasible against the operating deadlines {250, 375, 500} ms, so the deadline
  # cliff does not zero value at the contention boundary.
  expect_lt(estimate_latency(1.0, base_latency = 50, alpha = 50), 250)
})
