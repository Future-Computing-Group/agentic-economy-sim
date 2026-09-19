# The smoothing factor of the architecture x smoothing factorial.
#
# The factorial crosses encapsulation (per-tier market versus integrator slice)
# with EMA price smoothing. Three of its four cells already existed; the fourth,
# the per-resource market with the integrator's own smoothing, needs one
# optional argument on clear_multitier_market. That function is on the critical
# path of every other experiment, so the first test here is the identity guard
# that says the argument moved nothing.

# ---- identity guard: the existing arms are untouched -------------------------

test_that("the existing exp4 arms are unchanged by the smoothing argument", {
  # Recorded on a congested cell before the smoothing argument existed. A
  # congested cell is the guard that matters: the tatonnement moves prices
  # there, so a change in the clearing path shows up in the recorded metrics
  # rather than washing out. Two witnesses, one per clearing path; the legacy
  # level is not among them because at efficiency 1.0 it runs the integrator
  # exactly as the slice cell does and would witness nothing of its own.
  expected <- readRDS(test_path("fixtures", "exp4-existing-arms-sp-high-n55-seed1.rds"))
  actual   <- bind_rows(lapply(expected$architecture, function(a) {
    exp4_run_single(a, "sp", "high", N = 55L, seed = 1L, n_rounds = 20L)
  }))
  expect_identical(actual[names(expected)], expected)
})


# ---- fixtures ---------------------------------------------------------------

.ema_env <- function(n_agents = 20L) {
  init_environment(build_dependency_graph("sp"), "high",
                   n_agents = n_agents, graph_type = "sp")
}

.ema_tasks <- function(n, seed = 7L) {
  set.seed(seed)
  tibble(task_id    = paste0("t", seq_len(n)),
         agent_id   = rep(1:5, length.out = n),
         deadline   = sample(c(500L, 750L, 1000L), n, replace = TRUE),
         value_base = runif(n, 1, 2))
}

.ema_clear <- function(tasks, env, ms, ...) {
  clear_multitier_market(tasks, env, util_hat = 0.5,
                         base_latency = base_latency_for_bids(env),
                         market_state = ms, ...)
}


# ---- the smoothing argument -------------------------------------------------

test_that("beta = 0 leaves clear_multitier_market identical", {
  env <- .ema_env(); tk <- .ema_tasks(300L); ms <- init_market_state(env)
  expect_identical(.ema_clear(tk, env, ms, beta = 0), .ema_clear(tk, env, ms))
})

test_that("beta > 0 smooths the posted price without moving the allocation", {
  env <- .ema_env(); tk <- .ema_tasks(300L); ms <- init_market_state(env)
  raw    <- .ema_clear(tk, env, ms, beta = 0)
  smooth <- .ema_clear(tk, env, ms, beta = 0.8)

  # The EMA is a posting rule, not an allocation rule: packing runs at the raw
  # clearing prices in both runs, and only the price the agent is charged moves.
  expect_identical(smooth$allocation, raw$allocation)
  expect_identical(smooth$surplus, raw$surplus)
  expect_equal(smooth$clearing$prices$price,
               0.8 * ms$prices$price + 0.2 * raw$clearing$prices$price)
  # The clearing summary is recomputed from the smoothed prices, so the agent's
  # unit cost follows the posted price rather than the raw one.
  expect_false(isTRUE(all.equal(smooth$clearing$unit_cost,
                                raw$clearing$unit_cost)))

  # And the posting is a recursion on the posted price, not a one-step blend
  # against the raw path: what is carried into the next round is what this
  # round posted, so round 3 is built on round 2's posting and through it on
  # round 1's. This is the property that makes the arm the integrator's mirror,
  # whose carried slice price is likewise the smoothed one.
  # Rounds of different weight, and heavy enough to lift the tiers off their
  # reserve: on a repeated round, or on one the reserve pins, the posted and
  # the cleared price coincide and every smoothing rule looks alike.
  load   <- lapply(1:3, function(t) .ema_tasks(c(2000L, 100L, 4000L)[t], seed = t))
  state  <- ms
  posted <- vector("list", 3)
  for (t in seq_len(3)) {
    step        <- .ema_clear(load[[t]], env, state, beta = 0.8)
    posted[[t]] <- step$clearing$prices$price
    if (t == 2) entry3 <- step$market_state
    state       <- step$market_state
  }
  expect_identical(state$prices$price, posted[[3]])
  raw3 <- .ema_clear(load[[3]], env, entry3, beta = 0)$clearing$prices$price
  expect_identical(posted[[3]], ema_post(posted[[2]], raw3, 0.8))
  expect_false(isTRUE(all.equal(posted[[3]], raw3)))
})

test_that("smoothing reduces the dispersion of the posted price series", {
  # Alternating heavy and light rounds: the raw clearing price swings with
  # excess demand, and an EMA on it must swing less. Asserted as a property so
  # that a sign error in the convex combination cannot survive.
  #
  # Measured from a warmed market state, because the posted price feeds back as
  # the next round's entry price (as it does in the integrator): from an
  # arbitrary initial price the EMA arm spends roughly 1/(1 - beta) rounds
  # crawling to the operating level, and that opening ramp is a transient of the
  # initial condition rather than the dispersion this test is about.
  env <- .ema_env()
  rounds <- function(ms, ts, beta) {
    vals <- vapply(ts, function(t) {
      cleared <- .ema_clear(.ema_tasks(if (t %% 2 == 0) 400L else 20L, seed = t),
                            env, ms, beta = beta)
      ms <<- cleared$market_state
      cleared$clearing$unit_cost
    }, numeric(1))
    list(ms = ms, vals = vals)
  }
  series <- function(beta) {
    warm <- rounds(init_market_state(env), seq_len(12), beta)
    rounds(warm$ms, 13:24, beta)$vals
  }
  expect_lt(sd(series(0.8)), sd(series(0)))
})


# ---- the four cells of the factorial ----------------------------------------

# The four cells at the calibrated operating point, run once for the file. A
# contended cell is the only place the factors are separable: where capacity is
# slack the tier prices sit still, and smoothing a constant series is the
# identity, so all four cells legitimately coincide.
.factorial_cells <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      cells  <- c("naive", "naive_ema", "hybrid_noema", "hybrid_ema")
      cached <<- bind_rows(lapply(cells, function(a) {
        exp4_run_single(a, "sp", "high", N = 55L, seed = 1L, n_rounds = 20L)
      }))
    }
    cached
  }
})

# ---- the burn-in trimmed metric ---------------------------------------------

test_that("the trimmed volatility ignores the opening rounds", {
  # Every cell starts at a price its market did not clear at, and an EMA cell
  # takes about log(0.05)/log(beta) rounds to walk to its operating level. That
  # ramp is an initial condition, not price instability, and it contaminates the
  # cells unequally, so the factorial reads a metric that drops it from all four.
  spike <- c(10, rep(1, 19))
  expect_gt(agent_price_volatility(spike), 1)
  expect_equal(agent_price_volatility_tail(spike), 0)
  # A named fraction of the run, not a literal at the call site.
  expect_equal(burn_in_fraction, 0.2)
  expect_equal(agent_price_volatility_tail(spike),
               agent_price_volatility(spike[-seq_len(4)]))
})

test_that("the driver carries the trimmed volatility beside the full-run one", {
  four <- .factorial_cells()
  expect_true("mean_price_volatility_tail" %in% names(four))
  expect_false(isTRUE(all.equal(four$mean_price_volatility,
                                four$mean_price_volatility_tail)))
  # and it survives the aggregation, so the per-cell table a reader tallies
  # carries the column the headline contrast is taken on
  expect_true("mean_price_volatility_tail" %in% names(exp4_aggregate(four)))
})


test_that("every cell of the factorial is reachable and distinct", {
  four <- .factorial_cells()
  expect_equal(four$architecture,
               c("naive", "naive_ema", "hybrid_noema", "hybrid_ema"))
  # No level falls through to another branch: four cells, four distinct rows.
  expect_equal(nrow(distinct(select(four, -architecture))), 4L)
})

test_that("the crossed factors are what the cell names say they are", {
  # Encapsulation on means the integrator clears the market, which is what
  # posts a slice price; smoothing off means beta = 0 wherever it is applied.
  four <- .factorial_cells()
  cell <- function(a) four[four$architecture == a, ]
  expect_true(is.na(cell("naive_ema")$mean_slice_price))
  expect_false(is.na(cell("hybrid_noema")$mean_slice_price))
  # With smoothing off the integrator posts the round's raw clearing price, so
  # the arm is exactly the beta = 0 integrator and not a renamed hybrid_ema.
  expect_false(isTRUE(all.equal(cell("hybrid_noema")$mean_price_volatility,
                                cell("hybrid_ema")$mean_price_volatility)))
})


test_that("the pipeline runs the four cells and not the legacy level", {
  # The grid lives inside a tar_target call, which the constants reader cannot
  # reach, so the Exp.4 block is read off the pipeline source. The legacy
  # `hybrid` level is retained for the sweep and the experiments that reuse
  # this driver, but it is not a cell of the factorial and must not be a
  # headline arm: it carries an assumed demand reduction none of the others do.
  src  <- readLines(here::here("_targets.R"))
  at   <- grep("exp4_param_grid,", src)[1]
  line <- grep("architecture", src[at:(at + 8)], value = TRUE)[1]
  for (cell in c("naive", "naive_ema", "hybrid_noema", "hybrid_ema")) {
    expect_true(grepl(sprintf('"%s"', cell), line, fixed = TRUE), info = cell)
  }
  expect_false(grepl('"hybrid"', line, fixed = TRUE))
})


test_that("the smoothed arm carries no round of its own", {
  # Smoothing runs from the opening round, like the integrator's, and the
  # opening transient it produces is handled by the trimmed metric rather than
  # by exempting one cell from its own factor. The integrator has no
  # first-contact posting rule to mirror: its clamp is a floor on the entry
  # price, and at the calibrated cell it does not fire at all.
  one <- function(a) exp4_run_single(a, "sp", "high", N = 55L, seed = 1L,
                                     n_rounds = 1L)
  expect_false(isTRUE(all.equal(one("naive_ema")$mean_unit_cost,
                                one("naive")$mean_unit_cost)))
})


# ---- the figure reports the measurement the contrast is taken on ------------

test_that("the Exp.4 volatility panel plots the trimmed metric", {
  # A figure showing one dispersion measurement beside a decomposition taken on
  # another invites the reader to read the second off the first. The full-run
  # column stays in the prepared frame for the supplement.
  fix <- tidyr::expand_grid(architecture = c("naive", "hybrid_ema"),
                            graph_type = "sp", load_level = "high",
                            N = c(20L, 40L), seed = 1:3) %>%
    mutate(median_latency = 100, p95_latency = 120, drop_rate = 0.2,
           welfare = 20, efficiency = 0.8,
           mean_price_volatility      = 0.5,
           mean_price_volatility_tail = 0.01 * N +
             0.02 * (architecture == "naive"))

  df <- exp4_prepare(fix)
  expect_true("mean_price_volatility_mean" %in% names(df))

  panel_y <- function(p) sort(unique(ggplot2::ggplot_build(p)$data[[1]]$y))
  expect_equal(panel_y(plot_exp4_volatility(df)),
               sort(unique(df$mean_price_volatility_tail_mean)))
  expect_equal(panel_y(make_exp4_tufte(fix, "b")[[2]]),
               sort(unique(df$mean_price_volatility_tail_mean)))
})
