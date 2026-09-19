# Tests for the posted-price mechanism (deployed practice: cloud on-demand
# pricing). The operator posts one per-bundle price, the tasks whose expected
# value clears it participate, participants are packed by expected value under
# per-tier capacity, and every winner pays the posted price.

pp_env <- function(graph_type = "sp", load = "high", N = 30L) {
  init_environment(build_dependency_graph(graph_type), load_level = load,
                   n_agents = N, graph_type = graph_type)
}

# A SATURATED environment: per-tier capacity shrunk until only a handful of
# bundles fit, so the capacity half of the rule is what binds. Without it the
# screen is the only thing that ever binds and a packer that ignored capacity
# would pass.
pp_env_saturated <- function(cap = 30) {
  env <- pp_env(N = 8L)
  env$capacities <- dplyr::mutate(env$capacities, capacity = cap)
  env
}

pp_tasks <- function(n = 40L, n_agents = 8L, seed = 1L) {
  set.seed(seed)
  tibble::tibble(
    task_id    = sprintf("t%03d", seq_len(n)),
    agent_id   = sample.int(n_agents, n, replace = TRUE),
    deadline   = sample(c(500L, 750L, 1000L), n, replace = TRUE),
    value_base = runif(n, 1, 2)
  )
}

pp_ev <- function(tasks, env, util_hat = 0.5) {
  task_expected_value(tasks, util_hat, base_latency_for_bids(env),
                      init_success_model())
}


# ---- the participation screen and the capacity, asserted separately ---------

test_that("posted_price admits the tasks whose expected value clears the price", {
  env   <- pp_env()
  tasks <- pp_tasks()
  ev    <- pp_ev(tasks, env)
  p     <- stats::median(ev)
  alloc <- posted_price_allocate(tasks, env, ev, p)

  expect_true(all(ev[match(alloc$task_id, tasks$task_id)] > p))
  # Capacity is slack here, so the admitted set is exactly the participating set.
  expect_setequal(alloc$task_id, tasks$task_id[ev > p])
})

test_that("a task valued at exactly the posted price does not participate", {
  # The screen is strict in the sibling and strict here, and no fixture drawn
  # from a continuous distribution ever lands on the price, so the inequality
  # needs a hand-built tie to pin it. Weakening it to >= admits the middle task.
  env   <- pp_env()
  tasks <- tibble::tibble(
    task_id    = c("above", "exactly_at", "below"),
    agent_id   = 1:3,
    deadline   = rep(1000L, 3),
    value_base = c(2, 1, 0.5)
  )
  alloc <- posted_price_allocate(tasks, env, ev = c(2, 1, 0.5), p_post = 1)

  expect_setequal(alloc$task_id, "above")
  expect_false("exactly_at" %in% alloc$task_id)
})

test_that("posted_price packs participants under per-tier capacity", {
  env   <- pp_env_saturated(cap = 30)
  tasks <- pp_tasks()
  ev    <- pp_ev(tasks, env)
  p     <- min(ev) / 2                 # everybody participates; capacity binds alone
  alloc <- posted_price_allocate(tasks, env, ev, p)

  bundle <- task_bundle(env)
  cap    <- tier_capacities(env)
  used   <- nrow(alloc) * bundle$demand[match(cap$tier, bundle$tier)]
  expect_true(all(used <= cap$capacity))
  expect_lt(nrow(alloc), sum(ev > p))   # the fixture really is saturated
})


# ---- the mechanism's defining property --------------------------------------

test_that("every winner pays the posted price", {
  env   <- pp_env()
  tasks <- pp_tasks()
  ev    <- pp_ev(tasks, env)
  p     <- stats::median(ev)
  alloc <- posted_price_allocate(tasks, env, ev, p)

  expect_gt(nrow(alloc), 0)
  expect_true(all(alloc$payment == p))
})


# ---- the anchor: k = 1 posts the environment's own marginal cost -------------

test_that("at k = 1 the posted price is the marginal cost of one task bundle", {
  for (gt in c("tree", "sp", "entangled")) {
    env <- pp_env(gt)
    # The same quantity the integrator floors its own slice price at, so the
    # posted-price arm and the market arm are anchored identically.
    expect_equal(posted_price_anchor(env, 1),
                 env$reserve_price * sum(task_bundle(env)$demand))
    expect_equal(posted_price_anchor(env, 4), 4 * posted_price_anchor(env, 1))
  }
})


# ---- monotonicity of the screen (property over several seeds) ---------------

test_that("raising the markup weakly reduces the number of tasks admitted", {
  env     <- pp_env()
  # A grid that spans the expected-value distribution: at the low end every task
  # participates, at the high end none does. Sweeping only k >= 1 would assert
  # monotonicity on a range where the arm admits nothing either way.
  k_grid  <- c(0.5, 0.75, 1, 2)
  for (s in 1:6) {
    tasks <- pp_tasks(seed = s)
    ev    <- pp_ev(tasks, env)
    n     <- vapply(k_grid, function(k) {
      nrow(posted_price_allocate(tasks, env, ev, posted_price_anchor(env, k)))
    }, integer(1))
    expect_true(all(diff(n) <= 0),
                info = sprintf("seed %d: admitted %s", s, paste(n, collapse = " ")))
    expect_gt(n[1], n[length(n)])       # the grid really does span the screen
  }
})


# ---- drift guard: the rule, reimplemented independently ---------------------

# What the port carries across from the sibling is the participation screen and
# the flat payment, not the order participants are served in: the sibling packs
# them in arrival order, because its greedy walks the rows as given and its
# posted-price caller is the one caller that does not pre-sort, while this arm
# packs them by expected value on the packer the ablation already uses.
#
# The rule written out independently of the code under test, so a drift in
# either shows up as a different admitted set: screen at the price, then take
# participants by descending expected value while every tier still has a bundle
# of capacity left.
reference_posted_price <- function(tasks, env, ev, p_post) {
  cap       <- tier_capacities(env)
  bundle    <- task_bundle(env)
  remaining <- setNames(cap$capacity, cap$tier)
  demand    <- setNames(bundle$demand, bundle$tier)[names(remaining)]
  chosen    <- character(0)
  for (i in order(ev, decreasing = TRUE)) {
    if (!isTRUE(ev[i] > p_post)) next
    if (all(demand <= remaining)) {
      remaining <- remaining - demand
      chosen    <- c(chosen, tasks$task_id[i])
    }
  }
  chosen
}

test_that("the port agrees with the screen-plus-flat-payment rule on a shared fixture", {
  for (env in list(pp_env("tree"), pp_env("sp"), pp_env_saturated(cap = 30))) {
    tasks <- pp_tasks()
    ev    <- pp_ev(tasks, env)
    p     <- stats::median(ev)
    expect_setequal(posted_price_allocate(tasks, env, ev, p)$task_id,
                    reference_posted_price(tasks, env, ev, p))
  }
})


# ---- the arm inside Exp.6 ---------------------------------------------------

test_that("the posted price and the discovered price arrive on the same row", {
  posted <- exp6_run_single("posted_price", "naive", "sp", "high",
                            p_post_k = 2, N = 20L, seed = 1L, n_rounds = 10L)
  market <- exp6_run_single("market", "naive", "sp", "high",
                            N = 20L, seed = 1L, n_rounds = 10L)
  env    <- pp_env(N = 20L)

  expect_equal(posted$p_post_k, 2)
  expect_equal(posted$mean_unit_cost, posted_price_anchor(env, 2))
  # A posted price is a constant series, so its volatility is a real zero rather
  # than the absent price the rank schedulers report.
  expect_equal(posted$mean_price_volatility, 0)
  # The discovered price is on the same row and in the same units, which is the
  # whole point of carrying it: it cannot fall below the anchor the posted price
  # is a markup on.
  expect_gte(market$mean_unit_cost, posted_price_anchor(env, 1))
})

test_that("the posted-price arm runs on both architectures", {
  for (arch in c("naive", "hybrid")) {
    r <- exp6_run_single("posted_price", arch, "sp", "high",
                         p_post_k = 1, N = 20L, seed = 1L, n_rounds = 5L)
    expect_true(is.finite(r$welfare), info = arch)
    expect_equal(r$architecture, arch)
  }
})

test_that("the hybrid arm screens on the posted price too", {
  # Half the grid is hybrid, and that half does not go through
  # posted_price_allocate: it screens by handing the slice packer a non-positive
  # score, which the packer skips. Drop the screen there and the arm stops
  # reading the price at all, which makes it the value-greedy arm under another
  # name: the equivalence trap, on the half the collapse pre-checks miss.
  priced_out <- exp6_run_single("posted_price", "hybrid", "sp", "high",
                                p_post_k = 4, N = 55L, seed = 1L, n_rounds = 5L)
  expect_equal(priced_out$drop_rate, 1)

  at_cost <- exp6_run_single("posted_price", "hybrid", "sp", "high",
                             p_post_k = 1, N = 55L, seed = 1L, n_rounds = 5L)
  greedy  <- exp6_run_single("greedy_ev", "hybrid", "sp", "high",
                             N = 55L, seed = 1L, n_rounds = 5L)
  expect_false(isTRUE(all.equal(at_cost$welfare, greedy$welfare)))
})


# ---- the grid the pipeline branches over ------------------------------------

test_that("the markup varies only inside the posted-price arm", {
  g <- exp6_mechanism_grid(n_seeds = 10L)

  # Five mechanisms at one markup plus the posted price at three, crossed with
  # topology, load, architecture and seed.
  expect_equal(nrow(g), 8L * 3L * 2L * 2L * 10L)
  expect_setequal(unique(g$mechanism[g$p_post_k != 1]), "posted_price")
  expect_setequal(unique(g$p_post_k[g$mechanism == "posted_price"]), c(1, 2, 4))
  # Crossing the markup with every mechanism would run each of the others three
  # times under a price it never posts.
  expect_equal(sum(g$mechanism == "k8s"), 3L * 2L * 2L * 10L)
})


# ---- what the new arms carry downstream --------------------------------------

exp6_row <- function(mechanism, p_post_k, seed, welfare) {
  tibble::tibble(
    mechanism = mechanism, architecture = "naive", graph_type = "sp",
    load_level = "high", p_post_k = p_post_k, N = 20L, seed = seed,
    median_latency = 100, p95_latency = 150, utilisation = 0.5,
    drop_rate = 0.3, welfare = welfare, oracle_welfare = 10, efficiency = 0.5,
    mean_price_volatility = 0, mean_unit_cost = 0.44 * p_post_k
  )
}

test_that("exp6_aggregate keeps the markup in its grouping", {
  raw <- dplyr::bind_rows(lapply(c(1, 2, 4), function(k) {
    dplyr::bind_rows(exp6_row("posted_price", k, 1L, k),
                     exp6_row("posted_price", k, 2L, k))
  }))
  agg <- exp6_aggregate(list(raw))

  # Three markups are three rows. Grouped without the markup they average into
  # one, and the summary table reports a price nobody posted.
  expect_equal(nrow(agg), 3L)
  expect_setequal(agg$p_post_k, c(1, 2, 4))
  expect_setequal(agg$mean_unit_cost, c(0.44, 0.88, 1.76))
})

test_that("the four-mechanism figure keeps only its four arms", {
  arms <- dplyr::bind_rows(
    tibble::tibble(mechanism = c("random", "edf", "greedy_ev", "market", "k8s"),
                   p_post_k = 1),
    tibble::tibble(mechanism = "posted_price", p_post_k = c(1, 2, 4))
  )
  raw <- tidyr::expand_grid(arms, architecture = "naive", graph_type = "sp",
                            load_level = "high", seed = 1:2) %>%
    dplyr::mutate(median_latency = 100, p95_latency = 150, drop_rate = 0.3,
                  welfare = seed, mean_price_volatility = 0, efficiency = 0.5)
  df <- exp6_prepare(raw)

  # An arm the figure's factor does not name would otherwise arrive as an
  # unlabelled NA series rather than be left out.
  expect_setequal(as.character(df$mechanism),
                  c("Random", "EDF", "Greedy EV", "Market"))
  expect_false(any(is.na(df$mechanism)))
})
