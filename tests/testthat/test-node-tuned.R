# test-node-tuned.R
# ---------------------------------------------------------------------------
# One tuned knob per mechanism, and a set of seeds the tuning never sees.
#
# The posted price is compared at the level that suits it best, which is a
# maximum over levels selected on the very outcome the comparison reports; the
# market had no knob at all. This block gives each mechanism one -- the markup
# for the posted price, the reserve markup for the two market arms -- picks it
# on the tuning seeds, and reports every number on evaluation seeds that no
# cell of the mechanism block has run.
# ---------------------------------------------------------------------------

tuned_env <- function() node_run_env("tree", "high", 90L, "uniform", "off")

tuned_tasks <- function(env, n = 120L, seed = 3L) {
  set.seed(seed)
  tibble(task_id = sprintf("t%03d", seq_len(n)), agent_id = rep_len(1:9, n),
         deadline = 1000, value_base = runif(n, 1, 2),
         recipe = rep_len(rownames(env$anc), n))
}

tuned_clear <- function(env, tasks, ...) {
  clear_multitier_market(tasks, env, util_hat = 0.5,
                         base_latency = base_latency_for_bids(env),
                         market_state = init_market_state(env),
                         lambda_l_default = node_lambda_l(), ...)
}


test_that("the seeds a knob is tuned on are never the seeds it is reported on", {
  sp <- node_tuning_split()
  expect_length(intersect(sp$tuning, sp$evaluation), 0L)
  # and the evaluation seeds are new to the whole node evaluation, not just to
  # this block: the mechanism block runs seeds one to n_seeds.
  expect_true(all(sp$evaluation > targets_constants("n_seeds")$n_seeds))
})

test_that("the reserve markup lifts the floor the tatonnement clamps to", {
  env <- tuned_env(); tasks <- tuned_tasks(env)
  base <- tuned_clear(env, tasks)
  expect_identical(tuned_clear(env, tasks, reserve_markup = 1), base)

  up <- tuned_clear(env, tasks, reserve_markup = 2)
  expect_gte(min(up$clearing$prices$price), 2 * env$reserve_price)
  expect_gt(up$clearing$unit_cost, base$clearing$unit_cost)
  # A floor is a participation screen on the market side: the tasks whose
  # surplus it takes below zero stop clearing.
  expect_lt(nrow(up$allocation), nrow(base$allocation))
})

test_that("the driver carries the markup it ran at", {
  one <- function(r) node_run_single("tree", "high", N = 90L, seed = 1L,
                                     n_rounds = 6L, reserve_markup = r,
                                     lambda_l_default = node_lambda_l())
  lo <- one(1); hi <- one(2)
  expect_equal(lo$reserve_markup, 1)
  expect_equal(hi$reserve_markup, 2)
  expect_lt(hi$tokens_admitted, lo$tokens_admitted)
  # The knob moves the arm, not the references it is scored against.
  expect_equal(hi$optimum_ex_post, lo$optimum_ex_post)
})


# ---- the two grids and the table -------------------------------------------

test_that("the tuning grid gives each mechanism one knob and no other freedom", {
  g <- node_tuning_grid(c(1L, 2L))
  expect_setequal(unique(g$mechanism),
                  c("posted_price", "posted_price_fcfs", "posted_price_edf",
                    "market", "market_cc"))
  for (m in c("posted_price", "posted_price_fcfs", "posted_price_edf")) {
    expect_setequal(unique(g$p_post_k[g$mechanism == m]),
                    node_tuning_posted_levels())
    expect_setequal(unique(g$reserve_markup[g$mechanism == m]), 1)
  }
  expect_setequal(unique(g$p_post_k[!grepl("^posted_price", g$mechanism)]), 1)
  expect_setequal(unique(g$reserve_markup[g$mechanism == "market"]),
                  node_reserve_markups())
  # Twelve cells: three instances, two loads, two architectures; each at both
  # congestion levels. Three posted disciplines over the posted levels, two
  # market arms over the reserve markups.
  expect_equal(nrow(g),
               (3 * length(node_tuning_posted_levels()) +
                  2 * length(node_reserve_markups())) * 12 * 2 * 2)
})

# A tuning frame whose argmax is known by construction: the posted price peaks
# at 1.5, the market at 1.25 and the congestion-consistent arm at 2.
.tuning_fixture <- function(seeds = 1:2) {
  knobs <- dplyr::bind_rows(
    tibble(mechanism = "posted_price", p_post_k = node_tuning_posted_levels(),
           reserve_markup = 1),
    tidyr::expand_grid(mechanism = c("market", "market_cc"), p_post_k = 1,
                       reserve_markup = node_reserve_markups()))
  tidyr::expand_grid(knobs, graph_type = c("tree", "sp"), load_level = "high",
                     architecture = "naive", seed = seeds) %>%
    dplyr::mutate(welfare = dplyr::case_when(
      mechanism == "posted_price" ~ -abs(p_post_k - 1.5),
      mechanism == "market"       ~ -abs(reserve_markup - 1.25),
      TRUE                        ~ -abs(reserve_markup - 2)) + seed / 100)
}

test_that("the evaluation grid runs each mechanism at its tuning-set argmax", {
  g <- node_eval_grid(.tuning_fixture(), c(11L, 12L))

  expect_setequal(unique(g$seed), c(11L, 12L))
  expect_setequal(unique(g$p_post_k[g$mechanism == "posted_price"]), 1.5)
  expect_setequal(unique(g$reserve_markup[g$mechanism == "market"]), 1.25)
  expect_setequal(unique(g$reserve_markup[g$mechanism == "market_cc"]), 2)
  # The two arms with nothing to tune are evaluated at their only setting.
  expect_setequal(unique(g$mechanism),
                  c("posted_price", "market", "market_cc", "greedy_ev", "k8s"))
  expect_setequal(unique(g$p_post_k[g$mechanism == "k8s"]), 1)
  # Five mechanisms, one knob each, two cells, two seeds.
  expect_equal(nrow(g), 5L * 2L * 2L)
})

test_that("the tuned table reports the evaluation numbers at the chosen knob", {
  eval_raw <- node_eval_grid(.tuning_fixture(), c(11L, 12L)) %>%
    dplyr::mutate(welfare = 10 + seed, tokens_admitted = 50, median_latency = 200,
                  welfare_over_optimum = 0.4, alloc_ratio_true = 0.6)
  tuned <- node_tuned_table(eval_raw)

  expect_equal(nrow(tuned), 10L)
  expect_setequal(names(tuned),
                  c("graph_type", "load_level", "architecture", "mechanism",
                    "p_post_k", "reserve_markup", "n_eval_seeds", "welfare",
                    "tokens_admitted", "median_latency", "welfare_over_optimum",
                    "knob_at_boundary", "knob_flat", "alloc_ratio_true"))
  expect_true(all(tuned$n_eval_seeds == 2L))
  expect_true(all(tuned$welfare == 21.5))
  expect_equal(tuned$p_post_k[tuned$mechanism == "posted_price"], c(1.5, 1.5))
})

test_that("the tuned table carries the allocative ratio, averaged like welfare_over_optimum", {
  # Known per-seed alloc_ratio_true and welfare_over_ceiling values, so the
  # cell mean is checkable by hand; the two seeds are the evaluation seeds
  # .tuning_fixture()'s grid is scored on.
  eval_raw <- node_eval_grid(.tuning_fixture(), c(11L, 12L)) %>%
    dplyr::mutate(welfare = 10 + seed, tokens_admitted = 50, median_latency = 200,
                  welfare_over_optimum = 0.4,
                  alloc_ratio_true = ifelse(seed == 11L, 0.6, 0.8),
                  welfare_over_ceiling = ifelse(seed == 11L, 0.3, 0.5))
  tuned <- node_tuned_table(eval_raw)

  # Appended at the end, after every column the table already reported.
  expect_equal(names(tuned)[(ncol(tuned) - 1):ncol(tuned)],
               c("alloc_ratio_true", "welfare_over_ceiling"))
  # Existing columns keep their positions and values.
  expect_equal(names(tuned)[seq_len(ncol(tuned) - 2L)],
               c("graph_type", "load_level", "architecture", "mechanism",
                 "p_post_k", "reserve_markup", "n_eval_seeds", "welfare",
                 "tokens_admitted", "median_latency", "welfare_over_optimum",
                 "knob_at_boundary", "knob_flat"))
  expect_true(all(tuned$welfare == 21.5))
  expect_true(all(tuned$welfare_over_optimum == 0.4))
  # mean(0.6, 0.8) and mean(0.3, 0.5), exactly as welfare_over_optimum's mean.
  expect_true(all(tuned$alloc_ratio_true == 0.7))
  expect_true(all(tuned$welfare_over_ceiling == 0.4))
})

test_that("welfare_over_ceiling is skipped when the eval rows don't carry it", {
  eval_raw <- node_eval_grid(.tuning_fixture(), c(11L, 12L)) %>%
    dplyr::mutate(welfare = 10 + seed, tokens_admitted = 50, median_latency = 200,
                  welfare_over_optimum = 0.4, alloc_ratio_true = 0.7)
  tuned <- node_tuned_table(eval_raw)

  expect_false("welfare_over_ceiling" %in% names(tuned))
  expect_equal(names(tuned)[ncol(tuned)], "alloc_ratio_true")
})

test_that("the pipeline carries the tuned block as its own targets", {
  src <- paste(readLines(here::here("_targets.R")), collapse = "\n")
  for (nm in c("node_exp6_tuning_grid", "node_exp6_tuning_raw",
               "node_exp6_eval_grid", "node_exp6_eval_raw", "node_exp6_tuned")) {
    expect_true(grepl(paste0("tar_target\\(\\s*", nm, "[,\\s]"), src), info = nm)
  }
  # ... and the mechanism block's own grid is not the one being tuned on.
  expect_true(grepl("node_exp6_param_grid, node_exp6_mechanism_grid(n_seeds)",
                    src, fixed = TRUE))
})

# The evaluation grid is built from tuning RESULTS, whose architecture column
# already carries the driver's own level names. A target that re-maps a design
# label onto those names would send every contracted cell down the uncontracted
# path, so the grid must emit driver levels and the target must pass them on.
test_that("the evaluation grid's architecture values are driver levels, passed through unchanged", {
  tr <- tidyr::expand_grid(graph_type = "tree", load_level = "high",
                           architecture = c("naive", "hybrid_noema"),
                           mechanism = "posted_price", p_post_k = c(1, 2),
                           reserve_markup = 1, seed = 1:2)
  tr$welfare <- ifelse(tr$architecture == "naive", tr$p_post_k, 3 - tr$p_post_k)
  eg <- node_eval_grid(tr, seeds = 11:12)
  expect_true(all(eg$architecture %in% c("naive", "naive_ema", "hybrid_noema", "hybrid_ema")))
  knobs <- eg %>% dplyr::filter(mechanism == "posted_price") %>%
    dplyr::distinct(architecture, p_post_k)
  expect_equal(knobs$p_post_k[knobs$architecture == "naive"], 2)
  expect_equal(knobs$p_post_k[knobs$architecture == "hybrid_noema"], 1)
  # the same mapping the pipeline applies must be the identity on these levels
  expect_identical(node_eval_architecture(eg$architecture), eg$architecture)
})


# ---- how wide the grids are, and whether the chosen knob sat on their edge --
#
# At the calibrated queue term the market's tuned markup sat at the top of its
# grid in a quarter of the cells and the congestion-consistent arm's in most of
# them, while the posted price's sat at the bottom in two thirds: a maximum on
# the edge of a grid is a censored maximum, and the comparison it feeds reports
# the widest dial we happened to offer rather than the mechanism's own best
# setting. The grids are widened in both directions, and the tuned table says
# of every cell whether its knob still sits on an edge.

test_that("the widened grids extend the old ones rather than replace them", {
  # Appended, never inserted, so the rows already run keep their order. What
  # a tie resolves to is decided elsewhere: the knobs are summarised before
  # the maximum is taken, which orders them by value, so slice_max takes the
  # smallest knob on the grid rather than the one run first.
  expect_equal(node_reserve_markups()[1:4], c(1, 1.25, 1.5, 2))
  expect_true(all(c(3, 4) %in% node_reserve_markups()))
  expect_equal(node_tuning_posted_levels()[seq_along(node_posted_levels())],
               node_posted_levels())
  expect_true(all(c(0.5, 0.75) %in% node_tuning_posted_levels()))
  # The frontier's own family is untouched: its levels are the curve every
  # other arm is read against, and a point added to it moves every ratio on it.
  expect_equal(node_posted_levels(), c(1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 3, 4))
})

test_that("a markup below one lowers the floor rather than being clamped away", {
  # Why the below-one values are on the grid at all: the effective floor is
  # max(price_floor, reserve * markup) and the node callers leave price_floor
  # at zero, so the market really does clear against a lower floor there.
  env <- tuned_env(); tasks <- tuned_tasks(env)
  base <- tuned_clear(env, tasks)
  down <- tuned_clear(env, tasks, reserve_markup = 0.5)
  expect_lt(min(down$clearing$prices$price), env$reserve_price)
  expect_lt(down$clearing$unit_cost, base$clearing$unit_cost)
  expect_gt(nrow(down$allocation), nrow(base$allocation))
})

test_that("a posted level below one posts a lower price", {
  # The posted arm's counterpart of the same check: the anchor is k times the
  # path's reserve cost with nothing clamping it at k = 1.
  env <- tuned_env()
  a <- function(k) posted_price_anchor_per_leaf(env, env$anc, k = k)
  expect_true(all(a(0.5) < a(1)))
  expect_true(all(a(0.75) < a(1)))
  expect_equal(unname(a(0.5)), unname(0.5 * a(1)))
})

# One knob on the bottom of its grid, one on the top, one inside, one arm with
# no knob at all.
.boundary_eval_raw <- function() {
  knobs <- dplyr::bind_rows(
    tibble(mechanism = "posted_price", p_post_k = min(node_tuning_posted_levels()),
           reserve_markup = 1),
    tibble(mechanism = "market", p_post_k = 1,
           reserve_markup = max(node_reserve_markups())),
    tibble(mechanism = "market_cc", p_post_k = 1, reserve_markup = 1.25),
    tibble(mechanism = "k8s", p_post_k = 1, reserve_markup = 1))
  tidyr::expand_grid(graph_type = "tree", load_level = "high",
                     architecture = "naive", knobs, seed = 11:12) %>%
    dplyr::mutate(welfare = 10 + seed, tokens_admitted = 50,
                  median_latency = 200, welfare_over_optimum = 0.4,
                  alloc_ratio_true = 0.4)
}

test_that("the tuned table says whether the chosen knob sat on the grid's edge", {
  tuned <- node_tuned_table(.boundary_eval_raw())
  at    <- setNames(tuned$knob_at_boundary, tuned$mechanism)

  expect_true(at[["posted_price"]])
  expect_true(at[["market"]])
  expect_false(at[["market_cc"]])
  # An arm with nothing to tune has no edge to sit on.
  expect_false(at[["k8s"]])
  # The column is appended; every column the table already reported is intact.
  expect_equal(names(tuned)[(ncol(tuned) - 2L):ncol(tuned)],
               c("knob_at_boundary", "knob_flat", "alloc_ratio_true"))
  expect_true(all(tuned$welfare == 21.5))
})


# ---- was the welfare beside the chosen knob flat ---------------------------
#
# A knob on the edge of its grid is grid-limited only where welfare was still
# moving when the grid ran out. Where the chosen knob and the level next to it
# score the same on the tuning seeds, the maximum is a plateau the tie broke
# inside, and reporting that cell as grid-limited would claim a censored
# maximum the tuning frame does not show. The two flags are two questions and
# the table answers both.

.flat_tuning_raw <- function() {
  tidyr::expand_grid(graph_type = c("tree", "sp"), load_level = "high",
                     architecture = "naive", mechanism = "posted_price",
                     p_post_k = node_tuning_posted_levels(),
                     reserve_markup = 1, seed = 1:2) %>%
    # tree: every level scores the same, so the argmax is the whole grid and
    # the tie resolves inside a plateau. sp: welfare is still rising at the
    # bottom of the grid when it runs out.
    dplyr::mutate(welfare = ifelse(graph_type == "tree", 1, -p_post_k))
}

.flat_eval_raw <- function(tuning_raw) {
  node_eval_grid(tuning_raw, c(11L, 12L)) %>%
    dplyr::filter(mechanism == "posted_price") %>%
    dplyr::mutate(welfare = 10 + seed, tokens_admitted = 50,
                  median_latency = 200, welfare_over_optimum = 0.4,
                  alloc_ratio_true = 0.4)
}

test_that("the tuned table says whether the tuning welfare was flat there", {
  tr    <- .flat_tuning_raw()
  tuned <- node_tuned_table(.flat_eval_raw(tr), tr)
  flat  <- setNames(tuned$knob_flat, tuned$graph_type)

  # Both cells chose the smallest level the grid offered, so both sit on the
  # edge; only one of them was still climbing when it got there.
  expect_true(all(tuned$p_post_k == min(node_tuning_posted_levels())))
  expect_true(all(tuned$knob_at_boundary))
  expect_true(flat[["tree"]])
  expect_false(flat[["sp"]])
  # Appended after the flag it qualifies; every column before it is intact.
  expect_equal(names(tuned)[(ncol(tuned) - 2L):ncol(tuned)],
               c("knob_at_boundary", "knob_flat", "alloc_ratio_true"))
})

# Whether the welfare beside a knob moved is a question about seeds, and a
# fixed relative tolerance cannot ask it: at five tuning seeds a difference of
# a thousandth of the welfare is inside the spread the seeds already have, and
# a difference of a millionth is a different number to a tolerance that stops
# there. The criterion is the paired difference's own interval across seeds.
#
# Two cells, both choosing the bottom of the grid. One falls by a unit a level
# with almost no spread; the other falls by a thousandth against a spread of
# one. The seed swing sums to zero at every level, so the argmax is the slope's
# and the noise decides only the interval.
.t_paired_tuning_raw <- function(seeds = 1:5) {
  lv    <- sort(node_tuning_posted_levels())
  swing <- c(-1, -0.5, 0, 0.5, 1)
  tidyr::expand_grid(graph_type = c("steep", "level"), load_level = "high",
                     architecture = "naive", mechanism = "posted_price",
                     p_post_k = lv, reserve_markup = 1, seed = seeds) %>%
    dplyr::mutate(welfare =
      -ifelse(graph_type == "steep", 1, 0.001) * match(p_post_k, lv) +
       ifelse(graph_type == "steep", 0.001, 0.5) *
         (-1)^match(p_post_k, lv) * swing[match(seed, seeds)])
}

test_that("flatness is the paired difference's interval, not a tolerance", {
  tr    <- .t_paired_tuning_raw()
  tuned <- node_tuned_table(.flat_eval_raw(tr), tr)
  flat  <- setNames(tuned$knob_flat, tuned$graph_type)

  # Both cells chose the smallest level the grid offered, so both sit on the
  # edge and the flatness flag is what separates them.
  expect_true(all(tuned$p_post_k == min(node_tuning_posted_levels())))
  expect_true(all(tuned$knob_at_boundary))
  # A step of a thousandth against a seed spread of one is a difference the
  # interval cannot hold away from zero; a step of one against a spread of a
  # thousandth is a response the grid cut off while it was still moving.
  expect_true(flat[["level"]])
  expect_false(flat[["steep"]])

  # Neither difference is zero to a millionth, so the old relative tolerance
  # would have called both of them moving.
  mean_at <- function(g, k) mean(tr$welfare[tr$graph_type == g &
                                              tr$p_post_k == k])
  lv <- sort(node_tuning_posted_levels())
  for (g in c("steep", "level")) {
    d <- abs(mean_at(g, lv[1]) - mean_at(g, lv[2]))
    expect_gt(d, 1e-6 * abs(mean_at(g, lv[1])))
  }
})

test_that("one tuning seed answers the flatness question with NA", {
  # An interval needs a spread, and one seed has none to give. NA says the
  # question could not be asked rather than reporting a plateau or a slope.
  tr    <- .t_paired_tuning_raw(seeds = 1L)
  tuned <- node_tuned_table(.flat_eval_raw(tr), tr)
  expect_equal(nrow(tuned), 2L)
  expect_true(all(is.na(tuned$knob_flat)))
})

test_that("a knob with no neighbour and no tuning frame is not called flat", {
  tr    <- .flat_tuning_raw()
  tuned <- node_tuned_table(.boundary_eval_raw(), tr)
  flat  <- setNames(tuned$knob_flat, tuned$mechanism)

  # An arm with nothing to tune has no neighbour to be flat against.
  expect_false(flat[["k8s"]])
  # An arm the tuning frame never ran cannot be answered for, and NA says so
  # rather than reporting a response that was never measured.
  expect_true(is.na(flat[["market"]]))
  # Without a tuning frame the question cannot be asked at all.
  expect_true(all(is.na(node_tuned_table(.boundary_eval_raw())$knob_flat)))
})

test_that("the pipeline hands the tuned table the frame the knobs were chosen on", {
  src <- paste(readLines(here::here("_targets.R")), collapse = " ")
  expect_true(grepl(
    "node_tuned_table\\(\\s*bind_rows\\(node_exp6_eval_raw\\),\\s*bind_rows\\(node_exp6_tuning_raw\\)\\s*\\)",
    src, perl = TRUE))
})
