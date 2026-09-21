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
  expect_setequal(unique(g$mechanism), c("posted_price", "market", "market_cc"))
  expect_setequal(unique(g$p_post_k[g$mechanism == "posted_price"]),
                  node_posted_levels())
  expect_setequal(unique(g$p_post_k[g$mechanism != "posted_price"]), 1)
  expect_setequal(unique(g$reserve_markup[g$mechanism == "market"]),
                  node_reserve_markups())
  expect_setequal(unique(g$reserve_markup[g$mechanism == "posted_price"]), 1)
  # Twelve cells: three instances, two loads, two architectures; each at both
  # congestion levels.
  expect_equal(nrow(g),
               (length(node_posted_levels()) + 2 * length(node_reserve_markups())) *
                 12 * 2 * 2)
})

# A tuning frame whose argmax is known by construction: the posted price peaks
# at 1.5, the market at 1.25 and the congestion-consistent arm at 2.
.tuning_fixture <- function(seeds = 1:2) {
  knobs <- dplyr::bind_rows(
    tibble(mechanism = "posted_price", p_post_k = node_posted_levels(),
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
                  welfare_over_optimum = 0.4)
  tuned <- node_tuned_table(eval_raw)

  expect_equal(nrow(tuned), 10L)
  expect_setequal(names(tuned),
                  c("graph_type", "load_level", "architecture", "mechanism",
                    "p_post_k", "reserve_markup", "n_eval_seeds", "welfare",
                    "tokens_admitted", "median_latency", "welfare_over_optimum"))
  expect_true(all(tuned$n_eval_seeds == 2L))
  expect_true(all(tuned$welfare == 21.5))
  expect_equal(tuned$p_post_k[tuned$mechanism == "posted_price"], c(1.5, 1.5))
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
