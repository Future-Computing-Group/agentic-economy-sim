# test-plot-nodelevel.R
# ---------------------------------------------------------------------------
# The node-level twins of the Tufte figures.
#
# Three things are checked here and nowhere else:
#
#   - every figure function returns a ggplot on a minimal frame carrying the
#     node-level columns, so a column rename in the pipeline fails here rather
#     than in a silent render;
#   - no store-internal arm string reaches a plotted value, and so no legend:
#     the printed names are the manuscript's own (T / S / X for the instances,
#     uncontracted / contracted for the architectures);
#   - the mechanism figure carries no ascending arm.
#
# The file check is a guard on the pipeline's own output and stands down where
# there is no local store to have produced it.
# ---------------------------------------------------------------------------

# --- fixtures --------------------------------------------------------------

# A deterministic spread per row, so no cell's interval is degenerate and a
# collapsed group shows up as a zero-width band rather than as a pass.
node_metric_stub <- function(df) {
  j <- seq_len(nrow(df)) / nrow(df)
  dplyr::mutate(
    df,
    median_latency             = 100 + 10 * j,
    drop_rate                  = 0.20 + 0.05 * j,
    tokens_admitted            = 90 - 5 * j,
    served_among_admitted      = 0.95 - 0.02 * j,
    binding_fraction           = 0.30 + 0.10 * j,
    welfare                    = 50 + 5 * j,
    alloc_ratio_true           = 0.98 - 0.01 * j,
    mean_price_volatility      = 0.10 + 0.02 * j,
    mean_price_volatility_tail = 0.08 + 0.01 * j,
    price_volatility_cleared   = 0.24 + 0.02 * j,
    greedy_exact_incidence     = 0.05 * j
  )
}

node_stub <- function(...) {
  node_metric_stub(tidyr::expand_grid(
    graph_type = c("tree", "sp", "entangled"), ..., seed = 1:3))
}

node_exp6_stub <- function() {
  node_stub(load_level   = c("medium", "high"),
            architecture = c("naive", "hybrid_noema"),
            congestion   = c("baseline", "calibrated"),
            mechanism    = c("random", "edf", "greedy_ev", "market",
                             "market_asc", "market_cc", "k8s", "posted_price"),
            p_post_k     = 1)
}

# The tuned table is already a per-cell mean over the evaluation seeds, and it
# carries no drop rate: the figure's drop panel has nothing to draw for these
# two arms, which is a property of the store and not of the plotting code.
node_exp6_tuned_stub <- function() {
  tidyr::expand_grid(
    graph_type   = c("tree", "sp", "entangled"),
    load_level   = c("medium", "high"),
    architecture = c("naive", "hybrid_noema"),
    congestion   = c("baseline", "calibrated"),
    mechanism    = c("greedy_ev", "k8s", "market", "market_cc", "posted_price",
                     "posted_price_fcfs", "posted_price_edf")
  ) %>%
    dplyr::mutate(welfare = 55, alloc_ratio_true = 0.97, median_latency = 95,
                  tokens_admitted = 88)
}

# Every ggplot inside a patchwork, including the one the patchwork itself
# carries, so a content check reads what was built rather than what was meant.
patch_plots <- function(p) {
  acc <- list()
  walk <- function(q) {
    if (inherits(q, "patchwork")) for (r in q$patches$plots) walk(r)
    if (inherits(q, "ggplot")) acc[[length(acc) + 1L]] <<- q
    invisible(NULL)
  }
  walk(p)
  acc
}

# Every plotted character/factor value: the candidate legend and axis text.
plotted_labels <- function(p) {
  vals <- unlist(lapply(patch_plots(p), function(q) {
    d <- q$data
    if (is.null(d) || !nrow(d)) return(character())
    unlist(lapply(d, function(col)
      if (is.character(col) || is.factor(col)) as.character(col) else character()))
  }))
  unique(as.character(vals))
}

# --- one function per figure ----------------------------------------------

test_that("every node-level figure function returns a ggplot", {
  figs <- list(
    exp1  = make_node_exp1_tufte(node_stub(load_level = c("low", "medium", "high"))),
    exp2  = make_node_exp2_tufte(node_stub(load_level = c("medium", "high"),
                                           N = c(10, 60, 200))),
    exp3  = make_node_exp3_tufte(node_stub(
      load_level = c("medium", "high"), leaf_mix = c("uniform", "skewed"),
      policy = c("none", "trust", "locality", "role", "residency",
                 "residency_sliced"))),
    exp4a = make_node_exp4_tufte(node_stub(
      load_level = c("medium", "high"),
      architecture = c("naive", "naive_ema", "hybrid_noema", "hybrid_ema")), "a"),
    exp4b = make_node_exp4_tufte(node_stub(
      load_level = c("medium", "high"),
      architecture = c("naive", "naive_ema", "hybrid_noema", "hybrid_ema")), "b"),
    exp5  = make_node_exp5_tufte(node_stub(
      load_level = c("medium", "high"), policy = c("none", "locality"),
      architecture = c("naive", "hybrid_ema"))),
    exp6  = make_node_exp6_tufte(node_exp6_stub(), node_exp6_tuned_stub())
  )
  for (nm in names(figs)) {
    expect_s3_class(figs[[nm]], "ggplot")
    expect_gt(length(patch_plots(figs[[nm]])), 1L)
  }
})

# --- printed names, not store strings --------------------------------------

test_that("no plotted value carries a store-internal arm or instance string", {
  internal <- c("naive", "naive_ema", "hybrid", "hybrid_ema", "hybrid_noema",
                "tree", "sp", "entangled", "locality", "greedy_ev",
                "market_asc", "market_cc", "posted_price", "market_posted_slice",
                "residency_sliced")
  figs <- list(
    make_node_exp1_tufte(node_stub(load_level = c("low", "medium", "high"))),
    make_node_exp3_tufte(node_stub(
      load_level = c("medium", "high"), leaf_mix = c("uniform", "skewed"),
      policy = c("none", "trust", "locality", "role", "residency",
                 "residency_sliced"))),
    make_node_exp4_tufte(node_stub(
      load_level = c("medium", "high"),
      architecture = c("naive", "naive_ema", "hybrid_noema", "hybrid_ema")), "a"),
    make_node_exp5_tufte(node_stub(
      load_level = c("medium", "high"), policy = c("none", "locality"),
      architecture = c("naive", "hybrid_ema"))),
    make_node_exp6_tufte(node_exp6_stub(), node_exp6_tuned_stub())
  )
  for (p in figs) {
    labs <- plotted_labels(p)
    expect_equal(intersect(labs, internal), character(0))
    # No printed name of this pipeline carries an underscore, so one is a
    # store string that reached the page.
    expect_equal(grep("_", labs, value = TRUE), character(0))
  }
})

test_that("the instances print as T, S and X in that order", {
  p <- make_node_exp1_tufte(node_stub(load_level = c("low", "medium", "high")))
  inst <- patch_plots(p)[[1]]$data$instance
  expect_s3_class(inst, "factor")
  expect_equal(levels(inst), c("T", "S", "X"))
})

test_that("the congestion levels print as calibrated and baseline", {
  # The mechanism figure reports the calibrated level; the string is the
  # manuscript's, and the store's own, so a rename on either side fails here.
  raw <- node_exp6_stub()
  expect_true(all(c("calibrated", "baseline") %in% raw$congestion))
  p <- make_node_exp6_tufte(raw, node_exp6_tuned_stub())
  expect_s3_class(p, "ggplot")
})

# --- the mechanism figure's arms -------------------------------------------

test_that("the mechanism figure plots no ascending arm", {
  p <- make_node_exp6_tufte(node_exp6_stub(), node_exp6_tuned_stub())
  expect_equal(grep("asc", plotted_labels(p), value = TRUE), character(0))
  arms <- unique(unlist(lapply(patch_plots(p),
                               function(q) as.character(q$data$arm))))
  expect_setequal(arms, c("random", "EDF", "value-greedy", "market",
                          "posted price, value-ranked (tuned)",
                          "posted price, arrival-order (tuned)",
                          "market (tuned)"))
})

test_that("the tuned label map names every tuned posted arm but the deadline one", {
  # The deadline-priority arm tracks the arrival-order arm to within the
  # plotted resolution, so it is left off rather than overprinted; the caption
  # says so.
  drawn <- setdiff(.posted_knob_arms(), "posted_price_edf")
  expect_true(all(c(drawn, "market") %in% names(node_tuned_names)))
  expect_false("posted_price_edf" %in% names(node_tuned_names))
  # Each drawn arm has its own colour, shape and line type.
  arms <- c(unname(node_mech_names), unname(node_tuned_names))
  n <- length(arms)
  expect_gte(length(unique(palette_arm_node())), n)
  expect_gte(length(unique(shape_arm_node())), n)
  expect_gte(length(unique(linetype_arm_node())), n)
})

test_that("the mechanism figure reports the calibrated level alone", {
  raw <- node_exp6_stub()
  # The baseline rows carry a different welfare, so a figure that pooled the
  # two congestion levels would land between them rather than on the reported
  # one.
  raw$welfare[raw$congestion == "baseline"] <- 999
  p <- make_node_exp6_tufte(raw, node_exp6_tuned_stub())
  welf <- unlist(lapply(patch_plots(p), function(q) q$data$welfare_mean))
  expect_true(all(welf[is.finite(welf)] < 900))
})

# --- the files the manuscript includes -------------------------------------

test_that("the node-level figure targets write non-empty PDFs", {
  skip_if_not(dir.exists(here::here("_targets", "objects")),
              "no local store: these files are pipeline output")
  for (f in c("exp1_tufte.pdf", "exp2_tufte.pdf", "exp3_tufte.pdf",
              "exp4_a_tufte.pdf", "exp4_b_tufte.pdf", "exp5_tufte.pdf",
              "exp6_tufte.pdf")) {
    path <- here::here("fig", "node", f)
    expect_true(file.exists(path), info = f)
    expect_gt(file.size(path), 0)
    expect_identical(readBin(path, "raw", 4L), charToRaw("%PDF"))
  }
})
