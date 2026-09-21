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
                 "residency_sliced")))
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
                 "residency_sliced")))
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

# --- the files the manuscript includes -------------------------------------

test_that("the node-level figure targets write non-empty PDFs", {
  skip_if_not(dir.exists(here::here("_targets", "objects")),
              "no local store: these files are pipeline output")
  for (f in c("exp1_tufte.pdf", "exp2_tufte.pdf", "exp3_tufte.pdf")) {
    path <- here::here("fig", "node", f)
    expect_true(file.exists(path), info = f)
    expect_gt(file.size(path), 0)
    expect_identical(readBin(path, "raw", 4L), charToRaw("%PDF"))
  }
})
