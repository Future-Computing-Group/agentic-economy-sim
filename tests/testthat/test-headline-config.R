# The configuration every headline arm actually runs at.
#
# Two properties are pinned here because both are confounds rather than
# parameters. An integrator efficiency below 1.0 hands the hybrid arms an
# assumed demand reduction that no experiment measures, so the architectural
# effect and the assumed saving cannot be told apart in any reported number.
# A naive baseline run at a different tatonnement price step from the
# integrator's compares two architectures AND two price steps at once.
#
# The constants live in _targets.R, which the tests must not source: it builds
# a crew controller and the whole target list as a side effect. They are read
# with targets_constants() (helper-targets-constants.R), which evaluates the
# named top-level assignments out of the parsed file and nothing else.

# Every R source the pipeline loads, plus the pipeline file itself.
pipeline_sources <- function() {
  c(list.files(here::here("R"), full.names = TRUE, pattern = "\\.[Rr]$"),
    here::here("_targets.R"))
}

test_that("every headline arm runs at integrator efficiency 1.0", {
  k <- targets_constants(c("integ_efficiency_sp", "integ_efficiency_ent"))
  expect_equal(k$integ_efficiency_sp, 1.0)
  expect_equal(k$integ_efficiency_ent, 1.0)
})

test_that("no arm sets its efficiency from a literal", {
  wired <- grep("integ_efficiency\\s*=",
                readLines(here::here("_targets.R")), value = TRUE)
  expect_gt(length(wired), 0L)
  expect_false(any(grepl("integ_efficiency\\s*=\\s*[0-9]", wired)))
})

test_that("the naive baseline and the integrator share one price step", {
  step <- function(f, arg) eval(formals(f)[[arg]], globalenv())
  expect_equal(price_eta, 0.15)
  expect_equal(step(exp4_run_single, "eta"), step(exp4_run_single, "integ_eta"))
  expect_equal(step(exp4_run_single, "eta"), price_eta)
  expect_equal(step(clear_multitier_market, "eta"), price_eta)
  expect_equal(step(integrator_init, "eta"), price_eta)
  expect_equal(targets_constants("integ_eta")$integ_eta, price_eta)
})

test_that("both smoothed cells read one smoothing constant", {
  # The price step is not the only constant that would be a confound if the two
  # architectures ran at different values of it. Smoothing is a FACTOR of the
  # Exp.4 factorial, so a cell that smoothed at its own beta would make the
  # factor mean two different things in its two levels, and the contrast would
  # be a comparison of two architectures AND two smoothing rates at once.
  default <- function(f, arg) eval(formals(f)[[arg]], globalenv())
  expect_equal(default(exp4_run_single, "integ_beta"), default(integrator_init, "beta"))

  # Pinned on behaviour rather than on the source, because the value reaches
  # the two cells through two different call sites: whatever the driver is
  # given, both smoothed cells must move with it and neither unsmoothed cell
  # may move at all.
  cell <- function(a, b) exp4_run_single(a, "sp", "high", N = 55L, seed = 1L,
                                         n_rounds = 12L, integ_beta = b)
  for (a in c("naive_ema", "hybrid_ema")) {
    expect_false(isTRUE(all.equal(cell(a, 0.8)$mean_price_volatility,
                                  cell(a, 0.4)$mean_price_volatility)),
                 info = a)
  }
  for (a in c("naive", "hybrid_noema")) {
    expect_equal(cell(a, 0.8)$mean_price_volatility,
                 cell(a, 0.4)$mean_price_volatility, info = a)
  }
})


test_that("no price step is written as a literal outside its definition", {
  # (?<![A-Za-z_]) keeps `beta = 0.8` and `theta = ...` out of the sweep while
  # catching both `eta = 0.25` and `integ_eta = 0.15`. The one line allowed to
  # carry a number is the definition of the constant itself.
  pattern <- "(?<![A-Za-z_])(integ_)?eta\\s*=\\s*[0-9]"
  offenders <- unlist(lapply(pipeline_sources(), function(path) {
    hits <- grep(pattern, readLines(path), value = TRUE, perl = TRUE)
    if (length(hits)) paste(basename(path), trimws(hits), sep = ": ") else NULL
  }))
  expect_equal(offenders, NULL)
})


test_that("every driver defaults to no assumed demand reduction", {
  # The pipeline passes the constant at every integrator call site, so these
  # defaults are reached only by a caller that omits the argument: a test, a
  # probe, a future target. At 0.8 such a caller silently gets a 20 percent
  # demand reduction the paper no longer claims, which is the same hazard the
  # sweep driver's own default carried, one level down.
  default <- function(f, arg) eval(formals(f)[[arg]], globalenv())
  expect_equal(default(integrator_init, "efficiency_factor"), 1.0)
  expect_equal(default(exp4_run_single, "integ_efficiency"), 1.0)
  expect_equal(default(exp5_run_single, "integ_efficiency"), 1.0)
  expect_equal(default(exp6_run_single, "integ_efficiency"), 1.0)
})
