# Read named constants out of _targets.R without sourcing it.
#
# Sourcing the pipeline file builds a crew controller and the whole target list
# as a side effect, so only the top-level assignments asked for are evaluated.
# Shared by the tests that pin the headline configuration and the tests that
# pin the sensitivity sweep to it.

targets_constants <- function(names) {
  env <- new.env(parent = globalenv())
  for (expr in parse(here::here("_targets.R"))) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        is.name(expr[[2]]) && as.character(expr[[2]]) %in% names) {
      eval(expr, env)
    }
  }
  mget(names, env)   # errors loudly if _targets.R stopped defining one
}
