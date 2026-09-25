# A function that shares a target's name is dropped from the pipeline's
# globals, so an edit to it is not tracked as a dependency of anything.

test_that("no function in R/ shares a name with a target", {
  src   <- paste(readLines(here::here("_targets.R")), collapse = "\n")
  hits  <- regmatches(src, gregexpr("tar_target\\(\\s*[A-Za-z0-9_.]+", src))[[1]]
  names <- sub("tar_target\\(\\s*", "", hits)
  fns   <- Filter(function(n) is.function(get(n, envir = globalenv())),
                  intersect(names, ls(globalenv())))
  expect_equal(fns, character(0))
})
