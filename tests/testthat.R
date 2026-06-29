# Test entry point for the agentic-economy-sim repo.
# Run with:  Rscript tests/testthat.R
#
# The repo is a project-style codebase (not an R package), so we use
# testthat::test_dir() rather than test_check().

suppressPackageStartupMessages({
  library(testthat)
  library(here)
})

testthat::test_dir(here::here("tests", "testthat"), reporter = "summary")
