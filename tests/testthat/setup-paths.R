# testthat auto-sources files prefixed with `setup-` before any tests run.
# This file mirrors _targets.R's "auto-source R/" step so test fixtures and
# the system-under-test share the same module load order.

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

purrr::walk(
  list.files(here::here("R"), full.names = TRUE, pattern = "\\.[Rr]$"),
  source
)
