# Tests for export_env_json() — the environment constants the emulation testbed
# reads rather than re-derives. The testbed replays the simulator's allocations
# on real infrastructure, so every constant it needs (per-tier capacity, base
# latency, demand weight, deadlines, the load factor and the run's identity)
# must travel with the run instead of being duplicated in testbed code.

test_that("export_env_json writes every field the testbed consumes", {
  dir <- withr::local_tempdir()
  env <- init_environment(build_dependency_graph("agentic"), "high",
                          n_agents = 200L, graph_type = "agentic")
  path <- export_env_json(env, dir, n_rounds = 200L, seed = 1L,
                          deadlines = agentic_deadlines())
  expect_equal(basename(path), "env_high.json")

  spec <- jsonlite::fromJSON(path)
  expect_setequal(names(spec),
                  c("graph_type", "load_level", "load_factor", "n_agents",
                    "n_rounds", "seed", "capacities", "base_ms",
                    "demand_weight", "deadlines", "sim_git_sha"))
  expect_equal(spec$graph_type, "agentic")
  expect_equal(spec$load_level, "high")
  expect_equal(spec$load_factor, 1.5)
  expect_equal(spec$n_agents, 200L)
  expect_equal(spec$n_rounds, 200L)
  expect_equal(spec$seed, 1L)
  expect_equal(spec$capacities[c("device", "edge", "cloud")],
               list(device = 200, edge = 300, cloud = 500))
  expect_equal(spec$demand_weight[c("device", "edge", "cloud")],
               list(device = 1.11, edge = 1.0, cloud = 2.25))
  expect_equal(spec$deadlines, c(4200L, 5000L, 5900L))
  expect_equal(unlist(spec$base_ms[c("device", "edge", "cloud")]),
               agentic_base_latency())
})

test_that("export_env_json names the file after the load level it exports", {
  dir <- withr::local_tempdir()
  env <- init_environment(build_dependency_graph("agentic"), "medium",
                          n_agents = 200L, graph_type = "agentic")
  path <- export_env_json(env, dir, n_rounds = 10L, seed = 2L,
                          deadlines = agentic_deadlines())
  expect_equal(basename(path), "env_medium.json")
  expect_equal(jsonlite::fromJSON(path)$load_factor, 1.0)
})

test_that("export_env_json records the simulator's own commit", {
  dir  <- withr::local_tempdir()
  env  <- init_environment(build_dependency_graph("sp"), "high",
                           n_agents = 10L, graph_type = "sp")
  spec <- jsonlite::fromJSON(export_env_json(env, dir, n_rounds = 1L, seed = 1L))

  # Read independently, off .git, not by re-running the command under test.
  # In a linked worktree .git is a pointer FILE, HEAD lives in the worktree's
  # own git dir, and the branch ref lives in the common dir it points at.
  gitdir <- file.path(here::here(), ".git")
  if (!dir.exists(gitdir)) {
    gitdir <- sub("^gitdir: ", "", readLines(gitdir, warn = FALSE)[1])
  }
  head_ref <- readLines(file.path(gitdir, "HEAD"), warn = FALSE)
  common   <- file.path(gitdir, "commondir")
  roots    <- c(gitdir,
                if (file.exists(common))
                  file.path(gitdir, readLines(common, warn = FALSE)[1]))
  ref_path <- Filter(file.exists, file.path(roots, sub("^ref: ", "", head_ref)))
  skip_if_not(length(ref_path) > 0L, "HEAD ref is packed or detached")
  ref_path <- ref_path[1]

  expect_match(spec$sim_git_sha, "^[0-9a-f]{40}(-dirty)?$")
  expect_equal(sub("-dirty$", "", spec$sim_git_sha),
               readLines(ref_path, warn = FALSE))
})
