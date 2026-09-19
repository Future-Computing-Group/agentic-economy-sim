# test-node-dsic.R
# ---------------------------------------------------------------------------
# The exactness certificate and the two guards that read it.
#
# The identical-bundle test both incentive guards apply is SUFFICIENT for the
# allocation rule to be the welfare argmax and is not NECESSARY for it: on a
# laminar leaf-block family under unit leaf-token demand, value-greedy is the
# exact maximiser. The certificate replaces the proxy with the property, and
# the refusal on everything uncertified stays exactly where it was.
# ---------------------------------------------------------------------------

dsic_env <- function(nm = "T", size = "scale") {
  spec <- leaf_instance_specs(size)[[nm]]
  env  <- init_environment(build_leaf_graph(spec), "high", 8L, "leaf",
                           capacities   = leaf_capacities(spec),
                           base_latency = leaf_base_latency(spec))
  env$recipes <- leaf_recipes(spec, "unit")
  env$spec    <- spec
  env
}

dsic_tasks <- function(env, n = 12L, seed = 1L) {
  set.seed(seed)
  tibble(task_id    = sprintf("t%02d", seq_len(n)),
         agent_id   = rep_len(1:4, n),
         deadline   = 1000,
         value_base = runif(n, 1, 2),
         recipe     = rep_len(names(env$recipes), n))
}

# A packer that takes the odd rows of its input whatever they are worth. It is
# not the argmax, and on the fixture below removing an agent shifts the rows so
# the others end up with less than they had: a negative Clarke externality.
odd_row_packer <- function(rank_vec, tasks_all, env, max_tasks = Inf) {
  n <- nrow(tasks_all)
  if (n == 0L) return(integer(0))
  seq(1L, n, by = 2L)
}


# ---- the certificate ------------------------------------------------------

test_that("the certificate passes on tree and sp and fails on entangled", {
  for (nm in c("T", "S")) {
    env <- dsic_env(nm)
    expect_true(dsic_certificate(env, dsic_tasks(env)))
  }
  env_x <- dsic_env("X")
  expect_false(dsic_certificate(env_x, dsic_tasks(env_x)))

  # The failure is the submodularity clause, not the unit-demand clause: the
  # tasks are the same unit leaf tokens on all three instances.
  anc <- ancestor_matrix(env_x$spec)
  cert <- polymatroid_certificate(
    leaf_rank(anc, token_capacity(env_x$spec)), rownames(anc))
  expect_equal(unname(cert), c(TRUE, TRUE, FALSE))
})

test_that("the certificate fails when demand is not unit leaf-token", {
  spec <- leaf_instance_specs("scale")$T
  env  <- dsic_env("T")
  env$recipes <- leaf_recipes(spec, "bundle")
  tasks <- dsic_tasks(env)                       # labels are now leaf PAIRS
  expect_false(dsic_certificate(env, tasks))
})

test_that("the certificate is recomputed on the effective region", {
  cap_leaf <- function(env, leaf, tokens) {
    w <- env$spec$weight
    env$capacities <- dplyr::mutate(env$capacities, capacity = ifelse(
      tier == leaf, pmin(capacity, w * tokens), capacity))
    env
  }
  env <- dsic_env("X")
  tasks <- dsic_tasks(env)

  expect_true(dsic_certificate(cap_leaf(env, "l2", 0), tasks))
  expect_false(dsic_certificate(cap_leaf(env, "l4", 0), tasks))
  # A partial cap never repairs it: the crossing blocks still cross.
  expect_false(dsic_certificate(cap_leaf(env, "l2", 25), tasks))
})


# ---- the guards -----------------------------------------------------------

test_that("vcg_allocate still refuses a recipe environment with no certificate", {
  env   <- dsic_env("T")
  tasks <- dsic_tasks(env)
  expect_error(
    vcg_allocate(tasks, env, util_hat = 0.5, base_latency_for_bids(env),
                 init_success_model()),
    "no incentive claim is available")
})

test_that("exp7's guard refuses without a certificate and passes with one", {
  env <- dsic_env("T")
  expect_error(exp7_require_identical_bundles(env), "heterogeneous-recipe")
  env$dsic_status <- "certified"
  expect_true(exp7_require_identical_bundles(env))
  expect_true(exp7_require_identical_bundles(env, dsic_tasks(env)))
})

test_that("the Clarke externality assertion still aborts on the certified path", {
  env <- dsic_env("T")
  env$dsic_status <- "certified"
  tasks <- dsic_tasks(env, n = 4L)
  tasks$agent_id   <- 1:4
  tasks$value_base <- c(2, 0.1, 2, 0.1)

  local_global_stub(".greedy_pack_by", odd_row_packer)
  expect_error(
    vcg_allocate(tasks, env, util_hat = 0.5, base_latency_for_bids(env),
                 init_success_model()),
    "not the welfare argmax")
})

test_that("the uncertified arm clamps the externality and counts it", {
  env <- dsic_env("X")
  env$dsic_status <- "uncertified"
  tasks <- dsic_tasks(env, n = 4L)
  tasks$agent_id   <- 1:4
  tasks$value_base <- c(2, 0.1, 2, 0.1)

  local_global_stub(".greedy_pack_by", odd_row_packer)
  res <- vcg_allocate(tasks, env, util_hat = 0.5, base_latency_for_bids(env),
                      init_success_model())
  expect_true(all(res$vcg_payment >= 0))
  expect_gt(attr(res, "n_negative_externality"), 0)
})
