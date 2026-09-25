# The market under the joint-misreport test.
#
# The discovered-price market clears on reported values, so an agent that
# withholds part of its demand can lower the prices it faces. The joint-
# misreport block measures that gain on the market arm with the same deviation
# set as the VCG arm: the market's allocation, the market's payment (the node
# price cost of each winner's bundle), true value minus payment.

dr_env <- function() {
  env <- node_run_env("tree", "high", 3L, "uniform", "off")
  # Two tokens through l3's path: the binding node is e2.
  scale_capacities(env, 0.04)
}

dr_tasks <- function() tibble::tibble(
  task_id = c("a1", "a2", "b1"), agent_id = c(1L, 1L, 2L),
  deadline = 1000, value_base = c(2.5, 2.5, 1.6), recipe = "l3")

dr_ev <- function(tasks, env) task_expected_value(
  tasks, 0.5, base_latency_for_bids(env), init_success_model(),
  lambda_l_default = node_lambda_l())

test_that("truthful reporting reproduces the market's allocation", {
  env <- dr_env(); tasks <- dr_tasks()
  a  <- exp7_mechanism_allocate("market", tasks, env, 0.5,
                                base_latency_for_bids(env), init_success_model(),
                                lambda_l_default = node_lambda_l())
  ms <- init_market_state(env)
  cl <- clear_multitier_market(tasks, env, 0.5, base_latency_for_bids(env), ms,
                               lambda_l_default = node_lambda_l())
  expect_setequal(a$task_id, cl$allocation$task_id)
  # A winner pays its bundle at the node prices the round cleared at.
  A <- task_recipes(cl$allocation, env)
  p <- cl$clearing$prices$price[match(colnames(A), cl$clearing$prices$tier)]
  expect_equal(a$payment[match(cl$allocation$task_id, a$task_id)],
               as.vector(A %*% p))
})

test_that("where demand reduction pays, the market arm's gain is positive", {
  env <- dr_env(); tasks <- dr_tasks()
  env <- exp7_certify(env, tasks, "node")
  g <- exp7_agent_gains("market", tasks, env, aid = 1L,
                        true_ev = dr_ev(tasks, env), util_hat = 0.5,
                        blb = base_latency_for_bids(env), sm = init_success_model(),
                        lambda_l_default = node_lambda_l())
  expect_gt(max(g), 0)
  expect_equal(g[["truthful"]], 0)
  # Reporting its values low, agent 1 still wins both tasks, and the price
  # its own reports had pushed up falls to the reserve: the gain is the price
  # it no longer pays.
  expect_true(startsWith(names(g)[which.max(g)], "uniform_"))
  expect_lt(unname(g[["uniform_1.1"]]), 1e-12)
  a_true <- exp7_mechanism_allocate("market", tasks, env, 0.5,
                                    base_latency_for_bids(env),
                                    init_success_model(), node_lambda_l())
  shaded <- tasks; shaded$value_base[1:2] <- 0.7 * shaded$value_base[1:2]
  a_low  <- exp7_mechanism_allocate("market", shaded, env, 0.5,
                                    base_latency_for_bids(env),
                                    init_success_model(), node_lambda_l())
  expect_setequal(a_low$task_id[a_low$agent_id == 1L], c("a1", "a2"))
  expect_lt(sum(a_low$payment[a_low$agent_id == 1L]),
            sum(a_true$payment[a_true$agent_id == 1L]))
})

test_that("the VCG arm of the block is unchanged by the mechanism argument", {
  a <- exp7b_run_single(graph_type = "entangled", load_level = "high", N = 8L,
                        seed = 1L, substrate = "node", cap_scale = 0.1,
                        lambda_l_default = node_lambda_l(), n_rounds = 3L)
  b <- exp7b_run_single(graph_type = "entangled", load_level = "high", N = 8L,
                        seed = 1L, substrate = "node", cap_scale = 0.1,
                        lambda_l_default = node_lambda_l(), n_rounds = 3L,
                        mechanism = "vcg")
  expect_identical(a, b)
  expect_equal(a$mechanism, "vcg")
})

test_that("the market arm runs through the block and reports its gain", {
  r <- exp7b_run_single(graph_type = "tree", load_level = "high", N = 8L,
                        seed = 1L, substrate = "node", cap_scale = 0.25,
                        lambda_l_default = node_lambda_l(), n_rounds = 3L,
                        mechanism = "market")
  expect_equal(r$mechanism, "market")
  expect_true(is.finite(r$br_gain_max))
  expect_gte(r$br_gain_max, 0)
})

test_that("the grid appends the market arm at every point the VCG arm ran", {
  g <- node_exp7b_grid(1:10)
  vcg <- g[g$mechanism == "vcg", ]
  mk  <- g[g$mechanism == "market", ]
  expect_equal(nrow(vcg), 620L)
  expect_true(all(which(g$mechanism == "market") > 620L))
  key <- function(d) paste(d$graph_type, d$N, d$cap_scale, d$seed)
  expect_setequal(key(mk), unique(key(vcg)))
  expect_false(any(duplicated(key(mk))))
})
