temp_dir <- treepplr::tp_tempdir(temp_dir = NULL)
setwd(temp_dir)
require(testthat)
require(crayon)

cat(crayon::yellow("\nTest-run : Running TreePPL.\n"))

test_that("Test-run_1a : tp_run SMC", {
  cat("\tTest-run_1a : tp_run SMC \n")
  run_smc <- tp_run(
    sampler = tp_compile(model = "crbd", method = "smc-apf", sweeps = 2, particles = 5),
    data = tp_data(data_input = "crbd")
  )
  expect_equal(2, length(unique(run_smc$sweep)))
})

test_that("Test-run_1b : tp_run MCMC", {
  cat("\tTest-run_1b : tp_run MCMC \n")
  run_mcmc <- tp_run(
    sampler = tp_compile(model = "crbd", method = "mcmc", iterations = 10),
    data = tp_data(data_input = "crbd"),
    n_runs = 2,
    n_processes = 2
  )
  expect_equal(2, length(unique(run_mcmc$run)))
})

test_that("Test-run_1c : tp_run custom_name", {
  cat("\tTest-run_1c : tp_run custom_name \n")
  run_smc <- tp_run(
    sampler = tp_compile(model = "crbd", method = "smc-apf", sweeps = 2, particles = 5),
    data = tp_data(data_input = "crbd"),
    out_file_name = "test_out"
  )
  expect_equal(2, length(unique(run_smc$sweep)))
})

test_that("Test-run_1d : tp_run threading", {
  cat("\tTest-run_1d : tp_run threading \n")
  run_smc <- tp_run(
    sampler = tp_compile(model = "crbd", method = "smc-apf", sweeps = 2, particles = 5),
    data = tp_data(data_input = "crbd"),
    n_processes = 2
  )
  expect_equal(2, length(unique(run_smc$sweep)))
})

test_that("Test-run_1e : tp_run no_parser", {
  cat("\tTest-run_1e : tp_run no_parser \n")
  run_smc <- tp_run(
    sampler = tp_compile(model = "tree_inference", method = "smc-apf", sweeps = 2, particles = 5),
    data = tp_data(data_input = "tree_inference"),
    n_processes = 2
  )
  expect_true(is.character(run_smc))
})
