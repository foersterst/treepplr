temp_dir <- treepplr::tp_tempdir(temp_dir = NULL)
setwd(temp_dir)
require(testthat)
require(crayon)

#These tests are made for the interactions with the self-contain TreePPL

cat(crayon::yellow("\nTest-model : Compilation and modification of the options.\n"))

test_that("Test-model_1a : tp_compile MCMC", {
  cat("\tTest-model_1a : tp_compile MCMC \n")

  sampler <- treepplr::tp_compile("crbd")

  expect_no_error(readBin(sampler$exe_path, "raw", 10e6))

})

test_that("Test-model_1b : tp_compile method SMC", {
  cat("\tTest-model_1b : tp_compile method SMC \n")

  sampler <- treepplr::tp_compile("crbd", method = "smc-bpf")

  expect_no_error(readBin(sampler$exe_path, "raw", 10e6))
})

test_that("Test-model_2a : tp_compile model name", {
  cat("\tTest-model_2a : tp_compile\n")

  sampler <- treepplr::tp_compile("crbd")

  version <- list.files("/tmp",
                        pattern = paste0("treeppl-", TPPLC_VERSION),
                        full.names = TRUE)

  if (length(version) == 0) {
    version <- unlist(strsplit(Sys.getenv("MCORE_LIBS"), "treeppl="))[2]
  }

  model_right = system(paste0("find ", version, " -name crbd.tppl"), intern = T)

  expect_equal(readr::read_file(sampler$model_path), readr::read_file(model_right))
})

test_that("Test-model_2b : tp_compile model path ", {
  cat("\tTest-model_2b : tp_compile\n")

  version <- list.files("/tmp",
                        pattern = paste0("treeppl-", TPPLC_VERSION),
                        full.names = TRUE)

  if (length(version) == 0) {
    version <- unlist(strsplit(Sys.getenv("MCORE_LIBS"), "treeppl="))[2]
  }

  model_right = system(paste0("find ", version, " -name crbd.tppl"), intern = T)
  sampler <- treepplr::tp_compile(model_right)

  expect_equal(sampler$model_path, model_right)
})

test_that("Test-model_3a : tp_write path", {
  cat("\tTest-model_3a : tp_write\n")

  path <- treepplr::tp_write_model("model function bla() => Int  {let bla = 1; return bla;}")
  expect_true(file.exists(path))
})


test_that("Test-model_3b : tp_write content", {
  cat("\tTest-model_3b : tp_write\n")

  content_right <- "model function bli() => Int {let bli = 2; return bli;}"
  path <- treepplr::tp_write_model(content_right)

  content <- readLines(path, warn = FALSE)
  expect_equal(content_right, content)

})


test_that("Test-model_4 : tp_compile model string ", {
  cat("\tTest-model_4 : tp_compile\n")

  model_string <- "model function blo() => Int {let blo = 3; return blo;}"
  sampler <- treepplr::tp_compile(model_string)
  model_right <- paste0(temp_dir, "tmp_model_file.tppl")

  expect_equal(sampler$model_path, model_right)
})
