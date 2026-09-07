temp_dir <- treepplr::tp_tempdir(temp_dir = NULL)
setwd(temp_dir)
require(testthat)
require(crayon)

cat(crayon::yellow("\nTest-data : Import and convert data.\n"))

test_that("Test-data_1a : tp_data name", {
  cat("\tTest-data_1a \n")

    version <- list.files("/tmp",
                          pattern = paste0("treeppl-", TPPLC_VERSION),
                          full.names = TRUE)
    if (length(version) == 0) {
      version <- unlist(strsplit(Sys.getenv("MCORE_LIBS"), "treeppl="))[2]
    }

    data_right <- system(paste0("find ", version, " -name testdata_coin.json"), intern = T)

    data <- treepplr::tp_data("coin")

    expect_equal(readr::read_file(data), readr::read_file(data_right))
})

test_that("Test-data_1a_bis : tp_data name", {
  cat("\tTest-data_1a_bis (local installed version) \n")

  version <- unlist(strsplit(Sys.getenv("MCORE_LIBS"), "treeppl="))[2]
  if (!is.na(version)) {
    data_right <- system(paste0("find ", version, " -name testdata_coin.json"), intern = T)

    data <- treepplr::tp_data("coin")

    expect_equal(readr::read_file(data), readr::read_file(data_right))
  } else {
    expect_true(TRUE)
  }
})

test_that("Test-data_1b : tp_data content", {
  cat("\tTest-data_1b \n")

  data_right <-
    tp_list(
      coinflips =
        c(
          TRUE,
          TRUE,
          TRUE,
          FALSE,
          TRUE,
          FALSE,
          FALSE,
          TRUE,
          TRUE,
          FALSE,
          FALSE,
          FALSE,
          TRUE,
          FALSE,
          TRUE,
          FALSE,
          FALSE,
          TRUE,
          FALSE,
          FALSE
        )
    )

  path <- treepplr::tp_data("coin")
  content <- readLines(path, warn = FALSE)
  data <- jsonlite::fromJSON(content)

  expect_equal(data_right, data)

})


test_that("Test-data_2a : tp_write_data path", {
  cat("\tTest-data_2a \n")

  path_right <- paste0(temp_dir, "tmp_data_file.json")
  data <- list(test = "test_data")
  data_list <- tp_list(list(data))
  path <- treepplr::tp_write_data(data_list)

  expect_equal(path, path_right)

})
