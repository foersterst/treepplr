#' Run a TreePPL sampler
#'
#' @description
#' Executes a compiled TreePPL sampler on the given data, saves the raw JSON
#' output to disk, prints a run summary, and, when a parser is available for
#' the model/method combination, parses the output into tidy data frames.
#'
#' @param sampler a sampler produced by \code{tp_compile()}.
#' @param data input data, produced by \code{tp_data()}.
#' @param dir the full path to the directory where
#' you want to save the output. Defaults to \code{tp_tempdir()}.
#' @param out_file_name the name of the output file in JSON format. Defaults to `"out"`.
#' @param n_runs (\code{integer}) the number of sweeps (SMC) or runs
#' (MCMC).
#' @param n_processes (\code{integer}) the number of parallel processes to use.
#' Cannot be greater than `n_runs`.
#' @param ... See [treepplr::tp_runtime_options()] for all supported arguments.
#'
#' @details
#' If the model belongs to a category without an available parser (e.g.
#' `"host-repertoire-evolution"`, `"tree-inference"`), or if the inference
#' method is neither SMC nor MCMC, no parsing is attempted: a message is
#' printed pointing to the output directory, and the raw output file path(s)
#' are returned instead.
#'
#' @return
#' If a parser is available for the model/method combination, a parsed
#' tidy data frame of TreePPL output via `tp_parse_smc()` or `tp_parse_mcmc()`.
#' Otherwise, the full path(s) to the raw JSON output file(s),
#' along with a console message explaining that no parser is available.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # When using SMC
#' # compile model and create SMC inference machinery
#' exe_path <- tp_compile(model = "coin", method = "smc-bpf", particles = 2000)
#'
#' # prepare data
#' data_path <- tp_data(data_input = "coin")
#'
#' # run TreePPL
#' result <- tp_run(exe_path, data_path, n_runs = 2)
#'
#'
#' # When using MCMC
#' # compile model and create MCMC inference machinery
#' exe_path <- tp_compile(model = "coin", method = "mcmc-naive", iterations = 2000)
#'
#' # prepare data
#' data_path <- tp_data(data_input = "coin")
#'
#' # run TreePPL
#' result <- tp_run(exe_path, data_path)
#' }
tp_run <- function(
  sampler,
  data,
  dir = NULL,
  out_file_name = "out",
  n_runs = 1,
  n_processes = 3,
  ...
) {
  # start time
  tt <- Sys.time()

  if (is.null(dir)) {
    dir_path <- tp_tempdir()
  } else {
    dir_path <- dir
  }

  listFiles <- list.files(
    path = dir_path,
    pattern = out_file_name,
    full.names = TRUE
  )
  if (length(listFiles) != 0) {
    file.remove(listFiles)
  }

  output_path <- paste0(dir_path, out_file_name, ".json")

  # If a list have multiple time the same key
  # list[[key]] will return the first key
  # Exemple
  #> lis <- list(method = "mcmc", method = "smc")
  #> lis[["method"]] => "mcmc"
  # So the user list have priority
  tpplc_options <- list_to_options(tp_list(...))

  if (length(tpplc_options[["compile"]]) != 0) {
    stop("Can't give compile time options here")
  }
  # Empty LD_LIBRARY_PATH from R_env for this command specifically
  # due to conflict with internal env from treeppl self container
  command <- paste(
    "LD_LIBRARY_PATH= ",
    sampler$exe_path,
    data,
    options_to_string(tpplc_options[["runtime"]]),
    paste(">", output_path)
  )

  if (n_runs > 1) {
    if (n_processes > n_runs) {
      warning("n_processes reduce to be equal to n_runs.")
      n_processes <- n_runs
    }
    future::plan(future::multisession, workers = n_processes)
    future.apply::future_sapply(
      1:n_runs,
      FUN = function(i) {
        system(paste0(command, i))
      }
    )
  } else {
    system(command)
  }

  # the output (JSON) files
  listFiles <- list.files(
    path = dir_path,
    pattern = out_file_name,
    full.names = TRUE
  )

  # Run Info #
  # elapsed time
  et <- round(Sys.time() - tt, digits = 2)
  # take method & model from sampler
  mtd <- sampler$compile_options$method
  mod <- sampler$compile_options$model
  # output files
  of <- list.files(
    path = dir_path,
    pattern = out_file_name,
    full.names = FALSE
  )
  of <- paste(of, collapse = ", ")

  # run info summary
  run_info <- paste0(
    crayon::bold("Analysis Summary\n"),
    "-----------------------------\n",
    crayon::green("Status: "), "Completed\n",
    crayon::cyan("Time elapsed: "), et, "\n",
    crayon::cyan("Model: "), mod, "\n",
    crayon::cyan("Method: "), mtd, "\n",
    crayon::cyan("Output directory: "), dir_path, "\n",
    crayon::cyan("Output file(s): "), of, "\n"
  )

  # print run info summary
  cat(run_info)

  # parse JSON to tidy data frames & return #
  # get model category: this is needed because at the moment, we do not have parsers
  # for models that return trees. NB: This is a temporary solution while we come up
  # with new parsers.
  mc <- tp_model_library()
  mod_cat <- mc[mc$model_name == mod, ]$category

  # model categories with unavailable parsers
  no_parsers <- c(
    "host-repertoire-evolution",
    "tree-inference"
  )

  if (mod_cat %in% no_parsers) {
    message(
      "Sorry, we don't have a parser for this model and/or inference method yet.\n",
      paste0("The output file(s) can be found in: ", dir_path)
    )
    res <- listFiles
  } else if (grepl("smc", mtd, ignore.case = TRUE)) {
    res <- tp_parse_smc(listFiles)
  } else if (grepl("mcmc", mtd, ignore.case = TRUE)) {
    res <- tp_parse_mcmc(listFiles)
  } else {
    message(
      "Sorry, we don't have a parser for method '", mtd, "' yet.\n",
      paste0("The output file(s) can be found in: ", dir_path)
    )
    res <- listFiles
  }
  return(res)
}
