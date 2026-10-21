# Exported Functions ----------------------------------------------------------------

#' Run a TreePPL sampler
#'
#' @description
#' `tp_run()` executes a compiled TreePPL sampler on the given data, saves the
#' raw JSON output to disk, prints a run summary and, when a parser is available
#' for the model/method combination, imports the output to the current R session.
#'
#' @param sampler A TreePPL sampler (class `tppls`), produced by [tp_compile()].
#' @param data Path to the input data, produced by [tp_data()].
#' @param ... Runtime options, as `name = value` pairs or a single list (see
#'   [tp_list()]). See `tp_treeppl_options("runtime")` for all supported
#'   arguments. Names are written without `--`, and underscores are converted to
#'   hyphens (e.g., `subsample_size = 10` becomes `--subsample-size 10`).
#'   Compile options are not allowed here; pass them to [tp_compile()] instead.
#' @param dir Path to the directory where the output is saved. If `NULL`, uses
#'   [tp_tempdir()].
#' @param out_file_name Name of the output file, without extension. With more
#'   than one run, the run number is appended (e.g., `out1.json`, `out2.json`).
#' @param n_runs Number of independent runs of the sampler.
#' @param n_processes Number of parallel processes used when `n_runs > 1`. It is
#'   capped at `n_runs`.
#' @param verbose Logical. Print a run summary on completion?
#'
#' @details
#' Previous output files with the same `out_file_name` in `dir` are deleted
#' before running.
#'
#' If the model belongs to a category without an available parser (e.g.
#' `"host-repertoire-evolution"`, `"tree-inference"`), or if the inference
#' method is neither SMC nor MCMC, no parsing is attempted: a message points to
#' the output directory and the paths of the raw output files are returned.
#'
#' @return If a parser is available, a workable R object (usually a tidy data frame,
#' a list, or an object of class `phylo`, depending on the model).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # SMC
#' sampler <- tp_compile(model = "coin", method = "smc-bpf")
#' data_path <- tp_data(data_input = "coin")
#' result <- tp_run(sampler, data_path, particles = 2000, n_runs = 2)
#'
#' # MCMC, with runtime options given as a list
#' sampler <- tp_compile(model = "coin", method = "mcmc-naive")
#' result <- tp_run(sampler, data_path, tp_list(iterations = 2000))
#' }
tp_run <- function(
  sampler,
  data,
  dir = NULL,
  out_file_name = "out",
  n_runs = 1,
  n_processes = 3,
  verbose = TRUE,
  ...
) {
  start <- Sys.time()

  data <- data$data_path

  if (!methods::is(sampler, "tpSampler")) {
    cli::cli_abort("{.arg sampler} must be a sampler created by {.fn tp_sampler}.")
  }
  if (!.is_count(n_runs)) {
    cli::cli_abort("{.arg n_runs} must be a single positive whole number.")
  }
  if (!.is_count(n_processes)) {
    cli::cli_abort("{.arg n_processes} must be a single positive whole number.")
  }

  opts <- .list_to_options(tp_list(...))
  if (length(opts$compile) > 0) {
    cli::cli_abort(c(
      "Compile options can't be given to {.fn tp_run}: {.arg {names(opts$compile)}}.",
      "i" = "Pass them to {.fn tp_compile} instead."
    ))
  }

  dir <- tp_tempdir(dir)
  output_paths <- .output_paths(dir, out_file_name, n_runs)

  .run_sampler(
    exe_path = sampler$exe_path,
    data = data,
    args = .options_to_string(opts$runtime),
    output_paths = output_paths,
    n_processes = n_processes
  )

  model <- sampler$model
  method <- sampler$method

  if (verbose) {
    .print_run_summary(
      model = model,
      method = method,
      dir = dir,
      files = basename(output_paths),
      elapsed = difftime(Sys.time(), start)
    )
  }

  .parse_output(output_paths, model = model, method = method, dir = dir)
}


# Helper Functions ------------------------------------------------------------------

#' Model categories without an output parser
#'
#' @description
#' `.no_parser_categories` lists the model categories whose output can't be
#' parsed yet, because they return trees. This is a temporary solution until
#' parsers for these models are available.
#' @noRd
.no_parser_categories <- c(
  "host-repertoire-evolution",
  "tree-inference"
)

#' Check for a single positive whole number
#'
#' @param x Object to check.
#'
#' @return `TRUE` if `x` is a single, non-missing, positive whole number;
#'   `FALSE` otherwise.
#' @noRd
.is_count <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x) && x >= 1 && x == round(x)
}

#' Prepare the output files of a TreePPL run
#'
#' @description
#' `.output_paths()` deletes previous output files named after `out_file_name`
#' (`<name>.json` or `<name><number>.json`) in `dir`, and returns the paths of
#' the new output files: `<name>.json` for a single run, or `<name>1.json`, ...,
#' `<name><n_runs>.json` otherwise. Other files in `dir` are never touched.
#'
#' @param dir Path to the output directory, ending with a file separator (as
#'   returned by [tp_tempdir()]).
#' @param out_file_name Name of the output file, without extension.
#' @param n_runs Number of runs.
#'
#' @return A character vector of `n_runs` file paths.
#' @noRd
.output_paths <- function(dir, out_file_name, n_runs) {
  json_files <- list.files(dir, pattern = "\\.json$", full.names = TRUE)
  is_stale <- sub("[0-9]*\\.json$", "", basename(json_files)) == out_file_name
  file.remove(json_files[is_stale])

  run_ids <- if (n_runs == 1) "" else seq_len(n_runs)
  paste0(dir, out_file_name, run_ids, ".json")
}

#' Execute a compiled TreePPL sampler
#'
#' @description
#' `.run_sampler()` runs the executable once per output path, writing the
#' standard output of each run to its file. Several runs are executed in
#' parallel with [future::multisession]; the previous future plan is restored on
#' exit. `LD_LIBRARY_PATH` is emptied for these commands, as in `.tpplc()`.
#'
#' @param exe_path Path to the compiled executable.
#' @param data Path to the input data.
#' @param args String with the runtime options.
#' @param output_paths Paths of the output files, one per run.
#' @param n_processes Maximum number of parallel processes.
#'
#' @return The exit statuses of the runs (invisibly). Throws an error if any run
#'   failed.
#' @noRd
.run_sampler <- function(exe_path, data, args, output_paths, n_processes) {
  run_once <- function(output_path) {
    system2(
      command = exe_path,
      args = c(shQuote(data), args),
      env = "LD_LIBRARY_PATH=",
      stdout = output_path
    )
  }

  if (length(output_paths) == 1) {
    status <- run_once(output_paths)
  } else {
    old_plan <- future::plan(
      future::multisession,
      workers = min(n_processes, length(output_paths))
    )
    on.exit(future::plan(old_plan), add = TRUE)
    status <- future.apply::future_vapply(output_paths, run_once, integer(1))
  }

  failed <- basename(output_paths[status != 0L])
  if (length(failed) > 0) {
    cli::cli_abort("TreePPL run failed for output file{?s} {.file {failed}}.")
  }

  invisible(status)
}

#' Print the summary of a TreePPL run
#'
#' @param model The model used to compile the sampler.
#' @param method The inference method.
#' @param dir Path to the output directory.
#' @param files Names of the output files.
#' @param elapsed A `difftime` with the elapsed time.
#'
#' @return `NULL`, invisibly. Called for its side effect of printing to the
#'   console.
#' @noRd
.print_run_summary <- function(model, method, dir, files, elapsed) {
  cli::cli_h2("Analysis Summary")
  cli::cli_alert_success("Completed")
  cli::cli_dl(c(
    "Elapsed time" = format(round(elapsed, digits = 2)),
    "Model" = "{model}",
    "Method" = "{method}",
    "Output directory" = "{.path {dir}}",
    "Output file(s)" = "{.file {files}}"
  ))
}

#' Parse the output of a TreePPL run
#'
#' @description
#' `.parse_output()` parses the output files with [tp_parse_smc()] or
#' [tp_parse_mcmc()], depending on the inference method. If the model category
#' has no parser (see `.no_parser_categories`), or the method is neither SMC
#' nor MCMC, it informs where the output is and returns the file paths.
#'
#' @param files Paths of the output files.
#' @param model The model used to compile the sampler. Models that are not in
#'   [tp_model_library()] (e.g., custom code) are treated as parsable.
#' @param method The inference method.
#' @param dir Path to the output directory, shown in the message when no parser
#'   is available.
#'
#' @return A tidy data frame, or `files` if no parser is available.
#' @noRd
.parse_output <- function(files, model, method, dir) {
  library <- tp_model_library()
  category <- library$category[match(model, library$model_name)]

  if (!category %in% .no_parser_categories) {
    if (startsWith(method, "smc")) {
      return(tp_parse_smc(files))
    }
    if (startsWith(method, "mcmc")) {
      return(tp_parse_mcmc(files))
    }
  }

  cli::cli_inform(c(
    "No parser is available yet for this model with method {.val {method}}.",
    "i" = "The {cli::qty(length(files))}output file{?s} can be found in {.path {dir}}."
  ))
  files
}
