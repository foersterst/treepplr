#TODO : Automatic extraction from tpplc --help (sorted)
#The goal is to let the compiler do most of the work
#This list only exist to avoid to recompilate if not needed
tpplcCompileOptions <- c(
  "align",
  "cps",
  "drift",
  "dynamic-delay",
  "incremental-printing",
  "kernel",
  "method",
  "pigeons",
  "pigeons-no-global",
  "pigeons-explore-steps",
  "prune ",
  "resample",
  "static-delay"
)

#' Methods to [treepplr::tp_compile()]
#'
#' @returns  A data frame with methods that can be passed to [treepplr::tp_compile()]
#'
#' @export
tp_compile_methods <- function() {
  tpplc_path <- tp_installing_treeppl()
  # treeppl options
  cmd_opt <- system2(
    command = tpplc_path,
    args = "--help",
    env = "LD_LIBRARY_PATH= ",
    stdout = TRUE
  )

  # Preparing the output #

  # find the line between str_begin and str_end
  x <- which(cmd_opt == "Inference methods:")

  # extract everything after that line
  cmd_opt <- cmd_opt[(x + 1):length(cmd_opt)]
  cmd_opt <- trimws(cmd_opt)
  cmd_opt <- strsplit(cmd_opt, " ", perl = TRUE)


  opt_tab <- do.call(rbind, lapply(cmd_opt, function(x) {
    # if there is no description, make it NA
    data.frame(
      argument = x[2],
      description = trimws(paste(x[-2:-1], collapse = " ")),
      stringsAsFactors = FALSE
    )
  }))

  return(opt_tab)
}

#' Options to [treepplr::tp_compile()]
#'
#' @returns  A data frame with options that can be passed to [treepplr::tp_compile()]
#'
#' @export
tp_compile_options <- function() {
  tp_find_options("Compile options:", "Runtime options:")
}

#' Options to [treepplr::tp_run()]
#'
#' @returns  A data frame with options that can be passed to [treepplr::tp_run()]
#'
#' @export
tp_runtime_options <- function() {
  tp_find_options("Runtime options:", "Inference methods:")
}

tp_find_options <- function(str_begin, str_end) {
  tpplc_path <- tp_installing_treeppl()
  # treeppl options
  cmd_opt <- system2(
    command = tpplc_path,
    args = "--help",
    env = "LD_LIBRARY_PATH= ",
    stdout = TRUE
  )

  # Preparing the output #

  # find the line between str_begin and str_end
  x <- which(cmd_opt == str_begin)
  y <- which(cmd_opt == str_end)
  # extract everything after that line
  cmd_opt <- cmd_opt[(x + 1):(y - 2)]
  cmd_opt <- trimws(cmd_opt)
  cmd_opt <- strsplit(cmd_opt, " {2,}", perl = TRUE)

  opt_tab <- do.call(rbind, lapply(cmd_opt, function(x) {
    # if there is no description, make it NA
    if (length(x) == 1)
      x <- c(x, NA)
    data.frame(
      argument = x[1],
      description = x[2],
      stringsAsFactors = FALSE
    )
  }))

  # fix arguments (delete everything that comes after the first space)
  opt_tab$argument <- sub(" .*", "", opt_tab$argument)
  return(opt_tab)
}

#Separation between compile and over options (can be runtime or mistake,
#that the compiler to determine)
list_to_options <- function(user_list) {
  options <- list(compile = list(), runtime = list())
  if (length(user_list) != 0) {
    for (name in tpplcCompileOptions) {
      options[["compile"]][[name]] <- user_list[[name]]
    }
    for (name in names(user_list)) {
      if (is.null(options[["compile"]][[name]])) {
        options[["runtime"]][[name]] <- user_list[[name]]
      }
    }
  }
  options
}

#Convert options to a proper string of flags, e.g., `_` to `-`,
#adding `--` in the beginning, spaces between things, etc.
options_to_string <- function(options) {
  args_str <- c()
  if (length(options) != 0) {
    vec <- c()
    for (i in seq_along(options)) {
      if (!is.logical(options[[i]])) {
        str <- paste0("--", names(options[i]), " ", options[[i]])
      } else {
        if (options[[i]]) {
          str <- paste0("--", names(options[i]))
        }
      }
      vec <- c(vec, str)
    }
    args_str <- paste(vec, collapse = " ")
  }
  args_str
}

#' TreePPL model template
#'
#' @description
#' `sampler_T` template for TreePPL code carrying all the informations necessary
#' for compiling and running this model efficently

sampler_T <-
  methods::setRefClass(
    "sampler_T",
    fields = list(
      exe_path = "character",
      model_path = "character",
      compile_options = "list"
    )
  )

#' Compile a TreePPL model and create inference machinery
#'
#' @description
#' `compilation` compile a TreePPL model and create inference machinery to be
#' used by [treepplr::tp_run].
#'
#' @param path [base::character] to a treppl model
#' @param args_str [base::character] of options for treeppl compiler
#'
#' @return The path for the compiled TreePPL program.

compilation <- function(path, args_str) {
  options(scipen = 999)

  dir_path <- tp_tempdir()

  # output
  output_path <- paste0(dir_path, digest::digest(paste(path, args_str), "sha256"), ".exe")

  options <- paste(args_str, "--output", output_path)

  # Preparing the command line program
  tpplc_path <- tp_installing_treeppl()
  command <- paste(tpplc_path, options, path)

  # Compile program
  # Empty LD_LIBRARY_PATH from R_env for this command specifically
  # due to conflict with internal env from treeppl self-contained
  res <- system(paste0("LD_LIBRARY_PATH= ", command), intern = FALSE)
  if (res == 1L) {
    stop("Compilation failed")
  }
  return(output_path)
}

#' Write out a custom model to tp_tempdir()
#'
#' @param model A string containing the entire TreePPL code.
#' @param model_file_name An optional name to the file created
#'
#' @returns The path to the file created
#' @export
#'
tp_write_model <- function(model, model_file_name = "tmp_model_file") {
  path <- paste0(tp_tempdir(), model_file_name, ".tppl")
  cat(model, file = path)

  return(path)
}

#' Create a TreePPL model
#'
#' @description
#' `tp_compile` takes TreePPL model and create a sampler to be used by
#' [treepplr::tp_run()].
#'
#' @param model One of tree options:
#'   * The full path of the model file that contains the TreePPL code, OR
#'   * A string with the name of a model supported by treepplr
#' (see [treepplr::tp_model_library()]), OR
#'   * A string containing the entire TreePPL code.
#' @param method Inference method to be used. See [treepplr::tp_compile_methods()]
#' for all supported methods.
#' @param ... See [treepplr::tp_compile_options()] for all supported arguments.
#'
#' @return sampler from a sampler_T
#' @export

tp_compile <- function(model, method = "mcmc", ...) {
  if (!assertthat::is.string(model)) {
    stop("Input has to be a string.")
  }

  res <- try(file.exists(model), silent = TRUE)

  # If path exists, it's all good
  if (!is(res, "try-error") && res) {
    model_path <- model
    # If path doesn't exist
  } else {
    # It can be a model name in the library
    res_lib <- try(tp_find_model(model), silent = TRUE)
    # model has the name of a known model
    if (!is(res_lib, "try-error") && length(res_lib) != 0) {
      model_path <- res_lib
      # OR model contains the model
      #### (needs to be verified as an appropriate model later) ####
    } else {
      model_path <- tp_write_model(model)
    }
  }
  sampler <- methods::new("sampler_T", model_path = model_path)
  user_list <- append(list(method = method), tp_list(...))
  tmp <- list_to_options(user_list)

  sampler$compile_options <- c(tmp[["compile"]], "model" = model)
  full_options = append(tmp[["compile"]], tmp[["runtime"]])
  sampler$exe_path <- compilation(sampler$model_path, options_to_string(full_options))

  return(sampler)
}
