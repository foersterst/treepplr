# Exported Functions ----------------------------------------------------------------

#' TreePPL compiler options
#'
#' @description
#' `tp_treeppl_options()` reads the inference methods and options supported by
#' the TreePPL compiler (`tpplc --help`) and displays them in the console.
#' Inference methods are passed to the `method` argument of [tp_compile()],
#' compile options to the `...` of [tp_compile()], and runtime options to [tp_run()].
#'
#' @param what Which information to show: `"all"` (inference methods, compile
#'   options and runtime options), `"methods"`, `"compile"` or `"runtime"`.
#'
#' @return A data frame with columns `type` (`"methods"`, `"compile"` or
#'   `"runtime"`), `argument` and `description` (invisibly). For methods,
#'   `argument` is the method name (e.g., `"mcmc"`); for options, it is the flag
#'   (e.g., `"--particles"`).
#' @export
tp_treeppl_options <- function(what = c("all", "methods", "compile", "runtime")) {
  what <- match.arg(what)
  type <- if (what == "all") names(.option_headers) else what
  opts <- .tpplc_options(type)

  for (t in type) {
    section <- opts[opts$type == t, ]
    cli::cli_h1(.option_headers[[t]])
    cli::cli_dl(
      stats::setNames(section$description, cli::col_yellow(section$argument))
    )
  }

  invisible(opts)
}

#' Write a custom model to tp_tempdir()
#'
#' @description
#' `tp_write_model()` writes TreePPL code to a `.tppl` file in [tp_tempdir()].
#'
#' @param model A string containing the entire TreePPL code.
#' @param model_file_name Name of the file to create, without extension.
#'
#' @return The path to the file created.
#' @export
tp_write_model <- function(model, model_file_name = "tmp_model_file") {
  path <- paste0(tp_tempdir(), model_file_name, ".tppl")
  writeLines(model, path)

  path
}

#' Build a TreePPL sampler for a given model and inference method
#'
#' @description
#' `tp_compile()` compiles a TreePPL model and creates a sampler to be used by
#' [tp_run()].
#'
#' @param model One of three options:
#'   * The full path of a JSON file that contains the TreePPL code, OR
#'   * A string with the name of a model supported by treepplr
#'     (see [tp_model_library()]), OR
#'   * A string containing the entire TreePPL code.
#' @param method Inference method to be used. See
#'   `tp_treeppl_options("methods")` for all supported methods.
#' @param ... Compile and runtime options. See
#'   `tp_treeppl_options("compile")` and `tp_treeppl_options("runtime")` for all
#'   supported arguments. Names are written without `--`, and underscores are
#'   converted to hyphens (e.g., `no_early_stop = TRUE` becomes
#'   `--no-early-stop`).
#'
#' @return A TreePPL sampler object (class `tppls`).
#' @export
tp_compile <- function(model, method = "mcmc", ...) {
  if (!is.character(model) || length(model) != 1) {
    stop("Input has to be a string.")
  }

  user_list <- c(list(method = method), tp_list(...))
  names(user_list) <- gsub("_", "-", names(user_list))
  opts <- .list_to_options(user_list)

  if (file.exists(model)) {
    model_path <- normalizePath(model)
  } else {
    # `model` may be code, which is not a valid regular expression for .find()
    library_path <- tryCatch(
      .find(type = "model", model),
      error = function(e) character()
    )
    # otherwise `model` is the code itself (needs to be verified later)
    model_path <- if (length(library_path) > 0) {
      library_path[[1]]
    } else {
      tp_write_model(model)
    }
  }

  sampler <- methods::new("tppls", model_path = model_path)
  sampler$compile_options <- c(opts$compile, model = model)
  sampler$exe_path <- .compilation(
    sampler$model_path,
    .options_to_string(c(opts$compile, opts$runtime))
  )

  sampler
}


# Helper Functions ------------------------------------------------------------------

#' Section headers of the TreePPL help text
#'
#' @description
#' `.option_headers` maps each type of information to its section header in
#' `tpplc --help` (without the final colon), in the order of the help text.
#' @noRd
.option_headers <- c(
  methods = "Inference methods",
  compile = "Compile options",
  runtime = "Runtime options"
)

#' TreePPL sampler class
#'
#' @description
#' `.tppls` is the reference class that carries all the information
#' necessary for running a compiled TreePPL model with [tp_run()].
#'
#' @field exe_path Path to the compiled executable.
#' @field model_path Path to the TreePPL model file.
#' @field compile_options List of the options (and the model) used to compile.
#' @noRd
.tppls <- methods::setRefClass(
  "tppls",
  fields = list(
    exe_path = "character",
    model_path = "character",
    compile_options = "list"
  )
)

#' Run the TreePPL compiler
#'
#' @description
#' `.tpplc()` calls `tpplc` with the given arguments. `LD_LIBRARY_PATH` is
#' emptied for this command only, because R's value conflicts with the internal
#' environment of the self-contained TreePPL.
#'
#' @param args Character vector of arguments passed to `tpplc`.
#' @param stdout Passed to [base::system2()]; use `TRUE` to capture the output.
#'
#' @return The captured output if `stdout = TRUE`, otherwise the exit status
#' (invisibly).
#'
#' @noRd
.tpplc <- function(args, stdout = "") {
  system2(
    command = tp_installing_treeppl(),
    args = args,
    env = "LD_LIBRARY_PATH=",
    stdout = stdout
  )
}

#' TreePPL inference methods and options as a data frame
#'
#' @description
#' `.tpplc_options()` reads `tpplc --help` and returns the inference methods,
#' compile options and/or runtime options that it lists.
#'
#' @param type One or more of `"methods"`, `"compile"` and `"runtime"`.
#'
#' @return A data frame with columns `type`, `argument` and `description`. For
#'   methods, `argument` is the method name (e.g., `"mcmc"`); for options, it is
#'   the flag (e.g., `"--drift"`). Positional arguments (`<program>`) are not
#'   included.
#' @noRd
.tpplc_options <- function(type = names(.option_headers)) {
  type <- match.arg(type, several.ok = TRUE)
  help <- trimws(.tpplc("--help", stdout = TRUE))

  sections <- lapply(type, function(t) {
    header <- paste0(.option_headers[[t]], ":")
    if (!header %in% help) {
      stop("Section '", header, "' not found in the tpplc help.")
    }

    # lines between the section header and the next blank line
    lines <- help[-seq_len(match(header, help))]
    lines <- lines[seq_len(match("", lines, nomatch = length(lines) + 1) - 1)]

    # Descriptions are aligned in one column. The longest argument is separated
    # from its description by a single space, so the column is found from the
    # other lines, where the gap has two or more spaces.
    gap_end <- regexpr(" {2,}\\K", lines, perl = TRUE)
    description_col <- min(gap_end[gap_end > 0])
    section <- data.frame(
      argument = trimws(substr(lines, 1, description_col - 1)),
      description = substring(lines, description_col)
    )

    if (t == "methods") {
      # "--method mcmc" -> "mcmc"
      section$argument <- sub("^--method ", "", section$argument)
    } else {
      # keep only the flag, without its value: "--drift FLOAT" -> "--drift"
      section$argument <- sub(" .*", "", section$argument)
      # drop positional arguments such as <program>
      section <- section[startsWith(section$argument, "--"), ]
    }
    cbind(type = t, section)
  })

  do.call(rbind, sections)
}

#' Separate compile and runtime options
#'
#' @description
#' `.list_to_options()` splits a list of options in compile options (`method`
#' and the compile options listed by `tpplc`) and remaining options, which are
#' assumed to be runtime options. If an option is not valid, `tpplc` will
#' complain.
#'
#' @param user_list A named list of options.
#'
#' @return A list with elements `compile` and `runtime`.
#' @noRd
.list_to_options <- function(user_list) {
  compile_flags <- .tpplc_options("compile")$argument
  is_compile <- names(user_list) %in% c("method", sub("^--", "", compile_flags))

  list(compile = user_list[is_compile], runtime = user_list[!is_compile])
}

#' Convert options to command-line flags
#'
#' @description
#' `.options_to_string()` converts a named list of options to a string of
#' flags: `--name value` for most options, and `--name` for `TRUE` options.
#' `FALSE` and `NULL` options are dropped.
#'
#' @param options A named list of options.
#'
#' @return A string.
#' @noRd
.options_to_string <- function(options) {
  to_flag <- function(name, value) {
    if (is.null(value) || isFALSE(value)) {
      NA_character_
    } else if (isTRUE(value)) {
      paste0("--", name)
    } else {
      # no scientific notation (e.g., 1e+05), which tpplc cannot read
      value <- format(value, scientific = FALSE, trim = TRUE, digits = 15)
      paste0("--", name, " ", value)
    }
  }

  flags <- unlist(Map(to_flag, names(options), options))
  paste(flags[!is.na(flags)], collapse = " ")
}

#' Compile a TreePPL model
#'
#' @description
#' `.compilation()` compiles a TreePPL model with `tpplc`. The executable is
#' named after a hash of the model path and options, and stored in
#' [tp_tempdir()].
#'
#' @param path Path to a TreePPL model file.
#' @param args_str String with the options for the TreePPL compiler.
#'
#' @return The path to the compiled TreePPL program.
#' @noRd
.compilation <- function(path, args_str) {
  hash <- digest::digest(paste(path, args_str), algo = "sha256")
  output_path <- paste0(tp_tempdir(), hash, ".exe")

  status <- .tpplc(c(args_str, "--output", shQuote(output_path), shQuote(path)))
  if (status != 0L) {
    stop("Compilation failed")
  }

  output_path
}
