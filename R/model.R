# Exported Functions ----------------------------------------------------------------

#' TreePPL compiler options
#'
#' @description
#' `tp_treeppl_options()` reads the inference methods and options supported by
#' the TreePPL compiler (`tpplc --help`) and displays them in the console.
#' Inference methods are passed to the `method` argument of [tp_compile()],
#' compile options to the `...` of [tp_compile()], and runtime options to the
#' `...` of [tp_run()].
#'
#' @details
#' Options are shown in the form accepted by [tp_compile()] and [tp_run()],
#' with the type of value they expect. The type is translated from the
#' placeholder used in `tpplc --help`:
#' * `INT` or `N`: `<integer>`, e.g. `--subsample-size INT` is shown as
#'   `subsample_size = <integer>`;
#' * `FLOAT`: `<number>`;
#' * any other placeholder (`ARG`, `FILE`, `<option>`, ...): `<string>`;
#' * no placeholder (a flag without a value): `<logical>`, e.g.
#'   `subsample = <logical>`. Use `TRUE` to set the flag.
#'
#' @param what Which information to show: `"all"` (inference methods, compile
#'   options and runtime options), `"methods"`, `"compile"` or `"runtime"`.
#'
#' @return A data frame with columns `type` (`"methods"`, `"compile"` or
#'   `"runtime"`), `argument`, `value` and `description` (invisibly). For
#'   methods, `argument` is the method name (e.g., `"mcmc"`) and `value` is
#'   `NA`; for options, `argument` is the argument name (e.g.,
#'   `"subsample_size"`) and `value` the type of value expected: `"integer"`,
#'   `"number"`, `"string"` or `"logical"` (flags without a value).
#' @export
#'
#' @examples
#' \dontrun{
#' tp_treeppl_options("runtime")
#'
#' # the returned data frame can be searched
#' opts <- tp_treeppl_options("compile")
#' opts[grepl("resample", opts$argument), ]
#' }
tp_treeppl_options <- function(what = c("all", "methods", "compile", "runtime")) {
  what <- match.arg(what)
  type <- if (what == "all") names(.option_headers) else what
  opts <- .tpplc_options(type)

  # "--subsample-size INT" -> "subsample_size", "integer"
  is_option <- opts$type != "methods"
  opts$argument[is_option] <- gsub("-", "_", sub("^--", "", opts$argument[is_option]))
  opts$value <- ifelse(is_option, .value_type(opts$value), NA_character_)

  for (t in type) {
    section <- opts[opts$type == t, ]
    labels <- section$argument
    if (t != "methods") {
      labels <- paste0(labels, " = <", section$value, ">")
    }
    cli::cli_h1(.option_headers[[t]])
    cli::cli_dl(stats::setNames(section$description, cli::col_yellow(labels)))
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
#' `tp_sampler()` compiles a TreePPL model with the chosen inference method
#' and returns a sampler that can be run with [tp_run()]. The model is
#' compiled once; the sampler can then be run many times, with different data
#' or runtime options, without compiling again.
#'
#' @param model One of three options:
#'   * the path to a `.tppl` file with the TreePPL code, OR
#'   * the name of a model in the TreePPL model library (see
#'     [tp_model_library()]), e.g. `"crbd"`, OR
#'   * a string containing the entire TreePPL code.
#'
#'   These are tried in this order: an existing file first, then a library
#'   model, and otherwise the string is treated as code.
#' @param method Inference method to compile the model with. See
#'   `tp_treeppl_options("methods")` for all supported methods.
#' @param ... Compile options, as `name = value` pairs or a single list (see
#'   [tp_list()]). See `tp_treeppl_options("compile")` for all supported
#'   options. Names are written without `--`, and underscores are converted to
#'   hyphens (e.g., `incremental_printing = TRUE` becomes
#'   `--incremental-printing`). Runtime options are not allowed here; pass
#'   them to [tp_run()] instead.
#'
#' @return Invisibly, a list of class `tpSampler` with elements:
#'   * `exe_path`: path to the compiled sampler executable.
#'   * `model_path`: path to the `.tppl` file that was compiled.
#'   * `method`: the inference method.
#'   * `compile_opt`: the compile options used, as a named list.
#'
#' @seealso [tp_run()] to run the sampler, [tp_list()] to collect options,
#'   [tp_treeppl_options()] for the available methods and options.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Library model, SMC with aligned resampling and pruning
#' sampler <- tp_sampler(
#'   model = "crbd",
#'   method = "smc-bpf",
#'   resample = "align",
#'   prune = TRUE
#' )
#'
#' # Your own model file, default method (MCMC)
#' sampler <- tp_sampler(model = "path/to/my_model.tppl")
#' }
tp_sampler <- function(model, method = "mcmc", ...) {
  if (!is.character(model) || length(model) != 1) {
    cli::cli_abort(
      "{.arg model} must be a single string: a file path, model name or code."
    )
  }

  opts <- .list_to_options(c(list(method = method), tp_list(...)))
  if (length(opts$runtime) > 0) {
    cli::cli_abort(c(
      "Runtime options can't be given to {.fn tp_sampler}: {.arg {names(opts$runtime)}}.",
      "i" = "Pass them to {.fn tp_run} instead."
    ))
  }

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

  out <- structure(
    list(
      exe_path = .compilation(model_path, .options_to_string(opts$compile)),
      model = tools::file_path_sans_ext(basename(model)),
      model_path = model_path,
      method = method,
      compile_opt = opts$compile
    ),
    class = "tpSampler"
  )

  print(out)
  invisible(out)
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

#' Translate TreePPL value placeholders to R types
#'
#' @description
#' `.value_type()` translates the placeholders that `tpplc --help` uses for
#' option values to the R type expected by [tp_compile()] and [tp_run()]:
#' `INT` and `N` to `"integer"`, `FLOAT` to `"number"`, no placeholder (a flag
#' without a value) to `"logical"`, and any other placeholder (`ARG`, `FILE`,
#' `<option>`, ...) to `"string"`.
#'
#' @param placeholder Character vector of placeholders, `""` for none.
#'
#' @return A character vector of the same length as `placeholder`.
#' @noRd
.value_type <- function(placeholder) {
  numeric_types <- c(INT = "integer", N = "integer", FLOAT = "numeric")

  type <- unname(numeric_types[placeholder])
  type[is.na(type)] <- "character"
  type[placeholder == ""] <- "logical"
  type
}

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
#' @param stdout,stderr Passed to [base::system2()]; use `TRUE` to capture the
#'   output. When output is captured, a non-zero exit status is returned as the
#'   `status` attribute of the captured lines (and the [base::system2()] warning
#'   is suppressed).
#'
#' @return The captured output if `stdout` or `stderr` is `TRUE`, otherwise the
#'   exit status (invisibly).
#'
#' @noRd
.tpplc <- function(args, stdout = "", stderr = "") {
  suppressWarnings(system2(
    command = tp_installing_treeppl(),
    args = args,
    env = "LD_LIBRARY_PATH=",
    stdout = stdout,
    stderr = stderr
  ))
}

#' TreePPL inference methods and options as a data frame
#'
#' @description
#' `.tpplc_options()` reads `tpplc --help` and returns the inference methods,
#' compile options and/or runtime options that it lists.
#'
#' @param type One or more of `"methods"`, `"compile"` and `"runtime"`.
#'
#' @return A data frame with columns `type`, `argument`, `value` and
#'   `description`. For methods, `argument` is the method name (e.g., `"mcmc"`)
#'   and `value` is `""`; for options, `argument` is the flag (e.g., `"--drift"`)
#'   and `value` the kind of value it expects (e.g., `"FLOAT"`, or `""` for flags
#'   without a value). Positional arguments (`<program>`) are not included.
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
      section$value <- ""
    } else {
      # split the flag and its value: "--drift FLOAT" -> "--drift", "FLOAT"
      section$value <- trimws(sub("^\\S+", "", section$argument))
      section$argument <- sub(" .*", "", section$argument)
      # drop positional arguments such as <program>
      section <- section[startsWith(section$argument, "--"), ]
    }
    cbind(type = t, section[c("argument", "value", "description")])
  })

  do.call(rbind, sections)
}

#' Separate compile and runtime options
#'
#' @description
#' `.list_to_options()` splits a list of options in compile options (`method`
#' and the compile options listed by `tpplc`) and remaining options, which are
#' assumed to be runtime options. Names may use underscores or hyphens
#' (`no_early_stop` or `no-early-stop`) and are kept as given. If an option is
#' not valid, `tpplc` will complain.
#'
#' @param user_list A named list of options.
#'
#' @return A list with elements `compile` and `runtime`.
#' @noRd
.list_to_options <- function(user_list) {
  compile_flags <- .tpplc_options("compile")$argument
  user_flags <- gsub("_", "-", names(user_list))
  is_compile <- user_flags %in% c("method", sub("^--", "", compile_flags))

  list(compile = user_list[is_compile], runtime = user_list[!is_compile])
}

#' Convert options to command-line flags
#'
#' @description
#' `.options_to_string()` converts a named list of options to a string of
#' flags: `--name value` for most options, and `--name` for `TRUE` options.
#' `FALSE` and `NULL` options are dropped. Underscores in names are converted
#' to hyphens (e.g., `no_early_stop` becomes `--no-early-stop`).
#'
#' @param options A named list of options.
#'
#' @return A string.
#' @noRd
.options_to_string <- function(options) {
  names(options) <- gsub("_", "-", names(options))

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
#' @param call Environment used in the error message if compilation fails.
#'
#' @return The path to the compiled TreePPL program.
#' @noRd
.compilation <- function(path, args_str, call = rlang::caller_env()) {
  hash <- digest::digest(paste(path, args_str), algo = "sha256")
  output_path <- paste0(tp_tempdir(), hash, ".exe")

  output <- .tpplc(
    c(args_str, "--output", shQuote(output_path), shQuote(path)),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(output, "status")
  # the output is shown as-is, so braces must not be read as cli interpolation
  message <- gsub("}", "}}", gsub("{", "{{", output, fixed = TRUE), fixed = TRUE)
  names(message) <- rep(" ", length(message))

  if (!is.null(status) && status != 0L) {
    cli::cli_abort(c(
      "Compilation failed: {.code tpplc} exited with status {status}.",
      "i" = "Command: {.code tpplc {args_str}}",
      if (length(message) > 0) c("x" = "{.code tpplc} said:", message)
    ), call = call)
  }

  # compiler messages are captured above, so they are shown here instead
  if (length(message) > 0) {
    cli::cli_inform(message)
  }

  output_path
}


# Printing Methods ------------------------------------------------------------------

#' Print a TreePPL sampler
#'
#' Console method for objects returned by [tp_sampler()]. It shows the path to
#' the compiled executable, the model file, the inference method and, if any,
#' the compile options.
#'
#' @param x An object of class `tpSampler`.
#' @param ... Not used; present for compatibility with [print()].
#'
#' @return `x`, invisibly.
#'
#' @export
print.tpSampler <- function(x, ...) {
  cli::cli_h2("{cli::symbol$tick} TreePPL Sampler")
  cli::cli_text("Executable: {.path {x$exe_path}}")
  cli::cli_text("Model file: {.path {x$model_path}}")
  cli::cli_text("Method: {.val {x$method}}")

  if (length(x$compile_opt) > 0) {
    opts <- paste0(
      names(x$compile_opt), " = ",
      vapply(
        x$compile_opt,
        function(v) paste(format(v), collapse = ", "),
        character(1)
      )
    )
    cli::cli_text("Compile options: {.code {opts}}")
  }

  invisible(x)
}
