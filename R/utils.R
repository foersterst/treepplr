# Exported Functions ----------------------------------------------------------------

#' Platform-dependent TreePPL self-contained installation
#'
#' @description
#' `tp_installing_treeppl()` looks for the `tpplc` compiler associated with the
#' package and, if it is not found, downloads and unpacks it.
#'
#' @details
#' The search order is:
#' 1. the `TPPLC` environment variable;
#' 2. on Windows, a manual installation of `tpplc` (no self-contained build is
#'    available);
#' 3. an already unpacked copy in `/tmp`;
#' 4. a downloaded tarball in the `treeppl` directory next to the local
#'    treepplr installation (downloaded first if `download = TRUE`).
#'
#' @param download Logical. Download the associated `tpplc` version if it is not
#'   present locally.
#' @param keep_previous Logical. Keep previously downloaded `tpplc` versions
#'   instead of deleting them before the new download.
#'
#' @return The path to the TreePPL compiler.
#' @export
tp_installing_treeppl <- function(download = TRUE, keep_previous = FALSE) {
  tpplc_env <- Sys.getenv("TPPLC")
  if (tpplc_env != "") {
    return(tpplc_env)
  }

  # No self-contained build for Windows: tpplc must be installed manually
  if (Sys.info()[["sysname"]] == "Windows") {
    return("tpplc")
  }

  tpplc_path <- file.path("/tmp", paste0("treeppl-", TPPLC_VERSION), "tpplc")
  if (file.exists(tpplc_path)) {
    return(tpplc_path)
  }

  tarball <- list.files(.install_dir(), full.names = TRUE)
  if (length(tarball) == 0 && download) {
    tarball <- .fetch_treeppl(keep_previous)
  }

  if (length(tarball) > 0) {
    cli::cli_progress_step(
      "Initialising TreePPL",
      msg_done = "Ready!"
    )
    utils::untar(tarball[[1]], exdir = "/tmp", verbose = FALSE)
    cli::cli_progress_done()
  }

  tpplc_path
}

#' Temporary directory for running TreePPL
#'
#' @description
#' `tp_tempdir()` creates (if needed) and returns a normalized path to a
#' temporary directory where the executables can read and write files.
#'
#' @param temp_dir A path to be used, or `NULL` to use R's [base::tempdir()].
#' @param sub Optional name of a sub-directory inside `temp_dir`.
#'
#' @return A normalized path ending with the system-dependent file separator.
#' @export
tp_tempdir <- function(temp_dir = NULL, sub = NULL) {
  path <- if (is.null(temp_dir)) tempdir() else temp_dir
  if (!is.null(sub)) {
    path <- file.path(path, sub)
  }

  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  paste0(normalizePath(path), .Platform$file.sep)
}

#' Models implemented in treepplr
#'
#' @description
#' `tp_model_library()` lists all models in the TreePPL model library.
#'
#' @return A data frame of class `tp_model_library` with columns `category`
#'   and `model_name`, ordered by category and then by model name. Printing
#'   the object shows the models grouped by category. It can still be subset
#'   like a regular data frame, for example `tp_model_library()$model_name`.
#' @export
tp_model_library <- function() {
  paths <- .find("model")

  # Keep only models stored directly in a category: models/<category>/<model>
  paths <- paths[!grepl("models/[^/]+/[^/]+/[^/]+\\.tppl$", paths)]

  library <- data.frame(
    category = basename(dirname(paths)),
    model_name = tools::file_path_sans_ext(basename(paths))
  )
  library <- library[order(library$category, library$model_name), ]
  rownames(library) <- NULL
  class(library) <- c("tp_model_library", class(library))

  library
}

#' Create a flat list
#'
#' @description
#' `tp_list()` takes a variable number of arguments and returns a list.
#'
#' @param ... Either independent arguments (`name = value`), or a single
#'   structured list.
#'
#' @details
#' If a single list is supplied, it is returned as is, so that users can pass
#' either several arguments or one list of arguments.
#'
#' @return A list.
#' @export
tp_list <- function(...) {
  args <- list(...)

  if (length(args) == 1 && is.list(args[[1]])) {
    args <- args[[1]]
  }

  args
}


# Helper Functions ------------------------------------------------------------------

#' Directory holding the downloaded TreePPL tarball
#'
#' @description
#' `.install_dir()` builds the path of the directory, next to the local treepplr
#' installation, where the tarball of the associated `tpplc` version is stored.
#'
#' @return A path (character string).
#' @noRd
.install_dir <- function() {
  file.path(.libPaths()[[1]], "treeppl", TPPLC_VERSION)
}

#' Download the associated version of TreePPL
#'
#' @description
#' `.fetch_treeppl()` downloads the self-contained TreePPL release matching
#' `TPPLC_VERSION` for the current platform (Linux or macOS). Windows is not
#' supported and must be handled by the caller.
#'
#' @param keep_previous Logical. If `FALSE`, the `treeppl` directory is deleted
#'   before downloading, removing any other stored version.
#'
#' @return The path to the downloaded tarball (invisibly).
#' @noRd
.fetch_treeppl <- function(keep_previous = FALSE) {
  platform <- if (Sys.info()[["sysname"]] == "Linux") {
    "x86_64-linux"
  } else {
    "aarch64-darwin"
  }
  file_name <- paste0("treeppl-", TPPLC_VERSION, "-", platform, ".tar.gz")
  url <- paste0(
    "https://github.com/treeppl/treeppl/releases/download/v",
    TPPLC_VERSION, "/", file_name
  )

  install_dir <- .install_dir()
  if (!keep_previous) {
    unlink(dirname(install_dir), recursive = TRUE)
  }
  dir.create(install_dir, recursive = TRUE, showWarnings = FALSE)

  destfile <- file.path(install_dir, file_name)
  curl::curl_download(url, destfile = destfile, quiet = FALSE)

  invisible(destfile)
}

#' Find model or data files in the TreePPL model library
#'
#' @description
#' `.find()` locates `.tppl` model files or `.json` data files in the TreePPL
#' model library. The library is taken from the `MCORE_LIBS` environment
#' variable if set, otherwise from the self-contained TreePPL installation.
#'
#' @param type Either `"model"` (`.tppl` files) or `"data"` (`.json` files).
#' @param model_name Name of the model, without extension (treated as a regular
#'   expression). If `NULL`, all files of the given `type` are returned.
#'
#' @return A character vector of file paths.
#' @noRd
.find <- function(type = c("model", "data"), model_name = NULL) {
  type <- match.arg(type)
  ext <- if (type == "model") "tppl" else "json"
  name <- if (is.null(model_name)) ".*" else model_name
  pattern <- paste0("^", name, "\\.", ext, "$")

  # if MCORE_LIBS is set in ~/.Renviron
  mcore_libs <- Sys.getenv("MCORE_LIBS")
  if (grepl("treeppl=", mcore_libs)) {
    root <- sub(":.*$", "", sub(".*treeppl=", "", mcore_libs))
  } else {
    # root of the unpacked installation
    root <- dirname(tp_installing_treeppl())
    root <- list.files(root, pattern = "treeppl", full.names = TRUE)
    root <- paste0(root, "/lib/mcore/treeppl/models")
  }

  # search the whole root and keep files located inside a "models" directory
  paths <- list.files(
    path = root,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE
  )
  paths[grepl("/models/", paths, fixed = TRUE)]
}

#' Print the model library in the console
#'
#' Internal helper that prints the output of [tp_model_library()] grouped by
#' category, with each model name highlighted.
#'
#' @param x A data frame with columns `category` and `model_name`, as returned
#'   by [tp_model_library()].
#'
#' @return `NULL`, invisibly. Called for its side effect of printing to the
#'   console.
#'
#' @noRd
.print_model_library <- function(x) {
  cli::cli_div(theme = list(span.field = list(color = "green")))

  for (category in unique(x$category)) {
    models <- x$model_name[x$category == category]

    cli::cli_h2("{category}")
    cli::cli_ul()
    for (m in models) {
      cli::cli_li("{.field {m}}")
    }
    cli::cli_end()
  }

  cli::cli_end()
}

#' Print method for `tp_model_library` objects
#'
#' Prints the model library as a categorized, color-highlighted list instead
#' of a plain data frame.
#'
#' @param x An object of class `tp_model_library`.
#' @param ... Not used.
#'
#' @return `x`, invisibly.
#'
#' @export
#' @noRd
print.tp_model_library <- function(x, ...) {
  .print_model_library(x)
  invisible(x)
}

