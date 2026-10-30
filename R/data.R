# Exported Functions -----------------------------------------------------------

#' Write the JSON input file for a TreePPL model
#'
#' @description
#' `tp_data()` writes the R objects in `data` to a JSON file that a TreePPL
#' sampler can read, and checks the file against the model's arguments.
#' Pass the result to [tp_run()].
#'
#' @details
#' Each element of `data` must be named exactly like an argument of the model;
#' the order does not matter. Use [tp_expected_input()] to see the names and
#' the R object each argument expects.
#'
#' Trees must be given as `tpPhylo` objects, built from a `phylo` tree with
#' [tp_phylo_to_tppl()]. A `phylo` object in `data` is not converted
#' automatically and stops `tp_data()` with an error.
#'
#' The file is written with the settings TreePPL needs: values of length 1
#' become plain JSON values, numbers keep full precision, and matrices are
#' written row by row.
#'
#' @param model Name of a model in the TreePPL model library (see
#'   [tp_model_library()]), e.g. `"crbd"`, or the path to a `.tppl` model
#'   file.
#' @param data A named list with one element per model argument.
#' @param filename Path of the JSON file to write. If `NULL` (default), the
#'   file is written to [tp_tempdir()] and named after the model and a hash of
#'   `data`, so the same data always gets the same file name and different data
#'   a different one.
#' @param check Logical. If `TRUE` (default), check the written file against
#'   the model with [tp_input_check()] and print the result.
#' @param print Logical. If `TRUE` (default), print a summary of the input
#'   file: its path, the model and the arguments it expects.
#'
#' @return Invisibly, a list of class `tpData` with elements:
#'   * `data_path`: path to the JSON file.
#'   * `model_name`: the model's name.
#'   * `expect_args`: the model's argument names, in signature order.
#'   * `expect_types`: the TreePPL type of each argument.
#'   * `expect_class`: the R object expected for each argument.
#'
#'   Typing the object at the console prints its summary again (see
#'   [print.tpData()]).
#'
#' @seealso [tp_expected_input()] for the arguments a model expects,
#'   [tp_phylo_to_tppl()] to build trees, [tp_run()] to run a sampler on the
#'   file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' phy <- ape::read.tree(text = "((A:4,B:4):6,(C:7,D:7):3);")
#'
#' # Diversification model: a tree and the sampling fraction
#' d <- tp_data(
#'   model = "crbd",
#'   data = list(tree = tp_phylo_to_tppl(phy), rho = 0.5)
#' )
#' d            # prints the summary again
#' d$data_path  # path to the JSON file
#'
#' # Write to a chosen file, without the check or the summary
#' d <- tp_data(
#'   model = "crbd",
#'   data = list(tree = tp_phylo_to_tppl(phy), rho = 0.5),
#'   filename = "crbd_input.json",
#'   check = FALSE,
#'   print = FALSE
#' )
#' }
tp_data <- function(model, data, filename = NULL, check = TRUE, print = TRUE) {
  if (!is.list(data) || (length(data) > 0 && is.null(names(data)))) {
    cli::cli_abort("{.arg data} must be a named list.")
  }

  # check if there are trees in the <phylo> format in the data
  is_phylo <- vapply(data, inherits, logical(1), what = "phylo")

  if (any(is_phylo)) {
    phylo_args <- names(data)[is_phylo]
    cli::cli_abort(c(
      "{.arg data} can't contain a {.cls phylo} object.",
      "x" = "{cli::qty(phylo_args)}Element{?s} {.val {phylo_args}} {?is a/are} {.cls phylo}.",
      "i" = "Convert it with {.fn tp_phylo_to_tppl} first."
    ))
  }

  # Collect expected arguments
  suppressMessages(
    model_specs <- tp_expected_input(model)
  )

  # File name: same data gets same name, different data gets different name
  if (is.null(filename)) {
    hash <- digest::digest(data, algo = "sha256")
    filename <- file.path(
      tp_tempdir(),
      paste0("input-", model, "-", substr(hash, 1, 7), ".json")
    )
  }

  # Write JSON file
  suppressMessages(tp_write_tppl_json(x = data, file = filename))

  # Output List
  out <- structure(
    list(
      data_path = filename,
      model_name = model_specs$model_name,
      expect_args = model_specs$expect_args,
      expect_types = model_specs$expect_types,
      expect_class = vapply(
        model_specs$expect_types,
        .tppl_type_to_r_class,
        character(1),
        USE.NAMES = FALSE
      )
    ),
    class = "tpData"
  )

  if (print) print(out)

  # Check
  if (check) {
    tp_input_check(json = filename, tppl = model_specs$model_file)
  }

  invisible(out)
}

#' List the input arguments a TreePPL model expects
#'
#' Scans a `.tppl` file, finds its `model function` signature and reports the
#' name of every input argument together with the R object you should supply
#' for it. The result is printed to the console and also returned, invisibly,
#' so it can be stored and reused (for example to check a data list before
#' writing it to JSON).
#'
#' TreePPL types are translated to R objects as follows:
#'
#' | TreePPL type                      | R object            |
#' |-----------------------------------|---------------------|
#' | `Real`                            | numeric             |
#' | `Int`                             | integer             |
#' | `Bool`                            | logical             |
#' | `String`                          | character           |
#' | `Real[]`, `Int[]`, ...            | numeric vector, ... |
#' | `Real[][]`, `Int[][]`, ...        | numeric matrix, ... |
#' | `Matrix[Real]`                    | matrix              |
#' | any recursive tree type           | phylo               |
#' | anything else (e.g. record types) | the TreePPL type    |
#'
#' A type counts as a tree when it is `ClockTree` (TreePPL's built-in tree),
#' an alias of a tree type, or a type defined in the model file (or in a file
#' it imports with a relative path) whose fields refer to the type itself,
#' e.g. `Node {left: Tree, right: Tree, age: Real}`.
#'
#' @param tppl Path to a `.tppl` model file.
#'
#' @return Invisibly, a list of class `tpEdata` with elements:
#'   * `model_file`: the path given in `tppl`.
#'   * `model_name`: the model's name, i.e. the file name without `.tppl`
#'     (`"qt"` for `qt.tppl`).
#'   * `model_function`: the name of the `model function` inside the file,
#'     which can differ from `model_name` (`"qtbirds"` in `qt.tppl`).
#'   * `expect_args`: character vector of argument names, in signature order.
#'   * `expect_types`: character vector of the TreePPL type of each argument.
#'   * `expect_class`: character vector of the matching R object.
#'
#'   Models without arguments return zero-length vectors.
#'
#' @seealso [print.tpEdata()] for the console output.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tp_expected_input(tppl = "models/diversification/clads2.tppl")
#'
#' k <- tp_expected_input(tppl = "models/diversification/clads2.tppl")
#' k$expect_args
#' #> [1] "tree" "rho"
#' }
tp_expected_input <- function(tppl) {
  # if model name rather than a path is supplied, look into the model library
  if (!file.exists(tppl)) {
    filename <- basename(tppl)
    tppl <- .find(type = "model", model_name = tppl)
  }
  if (length(tppl) == 0) {
    cli::cli_abort("Can't find the file {.file {filename}}.")
  }

  code <- .read_tppl_code(tppl)
  signature <- .parse_model_signature(code, tppl)

  out <- structure(
    list(
      model_file = tppl,
      model_name = tools::file_path_sans_ext(basename(tppl)),
      model_function = signature$name,
      expect_args = signature$args$name,
      expect_types = signature$args$type,
      expect_class = vapply(
        signature$args$type,
        .tppl_type_to_r_class,
        character(1),
        USE.NAMES = FALSE
      )
    ),
    class = "tpEdata"
  )

  print(out)
  invisible(out)
}

#' Write an R list to a TreePPL JSON input file
#'
#' Writes `x` with the JSON settings TreePPL needs: length-1 vectors become
#' plain values (`"rho": 0.5`, not `"rho": [0.5]`), numbers keep their full
#' precision, and the file is indented so it is easy to read.
#'
#' Each top-level element of `x` must be named exactly like an argument of
#' the model function (see [tp_expected_input()]). A vector of length 1 that
#' must stay an array, such as an `Int[]` with a single value, has to be
#' wrapped in [I()].
#'
#' @param x A named list, one element per model argument.
#' @param file Path of the JSON file to write.
#'
#' @return `file`, invisibly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' phy <- ape::read.tree(text = "((A:4,B:4):6,(C:7,D:7):3);")
#' tp_write_tppl_json(
#'   x = list(tree = tp_phylo_to_tppl(phy), rho = 0.5),
#'   file = "testdata_crbd.json"
#' )
#' }
tp_write_tppl_json <- function(x, file) {
  if (!is.list(x) || (length(x) > 0 && is.null(names(x)))) {
    cli::cli_abort("{.arg x} must be a named list.")
  }
  if (length(x) == 0) {
    x <- stats::setNames(list(), character(0)) # written as {} instead of []
  }

  jsonlite::write_json(
    x,
    path = file,
    auto_unbox = TRUE, # length-1 vectors -> plain values
    digits = NA, # full numeric precision
    pretty = TRUE # indented, readable file
  )
  cli::cli_alert_success("Wrote {.file {file}}")

  invisible(file)
}

#' Convert an `ape::phylo` tree into a TreePPL tree
#'
#' Builds the nested list that TreePPL reads as a binary tree:
#'
#' ```
#' {"__constructor__": "Node", "__data__": {"left": .., "right": .., "age": 4}}
#' {"__constructor__": "Leaf", "__data__": {"age": 0}}
#' ```
#'
#' Ages are times before the present: tips of an ultrametric tree get age 0
#' and the root gets the tree height. A node's `left` and `right` children are
#' taken in the order they appear in `phy$edge`.
#'
#' Extra fields, such as a leaf `index` or a node `label`, are added with
#' `leaf_extra` and `node_extra`. Each receives the ape node number (tips are
#' `1:ape::Ntip(phy)`, the root is `ape::Ntip(phy) + 1`) and must return a
#' named list.
#'
#' @param phy A rooted, binary tree of class `phylo` with branch lengths.
#' @param leaf_ctor Constructor name used for tips, e.g. `"Leaf"` or
#'   `"QTLeaf"`.
#' @param node_ctor Constructor name used for internal nodes, e.g. `"Node"` or
#'   `"QTNode"`.
#' @param leaf_extra A function of one tip number returning a named list of
#'   extra leaf fields.
#' @param node_extra A function of one internal node number returning a named
#'   list of extra node fields.
#'
#' @return A nested list of class `tpPhylo`, ready for [tp_data()] and
#'   [tp_write_tppl_json()]. The tip labels of `phy` are kept in the
#'   attribute `"tip.label"`, in ape's tip order; they are not written to
#'   JSON. See [print.tpPhylo()] for the console output.
#'
#' @export
#'
#' @examples
#' phy <- ape::read.tree(text = "((A:4,B:4):6,(C:7,D:7):3);")
#'
#' # ClockTree (diversification models)
#' tree <- tp_phylo_to_tppl(phy)
#' tree
#'
#' # TreeLabeled (host-repertoire models): a label on every leaf and node
#' tree_labeled <- tp_phylo_to_tppl(
#'   phy,
#'   leaf_extra = function(i) list(label = i),
#'   node_extra = function(i) list(label = i)
#' )
tp_phylo_to_tppl <- function(
  phy,
  leaf_ctor = "Leaf",
  node_ctor = "Node",
  leaf_extra = function(i) list(),
  node_extra = function(i) list()
) {
  if (!inherits(phy, "phylo")) {
    cli::cli_abort("{.arg phy} must be a {.cls phylo} object.")
  }
  if (!ape::is.rooted(phy)) {
    cli::cli_abort("{.arg phy} must be rooted; see {.fn ape::root}.")
  }
  if (!ape::is.binary(phy)) {
    cli::cli_abort("{.arg phy} must be binary; see {.fn ape::multi2di}.")
  }
  if (is.null(phy$edge.length)) {
    cli::cli_abort("{.arg phy} must have branch lengths.")
  }

  # Distance from the root to every node (tips first, then internal nodes).
  # Age = time before the present; round() removes floating-point noise.
  depth <- ape::node.depth.edgelength(phy)
  ages <- round(max(depth) - depth, 10)
  n_tips <- ape::Ntip(phy)

  # Build the list for one node; calls itself on the node's two children
  # until it reaches the tips.
  build <- function(node) {
    if (node <= n_tips) {
      data <- c(list(age = ages[node]), leaf_extra(node))
      return(list("__constructor__" = leaf_ctor, "__data__" = data))
    }

    children <- phy$edge[phy$edge[, 1] == node, 2]
    data <- c(
      list(
        left = build(children[1]),
        right = build(children[2]),
        age = ages[node]
      ),
      node_extra(node)
    )
    list("__constructor__" = node_ctor, "__data__" = data)
  }

  # The root is ape node number n_tips + 1. The class goes on the outer list
  # only; "list" second, so jsonlite still writes it as a plain JSON object.
  structure(
    build(n_tips + 1),
    class = c("tpPhylo", "list"),
    tip.label = phy$tip.label
  )
}

#' Convert a DNA alignment into a TreePPL integer matrix
#'
#' Recodes an alignment as integers, taxa in rows and sites in columns:
#' `A = 0`, `C = 1`, `G = 2`, `T = 3`. Every other symbol (gaps `-`, `?`, `N`
#' and IUPAC ambiguity codes) becomes `missing`. Pass the result to
#' [tp_write_tppl_json()] after [unname()] to get an `Int[][]`.
#'
#' The missing-data code depends on the model: the tree-inference models use
#' `4L` (the default) and the phenotype-molecular model (`qt.tppl`) uses
#' `-1L`.
#'
#' @param aln A DNA alignment of class `DNAbin`, as returned by
#'   [ape::read.FASTA()] or [ape::read.dna()]. All sequences must have the
#'   same length.
#' @param missing Integer code used for any symbol other than A, C, G or T.
#'
#' @return An integer matrix with taxon names as row names.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' aln <- ape::read.FASTA("alignment.fasta")
#' tp_dna_to_int(aln) # tree-inference models
#' tp_dna_to_int(aln, missing = -1L) # qt model
#' }
tp_dna_to_int <- function(aln, missing = 4L) {
  if (!inherits(aln, "DNAbin")) {
    cli::cli_abort("{.arg aln} must be a {.cls DNAbin} alignment.")
  }

  # Character matrix, taxa x sites ("a", "c", "-", ...)
  chars <- ape::as.character.DNAbin(ape::as.matrix.DNAbin(aln))

  codes <- c(A = 0L, C = 1L, G = 2L, T = 3L)
  out <- unname(codes[toupper(chars)]) # non-ACGT symbols become NA
  out[is.na(out)] <- as.integer(missing)
  dim(out) <- dim(chars)
  rownames(out) <- rownames(chars)

  out
}

#' Convert an R matrix into a TreePPL `Matrix[Real]`
#'
#' TreePPL stores a matrix as `{"m": rows, "n": columns, "arr": values}` with
#' the values listed row by row. R stores matrices column by column, so the
#' matrix is transposed before it is flattened.
#'
#' @param mat A numeric matrix.
#'
#' @return A list with elements `m`, `n` and `arr`, ready for
#'   [tp_write_tppl_json()].
#'
#' @export
#'
#' @examples
#' q <- matrix(c(-1, 1, 1, -1), nrow = 2)
#' tp_matrix_to_tppl(mat = q)
tp_matrix_to_tppl <- function(mat) {
  if (!is.matrix(mat) || !is.numeric(mat)) {
    cli::cli_abort("{.arg mat} must be a numeric matrix.")
  }

  list(m = nrow(mat), n = ncol(mat), arr = as.vector(t(mat)))
}

#' Check a JSON input file against a TreePPL model
#'
#' Compares the top-level fields of a JSON input file with the arguments of
#' the `model function` in a `.tppl` file, and reports the result in the
#' console. Use it before running a model to catch misspelled, missing or
#' extra inputs.
#'
#' The check covers names only: it does not test whether each value has the
#' right type. See [tp_expected_input()] for the R object each argument
#' expects.
#'
#' @param json Path to a JSON input file, e.g. one written by
#'   [tp_write_tppl_json()].
#' @param tppl Path to the `.tppl` model file.
#'
#' @return Invisibly, a list of class `tpIcheck` with elements:
#'   * `json_file`, `model_file`: the paths given in `json` and `tppl`.
#'   * `model_name`: the file name of `tppl` without `.tppl`.
#'   * `valid`: `TRUE` if the JSON fields and the model arguments match
#'     exactly, `FALSE` otherwise.
#'   * `missing`: model arguments that are not in the JSON file.
#'   * `extra`: JSON fields that the model does not take.
#'   * `expect_args`: all model arguments, in signature order.
#'   * `fields`: all top-level JSON fields.
#'
#' @seealso [print.tpIcheck()] for the console output, [tp_expected_input()],
#'   [tp_write_tppl_json()].
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tp_input_check(
#'   json = "json/testdata_qt.json",
#'   tppl = "models/pheno-mol/qt.tppl"
#' )
#'
#' # Stop a script if the input does not match the model
#' if (!tp_input_check("input.json", "qt.tppl")$valid) {
#'   stop("Fix the input file first.")
#' }
#' }
tp_input_check <- function(json, tppl) {
  .check_tppl_file(tppl)
  fields <- .read_json_fields(json)

  signature <- .parse_model_signature(.read_tppl_code(tppl), tppl)
  expect_args <- signature$args$name
  missing <- setdiff(expect_args, fields)
  extra <- setdiff(fields, expect_args)

  out <- structure(
    list(
      json_file = json,
      model_file = tppl,
      model_name = tools::file_path_sans_ext(basename(tppl)),
      valid = length(missing) == 0 && length(extra) == 0,
      missing = missing,
      extra = extra,
      expect_args = expect_args,
      fields = fields
    ),
    class = "tpIcheck"
  )

  print(out)
  invisible(out)
}


# Helper Functions ------------------------------------------------------------------

#' Stop unless `tppl` is a path to an existing file
#'
#' `call` makes the error point at the user's call, e.g.
#' `tp_expected_input()`, instead of at this helper.
#' @noRd
.check_tppl_file <- function(tppl, call = parent.frame()) {
  if (!is.character(tppl) || length(tppl) != 1 || is.na(tppl)) {
    cli::cli_abort("{.arg tppl} must be a single file path.", call = call)
  }
  if (!file.exists(tppl)) {
    cli::cli_abort("Can't find the model file {.file {tppl}}.", call = call)
  }
  invisible(tppl)
}

#' Read a `.tppl` file as one string, without comments
#'
#' Removes `/* ... */` block comments and `// ...` line comments so that
#' commented-out code is never parsed.
#' @noRd
.read_tppl_code <- function(tppl) {
  code <- paste(readLines(tppl, warn = FALSE), collapse = "\n")
  code <- gsub("(?s)/\\*.*?\\*/", " ", code, perl = TRUE) # (?s): span lines
  gsub("//[^\n]*", "", code, perl = TRUE)
}

#' Parse the `model function` signature of a TreePPL model
#'
#' @return A list with `name` (model function name) and `args`, a data frame
#'   with columns `name` and `type`.
#' @noRd
.parse_model_signature <- function(code, tppl) {
  pattern <- "model\\s+function\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*\\("
  hits <- gregexpr(pattern, code, perl = TRUE)[[1]]

  if (hits[1] == -1) {
    cli::cli_abort("No {.code model function} found in {.file {tppl}}.")
  }
  if (length(hits) > 1) {
    cli::cli_warn(
      "{.file {tppl}} has {length(hits)} model functions; using the first."
    )
  }

  header <- regmatches(code, regexpr(pattern, code, perl = TRUE))
  name <- sub(pattern, "\\1", header, perl = TRUE)

  # Text between the opening "(" and its matching ")"
  open_at <- hits[1] + attr(hits, "match.length")[1] - 1
  inside <- .extract_balanced(code, open_at)

  pieces <- .split_top_level(inside, sep = ",")
  pieces <- pieces[pieces != ""]

  arg_pattern <- "^([A-Za-z_][A-Za-z0-9_]*)\\s*:\\s*(.+)$"
  bad <- !grepl(arg_pattern, pieces, perl = TRUE)
  if (any(bad)) {
    cli::cli_abort(c(
      "Can't read the arguments of {.fn {name}} in {.file {tppl}}.",
      "x" = "Unexpected argument{?s}: {.code {pieces[bad]}}"
    ))
  }

  args <- data.frame(
    name = sub(arg_pattern, "\\1", pieces, perl = TRUE),
    type = gsub("\\s+", "", sub(arg_pattern, "\\2", pieces, perl = TRUE)),
    stringsAsFactors = FALSE
  )

  list(name = name, args = args)
}

#' Return the text between a bracket at position `open_at` and its partner
#' @noRd
.extract_balanced <- function(code, open_at) {
  chars <- strsplit(substring(code, open_at), "")[[1]]
  depth <- 0

  for (i in seq_along(chars)) {
    if (chars[i] == "(") depth <- depth + 1
    if (chars[i] == ")") depth <- depth - 1
    if (depth == 0) {
      if (i <= 2) {
        return("") # "()" -> a model without arguments
      }
      return(paste(chars[2:(i - 1)], collapse = ""))
    }
  }

  cli::cli_abort("The model signature has no closing {.code )}.")
}

#' Split a string at `sep`, ignoring separators inside (), [] or {}
#' @noRd
.split_top_level <- function(text, sep = ",") {
  chars <- strsplit(text, "")[[1]]
  depth <- 0
  pieces <- character(0)
  current <- ""

  for (ch in chars) {
    if (ch %in% c("(", "[", "{")) depth <- depth + 1
    if (ch %in% c(")", "]", "}")) depth <- depth - 1

    if (ch == sep && depth == 0) {
      pieces <- c(pieces, current)
      current <- ""
    } else {
      current <- paste0(current, ch)
    }
  }

  trimws(c(pieces, current))
}

#' Find the names of all tree types visible to a model file
#'
#' Looks in the model file and, recursively, in every file it imports with a
#' relative path (`import "lib/types.tppl"`). Standard-library imports
#' (`stdlib::...`) and `.mc` files are skipped. `ClockTree`, TreePPL's
#' built-in tree, is always included.
#' @noRd
.find_tree_types <- function(tppl) {
  files <- .collect_tppl_files(tppl)
  code <- vapply(files, .read_tppl_code, character(1))
  code <- paste(code, collapse = "\n")

  tree_types <- "ClockTree"

  # Recursive types: a field of type X inside the definition of X
  type_pattern <- "\\btype\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*="
  hits <- gregexpr(type_pattern, code, perl = TRUE)
  starts <- hits[[1]]
  if (starts[1] != -1) {
    type_names <- sub(
      type_pattern, "\\1", regmatches(code, hits)[[1]],
      perl = TRUE
    )
    ends <- c(starts[-1] - 1, nchar(code)) # each body runs to the next `type`
    for (i in seq_along(starts)) {
      body <- substr(code, starts[i], ends[i])
      self_ref <- paste0(":\\s*", type_names[i], "\\b")
      if (grepl(self_ref, body, perl = TRUE)) {
        tree_types <- c(tree_types, type_names[i])
      }
    }
  }

  # Aliases of tree types, e.g. `type alias Tree = ClockTree`
  alias_pattern <- paste0(
    "\\btype\\s+alias\\s+([A-Za-z_][A-Za-z0-9_]*)",
    "\\s*=\\s*([A-Za-z_][A-Za-z0-9_]*)"
  )
  aliases <- regmatches(code, gregexpr(alias_pattern, code, perl = TRUE))[[1]]
  for (alias in aliases) {
    from <- sub(alias_pattern, "\\1", alias, perl = TRUE)
    to <- sub(alias_pattern, "\\2", alias, perl = TRUE)
    if (to %in% tree_types) {
      tree_types <- c(tree_types, from)
    }
  }

  unique(tree_types)
}

#' List a `.tppl` file and every `.tppl` file it imports (relative paths)
#' @noRd
.collect_tppl_files <- function(tppl, seen = character(0)) {
  tppl <- normalizePath(tppl, mustWork = FALSE)
  if (tppl %in% seen || !file.exists(tppl)) {
    return(seen)
  }
  seen <- c(seen, tppl)

  code <- .read_tppl_code(tppl)
  imports <- regmatches(
    code,
    gregexpr('import\\s+"([^"]+)"', code, perl = TRUE)
  )[[1]]
  imports <- sub('import\\s+"([^"]+)"', "\\1", imports, perl = TRUE)
  imports <- imports[!grepl("::", imports) & grepl("\\.tppl$", imports)]

  for (path in file.path(dirname(tppl), imports)) {
    seen <- .collect_tppl_files(path, seen)
  }

  seen
}

#' Translate a TreePPL type into the R object a user should supply
#'
#' Tree types are recognised by name: the known ones (`ClockTree`, its alias
#' `Tree`, `TreeLabeled`, `GTR_Tree`, `QTTree`) and any other type whose name
#' contains "Tree". All of them map to "tpPhylo".
#' @noRd
.tppl_type_to_r_class <- function(type) {
  # Count trailing "[]" pairs: Real[][] -> base "Real", 2 dimensions
  n_dims <- 0
  base <- type
  while (grepl("\\[\\]$", base)) {
    base <- sub("\\[\\]$", "", base)
    n_dims <- n_dims + 1
  }

  atomic <- c(
    Real = "numeric",
    Int = "integer",
    Bool = "logical",
    String = "character"
  )

  if (base %in% names(atomic)) {
    r_base <- atomic[[base]]
    return(
      switch(as.character(min(n_dims, 2)),
        "0" = r_base,
        "1" = paste(r_base, "vector"),
        "2" = paste(r_base, "matrix")
      )
    )
  }

  if (grepl("^Matrix\\[", base)) {
    return(if (n_dims == 0) "matrix" else "list of matrices")
  }

  # Any TreePPL tree type: the user supplies a tree built with
  # tp_phylo_to_tppl() (class "tpPhylo")
  known_trees <- c(
    "Tree",
    "ClockTree",
    "TreeLabeled",
    "GTR_Tree",
    "QTTree"
  )

  if (base %in% known_trees || grepl("Tree", base, fixed = TRUE)) {
    return(if (n_dims == 0) "tpPhylo" else "list of tpPhylo")
  }

  # Records and other user-defined types: show the TreePPL type itself
  if (n_dims == 0) base else paste("list of", base)
}

#' Read the top-level field names of a JSON input file
#'
#' Stops with a clear message if the file is missing, is not valid JSON, or
#' holds an array instead of an object. `{}` and `[]` give zero fields.
#' @noRd
.read_json_fields <- function(json, call = parent.frame()) {
  if (!is.character(json) || length(json) != 1 || is.na(json)) {
    cli::cli_abort("{.arg json} must be a single file path.", call = call)
  }
  if (!file.exists(json)) {
    cli::cli_abort("Can't find the JSON file {.file {json}}.", call = call)
  }

  input <- tryCatch(
    jsonlite::read_json(json, simplifyVector = FALSE),
    error = function(e) {
      reason <- conditionMessage(e)
      cli::cli_abort(
        c("Can't read {.file {json}} as JSON.", "x" = "{reason}"),
        call = call
      )
    }
  )

  if (length(input) > 0 && is.null(names(input))) {
    cli::cli_abort(
      c(
        "{.file {json}} must contain a JSON object, {.code {{...}}}.",
        "x" = "It contains an array, {.code [...]}, instead."
      ),
      call = call
    )
  }

  if (is.null(names(input))) character(0) else names(input)
}

#' Summarise a TreePPL tree by walking it once
#'
#' Collects the number of tips and internal nodes, the root age, the range of
#' tip ages, the constructor names and the extra fields (everything in
#' `__data__` other than `age`, `left` and `right`).
#' @noRd
.tp_tree_summary <- function(tree) {
  tip_ages <- numeric(0)
  n_nodes <- 0
  leaf_ctor <- node_ctor <- character(0)
  leaf_fields <- node_fields <- character(0)

  walk <- function(node) {
    data <- node[["__data__"]]
    ctor <- node[["__constructor__"]]

    if (is.null(data$left) || is.null(data$right)) {
      tip_ages <<- c(tip_ages, data$age)
      leaf_ctor <<- union(leaf_ctor, ctor)
      leaf_fields <<- union(leaf_fields, setdiff(names(data), "age"))
      return(invisible())
    }

    n_nodes <<- n_nodes + 1
    node_ctor <<- union(node_ctor, ctor)
    node_fields <<- union(
      node_fields, setdiff(names(data), c("age", "left", "right"))
    )
    walk(data$left)
    walk(data$right)
  }

  walk(tree)

  list(
    n_tips = length(tip_ages),
    n_nodes = n_nodes,
    root_age = tree[["__data__"]]$age,
    min_tip_age = min(tip_ages),
    max_tip_age = max(tip_ages),
    leaf_ctor = leaf_ctor,
    node_ctor = node_ctor,
    leaf_fields = leaf_fields,
    node_fields = node_fields
  )
}

# Printing Methods ------------------------------------------------------------------

#' Print the expected input of a TreePPL model
#'
#' Console method for objects returned by [tp_expected_input()]. It shows the
#' model file, the model name and one line per argument, in green, with the R
#' object to supply in brackets.
#'
#' @param x An object of class `tpEdata`.
#' @param ... Not used; present for compatibility with [print()].
#'
#' @return `x`, invisibly.
#'
#' @export
print.tpEdata <- function(x, ...) {
  cli::cli_h2("Expected Input")
  cli::cli_text("Model file: {.val {x$model_file}}")
  cli::cli_text("Model name: {x$model_name}")

  if (length(x$expect_args) == 0) {
    cli::cli_text(
      "Expected arguments: none (write an empty JSON object, {.code {{}}})"
    )
    return(invisible(x))
  }

  cli::cli_text("Expected arguments:")
  lines <- cli::col_green(paste0(x$expect_args, " (", x$expect_class, ")"))
  cli::cli_ul()
  for (line in lines) {
    cli::cli_li("{line}")
  }
  cli::cli_end()

  invisible(x)
}

#' Print the result of a TreePPL input check
#'
#' Console method for objects returned by [tp_input_check()]. A match is
#' shown with a green tick; otherwise missing arguments are shown in red,
#' extra fields in yellow, and a pointer to [tp_expected_input()].
#'
#' @param x An object of class `tpIcheck`.
#' @param ... Not used; present for compatibility with [print()].
#'
#' @return `x`, invisibly.
#'
#' @export
print.tpIcheck <- function(x, ...) {
  cli::cli_h2("Input Check")
  cli::cli_text("JSON file: {.val {x$json_file}}")
  cli::cli_text("Model name: {x$model_name}")

  n_args <- length(x$expect_args)

  if (x$valid && n_args == 0) {
    cli::cli_alert_success("Model {.val {x$model_name}} takes no arguments.")
  } else if (x$valid) {
    cli::cli_alert_success(
      "All {n_args} argument{?s} found: {.arg {x$expect_args}}."
    )
  } else {
    if (length(x$missing) > 0) {
      cli::cli_alert_danger(
        "{cli::qty(x$missing)}Missing argument{?s}: {.arg {x$missing}}."
      )
    }
    if (length(x$extra) > 0) {
      cli::cli_alert_warning("Not used by the model: {.arg {x$extra}}.")
    }
    cli::cli_alert_info("See {.run tp_expected_input(\"{x$model_file}\")}.")
  }

  invisible(x)
}

#' Print the JSON input of a TreePPL model
#'
#' Console method for objects returned by [tp_data()]. It shows the path to
#' the JSON file, the model name and the arguments written to the file, with
#' the R object each one was built from.
#'
#' @param x An object of class `tpData`.
#' @param ... Not used; present for compatibility with [print()].
#'
#' @return `x`, invisibly.
#'
#' @export
print.tpData <- function(x, ...) {
  cli::cli_h2("Data Input")
  cli::cli_text("Data file: {.val {x$data_path}}")
  cli::cli_text("Model: {.val {x$model_name}}")

  if (length(x$expect_args) == 0) {
    cli::cli_text(
      "Expected arguments: none (write an empty JSON object, {.code {{}}})"
    )
    return(invisible(x))
  }

  lines <- cli::col_cyan(paste0(x$expect_args, " (", x$expect_class, ")"))
  cli::cli_text("Arguments: {lines}")
  cli::cli_end()

  invisible(x)
}

#' Print a TreePPL tree
#'
#' Console method for objects returned by [tp_phylo_to_tppl()]. Like ape's
#' `print.phylo()`, it gives the number of tips and internal nodes and the
#' root age.
#'
#' @param x An object of class `tpPhylo`.
#' @param ... Not used; present for compatibility with [print()].
#'
#' @return `x`, invisibly.
#'
#' @export
print.tpPhylo <- function(x, ...) {
  info <- .tp_tree_summary(x)
  root_age <- signif(info$root_age, 6)

  cli::cli_text(
    "Phylogenetic tree of class {.cls tpPhylo}, with {info$n_tips} tip{?s} ",
    "and {info$n_nodes} internal node{?s}."
  )
  cli::cli_text("")
  cli::cli_text("Rooted; includes branch lengths; root age = {root_age}")

  invisible(x)
}

