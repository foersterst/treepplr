# Exported Functions ----------------------------------------------------------------

#' Parse the output of a TreePPL SMC or IS run
#'
#' @description
#' `tp_parse_smc()` reads the JSON output of a TreePPL run with a sequential
#' Monte Carlo method (e.g. `"smc-bpf"`, `"smc-apf"`) or with importance
#' sampling (`"is"`), and returns the samples, their weights and the
#' normalising constant of each sweep as a tidy data frame (tibble).
#'
#' @details
#' Each line of the output is one sweep, with the particles' `samples`, their
#' log `weights` and the sweep's log normalising constant (`normConst`).
#' Output written with `subsample = TRUE` has the same format, with fewer
#' particles per sweep. TreePPL writes NaN and infinite values as
#' `{"__float__": "-inf"}`; these are read as R's `NaN`, `Inf` and `-Inf`.
#'
#' Sweeps are skipped when they have no samples (every particle had zero
#' weight) or a normalising constant that is NaN or `-Inf`; a message reports
#' how many. Particles with a non-finite log weight are dropped. If no sweep
#' has usable samples, the function stops with an error: try more particles,
#' or check the input data and priors.
#'
#' The normalised weight of a particle is
#' `exp(log_weight + norm_constant - max(log_weight + norm_constant))`, taken
#' over all particles of all sweeps, so the heaviest particle gets weight 1.
#'
#' Parameters are named after the fields of the model's return type when the
#' output has them (e.g. `lambda`, `mu`), and `x1`, `x2`, ... otherwise.
#' Non-scalar values (vectors, trees) are kept as list-columns; for trees, use
#' [tp_summarise_trees()].
#'
#' @param json_path Path(s) to one or more JSON output files, as returned by
#'   [tp_run()]. Sweeps are numbered consecutively across files.
#'
#' @return A tibble (tidy data frame) with one row per particle and columns:
#'   * `sweep`: sweep number.
#'   * `particle`: particle number within its sweep.
#'   * `log_weight`: the particle's log weight.
#'   * `norm_constant`: the sweep's log normalising constant.
#'   * `norm_weight`: the normalised weight (see Details).
#'   * one column per parameter.
#'
#' @seealso [tp_run()], which calls this function for SMC and IS runs, and
#'   [tp_parse_mcmc()] for MCMC output.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' samples <- tp_parse_smc("out.json")
#' }
tp_parse_smc <- function(json_path) {
  lines <- .read_tppl_lines(json_path)

  sweeps <- lapply(seq_along(lines), function(i) {
    sweep <- jsonlite::parse_json(lines[i])
    norm_const <- .as_num(sweep$normConst)

    # Skip sweeps with no samples or a NaN / -Inf normalising constant
    if (length(sweep$samples) == 0 || length(norm_const) != 1 ||
        !is.finite(norm_const)) {
      return(NULL)
    }

    samples <- .samples_to_df(sweep$samples)
    log_weight <- .as_num(unlist(sweep$weights, use.names = FALSE))
    if (length(log_weight) != nrow(samples)) {
      cli::cli_abort(
        "Sweep {i}: {nrow(samples)} samples but {length(log_weight)} weights."
      )
    }

    c(
      list(
        sweep = rep(i, nrow(samples)),
        particle = seq_len(nrow(samples)),
        log_weight = log_weight,
        norm_constant = rep(norm_const, nrow(samples))
      ),
      samples
    )
  })

  kept <- !vapply(sweeps, is.null, logical(1))
  if (!any(kept)) {
    cli::cli_abort(c(
      "No sweep returned usable samples: every particle had zero weight.",
      "i" = "Try more particles, or check the input data and priors."
    ))
  }
  if (!all(kept)) {
    n_skipped <- sum(!kept)
    cli::cli_inform(c(
      "i" = "Skipped {n_skipped} of {length(lines)} sweep{?s} with no usable samples."
    ))
  }

  df <- .bind_columns(sweeps[kept])
  df <- df[is.finite(df$log_weight), , drop = FALSE]
  total <- df$log_weight + df$norm_constant
  df$norm_weight <- exp(total - max(total))

  id_cols <- c("sweep", "particle", "log_weight", "norm_constant", "norm_weight")
  df[c(id_cols, setdiff(names(df), id_cols))]
}

#' Parse the output of a TreePPL MCMC run
#'
#' @description
#' `tp_parse_mcmc()` reads the JSON output of a TreePPL run with a Markov chain
#' Monte Carlo method (e.g. `"mcmc"`, `"mcmc-naive"`) and returns the samples
#' as a tidy data frame (tibble), with one row per iteration.
#'
#' @details
#' Both TreePPL output formats are read, and the format is detected
#' automatically:
#' * by default, the file holds one line with all samples
#'   (`{"samples": [...], "weights": [...], "normConst": ...}`);
#' * with incremental printing (`incremental_printing = TRUE` in
#'   [tp_sampler()]), each sample is on its own line, followed by an empty
#'   summary line, which is ignored.
#'
#' TreePPL writes NaN and infinite values as `{"__float__": "nan"}`; these are
#' read as R's `NaN`, `Inf` and `-Inf`. MCMC weights are all equal, so they
#' are not returned.
#'
#' Parameters are named after the fields of the model's return type when the
#' output has them (e.g. `lambda`, `mu`), and `x1`, `x2`, ... otherwise.
#' Non-scalar values (vectors, trees) are kept as list-columns; for trees, use
#' [tp_summarise_trees()].
#'
#' Files without samples are skipped with a warning. If no file has samples,
#' the function stops with an error.
#'
#' @param json_path Path(s) to one or more JSON output files, as returned by
#'   [tp_run()]. Each file is one run.
#'
#' @return A tibble (tidy data frame) with one row per iteration and columns:
#'   * `run`: run number, in the order of `json_path`.
#'   * `iteration`: iteration number within its run.
#'   * one column per parameter.
#'
#' @seealso [tp_run()], which calls this function for MCMC runs, and
#'   [tp_parse_smc()] for SMC and IS output.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' samples <- tp_parse_mcmc(c("out1.json", "out2.json"))
#' }
tp_parse_mcmc <- function(json_path) {
  runs <- lapply(seq_along(json_path), function(run_id) {
    samples <- .read_mcmc_samples(json_path[run_id])

    if (length(samples) == 0) {
      cli::cli_warn(
        "No samples found in {.file {json_path[run_id]}}; skipping it."
      )
      return(NULL)
    }

    samples <- .samples_to_df(samples)
    c(
      list(
        run = rep(run_id, nrow(samples)),
        iteration = seq_len(nrow(samples))
      ),
      samples
    )
  })

  runs <- runs[!vapply(runs, is.null, logical(1))]
  if (length(runs) == 0) {
    cli::cli_abort("No run returned any samples.")
  }

  .bind_columns(runs)
}

#' Summarise the trees sampled in a TreePPL run
#'
#' @description
#' `tp_summarise_trees()` reads the trees from the JSON output of a TreePPL
#' tree-inference run (SMC, IS or MCMC) and summarises them with one tree: the
#' maximum a posteriori (MAP) topology or the maximum clade credibility (MCC)
#' tree. It can also return several trees to represent the uncertainty around
#' that summary.
#'
#' @details
#' **Weights.** Each sampled tree gets its posterior weight: for SMC and IS,
#' `exp(log_weight + norm_constant)` of its particle, normalised over all
#' particles of all sweeps; for MCMC, all iterations weigh the same. Sweeps
#' without samples, or with a normalising constant of `-Inf`, are skipped.
#'
#' **Clades and topologies.** Trees are treated as rooted. A clade is the set
#' of tips below an internal node; a topology is the set of clades of a tree.
#' * `method = "MAP"`: the score of a tree is the posterior probability of
#'   its topology (the summed weight of all sampled trees with that topology).
#'   The summary tree is the topology with the highest probability.
#' * `method = "MCC"`: the score of a tree is its clade credibility, the
#'   product of the posterior probabilities of its clades. The summary tree is
#'   the sampled topology with the highest clade credibility.
#'
#' **Node ages.** In a summary tree (and in the trees returned with
#' `random = FALSE`), each node's age is the weighted mean age of that clade:
#' over the sampled trees with the same topology (MAP), or over all sampled
#' trees that contain the clade (MCC). Branch lengths are differences of ages.
#' Trees returned with `random = TRUE` keep their own sampled branch lengths.
#'
#' **Tips.** TreePPL identifies tips by their `index`, the row of the
#' alignment they came from. Give the taxon names with `tip_labels`, in the
#' same order as the rows of the alignment (e.g. `rownames()` of the matrix
#' from [tp_dna_to_int()]); otherwise tips are labelled `"1"`, `"2"`, ...
#'
#' @param trees Path(s) to one or more JSON output files of a tree-inference
#'   model, as returned by [tp_run()].
#' @param method Summary method: `"MAP"` or `"MCC"` (see Details).
#' @param samples Number of trees to return to represent uncertainty. With
#'   `samples = 1` (default) only the summary tree is returned.
#' @param random Logical. How the `samples` trees are chosen: if `TRUE`
#'   (default), drawn at random from the posterior sample according to their
#'   weights; if `FALSE`, the `samples` best-scoring distinct topologies
#'   ranked right below the summary tree (which is not repeated).
#' @param tip_labels Optional character vector of taxon names, in the order
#'   of the alignment rows.
#'
#' @return A list with elements:
#'   * `tree`: the summary tree, a rooted `ape::phylo` object.
#'   * `tree_score`: its score, a number named after `method`
#'     (e.g. `c(MAP = 0.42)`).
#'   * `sample`: only if `samples > 1`, an `ape::multiPhylo` object with the
#'     `samples` trees.
#'   * `sample_scores`: only if `samples > 1`, a tibble with one row per tree
#'     in `sample`: `tree_id`, its position in `sample` (`sample[[tree_id]]`),
#'     and `score`, its score under `method`.
#'
#' @seealso [tp_run()] to produce the output, [tp_parse_smc()] and
#'   [tp_parse_mcmc()] for models whose output is not trees.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' aln <- ape::read.FASTA("alignment.fasta")
#'
#' res <- tp_summarise_trees(
#'   trees = "out.json",
#'   method = "MCC",
#'   samples = 100,
#'   tip_labels = names(aln)
#' )
#' plot(res$tree)
#' res$tree_score
#'
#' # The 5 most probable topologies after the MAP topology
#' top <- tp_summarise_trees("out.json", method = "MAP", samples = 5,
#'                           random = FALSE, tip_labels = names(aln))
#' top$sample_scores
#' }
tp_summarise_trees <- function(trees,
                               method = c("MAP", "MCC"),
                               samples = 1,
                               random = TRUE,
                               tip_labels = NULL) {
  method <- match.arg(method)
  .check_summary_args(samples, random)

  draws <- .read_tree_samples(trees)
  parts <- lapply(draws$trees, .tppl_tree_parts)
  weight <- exp(draws$log_weight - max(draws$log_weight))
  weight <- weight / sum(weight)
  labels <- .tip_labels(parts, tip_labels)

  scored <- .score_trees(parts, weight, method)
  build_summary <- function(i) {
    in_set <- if (method == "MAP") {
      scored$topology == scored$topology[i]
    } else {
      rep(TRUE, length(parts))
    }
    .summary_phylo(parts, i, in_set, weight, labels)
  }

  best <- scored$ranked[1]
  out <- list(
    tree = build_summary(best),
    tree_score = stats::setNames(scored$score[best], method)
  )

  if (samples > 1) {
    if (random) {
      idx <- sample.int(
        length(parts), samples,
        replace = samples > sum(weight > 0), prob = weight
      )
      sampled <- lapply(idx, function(i) .parts_to_phylo(parts[[i]], labels))
    } else {
      idx <- .top_topologies(scored$ranked, samples)
      sampled <- lapply(idx, build_summary)
    }
    if (length(idx) == 0) {
      return(out) # a single topology was sampled: nothing to rank below it
    }
    out$sample <- structure(sampled, class = "multiPhylo")
    out$sample_scores <- tibble::tibble(
      tree_id = seq_along(idx),
      score = scored$score[idx]
    )
  }

  out
}

# Helper Functions ------------------------------------------------------------------

#' Read the non-empty lines of TreePPL output files
#'
#' TreePPL writes NaN and infinities as `{"__float__": "-inf"}`. They are
#' turned into strings (`"-inf"`, `"inf"`, `"nan"`) that `as.numeric()`
#' understands, so that samples and weights simplify to plain vectors.
#' @noRd
.read_tppl_lines <- function(json_path) {
  missing <- json_path[!file.exists(json_path)]
  if (length(missing) > 0) {
    cli::cli_abort("Can't find {.file {missing}}.", call = parent.frame())
  }

  lines <- unlist(lapply(json_path, readLines, warn = FALSE), use.names = FALSE)
  # drop empty lines; only short lines can be whitespace-only, so only those
  # are checked with a regular expression (fast on very long lines)
  lines <- lines[nzchar(lines)]
  short <- nchar(lines, type = "bytes") < 100
  blank <- short & !grepl("\\S", lines)
  lines <- lines[!blank]

  # rewrite {"__float__": ...} only in lines that contain it
  has_float <- grepl("__float__", lines, fixed = TRUE)
  lines[has_float] <- gsub(
    '\\{\\s*"__float__"\\s*:\\s*"([^"]*)"\\s*\\}', '"\\1"',
    lines[has_float],
    perl = TRUE
  )
  lines
}

#' Read the samples of one MCMC output file, in either output format
#'
#' Default format: one summary line holding all samples. Incremental printing:
#' one sample per line, then an (empty) summary line, which is dropped.
#' @noRd
.read_mcmc_samples <- function(file) {
  lines <- .read_tppl_lines(file)
  is_summary <- grepl('^\\s*\\{\\s*"samples"\\s*:', lines)
  sample_lines <- lines[!is_summary]

  if (length(sample_lines) > 0) {
    # Parse all sample lines at once by wrapping them in one JSON array
    return(jsonlite::parse_json(
      paste0("[", paste(sample_lines, collapse = ","), "]")
    ))
  }
  if (any(is_summary)) {
    return(jsonlite::parse_json(lines[which(is_summary)[1]])$samples)
  }
  NULL
}

#' Turn parsed samples into a data frame of columns
#'
#' `samples` is a list as returned by jsonlite::parse_json(), one element per
#' sample. Simplifying it here is much faster than letting jsonlite do it.
#' Each sample is either a number (one parameter), an array of numbers
#' (several unnamed parameters: x1, x2, ...) or a record whose fields are under
#' `__data__` (named parameters). Fields that are not single numbers (vectors,
#' trees) become list-columns.
#' @noRd
.samples_to_df <- function(samples) {
  first <- samples[[1]]

  if (is.list(first) && !is.null(first[["__data__"]])) {
    fields <- names(first[["__data__"]])
    cols <- lapply(fields, function(field) {
      lapply(samples, function(s) s[["__data__"]][[field]])
    })
    names(cols) <- fields
  } else if (is.list(first) && is.null(names(first))) {
    n_par <- length(first)
    lengths_ok <- all(lengths(samples) == n_par)
    flat <- unlist(samples, use.names = FALSE)
    if (lengths_ok && length(flat) == n_par * length(samples)) {
      # every sample is an array of n_par scalars: fill a matrix by row
      mat <- matrix(flat, ncol = n_par, byrow = TRUE)
      cols <- lapply(seq_len(n_par), function(j) mat[, j])
    } else {
      cols <- lapply(seq_len(n_par), function(j) lapply(samples, `[[`, j))
    }
    names(cols) <- paste0("x", seq_len(n_par))
  } else {
    cols <- list(x1 = samples)
  }

  cols <- lapply(cols, .simplify_column)
  structure(cols, class = "data.frame", row.names = c(NA, -length(samples)))
}

#' Turn a list of single values into a numeric vector; keep anything else
#'
#' `"nan"`, `"inf"` and `"-inf"` strings (see .read_tppl_lines()) become
#' `NaN`, `Inf` and `-Inf`.
#' @noRd
.simplify_column <- function(col) {
  if (!is.list(col)) {
    return(if (is.character(col)) .as_num(col) else col)
  }
  if (all(lengths(col) == 1L) && !any(vapply(col, is.list, logical(1)))) {
    values <- unlist(col, use.names = FALSE)
    return(if (is.character(values)) .as_num(values) else values)
  }
  col
}

#' as.numeric() without warnings ("nan", "-inf" become NaN, -Inf)
#' @noRd
.as_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

#' Bind lists of equal-named columns into one tibble
#'
#' Faster than rbind() on many data frames: each column is combined once.
#' @noRd
.bind_columns <- function(parts) {
  cols <- names(parts[[1]])
  out <- lapply(cols, function(col) {
    pieces <- lapply(parts, `[[`, col)
    if (all(vapply(pieces, is.atomic, logical(1)))) {
      unlist(pieces, use.names = FALSE)
    } else {
      do.call(c, lapply(pieces, as.list))
    }
  })
  names(out) <- cols
  tibble::as_tibble(out)
}

#' Check the `samples` and `random` arguments of tp_summarise_trees()
#' @noRd
.check_summary_args <- function(samples, random, call = parent.frame()) {
  is_count <- is.numeric(samples) && length(samples) == 1 &&
    isTRUE(samples >= 1 && samples == round(samples))
  if (!is_count) {
    cli::cli_abort(
      "{.arg samples} must be a single positive whole number.", call = call
    )
  }
  if (!isTRUE(random) && !isFALSE(random)) {
    cli::cli_abort(
      "{.arg random} must be {.code TRUE} or {.code FALSE}.", call = call
    )
  }
}

#' Tip labels for the trees: `tip_labels`, or "1", "2", ... (the tip index)
#' @noRd
.tip_labels <- function(parts, tip_labels, call = parent.frame()) {
  n_tips <- unique(vapply(parts, `[[`, integer(1), "n_tips"))
  if (length(n_tips) != 1) {
    cli::cli_abort(
      "The sampled trees don't all have the same number of tips.", call = call
    )
  }
  if (is.null(tip_labels)) {
    return(as.character(seq_len(n_tips)))
  }
  if (length(tip_labels) != n_tips) {
    cli::cli_abort(c(
      "{.arg tip_labels} must have one name per tip.",
      "x" = paste(
        "The trees have {n_tips} tips;",
        "{.arg tip_labels} has {length(tip_labels)}."
      )
    ), call = call)
  }
  as.character(tip_labels)
}

#' Score every sampled tree and rank the distinct topologies
#'
#' MAP score: posterior probability of the tree's topology. MCC score: product
#' of the posterior probabilities of the tree's clades.
#'
#' @return A list with `score` (one per tree), `topology` (one key per tree)
#'   and `ranked` (one tree per distinct topology, best first; ties broken by
#'   the topology's posterior probability).
#' @noRd
.score_trees <- function(parts, weight, method) {
  clade_keys <- lapply(parts, `[[`, "clades")
  clade_tree <- rep(seq_along(parts), lengths(clade_keys))
  clade_prob <- tapply(
    weight[clade_tree], unlist(clade_keys, use.names = FALSE), sum
  )

  topology <- vapply(parts, `[[`, character(1), "topology")
  topo_prob <- tapply(weight, topology, sum)

  score <- if (method == "MAP") {
    unname(topo_prob[topology])
  } else {
    vapply(clade_keys, function(k) prod(clade_prob[k]), numeric(1))
  }

  ranked <- which(!duplicated(topology))
  ranked <- ranked[order(-score[ranked], -topo_prob[topology[ranked]])]

  list(score = score, topology = topology, ranked = ranked)
}

#' The `samples` distinct topologies ranked right below the summary tree
#'
#' `ranked[1]` is the summary tree itself, so it is skipped.
#' @noRd
.top_topologies <- function(ranked, samples) {
  below <- ranked[-1]
  if (samples > length(below)) {
    n <- length(below)
    cli::cli_inform(c(
      "i" = "Only {n} other distinct topolog{?y/ies} sampled; returning all of them."
    ))
  }
  utils::head(below, samples)
}

#' Read the sampled trees and their log weights from TreePPL output
#'
#' Fields holding arrays inside the trees (likelihood messages `msg`,
#' sequences `seq`, ...) are removed from the text before parsing: they make
#' up nearly all of the file but are not needed to rebuild the trees.
#'
#' @return A list with `trees` (one parsed tree per sample) and `log_weight`.
#' @noRd
.read_tree_samples <- function(trees) {
  lines <- .read_tppl_lines(trees)
  lines <- .strip_tree_arrays(lines)

  is_summary <- grepl('^\\s*\\{\\s*"samples"\\s*:', lines)
  out_trees <- list()
  out_lw <- numeric(0)

  # Default format (SMC, IS, MCMC): one line per sweep with samples + weights
  for (line in lines[is_summary]) {
    sweep <- jsonlite::parse_json(line)
    if (length(sweep$samples) == 0) next
    norm_const <- .as_num(sweep$normConst)
    if (length(norm_const) != 1 || identical(norm_const, -Inf)) next
    lw <- .as_num(unlist(sweep$weights, use.names = FALSE))
    # MCMC writes a NaN normalising constant: the weights alone count
    if (is.finite(norm_const)) lw <- lw + norm_const
    out_trees <- c(out_trees, sweep$samples)
    out_lw <- c(out_lw, lw)
  }

  # Incremental printing (MCMC): one tree per line, all with equal weight
  if (any(!is_summary)) {
    inc <- jsonlite::parse_json(
      paste0("[", paste(lines[!is_summary], collapse = ","), "]")
    )
    out_trees <- c(out_trees, inc)
    out_lw <- c(out_lw, rep(0, length(inc)))
  }

  keep <- is.finite(out_lw)
  if (!any(keep)) {
    cli::cli_abort(c(
      "No trees with a non-zero weight found in {.file {trees}}.",
      "i" = "Try more particles or iterations."
    ), call = parent.frame())
  }

  # A model may return an array of trees (e.g. MsgTree[]): use its one tree
  out_trees <- lapply(out_trees[keep], function(s) {
    if (is.null(s[["__constructor__"]]) && is.null(names(s))) s[[1]] else s
  })

  list(trees = out_trees, log_weight = out_lw[keep])
}

#' Remove array-valued fields (except samples and weights) from JSON lines
#'
#' A recursive regular expression matches a whole bracketed array, however
#' deeply nested. The field is removed together with one neighbouring comma,
#' so the JSON stays valid.
#' @noRd
.strip_tree_arrays <- function(lines) {
  key <- '"(?!samples"|weights")[^"]+"\\s*:\\s*'
  arr <- "(\\[(?:[^\\[\\]]++|(?-1))*\\])"
  lines <- gsub(paste0(",\\s*", key, arr), "", lines, perl = TRUE)
  gsub(paste0(key, arr, "\\s*,?"), "", lines, perl = TRUE)
}

#' Take a parsed TreePPL tree apart
#'
#' Walks the tree once (each node after its children) and records, with ape's
#' numbering (tips = their `index`, internal nodes from n_tips + 1, root
#' first):
#' * `edge`, `age`: the edge matrix and the age of every node;
#' * `clades`: for each internal node, its tips as a sorted string key;
#' * `topology`: all clade keys combined, identical for identical topologies.
#' @noRd
.tppl_tree_parts <- function(tree) {
  parent <- integer(0)
  child <- integer(0)
  node_age <- numeric(0)
  tip_age <- numeric(0)
  clade <- character(0)
  n_internal <- 0L

  # Returns the tips below `node`; internal ids are temporary (negative)
  walk <- function(node, parent_id) {
    data <- node[["__data__"]]

    if (is.null(data$left)) {
      if (is.null(data$index)) {
        cli::cli_abort(c(
          "Can't identify the tips: these trees' leaves have no {.field index}.",
          "i" = paste(
            "Use a model whose leaves keep their row in the alignment,",
            "e.g. {.val tree_inference_pruning}."
          )
        ), call = NULL)
      }
      tip <- as.integer(data$index)
      tip_age[tip] <<- data$age
      if (!is.na(parent_id)) {
        parent <<- c(parent, parent_id)
        child <<- c(child, tip)
      }
      return(tip)
    }

    n_internal <<- n_internal + 1L
    k <- n_internal # this node's number; n_internal grows in the recursion
    id <- -k
    node_age[k] <<- data$age
    if (!is.na(parent_id)) {
      parent <<- c(parent, parent_id)
      child <<- c(child, id)
    }

    tips <- c(walk(data$left, id), walk(data$right, id))
    clade[k] <<- paste(sort(tips), collapse = " ")
    tips
  }

  walk(tree, NA_integer_)

  n_tips <- length(tip_age)
  # internal node k (1 = root) becomes ape node n_tips + k
  to_ape <- function(x) ifelse(x < 0, n_tips - x, x)
  edge <- cbind(to_ape(parent), to_ape(child))

  list(
    n_tips = n_tips,
    edge = edge,
    age = c(tip_age, node_age),
    clades = clade,
    topology = paste(sort(clade), collapse = "|")
  )
}

#' Build an ape::phylo tree from tree parts, using their own node ages
#' @noRd
.parts_to_phylo <- function(parts, labels, age = parts$age) {
  edge <- parts$edge
  storage.mode(edge) <- "integer"
  structure(
    list(
      edge = edge,
      edge.length = age[edge[, 1]] - age[edge[, 2]],
      tip.label = labels,
      Nnode = length(parts$clades)
    ),
    class = "phylo",
    order = "cladewise"
  )
}

#' Build the summary tree for the topology of tree `i`
#'
#' Node ages are the weighted mean age of each clade over the trees in
#' `in_set`; tip ages are averaged the same way.
#' @noRd
.summary_phylo <- function(parts, i, in_set, weight, labels) {
  target <- parts[[i]]
  n_tips <- target$n_tips
  set <- which(in_set & weight > 0)
  if (length(set) == 0) set <- which(in_set)

  # weighted mean age of every clade over the trees in the set
  keys <- unlist(lapply(parts[set], `[[`, "clades"), use.names = FALSE)
  ages <- unlist(
    lapply(parts[set], function(p) p$age[(p$n_tips + 1):length(p$age)]),
    use.names = FALSE
  )
  w <- rep(weight[set], lengths(lapply(parts[set], `[[`, "clades")))
  mean_age <- tapply(w * ages, keys, sum) / tapply(w, keys, sum)

  tip_ages <- vapply(
    parts[set], function(p) p$age[seq_len(n_tips)], numeric(n_tips)
  )
  tip_ages <- if (is.matrix(tip_ages)) {
    drop(tip_ages %*% weight[set]) / sum(weight[set])
  } else {
    tip_ages
  }

  age <- c(tip_ages, unname(mean_age[target$clades]))
  .parts_to_phylo(target, labels, age)
}
