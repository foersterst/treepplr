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
#' output has them (e.g. `lambda`, `mu`). Otherwise, if `model` is given, they
#' are named after the values in the model's `return` statement: for
#' `return [lambda, mu, int2real(m)];` the columns are `lambda`, `mu` and `m`.
#' Without `model`, or when the names can't be worked out, they are `x1`,
#' `x2`, ...
#' Non-scalar values (vectors, trees) are kept as list-columns; for trees, use
#' [tp_summarise_trees()].
#'
#' @param json_path Path(s) to one or more JSON output files, as returned by
#'   [tp_run()]. Sweeps are numbered consecutively across files.
#' @param model Optional: the model that produced the output, as a model name
#'   from [tp_model_library()] (e.g. `"clads2"`) or the path to its `.tppl`
#'   file. Used to name the parameter columns (see Details).
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
#' samples <- tp_parse_smc("out.json", model = "clads2")
#' }
tp_parse_smc <- function(json_path, model = NULL) {
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
  df <- df[c(id_cols, setdiff(names(df), id_cols))]
  .name_parameters(df, model)
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
#' output has them (e.g. `lambda`, `mu`). Otherwise, if `model` is given, they
#' are named after the values in the model's `return` statement: for
#' `return [lambda, mu, int2real(m)];` the columns are `lambda`, `mu` and `m`.
#' Without `model`, or when the names can't be worked out, they are `x1`,
#' `x2`, ...
#' Non-scalar values (vectors, trees) are kept as list-columns; for trees, use
#' [tp_summarise_trees()].
#'
#' Files without samples are skipped with a warning. If no file has samples,
#' the function stops with an error.
#'
#' @param json_path Path(s) to one or more JSON output files, as returned by
#'   [tp_run()]. Each file is one run.
#' @param model Optional: the model that produced the output, as a model name
#'   from [tp_model_library()] (e.g. `"clads2"`) or the path to its `.tppl`
#'   file. Used to name the parameter columns (see Details).
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
#' samples <- tp_parse_mcmc(c("out1.json", "out2.json"), model = "clads2")
#' }
tp_parse_mcmc <- function(json_path, model = NULL) {
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

  .name_parameters(.bind_columns(runs), model)
}

#' Summarise the trees sampled in a TreePPL run
#'
#' @description
#' `tp_summarise_trees()` reads the trees from the JSON output of a TreePPL
#' tree-inference run (SMC, IS or MCMC) and summarises them with one tree: the
#' maximum a posteriori (MAP) topology or the maximum clade credibility (MCC)
#' tree. It also returns the support of each clade of that tree and the
#' uncertainty in its node ages, and it can return several trees to represent
#' the uncertainty around the summary.
#'
#' @details
#' **Weights.** Each sampled tree gets its posterior weight: for SMC and IS,
#' `exp(log_weight + norm_constant)` of its particle, normalised over all
#' particles of all sweeps; for MCMC, all iterations weigh the same. Sweeps
#' without samples, or with a normalising constant of `-Inf`, are skipped.
#'
#' **Clades and topologies.** Trees are treated as rooted. A clade is the set
#' of tips below an internal node; a topology is the set of clades of a tree.
#' The posterior probability of a clade is the summed weight of the sampled
#' trees that contain it.
#' * `method = "MAP"`: the score of a tree is the posterior probability of
#'   its topology (the summed weight of all sampled trees with that topology).
#'   The summary tree is the topology with the highest probability.
#' * `method = "MCC"`: the score of a tree is its clade credibility, the
#'   product of the posterior probabilities of its clades. The summary tree is
#'   the sampled topology with the highest clade credibility.
#'
#' **Node ages.** In a summary tree (and in the trees returned with
#' `random = FALSE`), each node's age is the weighted mean or median
#' (`heights`) age of that clade over the sampled trees with the same
#' topology (MAP), or over all sampled trees that contain the clade (MCC).
#' Branch lengths are differences of ages. Trees returned with
#' `random = TRUE` keep their own sampled branch lengths.
#'
#' With `method = "MAP"`, all node ages come from the same trees, so every
#' parent is at least as old as its children. With `method = "MCC"`, a parent
#' and its child are averaged over different sets of trees, and a child can
#' come out older than its parent, giving a negative branch length.
#' This is reported with a warning; it is most likely
#' when the clades involved have low support.
#'
#' **Uncertainty.** `node_summary` gives, for each internal node of the
#' summary tree, the posterior probability of its clade and the weighted mean,
#' median and 95% highest posterior density (HPD) interval of its age, over
#' the same trees used for the node ages. The HPD interval is the shortest
#' interval that holds 95% of the weight. The clade
#' probabilities are also stored as the node labels of `tree`, so
#' `plot(res$tree, show.node.label = TRUE)` shows them.
#'
#' **Tips.** TreePPL identifies tips by their `index`, the row of the
#' alignment they came from. Give the taxon names with `tip_labels`, in the
#' same order as the rows of the alignment (e.g. `rownames()` of the matrix
#' from [tp_dna_to_int()]); otherwise tips are labelled `"1"`, `"2"`, ...
#'
#' @param trees Path(s) to one or more JSON output files of a tree-inference
#'   model, as returned by [tp_run()].
#' @param method Summary method: `"MAP"` or `"MCC"` (see Details).
#' @param heights Node ages of the summary tree: the weighted `"mean"`
#'   (default) or `"median"` age of each clade (see Details).
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
#'   * `tree`: the summary tree, a rooted `ape::phylo` object, with the clade
#'     probabilities as node labels.
#'   * `tree_score`: its score, a number named after `method`
#'     (e.g. `c(MAP = 0.42)`).
#'   * `node_summary`: a tibble with one row per internal node of `tree`:
#'     `node` (its number in `tree`), `posterior` (probability of the clade),
#'     `age_mean`, `age_median`, `age_lower` and `age_upper` (95% HPD
#'     interval), and `n_trees` (number of sampled trees used for that node).
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
#'   heights = "median",
#'   samples = 100,
#'   tip_labels = names(aln)
#' )
#' plot(res$tree, show.node.label = TRUE)
#' res$tree_score
#' res$node_summary
#'
#' # The 5 most probable topologies after the MAP topology
#' top <- tp_summarise_trees("out.json",
#'   method = "MAP", samples = 5,
#'   random = FALSE, tip_labels = names(aln)
#' )
#' top$sample_scores
#' }
tp_summarise_trees <- function(
  trees,
  method = c("MAP", "MCC"),
  heights = c("mean", "median"),
  samples = 1,
  random = TRUE,
  tip_labels = NULL
) {
  method <- match.arg(method)
  heights <- match.arg(heights)
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
    .summary_phylo(
      parts, i, in_set, weight, labels, heights, scored$clade_prob
    )
  }

  best <- scored$ranked[1]
  summary <- build_summary(best)
  out <- list(
    tree = summary$tree,
    tree_score = stats::setNames(scored$score[best], method),
    node_summary = summary$nodes
  )
  built <- list(out$tree) # trees whose ages were summarised, to check below

  if (samples > 1) {
    if (random) {
      idx <- sample.int(
        length(parts), samples,
        replace = samples > sum(weight > 0), prob = weight
      )
      sampled <- lapply(idx, function(i) .parts_to_phylo(parts[[i]], labels))
    } else {
      idx <- .top_topologies(scored$ranked, samples)
      sampled <- lapply(idx, function(i) build_summary(i)$tree)
      built <- c(built, sampled)
    }
    if (length(idx) > 0) {
      out$sample <- structure(sampled, class = "multiPhylo")
      out$sample_scores <- tibble::tibble(
        tree_id = seq_along(idx),
        score = scored$score[idx]
      )
    }
  }

  .check_branch_lengths(built)
  out
}

#' Parse the output of a TreePPL host-repertoire run
#'
#' @description
#' `tp_parse_hrm()` reads the JSON output of a host-repertoire model (e.g.
#' `"flat-root-prior-HRM"`, `"subroot-HRM"`) and splits each sample into its
#' model parameters, the host repertoire at every node of the symbiont tree,
#' and the host gains and losses along every branch.
#'
#' @details
#' Each sample of a host-repertoire run holds the model parameters and the
#' symbiont tree with a sampled history: the repertoire (one state per host)
#' at each node, and the events that changed it along the branch above.
#' Host states are `0` (non-host), `1` (potential host) and `2` (actual
#' host); in the two-state version of the model only `0` and `2`, or `0` and
#' `1`, occur.
#'
#' The relative transition rates (`lambda`) are named after the transition
#' they stand for, `lambda_<from><to>`: with three states `lambda_01`,
#' `lambda_10`, `lambda_12` and `lambda_21`; with two states `lambda_01` and
#' `lambda_10` (or `lambda_02` and `lambda_20` when the states are `0` and
#' `2`).
#'
#' The three tables are linked by `sample`, the row number in `parameters`.
#' Nodes are identified by their `label` in the input tree (tips `1, 2, ...`
#' in the order of the rows of `interactions`), which is also how they are
#' labelled in `tree`. Hosts are numbered in the order of the columns of
#' `interactions`.
#'
#' Sweeps, particles and weights are handled as in [tp_parse_smc()] (SMC and
#' IS) and [tp_parse_mcmc()] (MCMC), including skipped sweeps and particles
#' with zero weight.
#'
#' @param json_path Path(s) to one or more JSON output files, as returned by
#'   [tp_run()].
#' @param method Inference method of the run: `"smc"`, `"is"` or `"mcmc"`.
#'
#' @return A list with elements:
#'   * `parameters`: a tibble with one row per sample: `sample`, then
#'     `sweep`, `particle`, `log_weight`, `norm_constant` and `norm_weight`
#'     (SMC and IS) or `run` and `iteration` (MCMC), then the `lambda_*` rates,
#'     `mu` and `beta`.
#'   * `node_repertoires`: a tibble with one row per sample, node and host:
#'     `sample`, `node`, `host`, `state`.
#'   * `events`: a tibble with one row per event: `sample`, `node` (the node
#'     at the end of the branch where it happened), `host`, `from_state`,
#'     `to_state` and `time` (the event's age, in the units of the tree).
#'   * `tree`: the symbiont tree as an `ape::phylo` object, with the input
#'     labels as tip and node labels.
#'
#' @seealso [tp_parse_smc()] and [tp_parse_mcmc()] for other models.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' hrm <- tp_parse_hrm("out.json", method = "smc")
#' hrm$parameters
#'
#' # Posterior probability that each host is an actual host at each node
#' w <- hrm$parameters$norm_weight / sum(hrm$parameters$norm_weight)
#' reps <- hrm$node_repertoires
#' reps$w <- w[reps$sample]
#' aggregate(w ~ node + host, data = reps[reps$state == 2, ], FUN = sum)
#' }
tp_parse_hrm <- function(json_path, method = c("smc", "is", "mcmc")) {
  method <- match.arg(method)

  # Reuse the general parsers: they handle the output formats, weights and
  # skipped sweeps. Records give named columns: lambda, mu, beta and tree
  # (lambda and tree as list-columns).
  params <- if (method == "mcmc") {
    tp_parse_mcmc(json_path)
  } else {
    tp_parse_smc(json_path)
  }
  if (!all(c("lambda", "tree") %in% names(params))) {
    cli::cli_abort(
      "{.file {json_path}} doesn't look like host-repertoire output."
    )
  }

  # Walk every sampled tree once, collecting nodes and events
  walks <- lapply(params$tree, .hrm_walk)
  node_repertoires <- .hrm_bind(walks, "nodes")
  events <- .hrm_bind(walks, "events")

  # Spread lambda into one named column per transition
  lambda <- do.call(rbind, lapply(params$lambda, unlist))
  colnames(lambda) <- .lambda_names(ncol(lambda), unique(node_repertoires$state))

  id_cols <- setdiff(names(params), c("lambda", "tree", "mu", "beta"))
  parameters <- tibble::as_tibble(cbind(
    sample = seq_len(nrow(params)),
    as.data.frame(params[id_cols]),
    as.data.frame(lambda),
    as.data.frame(params[intersect(c("mu", "beta"), names(params))])
  ))

  list(
    parameters = parameters,
    node_repertoires = node_repertoires,
    events = events,
    tree = .hrm_phylo(walks[[1]])
  )
}


# Helper Functions ------------------------------------------------------------------

#' Rename the x1, x2, ... columns after the values the model returns
#'
#' Only unnamed parameter columns (`x1`, `x2`, ...) are renamed, and only when
#' the model's `return` statement gives exactly one name per column; otherwise
#' the columns are kept as they are, with a message saying why.
#' @noRd
.name_parameters <- function(df, model) {
  if (is.null(model)) {
    return(df)
  }

  x_cols <- grep("^x[0-9]+$", names(df), value = TRUE)
  if (length(x_cols) == 0) {
    return(df) # parameters are already named (records)
  }

  tppl <- .model_path(model)
  if (is.null(tppl)) {
    cli::cli_inform(c(
      "i" = "Can't find model {.val {model}}; keeping columns {.field {x_cols}}."
    ))
    return(df)
  }

  par_names <- .model_return_names(tppl)
  if (is.null(par_names) || length(par_names) != length(x_cols)) {
    cli::cli_inform(c(
      "i" = paste(
        "Can't name the parameters from the {.code return} statement of",
        "{.file {tppl}}; keeping columns {.field {x_cols}}."
      )
    ))
    return(df)
  }

  names(df)[match(x_cols, names(df))] <- par_names
  df
}

#' Path to a model's .tppl file, from a path or a model-library name
#' @noRd
.model_path <- function(model) {
  if (is.character(model) && length(model) == 1 && file.exists(model)) {
    return(model)
  }
  path <- tryCatch(
    .find(type = "model", model_name = model),
    error = function(e) character(0)
  )
  if (length(path) == 0) NULL else path[[1]]
}

#' Names of the values a TreePPL model returns
#'
#' Reads the `return` statement of the `model function` in a `.tppl` file and
#' turns each returned value into a name:
#' * `return [lambda, mu, int2real(m)];` gives `lambda`, `mu`, `m`;
#' * `return p;` or `return(p);` gives `p`;
#' * a field access such as `gdist.alpha` is kept as `gdist.alpha`;
#' * a function call with one plain argument, such as `int2real(m)`, is
#'   named after its argument; any other expression gets `x<i>`.
#'
#' `return` statements inside functions defined in the model body are
#' ignored. Records (`return ReturnType { ... }`) return `NULL`: their output
#' already carries field names.
#'
#' @return A character vector of names, or `NULL` when the names can't be
#'   worked out (no return found, records, or different `return` statements
#'   that don't agree).
#' @noRd
.model_return_names <- function(tppl) {
  code <- .read_tppl_code(tppl)
  body <- .model_body(code)
  if (is.null(body)) {
    return(NULL)
  }

  exprs <- .return_expressions(.drop_inner_functions(body))
  if (length(exprs) == 0) {
    return(NULL)
  }

  name_sets <- lapply(exprs, .names_from_return)
  if (any(vapply(name_sets, is.null, logical(1)))) {
    return(NULL)
  }
  if (length(unique(name_sets)) > 1) {
    return(NULL) # several returns that name their values differently
  }

  make.unique(name_sets[[1]], sep = "_")
}

#' Text of the `model function` body, between its `{` and matching `}`
#' @noRd
.model_body <- function(code) {
  pattern <- "model\\s+function\\s+[A-Za-z_][A-Za-z0-9_]*\\s*\\("
  start <- regexpr(pattern, code, perl = TRUE)
  if (start == -1) {
    return(NULL)
  }

  # skip the argument list, then take the first block after it
  open_args <- start + attr(start, "match.length") - 1
  args <- .extract_balanced(code, open_args)
  after_args <- open_args + nchar(args) + 2
  open_body <- regexpr("{", substring(code, after_args), fixed = TRUE)
  if (open_body == -1) {
    return(NULL)
  }
  .extract_balanced(code, after_args + open_body - 1, open = "{", close = "}")
}

#' Remove the bodies of functions defined inside a model body
#'
#' Anonymous functions (`function(x: Int) => Real { ... }`) have their own
#' `return` statements, which are not the model's return value.
#' @noRd
.drop_inner_functions <- function(body) {
  repeat {
    at <- regexpr("\\bfunction\\s*\\(", body, perl = TRUE)
    if (at == -1) {
      return(body)
    }
    open <- regexpr("{", substring(body, at), fixed = TRUE)
    if (open == -1) {
      return(body)
    }
    open <- at + open - 1
    inner <- .extract_balanced(body, open, open = "{", close = "}")
    body <- paste0(
      substr(body, 1, at - 1), "NULL",
      substring(body, open + nchar(inner) + 2)
    )
  }
}

#' The expressions of all `return` statements in a (function-free) body
#' @noRd
.return_expressions <- function(body) {
  hits <- gregexpr("\\breturn\\b", body, perl = TRUE)[[1]]
  if (hits[1] == -1) {
    return(character(0))
  }
  vapply(hits, function(at) {
    rest <- strsplit(substring(body, at + 6), "")[[1]]
    # the expression runs to the first ";" outside brackets
    depth <- cumsum(rest %in% c("(", "[", "{")) -
      cumsum(rest %in% c(")", "]", "}"))
    end <- which(rest == ";" & depth == 0)[1]
    if (is.na(end)) end <- length(rest) + 1
    trimws(paste(rest[seq_len(end - 1)], collapse = ""))
  }, character(1))
}

#' Names for the values of one return expression, or NULL
#' @noRd
.names_from_return <- function(expr) {
  expr <- trimws(expr)

  # strip redundant outer parentheses: return(p) -> p
  while (startsWith(expr, "(") && endsWith(expr, ")") &&
    nchar(.extract_balanced(expr, 1)) == nchar(expr) - 2) {
    expr <- trimws(substr(expr, 2, nchar(expr) - 1))
  }

  # a record such as ReturnType { ... }: its output is already named
  if (grepl("^[A-Za-z_][A-Za-z0-9_]*\\s*\\{", expr)) {
    return(NULL)
  }

  items <- if (startsWith(expr, "[") && endsWith(expr, "]")) {
    .split_top_level(substr(expr, 2, nchar(expr) - 1), sep = ",")
  } else {
    expr
  }
  items <- items[items != ""]
  if (length(items) == 0) {
    return(NULL)
  }

  vapply(seq_along(items), function(i) .value_name(items[i], i), character(1))
}

#' A name for one returned value: the variable, the argument of a one-argument
#' call, or `x<i>` for anything else
#' @noRd
.value_name <- function(item, i) {
  ident <- "[A-Za-z_][A-Za-z0-9_]*(\\.[A-Za-z_][A-Za-z0-9_]*)*"
  item <- trimws(item)

  if (grepl(paste0("^", ident, "$"), item, perl = TRUE)) {
    return(item)
  }
  call <- paste0("^[A-Za-z_][A-Za-z0-9_]*\\s*\\(\\s*(", ident, ")\\s*\\)$")
  if (grepl(call, item, perl = TRUE)) {
    return(sub(call, "\\1", item, perl = TRUE))
  }
  paste0("x", i)
}

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
      "{.arg samples} must be a single positive whole number.",
      call = call
    )
  }
  if (!isTRUE(random) && !isFALSE(random)) {
    cli::cli_abort(
      "{.arg random} must be {.code TRUE} or {.code FALSE}.",
      call = call
    )
  }
}

#' Tip labels for the trees: `tip_labels`, or "1", "2", ... (the tip index)
#' @noRd
.tip_labels <- function(parts, tip_labels, call = parent.frame()) {
  n_tips <- unique(vapply(parts, `[[`, integer(1), "n_tips"))
  if (length(n_tips) != 1) {
    cli::cli_abort(
      "The sampled trees don't all have the same number of tips.",
      call = call
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
#' @return A list with `score` (one per tree), `topology` (one key per tree),
#'   `clade_prob` (posterior probability of every clade, by key) and
#'   `ranked` (one tree per distinct topology, best first; ties broken by
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

  list(
    score = score,
    topology = topology,
    ranked = ranked,
    clade_prob = clade_prob
  )
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
#' Node ages are the weighted mean or median (`heights`) age of each clade
#' over the trees in `in_set`; tip ages are the weighted mean. Only the
#' clades of tree `i` are summarised.
#'
#' @return A list with `tree` (ape::phylo, clade probabilities as node
#'   labels) and `nodes` (tibble: one row per internal node of `tree`).
#' @noRd
.summary_phylo <- function(parts, i, in_set, weight, labels, heights,
                           clade_prob) {
  target <- parts[[i]]
  n_tips <- target$n_tips
  set <- which(in_set & weight > 0)
  if (length(set) == 0) set <- which(in_set)

  # ages of the target's clades in every tree of the set that has them
  keys <- unlist(lapply(parts[set], `[[`, "clades"), use.names = FALSE)
  ages <- unlist(
    lapply(parts[set], function(p) p$age[-seq_len(p$n_tips)]),
    use.names = FALSE
  )
  w <- rep(weight[set], lengths(lapply(parts[set], `[[`, "clades")))
  keep <- keys %in% target$clades
  by_clade <- split(
    which(keep), factor(keys[keep], levels = target$clades)
  )
  stats <- vapply(
    by_clade, function(j) .age_stats(ages[j], w[j]), numeric(5)
  )

  tip_ages <- vapply(
    parts[set], function(p) p$age[seq_len(n_tips)], numeric(n_tips)
  )
  tip_ages <- if (is.matrix(tip_ages)) {
    drop(tip_ages %*% weight[set]) / sum(weight[set])
  } else {
    tip_ages
  }

  posterior <- as.vector(clade_prob[target$clades])
  node_age <- stats[if (heights == "mean") "mean" else "median", ]
  tree <- .parts_to_phylo(target, labels, c(tip_ages, unname(node_age)))
  tree$node.label <- as.character(signif(posterior, 3))

  nodes <- tibble::tibble(
    node = n_tips + seq_along(target$clades),
    posterior = posterior,
    age_mean = unname(stats["mean", ]),
    age_median = unname(stats["median", ]),
    age_lower = unname(stats["lower", ]),
    age_upper = unname(stats["upper", ]),
    n_trees = as.integer(stats["n", ])
  )
  list(tree = tree, nodes = nodes)
}

#' Weighted mean, median and HPD interval of a set of ages
#'
#' The HPD interval is the shortest interval of sorted ages whose weights sum
#' to at least `level`.
#' @return A named vector: `mean`, `median`, `lower`, `upper`, `n`.
#' @noRd
.age_stats <- function(x, w, level = 0.95) {
  o <- order(x)
  x <- x[o]
  w <- w[o]
  if (sum(w) <= 0) w <- rep(1, length(w))
  w <- w / sum(w)
  cw <- cumsum(w)
  tol <- 1e-12

  # for each start i, the first end j with weight(x[i..j]) >= level
  end <- findInterval(cw - w + level - tol, cw, left.open = TRUE) + 1L
  ok <- end <= length(x)
  start <- which(ok)
  best <- start[which.min(x[end[ok]] - x[start])]

  c(
    mean = sum(w * x),
    median = x[which(cw >= 0.5 - tol)[1]],
    lower = x[best],
    upper = x[end[best]],
    n = length(x)
  )
}

#' Warn when summarised node ages give negative branch lengths
#'
#' Can only happen with method = "MCC", where a parent and its child are
#' averaged over different sets of trees.
#' @noRd
.check_branch_lengths <- function(trees, call = parent.frame()) {
  n_neg <- vapply(trees, function(t) sum(t$edge.length < 0), numeric(1))
  if (sum(n_neg) == 0) {
    return(invisible())
  }
  n_trees <- sum(n_neg > 0)
  cli::cli_warn(c(
    "{sum(n_neg)} branch{?es} in {n_trees} summary tree{?s} {?has/have} a negative length.",
    "i" = paste(
      "A child clade came out older than its parent: the clades involved",
      "probably have low support (see {.field node_summary})."
    ),
    "i" = "Try {.code method = \"MAP\"} or more samples."
  ), call = call)
}

#' Collect the nodes and events of one sampled host-repertoire tree
#'
#' Walks the tree from the root, each node before its children. Values are
#' gathered in plain vectors and lists (no data frames per node), which keeps
#' the walk fast for large outputs.
#'
#' @return A list with, per node, `node` (label), `parent` (label), `age`,
#'   `is_tip` and `repertoire` (a list of state vectors); and, per event,
#'   `event` (the event's fields) and `event_node` (the node below the branch).
#' @noRd
.hrm_walk <- function(tree) {
  node <- integer(0)
  parent <- integer(0)
  age <- numeric(0)
  is_tip <- logical(0)
  repertoire <- list()
  event <- list()
  event_node <- integer(0)

  walk <- function(x, parent_label) {
    data <- x[["__data__"]]
    label <- as.integer(data$label)

    node <<- c(node, label)
    parent <<- c(parent, parent_label)
    age <<- c(age, data$age)
    is_tip <<- c(is_tip, is.null(data$left))
    repertoire[[length(repertoire) + 1]] <<- as.integer(unlist(data$repertoire))

    if (length(data$history) > 0) {
      event <<- c(event, lapply(data$history, `[[`, "__data__"))
      event_node <<- c(event_node, rep(label, length(data$history)))
    }

    if (!is.null(data$left)) {
      walk(data$left, label)
      walk(data$right, label)
    }
  }

  walk(tree, NA_integer_)
  list(
    node = node, parent = parent, age = age, is_tip = is_tip,
    repertoire = repertoire, event = event, event_node = event_node
  )
}

#' Bind the nodes or events of all samples into one tibble, with `sample`
#' @noRd
.hrm_bind <- function(walks, what) {
  if (what == "nodes") {
    reps <- lapply(walks, `[[`, "repertoire")
    n_hosts <- lapply(reps, lengths)
    return(tibble::as_tibble(list(
      sample = rep(seq_along(walks), vapply(n_hosts, sum, numeric(1))),
      node = unlist(Map(rep, lapply(walks, `[[`, "node"), n_hosts),
        use.names = FALSE
      ),
      host = unlist(lapply(unlist(n_hosts), seq_len), use.names = FALSE),
      state = unlist(reps, use.names = FALSE)
    )))
  }

  event <- unlist(lapply(walks, `[[`, "event"), recursive = FALSE)
  field <- function(name) {
    vapply(event, function(e) as.numeric(e[[name]]), numeric(1))
  }
  tibble::as_tibble(list(
    sample = rep(
      seq_along(walks), vapply(walks, function(w) length(w$event), integer(1))
    ),
    node = unlist(lapply(walks, `[[`, "event_node"), use.names = FALSE),
    host = as.integer(field("host")),
    from_state = as.integer(field("fromState")),
    to_state = as.integer(field("toState")),
    time = field("eventTime")
  ))
}

#' Names of the lambda rates, after the transitions they stand for
#'
#' Four rates: three states (0, 1, 2). Two rates: two states, `0` and `1`, or
#' `0` and `2` when that is how the repertoires are coded.
#' @noRd
.lambda_names <- function(n_lambda, states) {
  if (n_lambda == 4) {
    return(c("lambda_01", "lambda_10", "lambda_12", "lambda_21"))
  }
  if (n_lambda == 2) {
    if (2 %in% states && !1 %in% states) {
      return(c("lambda_02", "lambda_20"))
    }
    return(c("lambda_01", "lambda_10"))
  }
  paste0("lambda_", seq_len(n_lambda)) # unknown layout: number them
}

#' Build the ape::phylo symbiont tree from the walk of one sample
#'
#' Tips keep their labels as ape numbers (1 to n_tips); internal nodes are
#' numbered n_tips + 1, ... in the order walked (root first). The input labels
#' are kept as tip and node labels.
#' @noRd
.hrm_phylo <- function(walk) {
  label <- walk$node
  parent <- walk$parent
  age <- walk$age
  is_tip <- walk$is_tip

  n_tips <- sum(is_tip)
  ape_id <- integer(length(label))
  ape_id[is_tip] <- label[is_tip]
  ape_id[!is_tip] <- n_tips + seq_len(sum(!is_tip))

  to_ape <- function(x) ape_id[match(x, label)]
  has_parent <- !is.na(parent)
  edge <- cbind(to_ape(parent[has_parent]), ape_id[has_parent])
  age_by_id <- age[order(ape_id)]

  structure(
    list(
      edge = edge,
      edge.length = age_by_id[edge[, 1]] - age_by_id[edge[, 2]],
      tip.label = as.character(sort(label[is_tip])),
      Nnode = sum(!is_tip),
      node.label = as.character(label[!is_tip])
    ),
    class = "phylo",
    order = "cladewise"
  )
}
