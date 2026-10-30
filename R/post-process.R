# Exported Functions ----------------------------------------------------------------

#' Parse the output of a TreePPL SMC run
#'
#' @description
#' `tp_parse_smc()` reads the JSON output of a TreePPL run with a sequential
#' Monte Carlo method (e.g. `"smc-bpf"`, `"smc-apf"`) and returns the samples,
#' their weights and the normalising constant of each sweep as a tibble.
#'
#' @details
#' Each line of an SMC output file is one sweep, with the particles' `samples`,
#' their log `weights` and the sweep's log normalising constant (`normConst`).
#' TreePPL writes NaN and infinite values as `{"__float__": "-inf"}`; these are
#' read as R's `NaN`, `Inf` and `-Inf`.
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
#' Non-scalar values (vectors, trees) are kept as list-columns.
#'
#' @param json_path Path(s) to one or more JSON output files, as returned by
#'   [tp_run()]. Sweeps are numbered consecutively across files.
#' @param wide Logical. If `TRUE` (default), one row per particle and one
#'   column per parameter. If `FALSE`, one row per particle and parameter, with
#'   columns `parameter` and `sample`.
#'
#' @return A tibble with columns:
#'   * `sweep`: sweep number.
#'   * `particle`: particle number within its sweep.
#'   * `log_weight`: the particle's log weight.
#'   * `norm_constant`: the sweep's log normalising constant.
#'   * `norm_weight`: the normalised weight (see Details).
#'   * one column per parameter (`wide = TRUE`), or `parameter` and `sample`
#'     (`wide = FALSE`).
#'
#' @seealso [tp_run()], which calls this function for SMC runs, and
#'   [tp_parse_mcmc()] for MCMC output.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sampler <- tp_sampler(model = "crbd", method = "smc-bpf")
#' data <- tp_data(model = "crbd", dataList = list(tree = tree, rho = 0.5))
#' run <- tp_run(sampler, data, particles = 1000, sweeps = 10)
#'
#' # Parse an output file directly
#' samples <- tp_parse_smc("out.json")
#' samples_long <- tp_parse_smc("out.json", wide = FALSE)
#' }
tp_parse_smc <- function(json_path, wide = TRUE) {
  # One line = one sweep
  lines <- unlist(lapply(json_path, readLines, warn = FALSE))
  lines <- lines[nzchar(lines)]

  # TreePPL writes NaN/Inf as {"__float__": "-inf"}; turn them into strings
  # ("-inf", "inf", "nan") that as.numeric() understands, so every sweep
  # simplifies to plain vectors and matrices
  lines <- gsub(
    '\\{\\s*"__float__"\\s*:\\s*"([^"]*)"\\s*\\}', '"\\1"', lines, perl = TRUE
  )

  sweeps <- lapply(seq_along(lines), function(i) {
    sweep <- jsonlite::fromJSON(lines[i], simplifyVector = TRUE)
    norm_const <- suppressWarnings(as.numeric(sweep$normConst))

    # Skip sweeps with no samples or a NaN / -Inf normalising constant
    if (length(sweep$samples) == 0 || !is.finite(norm_const)) {
      return(NULL)
    }

    samples <- sweep$samples
    if (is.data.frame(samples)) {
      samples <- samples[["__data__"]] # named parameters
    } else {
      samples <- as.data.frame(samples) # unnamed: x1, x2, ...
      names(samples) <- paste0("x", seq_along(samples))
    }

    log_weight <- suppressWarnings(as.numeric(sweep$weights))
    if (length(log_weight) != nrow(samples)) {
      cli::cli_abort(
        "Sweep {i}: {nrow(samples)} samples but {length(log_weight)} weights."
      )
    }

    data.frame(
      sweep = i,
      particle = seq_along(log_weight),
      log_weight = log_weight,
      norm_constant = norm_const,
      samples
    )
  })

  n_kept <- sum(!vapply(sweeps, is.null, logical(1)))
  if (n_kept == 0) {
    cli::cli_abort(c(
      "No sweep returned usable samples: every particle had zero weight.",
      "i" = "Try more particles, or check the input data and priors."
    ))
  }
  if (n_kept < length(lines)) {
    n_skipped <- length(lines) - n_kept
    cli::cli_inform(c(
      "i" = "Skipped {n_skipped} of {length(lines)} sweep{?s} with no usable samples."
    ))
  }

  df <- do.call(rbind, sweeps)
  df <- df[is.finite(df$log_weight), , drop = FALSE]
  total <- df$log_weight + df$norm_constant
  df$norm_weight <- exp(total - max(total))

  id_cols <- c("sweep", "particle", "log_weight", "norm_constant", "norm_weight")
  params <- setdiff(names(df), id_cols)
  df <- tibble::as_tibble(df[c(id_cols, params)])

  if (!wide) {
    return(tidyr::pivot_longer(
      df, tidyr::all_of(params),
      names_to = "parameter", values_to = "sample"
    ))
  }
  df
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
#' Non-scalar values (vectors, trees) are kept as list-columns.
#'
#' Files without samples are skipped with a warning. If no file has samples,
#' the function stops with an error.
#'
#' @param json_path Path(s) to one or more JSON output files, as returned by
#'   [tp_run()]. Each file is one run.
#' @param wide Logical. If `TRUE` (default), one row per iteration and one
#'   column per parameter. If `FALSE`, one row per iteration and parameter,
#'   with columns `parameter` and `sample`.
#'
#' @return A tibble (tidy data frame) with columns:
#'   * `run`: run number, in the order of `json_path`.
#'   * `iteration`: iteration number within its run.
#'   * one column per parameter (`wide = TRUE`), or `parameter` and `sample`
#'     (`wide = FALSE`).
#'
#' @seealso [tp_run()], which calls this function for MCMC runs, and
#'   [tp_parse_smc()] for SMC output.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sampler <- tp_sampler(model = "crbd", method = "mcmc")
#' run <- tp_run(sampler, data, iterations = 1000, n_runs = 2)
#'
#' # Parse output files directly
#' samples <- tp_parse_mcmc(c("out1.json", "out2.json"))
#' samples_long <- tp_parse_mcmc("out1.json", wide = FALSE)
#'
#' # Incremental printing gives the same result
#' sampler <- tp_sampler(model = "crbd", method = "mcmc", incremental_printing = TRUE)
#' run <- tp_run(sampler, data, iterations = 1000)
#' }
tp_parse_mcmc <- function(json_path, wide = TRUE) {
  runs <- lapply(seq_along(json_path), function(run_id) {
    lines <- readLines(json_path[run_id], warn = FALSE)
    lines <- lines[nzchar(trimws(lines))]

    # TreePPL writes NaN/Inf as {"__float__": "nan"}; turn them into strings
    # that as.numeric() understands, so samples simplify to plain vectors
    lines <- gsub(
      '\\{\\s*"__float__"\\s*:\\s*"([^"]*)"\\s*\\}', '"\\1"', lines, perl = TRUE
    )

    # Two output formats:
    # * default: one summary line {"samples": [...], "weights": ..., ...}
    # * --incremental-printing: one sample per line, then an (empty) summary
    is_summary <- grepl('^\\s*\\{\\s*"samples"\\s*:', lines)
    sample_lines <- lines[!is_summary]

    samples <- if (length(sample_lines) > 0) {
      # Parse all sample lines in one call by wrapping them in a JSON array
      jsonlite::fromJSON(
        paste0("[", paste(sample_lines, collapse = ","), "]"),
        simplifyVector = TRUE
      )
    } else if (any(is_summary)) {
      jsonlite::fromJSON(lines[which(is_summary)[1]], simplifyVector = TRUE)$samples
    }

    if (length(samples) == 0) {
      cli::cli_warn("No samples found in {.file {json_path[run_id]}}; skipping it.")
      return(NULL)
    }

    if (is.data.frame(samples)) {
      samples <- samples[["__data__"]] # named parameters
    } else {
      samples <- as.data.frame(matrix(
        suppressWarnings(as.numeric(samples)),
        nrow = NROW(samples)
      ))
      names(samples) <- paste0("x", seq_along(samples))
    }

    data.frame(run = run_id, iteration = seq_len(nrow(samples)), samples)
  })

  runs <- runs[!vapply(runs, is.null, logical(1))]
  if (length(runs) == 0) {
    cli::cli_abort("No run returned any samples.")
  }

  df <- tibble::as_tibble(do.call(rbind, runs))

  if (!wide) {
    params <- setdiff(names(df), c("run", "iteration"))
    return(tidyr::pivot_longer(
      df, tidyr::all_of(params),
      names_to = "parameter", values_to = "sample"
    ))
  }
  df
}





#' Parse TreePPL json output for host repertoire model
#'
#' @description
#' `tp_parse_host_rep` takes TreePPL json output from inference with the
#' model of host repertoire evolution and returns a data.frame
#'
#' @param treeppl_out a character vector giving the TreePPL json output
#' produced by [tp_run].
#'
#' @return A list (n = sweeps) of data frames with the output from inference
#' in TreePPL under the host repertoire evolution model.
#' @export

tp_parse_host_rep <- function(treeppl_out) {
  result_list <- list()

  for (index in seq_along(treeppl_out)) {
    output_trppl <- treeppl_out[[index]]

    nbr_lam <- length(output_trppl[1][[1]][[1]][[1]]$lambda)
    nbr_col <- 14 + nbr_lam
    name_lam <- c()

    for (i in 1:nbr_lam) {
      name_lam <- c(name_lam, paste0("lambda", i))
    }

    result <- data.frame(matrix(ncol = nbr_col, nrow = 0))

    colnames(result) <- c(
      "iteration",
      "log_weight",
      "log_norm_const",
      "mu",
      "beta",
      name_lam,
      "node_index",
      "branch_start_time",
      "branch_end_time",
      "start_state",
      "end_state",
      "transition_time",
      "parent_index",
      "child1_index",
      "child2_index"
    )

    # for (i in seq_along(output_trppl[1][[1]])) {
    for (i in seq_along(output_trppl$samples)) {
      res <- data.frame(matrix(ncol = nbr_col, nrow = 0))
      colnames(res) <- c(
        "iteration",
        "log_weight",
        "log_norm_const",
        "mu",
        "beta",
        name_lam,
        "node_index",
        "branch_start_time",
        "branch_end_time",
        "start_state",
        "end_state",
        "transition_time",
        "parent_index",
        "child1_index",
        "child2_index"
      )

      tree <- output_trppl[1][[1]][[i]][[1]]$tree$`__data__`

      state <- paste(tree$repertoire, collapse = "")

      lambda <- output_trppl[1][[1]][[i]][[1]]$lambda

      names(lambda) <- name_lam

      res <- peel_tree(
        tree,
        i,
        pindex = NA,
        output_trppl$weights[i],
        output_trppl$normConst,
        output_trppl[1][[1]][[i]][[1]]$mu,
        output_trppl[1][[1]][[i]][[1]]$beta,
        lambda,
        prev_age = NA,
        state,
        res
      )
      result <- rbind(result, res)
    }
    result_list[[index]] <- result
  }
  return(result_list)
}

# Recursive function to go deep in the tree
peel_tree <- function(subtree,
                      index,
                      pindex,
                      lweight,
                      lnorm_const,
                      mu,
                      beta,
                      lambda,
                      prev_age,
                      start_state,
                      result) {
  base <- c(
    iteration = as.numeric(index - 1),
    log_weight = as.numeric(lweight),
    log_norm_const = as.numeric(lnorm_const),
    mu = as.numeric(mu),
    beta = as.numeric(beta),
    lambda,
    node_index = as.numeric(subtree$label - 1),
    branch_start_time = as.numeric(prev_age),
    branch_end_time = as.numeric(subtree$age),
    start_state = as.numeric(start_state),
    end_state = NA,
    transition_time = NA,
    parent_index = as.numeric(pindex),
    child1_index = NA,
    child2_index = NA
  )

  if (!is.null(subtree$left)) {
    base[["child1_index"]] <-
      as.numeric(subtree$left$`__data__`$label - 1)
    base[["child2_index"]] <-
      as.numeric(subtree$right$`__data__`$label - 1)
  }

  base[["end_state"]] <- base[["start_state"]]

  chang_nbr <- length(subtree$history)
  if (chang_nbr != 0) {
    df <- data.frame(matrix(ncol = 2, nrow = chang_nbr))
    for (i in 1:chang_nbr) {
      # "end_state"
      df[i, 1] <-
        as.numeric(paste(subtree$history[[i]]$`__data__`$repertoire,
          collapse = ""
        ))
      # "transition_time"
      df[i, 2] <- as.numeric(subtree$history[[i]]$`__data__`$age)
    }
    df <- df[order(-df$X2), ]
    for (j in 1:chang_nbr) {
      base[["start_state"]] <- base[["end_state"]]
      base[["end_state"]] <- df[j, 1]
      base[["transition_time"]] <- df[j, 2]
      result[nrow(result) + 1, ] <- base
    }
  } else {
    result[nrow(result) + 1, ] <- base
  }

  if (!is.null(subtree$left)) {
    result <- peel_tree(
      subtree$left$`__data__`,
      index,
      subtree$label - 1,
      lweight,
      lnorm_const,
      mu,
      beta,
      lambda,
      subtree$age,
      base[["end_state"]],
      result
    )
    result <- peel_tree(
      subtree$right$`__data__`,
      index,
      subtree$label - 1,
      lweight,
      lnorm_const,
      mu,
      beta,
      lambda,
      subtree$age,
      base[["end_state"]],
      result
    )
  }
  result
}


#' Check for convergence across multiple SMC sweeps.
#'
#' @param treeppl_out a data frame outputted by [tp_parse_smc()].
#'
#' @returns Variance in the normalizing constants across SMC sweeps.
#' @export
#'
tp_smc_convergence <- function(treeppl_out) {
  zs <- treeppl_out |>
    dplyr::slice_head(n = 1, by = .data$sweep) |>
    dplyr::pull(.data$norm_constant)

  return(stats::var(zs))
}


#' Assess MCMC convergence for a TreePPL analysis
#'
#' Computes per-parameter effective sample size (ESS) and, when multiple
#' runs are available, the upper limit of the Gelman-Rubin potential scale
#' reduction factor (R-hat), using the \pkg{coda} package.
#'
#' @param treeppl_out A tibble produced by \code{tp_parse_mcmc()}, in either long
#' or wide format.
#'
#' @details
#' Output produced from an unnamed return type in TreePPL (`.tppl` file) is not
#' supported and will raise an error, since there is no reliable way to
#' distinguish multiple parameters.
#'
#' @return A tibble with one row per parameter, containing:
#'   \describe{
#'     \item{parameter}{Parameter name.}
#'     \item{ess}{Effective sample size, pooled across all runs.}
#'     \item{rhat_upper}{Upper limit of the Gelman-Rubin R-hat statistic.
#'       Only computed when `treeppl_out` contains more than one run;
#'       otherwise `NA`, with a message explaining why.}
#'   }
#'
#' @examples
#' \dontrun{
#'
#' # CRBD model using MCMC
#' run_mcmc <- tp_run(
#' sampler = tp_compile(model = "crbd", method = "mcmc"),
#' data = tp_data(data_input = "crbd"),
#' iterations = 10,
#' n_runs = 2,
#' n_processes = 2
#' )
#'
#' # tp_run() already returns the output of tp_parse_mcmc(), so we can call
#' # tp_mcmc_convergence() directly:
#' tp_mcmc_convergence(run_mcmc)
#' }
#'
#' @export
tp_mcmc_convergence <- function(treeppl_out) {
  # if the input is in wide format
  if (!"sample" %in% colnames(treeppl_out)) {
    treeppl_out <- treeppl_out |>
      tidyr::pivot_longer(
        cols = -c("run", "iteration"),
        names_to = "parameter",
        values_to = "sample"
      )
  }

  # sanity check
  has_parameter <- "parameter" %in% names(treeppl_out)
  if (!has_parameter) {
    stop(
      "Output JSON has no parameter names.\n",
      "Re-run the TreePPL model with a named return type (.tppl file) ",
      "so that parameters can be identified."
    )
  }

  # coda::mcmc objects
  runs <- sort(unique(treeppl_out$run))
  parameters <- unique(treeppl_out$parameter)

  chains_by_parameter <- purrr::map(parameters, function(p) {
    chains <- purrr::map(runs, function(r) {
      vals <- treeppl_out |>
        dplyr::filter(.data$run == r, .data$parameter == p) |>
        dplyr::arrange(.data$iteration) |>
        dplyr::pull(.data$sample)
      coda::mcmc(vals)
    })
    coda::mcmc.list(chains)
  })
  names(chains_by_parameter) <- parameters
  # ESS
  ess <- purrr::map_dbl(chains_by_parameter, coda::effectiveSize)
  result <- tibble::tibble(parameter = names(ess), ess = ess)

  # Gelman and Rubin's R-hat
  if (length(runs) > 1) {
    rhat_upper <- purrr::map_dbl(chains_by_parameter, function(chain_list) {
      coda::gelman.diag(chain_list)$psrf[, "Upper C.I."]
    })
    result$rhat_upper <- rhat_upper[result$parameter]
  } else {
    result$rhat_upper <- NA
    message(
      "Only one run detected; Gelman-Rubin R-hat requires >= 2 runs and was not computed."
    )
  }

  if (!has_parameter) {
    result <- dplyr::select(result, -"parameter")
  }
  return(result)
}


#' Find the Maximum A Posteriori (MAP) Tree from weighted samples
#'
#' @param trees_out The list returned by [treepplr::tp_json_to_phylo]
#' (containing $trees and $weights)
#'
#' @returns The MAP tree as a phylo object
#' @export
#'
tp_map_tree <- function(trees_out) {
  trees <- trees_out$trees
  weights <- trees_out$weights

  # Handle Log-Weights (Optional Check)
  # If weights are negative (log-scale), convert them to probabilities first
  if (any(weights < 0)) {
    message("Log-weights detected. converting to relative probabilities...")
    # Subtract max to avoid underflow/overflow issues
    weights <- exp(weights - max(weights))
  }

  # Identify unique topologies
  trees_ready <- lapply(trees, function(tree) {
    # normalize edge lengths for the tip reordering
    tree$edge.length <- tree$edge.length / max(tree$edge.length)
    # Ladderize to fix edge indices
    tree_lad <- ladderize_tree(tree)
    # Order tip labels as similarly as possible
    tree_ord <- bnpsd::tree_reorder(tree_lad, sort(tree_lad$tip.label))
    # Remove edge lengths to only focus on topology
    tree_ord$edge.length <- NULL
    return(tree_ord)
  })

  # This compresses the list into unique tree topologies
  unique_topologies <- ape::unique.multiPhylo(trees_ready, use.edge.length = FALSE)

  # Map every original tree to a unique topology index
  # match_indices <- match(trees_ready, unique_topologies)
  match_indices <- attr(unique_topologies, "old.index")

  # Sum weights for each unique topology
  # tapply splits the weights by the index and sums them
  topology_probs <- tapply(weights, match_indices, sum)

  # Identify the Best Topology
  best_index <- as.numeric(names(which.max(topology_probs)))

  # Calculate posterior probability of this MAP topology
  map_prob <- max(topology_probs) / sum(topology_probs)

  # Compute Mean Branch Lengths for the MAP Topology
  # We take all samples that matched the MAP topology...
  matching_indices <- which(match_indices == best_index)
  matching_trees <- trees[matching_indices]
  matching_weights <- weights[matching_indices]

  # ...and compute a consensus to average their branch lengths.
  # Ideally, we should do a weighted average of the lengths,
  # but ape::consensus uses simple mean. For most purposes, this is sufficient.
  final_map <- map <- phangorn::allCompat(matching_trees, rooted = TRUE) |>
    phangorn::add_edge_length(matching_trees,
      fun = function(x) stats::weighted.mean(x, matching_weights)
    )

  print(paste("MAP Topology found"))
  print(paste("Posterior Probability:", round(map_prob, 4)))
  print(paste(
    "Based on the topology of", length(matching_indices),
    "samples out of", length(trees)
  ))

  return(final_map)
}
